import gymnasium as gym
import numpy as np
import pandas as pd
from pandas import DataFrame
from gymnasium.spaces import Discrete, Box, Space

from typing import List, Dict, Tuple, Any

# from RProcess import RProcess  # for mypy

class MCPModEnv(gym.Env):
    """Custom environment for the reinforcement leaning allocation"""
    
    doses: np.ndarray
    K: int
    dr_models: List[str]
    dr_models_weights: List[float]
    
    N_total: int
    N_ini: List[int]
    N_block: int
    outcome_type: str
    sd_normal: float
    
    r_process: RProcess
    
    true_response_dict: Dict[str, np.ndarray]
    
    action_space: Space
    observation_space: Space
    
    true_dr_model: str
    true_response: np.ndarray
    
    simulated_actions: List[int]
    simulated_responses: List[float] | List[int]

    def __init__(self, config: Dict[str, Any]) -> None:
        self.doses     = np.array(config["doses"])
        self.K         = config["K"]
        self.dr_models = config["dr_models_names"]
        self.dr_models_weights = config["dr_models_weights"]

        self.N_total      = config["N_total"]
        self.N_ini        = config["N_ini"]
        self.N_block      = config["N_block"]
        self.outcome_type = config["outcome_type"]
        self.sd_normal    = config["sd_normal"]

        self.r_process = RProcess(config["r_home"])
        self.r_process.execute(config["r_code_to_setup"])
        
        self.true_response_dict = self._get_true_response_dict()

        self.action_space      = Discrete(self.K)
        self.observation_space = self._create_observation_space()

    def _get_true_response_dict(self) -> Dict[str, np.ndarray]:
        """Retrieve the true responses for each DR model from the R process.
        
        Returns:
            Dict[str, np.ndarray]: A dictionary where the true responses are 
            stored for each DR model name as a key.
        """
        K: int = self.K
        dr_models: List[str] = self.dr_models
        
        responses = np.array(self.r_process.get_value("true_responses"))
        response_list = [responses[i:i+K] for i in range(0, len(responses), K)]
        response_dict = {dr_models[i]: resps for i, resps in enumerate(response_list)}
        return response_dict

    def _create_observation_space(self) -> Space:
        """Create an observation space.
        
        Here, the observation space is the space of possible values for
        the state s described in Section 2.3 of the original paper.
        
        Returns:
            gymnasium.spaces.Space: Observation space.
        """
        shifted_mean_low  = np.repeat(-np.inf, self.K - 1)
        shifted_mean_high = np.repeat( np.inf, self.K - 1)
        std_dev_low       = np.repeat(0.0, self.K)
        std_dev_high      = np.repeat(np.inf, self.K)
        ratio_low         = np.repeat(0.0, self.K)
        ratio_high        = np.repeat(1.0, self.K)

        observation_space: Space = Box(
            low=np.concatenate((shifted_mean_low, std_dev_low, ratio_low)),
            high=np.concatenate((shifted_mean_high, std_dev_high, ratio_high)),
            dtype=np.float32
        )
        return observation_space

    def reset(
        self, seed: int | None = None, 
        options: Dict[str, Any] | None = None
    ) -> Tuple[np.ndarray, Dict[str, Any]]:

        super().reset(seed=seed)

        self.true_dr_model = self.np_random.choice(self.dr_models, p=self.dr_models_weights)
        self.true_response = self.true_response_dict[self.true_dr_model]
        
        # Initialize simulation values
        self.simulated_actions = np.repeat(np.arange(self.K), self.N_ini).tolist()
        if self.outcome_type == "continuous":
            self.simulated_responses = self._generate_new_continuous_responses(self.simulated_actions)
        elif self.outcome_type == "binary":
            self.simulated_responses = self._generate_new_binary_responses(self.simulated_actions)
        
        state = self._compute_state()
        info = {}
        
        return state, info

    def _generate_new_continuous_responses(self, actions: List[int]) -> List[float]:
        """Simulate responses to the actions randomly.
        
        The responses are generated from a normal distribution with the
        specified variance 'self.sd_normal', where the mean is the value of 
        the true dose-response curve. For details, see Section 3.1 of the
        original paper.
        
        Returns:
            List[float]: Responses to the actions.
        """
        return self.np_random.normal(
            self.true_response[actions], self.sd_normal).tolist()

    def _expit(self, x: np.ndarray) -> np.ndarray:
        """Stably compute the expit values corresponding to each element of x.
        
        Returns:
            np.ndarray: probabilities
        """
        x0 = np.vstack([x, np.zeros(len(x))])
        exp_x = np.exp(x0 - np.max(x0, axis=0))
        return exp_x[0, :] / np.sum(exp_x, axis=0)
    
    def _generate_new_binary_responses(self, actions: List[int]) -> List[int]:
        """Simulate responses to the actions randomly.
        
        The responses are generated from a Bernoulli distribution, 
        where the probability is the inv_logit value of the true dose-response curve.
        
        Returns:
            List[int]: Responses to the actions.
        """
        return self.np_random.binomial(
            1, self._expit(self.true_response[actions]), size=len(actions)).tolist()
            
    def _compute_state(self) -> np.ndarray:
        """Calculate state s from simulated values.
        
        Here, the state s is described in Section 2.3 of the original paper.
        
        Returns:
            The specific value of the state s.
        """
        df = DataFrame({"action": self.simulated_actions, 
                        "response": self.simulated_responses})
        df_grouped = df.groupby("action")

        mean_response = df_grouped.mean()["response"].values
        shifted_mean_response = mean_response[1:] - mean_response[0]
        std_dev_response = df_grouped.std(ddof=0)["response"].values
        count_per_action = df_grouped.size().values
        ratio_per_action = count_per_action / self.N_total
        
        state = np.concatenate(
            (shifted_mean_response, std_dev_response, ratio_per_action))
        
        return state.astype(np.float32)

    def step(self, 
        action: int
    ) -> Tuple[np.ndarray, float, bool, bool, Dict[str, Any]]:
        
        new_actions = [action] * self.N_block

        if self.outcome_type == "continuous":
            new_responses = self._generate_new_continuous_responses(new_actions)
        elif self.outcome_type == "binary":
            new_responses = self._generate_new_binary_responses(new_actions)

        # Update simulation values
        self.simulated_actions   += new_actions
        self.simulated_responses += new_responses

        simulation_size = len(self.simulated_actions)

        reward: float
        terminated: bool
        if simulation_size >= self.N_total:
            # For sending to R
            true_model_name = RProcess.to_R_notation(self.true_dr_model)
            simulated_dose = RProcess.to_R_notation(
                self.doses[self.simulated_actions].tolist())
            simulated_response = RProcess.to_R_notation(self.simulated_responses)
            
            self.r_process.execute(f"""
                reward <- compute_reward({true_model_name},
                                         {simulated_dose},
                                         {simulated_response})
            """)
            reward = self.r_process.get_value("reward")[0]
            terminated = True
        else:
            reward = 0
            terminated = False

        state = self._compute_state()
        truncated = False
        info = {}
        
        return state, reward, terminated, truncated, info
