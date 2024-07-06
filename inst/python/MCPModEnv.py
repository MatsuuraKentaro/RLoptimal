import gymnasium as gym
import numpy as np
import pandas as pd
from pandas import DataFrame
from pandas.core.groupby import DataFrameGroupBy
from gymnasium.spaces import Discrete, Box, Space

from inst.python.RProcess import RProcess

from typing import List, Dict, Tuple, Any

class MCPModEnv(gym.Env):
    """Custom environment for the reinforcement leaning allocation"""
    
    doses: np.ndarray
    K: int
    dr_models: List[str]
    dr_models_weights: List[float]
    
    N_total: int
    N_ini: List[int]
    N_block: int
    std_dev: float
    
    optimization_metric: str
    
    r_process: RProcess
    
    true_response_dict: Dict[str, np.ndarray]
    
    action_space: Space
    observation_space: Space
    
    true_dr_model: str
    true_response: np.ndarray
    
    simulated_actions: np.ndarray
    simulated_responses: np.ndarray

    def __init__(self, config: Dict[str, Any]) -> None:
        self.doses     = np.array(config["doses"])
        self.K         = config["K"]
        self.dr_models = config["dr_models_names"]
        self.dr_models_weights = config["dr_models_weights"]

        self.N_total = config["N_total"]
        self.N_ini   = config["N_ini"]
        self.N_block = config["N_block"]
        self.std_dev = config["std_dev"]

        self.optimization_metric = config["optimization_metric"]
        
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
        
        responses: np.ndarray = np.array(self.r_process.get_value("true_responses"))
        response_list: List[np.ndarray] = \
            [responses[i:i+K] for i in range(0, len(responses), K)]
        response_dict: Dict[str, np.ndarray] = \
            {dr_models[i]: resps for i, resps in enumerate(response_list)}
        return response_dict

    def _create_observation_space(self) -> Space:
        """Create an observation space.
        
        Here, the observation space is the space of possible values for
        the state s described in Section 2.3 of the original paper.
        
        Returns:
            gymnasium.spaces.Space: Observation space.
        """
        shifted_mean_low : np.ndarray = np.repeat(-np.inf, self.K - 1)
        shifted_mean_high: np.ndarray = np.repeat( np.inf, self.K - 1)
        std_dev_low : np.ndarray = np.repeat(0.0, self.K)
        std_dev_high: np.ndarray = np.repeat(np.inf, self.K)
        ratio_low : np.ndarray = np.repeat(0.0, self.K)
        ratio_high: np.ndarray = np.repeat(1.0, self.K)

        observation_space: Space = Box(
            low=np.concatenate((shifted_mean_low, std_dev_low, ratio_low)),
            high=np.concatenate((shifted_mean_high, std_dev_high, ratio_high)),
            dtype=np.float32
        )
        return observation_space

    def reset(self, seed=None, options=None) -> Tuple[np.ndarray, Dict[str, Any]]:
        super().reset(seed=seed)

        self.true_dr_model = self.np_random.choice(self.dr_models, p=self.dr_models_weights)
        self.true_response = self.true_response_dict[self.true_dr_model]
        
        # Initialize simulation values
        self.simulated_actions = np.repeat(np.arange(self.K), self.N_ini).tolist()
        self.simulated_responses = self._generate_new_responses(self.simulated_actions)
        
        state: np.ndarray = self._compute_state()
        info: Dict = {}
        
        return state, info

    def _generate_new_responses(self, actions: np.ndarray) -> np.ndarray:
        """Simulate responses to the actions randomly.
        
        The responses are generated from a normal distribution with the
        specified variance 'self.std_dev', where the mean is the value of 
        the true dose-response curve. For details, see Section 3.1 of the
        original paper.
        
        Returns:
            np.ndarray: Responses to the actions.
        """
        return self.np_random.normal(
            self.true_response[actions], self.std_dev).tolist()
            
    def _compute_state(self) -> np.ndarray:
        """Calculate state s from simulated values.
        
        Here, the state s is described in Section 2.3 of the original paper.
        
        Returns:
            The specific value of the state s.
        """
        actions = self.simulated_actions
        responses = self.simulated_responses
        N_total = self.N_total
        return MCPModEnv.compute_state(actions, responses, N_total)

    @staticmethod
    def compute_state(actions, responses, N_total) -> np.ndarray:
        """Calculate state s from simulated values.
        
        Here, the state s is described in Section 2.3 of the original paper.
        
        Returns:
            The specific value of the state s.
        """
        df: DataFrame = DataFrame({"action": actions, "response": responses})
        df_grouped: DataFrameGroupBy = df.groupby("action")

        mean_response: np.ndarray = np.array(df_grouped.mean()["response"].values)
        shifted_mean_response: np.ndarray = mean_response[1:] - mean_response[0]
        std_dev_response: np.ndarray = np.array(df_grouped.std(ddof=0)["response"].values)
        count_per_action: np.ndarray = np.array(df["action"].value_counts().values)
        ratio_per_action: np.ndarray = count_per_action / N_total
        
        state: np.ndarray = np.concatenate(
            (shifted_mean_response, std_dev_response, ratio_per_action))
        
        return state

    def step(self, action, action_array=False):
        # TODO
        if action_array:
            raise ValueError("action_array is expected to be FALSE.")
        
        new_actions   = [action] * self.N_block
        new_responses = self._generate_new_responses(new_actions)

        # Update simulation values
        self.simulated_actions   += new_actions
        self.simulated_responses += new_responses
        
        simulation_size = len(self.simulated_actions)

        if simulation_size >= self.N_total:
            # For sending to R
            true_dr_model_name = RProcess.to_R_notation(self.true_dr_model)
            simulated_dose = self.doses[self.simulated_actions].tolist()
            simulated_dose = RProcess.to_R_notation(simulated_dose)
            simulated_response = RProcess.to_R_notation(self.simulated_responses)
            
            self.r_process.execute(f"""
                scores <- compute_scores({true_dr_model_name}, 
                                         {simulated_dose},
                                         {simulated_response})
            """)
            scores = self.r_process.get_value("scores", type = "str")
            selmod = scores.pop(1)
            med = scores.pop(1)  # keep str because it is possibly 'NA'
            pval, score_power, score_MS, score_TD, score_MAE = [float(val) for val in scores]
            info = {"pval": pval, "selmod": selmod, "med": med, 
                    "score_power": score_power, "score_MS": score_MS, 
                    "score_TD": score_TD, "score_MAE": score_MAE}
            optimization_metric = "score_" + self.optimization_metric
            if optimization_metric not in info: 
                raise ValueError(f"Metric '{self.optimization_metric}' must be one of power, MS, TD, or MAE.")

            reward = info[optimization_metric]
            terminated = True            
        else:
            reward = 0
            terminated = False
            info = {}

        state: np.ndarray = self._compute_state()
        truncated = False
        info.update(**{"dose": self.simulated_actions, "resp": self.simulated_responses})
        
        return state, reward, terminated, truncated, info
