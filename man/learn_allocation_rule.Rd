% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/learn_allocation_rule.R
\name{learn_allocation_rule}
\alias{learn_allocation_rule}
\title{Build an Optimal Adaptive Allocation Rule using Reinforcement Learning}
\usage{
learn_allocation_rule(
  models,
  N_total,
  N_ini,
  N_block,
  Delta,
  sd_normal,
  optimization_metric = c("MAE", "TD", "power", "MS"),
  rl_models = models,
  rl_models_prior = NULL,
  rl_seed = NULL,
  rl_config = rl_config(),
  alpha = 0.025,
  selModel = c("AIC", "maxT", "aveAIC"),
  Delta_range = c(0.9, 1.1) * Delta,
  output_dir = format(Sys.time(), "\%Y\%m\%d_\%H\%M\%S"),
  output_base_dir = "allocation_rules",
  checkpoint_dir = "checkpoints"
)
}
\arguments{
\item{models}{An object of class \link[DoseFinding]{Mods} specifying assumed
dose-response models.}

\item{N_total}{A positive integer value. The total number of subjects.}

\item{N_ini}{A positive integer vector in which each element is greater than
or equal to 2. The number of subjects initially assigned to each dose.}

\item{N_block}{A positive integer value. The number of subjects allocated
adaptively in each round.}

\item{Delta}{A positive numeric value. The clinically relevant target effect.
See \link[DoseFinding]{TD} for details.}

\item{sd_normal}{A positive numeric value. The standard deviation of the
observation noise.}

\item{optimization_metric}{A character value specifying the metric to
optimize. Possible values are "MAE" (default), "TD", "power", or "MS".
See Section 2.2 of the original paper for details.}

\item{rl_models}{An object of class \link[DoseFinding]{Mods}. True dose-response
models in simulations for reinforcement learning. The default is the
same as the 'models' argument. Empirically, employing a wide variety of
models tends to improve performance.}

\item{rl_models_prior}{A positive numeric vector. The probability or weight
with which each model in rl_models is selected as the true model in
the simulation. The default is NULL, which specifies equal probability
for each model.}

\item{rl_seed}{An integer value. Random seed for reinforcement learning.}

\item{rl_config}{A list. Other settings for reinforcement learning. See
\link{rl_config} for details.}

\item{alpha}{A positive numeric value. The significance level. Default is 0.025.}

\item{selModel}{A character value specifying the model selection criterion
for dose estimation. Possible values are "AIC" (default), "maxT", or
"aveAIC". See \link[DoseFinding]{MCPMod} for details.}

\item{Delta_range}{A numeric vector of length 2. The lower and upper bounds
of Delta where the estimated target dose is correct. Default is
\code{c(0.9, 1.1) * Delta}.}

\item{output_dir}{A character value. Directory name or path to store the
built allocation rule. Default is the current datetime.}

\item{output_base_dir}{A character value. Parent directory path where the
built allocation rule will be stored. Valid only if 'output_dir' does
not contain '/'. Default is "allocation_rules".}

\item{checkpoint_dir}{A character value. Parent directory path to save
checkpoints. It enables you to resume learning from that point onwards.
Default is "checkpoints".}
}
\value{
An \link{AllocationRule} object.
}
\description{
Build an Optimal Adaptive Allocation Rule using Reinforcement Learning
}
