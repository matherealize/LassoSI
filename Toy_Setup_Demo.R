# Code to produce data from toy simulation scenario ############################
# 
# Author: Michael Kammer
# Date: 21-02-2022

library(Matrix)
library(simdata)

# Helper functions #############################################################
#' @title Correlation matrix with constant off-diagonal elements
#' 
#' @param p
#' Number of variables.
#' @param off_diag
#' Values for off diagonal elements.
#' 
#' @return 
#' Correlation matrix with diagonal elements set to 1.
create_cor_constant <- function(p, off_diag = 0) {
    cmat = matrix(off_diag, nrow = p, ncol = p)
    diag(cmat) = 1
    cmat
}

#' @title Block correlation matrix
#' 
#' @param p
#' Number of variables.
#' @param off_diag
#' Values for blocks.
#' 
#' @return 
#' Correlation matrix with diagonal elements set to 1.
create_cor_block <- function(p, off_diag = rep(0, length(p))) {
    cmat = list()
    for (i in 1:length(p)) {
        cmat[[i]] = matrix(off_diag[i], nrow = p[i], ncol = p[i])
    }
    cmat = bdiag(cmat)
    diag(cmat) = 1
    as.matrix(cmat)
}

# Toy simulationsetup ##########################################################
# - Simple data with 4 variables
# - Explores many possible correlation structures for 4 variables
# - Explores many different effect structurs for 4 variables

# Correlation structures 
design_structure = list(
    uncorrelated = simdesign_mvtnorm(
        relations = create_cor_constant(4)
    ), 
    correlated = simdesign_mvtnorm(
        relations = create_cor_constant(4, 0.8)
    ), 
    correlated_neg = simdesign_mvtnorm(
        relations = create_cor_constant(4, -0.3)
    ), 
    blocks_2_2 = simdesign_mvtnorm(
        relations = create_cor_block(c(2, 2), c(0.8, 0.8))
    ), 
    blocks_2_2_neg = simdesign_mvtnorm(
        relations = create_cor_block(c(2, 2), c(0.8, -0.8))
    ), 
    blocks_1_3 = simdesign_mvtnorm(
        relations = create_cor_block(c(1, 3), c(0, 0.8))
    ),
    blocks_1_3_neg = simdesign_mvtnorm(
        relations = create_cor_block(c(1, 3), c(0, -0.45))
    )
)

# Coefficient vectors for outcome ############################################## 
coef_structure = list(
    "v1" = c(1, 0, 0, 0),
    "v12" = c(1, 1, 0, 0),
    "v1234" = c(1, 1, 1, 1),
    "v3" = c(0, 0, 1, 0),
    "v34" = c(0, 0, 1, 1),
    "v13" = c(1, 0, 1, 0),
    "v12_dec" = c(1, 0.1, 0, 0),
    "v34_dec" = c(0, 0, 1, 0.1),
    "v13_dec" = c(1, 0, 0.1, 0),
    "v13_inc" = c(0.1, 0, 1, 0)
)

# Data and outcome generation ##################################################
# set design from which data should be simulated
design = 1
# set coefficients
coef = 1
# set number of observations
n_obs = 100

# simulate data
set.seed(1)
X = simulate_data(design_structure[[design]], n_obs)

# compute linear predictor (no noise, data is assumed to be standardized)
y = scale(X) %*% coef_structure[[coef]]
