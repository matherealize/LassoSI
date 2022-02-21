# Code to produce data from realistic simulation scenario ######################
# 
# Author: Michael Kammer
# Date: 21-02-2022

library(simdata)

# Binder simulationsetup #######################################################
# - Based on Binder H, Sauerbrei W, Royston P. Multivariable model-building with 
#   continuous covariates: 1. Performance measures and simulation. 
#   Germany: University of Freiburg; 2011
# - Uses a slightly modified correlation matrix to achieve stronger correlations
# - The multiple correlations of the final variables are
#   v1   v2   v3   v4   v5   v6   v7   v8   v9  v10  v11  v12  v13  v14  v15  v16  v17 
# 0.63 0.58 0.52 0.44 0.35 0.68 0.66 0.65 0.43 0.46 0.47 0.01 0.71 0.63 0.01 0.61 0.01 
# - Thus one could identify 3 kinds of variables: those with low dependence
#   on the rest (v12, v15, v17), those with medium dependence 
#   (v3, v4, v5, v9, v10, v11) and those with strong dependence 
#   (v1, v2, v6, v7, v8, v13, v16)

# Simulation design ############################################################
# initial correlation matrix
relations = cor_from_upper(
    15, 
    rbind(c(1,2,0.8), c(1,9,0.5), 
          c(3,5,0.5), c(3,9,-0.8), 
          c(4,6,-0.8), c(4,7,-0.5),
          c(5,6,-0.5), c(5,12,0.8),
          c(6,7,0.8), c(6,11,0.8), c(6,14,0.5),
          c(7,11,0.5), c(7,14,0.5),
          c(8,9,-0.5), c(8,11,0.5),
          c(11,14,0.8))
)
design_structure = list(
    simdesign_mvtnorm(
        relations = relations,
        # transformations for initial data
        transform_initial = function_list(
            v1 = function(z) floor(10 * z[,1] + 55), 
            v2 = function(z) z[,2] < 0.6, 
            v3 = function(z) exp(0.4 * z[,3] + 3),
            v4 = function(z) z[,4] >= -1.2,
            v5 = function(z) z[,4] >= 0.75,
            v6 = function(z) exp(0.5 * z[,5] + 1.5),
            v7 = function(z) floor(pmax(0, 100 * exp(z[,6]) - 20)),
            v8 = function(z) floor(pmax(0, 80 * exp(z[,7]) - 20)),
            v9 = function(z) z[,8] < -0.35,
            v10 = function(z) (z[,9] >= 0.5) * (z[,9] < 1.5),
            v11 = function(z) z[,9] >= 1.5,
            v12 = function(z) 0.01*floor(100 * (z[,10] + 4)^2),
            v13 = function(z) floor(10 * z[,11] + 55),
            v14 = function(z) floor(10 * z[,12] + 55),
            v15 = function(z) floor(10 * z[,13] + 55),
            v16 = function(z) z[,14] < 0,
            v17 = function(z) z[,15] < 0),
        # processing of final data to prevent extreme outliers
        process_final = list(
            process_truncate_by_iqr = list(
                truncate_multipliers = 
                    c(5, NA, 5, NA, NA, 
                      5, 5, 5, NA, NA, 
                      NA, 5, 5, 5, 5, 
                      NA, NA)
            )
        )
    )
)

# Coefficient vectors for outcome ##############################################
# - Different scenarios use different coefficient vectors
# - These are based on clusters of variables
# - Clusters were defined by dependence structure: 
#   1) 3 totally independent vars (v12, v15, v17) - simple scenario
#   2) 3 far apart (v2, v4, v14) - quasi independent, but high dependence to 
#       other variables
#   3) 3 highly positively correlated (v7, v8, v13), otherwise mostly negative 
#       correlation
#   4) 6 clustered variables, positive and negative correlations 
#       (v7, v8, v13, v3, v4, v5, v16)
# - The remaining variables are "bridges" or intermediates between these
#   extremes and less interesting in terms of coefficient structures
#   eg. v3, v9, v10, v11 are similar to v3

# coefficient vectors
variable_names = design_structure[[1]]$names_final
c0 = rep(0, length(variable_names))
names(c0) = variable_names

coef_structure = list()

# 1) cluster 2: 3 quasi independent variables
coef_structure$c2 = c0
coef_structure$c2[c("v2", "v4", "v14")] = 1

# 2) cluster 3: 3 highly positively correlated variables
coef_structure$c3 = c0
coef_structure$c3[c("v7", "v8", "v13")] = 1

# 3) cluster 4: 6 highly correlated variables
coef_structure$c34 = c0
coef_structure$c34[c("v7", "v8", "v13", "v4", "v5", "v16")] = 1
# 4) Weaker effects for one block
coef_structure$c3w4 = coef_structure$c34
coef_structure$c3w4[c("v7", "v8", "v13")] = 0.1  
# 5)
coef_structure$c34w = coef_structure$c34
coef_structure$c34w[c("v4", "v5", "v16")] = 0.1
# 6) negative effects
coef_structure$c3neg4 = coef_structure$c34
coef_structure$c3neg4[c("v7", "v8", "v13")] = -1
# 7)
coef_structure$c34neg = coef_structure$c34
coef_structure$c34neg[c("v7", "v8", "v13")] = -1

# 8) cluster 2 + 3 combined
coef_structure$c23 = c0
coef_structure$c23[c("v2", "v4", "v14", "v7", "v8", "v13")] = 1
# 9) weaker effects for one block
coef_structure$c2w3 = coef_structure$c23
coef_structure$c2w3[c("v2", "v4", "v14")] = 0.1 
# 10) 
coef_structure$c23w = coef_structure$c23
coef_structure$c23w[c("v7", "v8", "v13")] = 0.1
# 11) negative effects
coef_structure$c2neg3 = coef_structure$c23
coef_structure$c2neg3[c("v7", "v8", "v13")] = -1
# 12)
coef_structure$c23neg = coef_structure$c23
coef_structure$c23neg[c("v7", "v8", "v13")] = -1

# 13) cluster 2 + 4 combined
coef_structure$c234 = c0
coef_structure$c234[c("v2", "v4", "v14", "v7", "v8", "v13", "v5", "v16")] = 1

# Data and outcome generation ##################################################
# set coefficients by indexing into coefficient structure
coef = 1
# set number of observations
n_obs = 100

# simulate data
set.seed(1)
X = simulate_data(design_structure[[1]], n_obs)

# compute linear predictor (no noise, data is assumed to be standardized)
y = scale(as.matrix(X)) %*% coef_structure[[coef]]
