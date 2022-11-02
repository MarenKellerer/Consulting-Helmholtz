library(pscl)
library(tidyverse)
library(countreg)
library(mice)

set.seed(09101999)

data <- readRDS("./Data/data_clean.RDS")

# 1. Model with complete cases --------------------------------------------

data_na_omit <- na.omit(data[, c("impact_factor", "references_count",
                                 "citation_count", "h_index",
                                 "oa_status", "journal_subject",
                                 "time_since_pub")])

                                    
# hurdle model with poisson distribution with interaction term:
# impact_factor * h_index
hurdle_poi_complete_cases <- hurdle(citation_count ~ oa_status
                                    + impact_factor * h_index + journal_subject
                                    + references_count + 
                                      offset(log(time_since_pub))
                                    | oa_status
                                    + impact_factor * h_index + journal_subject
                                    + references_count + time_since_pub, 
                                    dist = "poisson", data = data)

# hurdle model with negative binomial distribution with interaction term:
# impact_factor*h_index
hurdle_negbin_complete_cases <- hurdle(citation_count ~ oa_status
                                       + impact_factor * h_index + journal_subject
                                       + references_count + 
                                         offset(log(time_since_pub))
                                       | oa_status
                                       + impact_factor * h_index + journal_subject
                                       + references_count + time_since_pub,
                                       dist = "negbin", data = data)


# 2. Model with mice Imputation -------------------------------------------

# imputated data by the package mice

imp <- readRDS("./Data/imp.RDS")

hurdle_negbin_imputation <- with(imp, hurdle(citation_count ~ oa_status
                                             + impact_factor * h_index + 
                                               journal_subject
                                             + references_count + 
                                               offset(log(time_since_pub))
                                             | oa_status
                                             + impact_factor * h_index + 
                                               journal_subject
                                             + references_count + time_since_pub, 
                                 dist = "negbin"))

# pool is not supported for hurdle -> write own code

## count_model

# use summary of first imp to get rownames and n of params
summary_count_1 <- summary(
  hurdle_negbin_imputation$analyses[[1]])$coefficients$count
nrow_param_count <- nrow(summary_count_1)

# initialize empty vectors to store results
pool_coef_count <- c()
pool_var_count <- c()
for (i in 1:nrow_param_count) { # go through all params
  # initialize empty vectors to store results
  coefficient <- c()
  variable <- c()
  for (j in 1:length(hurdle_negbin_imputation$analyses)) { # go through all imp
    # get coefficient and se
    coef <- summary(hurdle_negbin_imputation$analyses[[j]])$coefficients$count[[i]]
    std_error <- summary(hurdle_negbin_imputation$analyses[[j]])$coefficients$count[i, 2]
    variance <- std_error * std_error # calculate variance
    # save in vectors
    coefficient <- c(coefficient, coef)
    variable <- c(variable, variance)
  }
  # pool and save the results
  pool <- pool.scalar(coefficient, variable, hurdle_negbin_imputation$analyses[[1]]$n)
  pool_coef_count <- c(pool_coef_count, pool$qbar)
  pool_var_count <- c(pool_var_count, pool$ubar)
}

# give right names
names(pool_coef_count) <- rownames(summary_count_1)
names(pool_var_count) <- rownames(summary_count_1)

## zero_model

# use summary of first imp to get rownames and n of params
summary_zero_1 <- summary(
    hurdle_negbin_imputation$analyses[[1]])$coefficients$zero
n_param_zero <- nrow(summary_zero_1)

# initialize empty vectors to store results
pool_coef_zero <- c()
pool_var_zero <- c()
for (i in 1:n_param_zero) {# go through all params
  # initialize empty vectors to store results
  coefficient <- c()
  varariable <- c()
  for (j in 1:length(hurdle_negbin_imputation$analyses)) {# go through all imp
    # get coefficient and se
    coef <- summary(hurdle_negbin_imputation$analyses[[j]])$coefficients$zero[[i]]
    std_error <- summary(hurdle_negbin_imputation$analyses[[j]])$coefficients$zero[i,2]
    variance <- std_error * std_error # calculate variance
    # save in vectors
    coef <- c(coefficient, coef)
    variable <- c(variable, variance)
  }
  # pool and save the results
  pool <- pool.scalar(coefficient, variable, hurdle_negbin_imputation$analyses[[1]]$n)
  pool_coef_zero <- c(pool_coef_zero, pool$qbar)
  pool_var_zero <- c(pool_var_zero, pool$ubar)
}

# give right names
names(pool_coef_zero) <- rownames(summary_zero_1)
names(pool_var_zero) <- rownames(summary_zero_1)


# 3. Diagnostic -----------------------------------------------------------

# the rootograms are plotted in the file "plotting.R" and can be found in the
# folder Plots