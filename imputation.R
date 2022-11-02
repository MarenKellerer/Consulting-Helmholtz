library(tidyverse)
library(mice)
library(gridExtra) # to save table
library(foreach) # loop to evaluate

set.seed(09101999)

source("./imputation_functions.R")

# Get data set -----------------------------------------------------------

# load data

mice_data <- readRDS("./Data/data_clean.RDS")

# only variables from the dag needed for imputation
mice_data <- mice_data[c("h_index", "impact_factor", "gender", "length",
                         "citation_count", "journal_subject",
                         "oa_status", "references_count", "time_since_pub")]

# delete obs from the full dataset
deleted_obs <- delete_obs(mice_data, 
                          var_with_missings = c("h_index", "impact_factor", 
                                                "length", "gender",
                                                "journal_subject"),
                          percentage = c(0.03, 0.06, 0.4, 0.21, 0.01))

data_missings <- deleted_obs$data_missings
rows <- deleted_obs$rows
data_no_NA <- deleted_obs$data_no_NA


# Imputation --------------------------------------------------------------

imp <- mice(mice_data, method = "cart", print = F)
saveRDS(imp, "./Data/imp.RDS") # save


# Get diagnostics and compare diff methods --------------------------------

# get diagnostic plots for cart

mice_with_diagnostic(data_missings, data_no_NA, rows,
                     var_continuous = c("h_index", "impact_factor", "length"),
                     maxit = 5, seed = 500, meth = 'cart', print = F)

densityplot(imp)
stripplot(imp)

# check for convergence
imp_test <- mice(mice_data, meth = "cart", print = F)
plot(imp_test)
imp40_test <- mice.mids(imp_test, maxit = 35, print = F)
plot(imp40_test)

# relevant variables with missings
var <- c("h_index", "impact_factor", "length", "gender", "journal_subject")

eval1 <- get_evaluation(mice(data_missings, method = "pmm", print = FALSE))
eval2 <- get_evaluation(mice(data_missings, method = "midastouch",
                             print = FALSE, maxit = 0)) # problem with midastouch
# https://giters.com/amices/mice/issues/410?amp=1
eval3 <- get_evaluation(mice(data_missings, method = "rf", print = FALSE))
eval4 <- get_evaluation(mice(data_missings, method = "cart", print = FALSE))
eval5 <- get_evaluation(mice(data_missings, 
                             defaultMethod = c("pmm", "logreg", "polyreg", 
                                               "polr"), print = FALSE))
eval6 <- get_evaluation(mice(data_missings, 
                             defaultMethod = c("pmm", "logreg.boot", "polyreg",
                                               "polr"), print = FALSE))
eval7 <- get_evaluation(mice(data_missings, 
  defaultMethod = c("midastouch", "logreg", "polyreg", "polr"), print = FALSE))
eval8 <- get_evaluation(mice(data_missings, 
  defaultMethod = c("midastouch", "logreg.boot", "polyreg", "polr"), 
  print = FALSE))
eval9 <- get_evaluation(mice(data_missings, 
                             defaultMethod = c("rf", "logreg", "polyreg",
                                               "polr"), print = FALSE))
eval10 <- get_evaluation(mice(data_missings, 
                             defaultMethod = c("rf", "logreg.boot", "polyreg",
                                               "polr"), print = FALSE))
eval11 <- get_evaluation(mice(data_missings, 
                             defaultMethod = c("cart", "logreg", "polyreg", 
                                               "polr"), print = FALSE))
eval12 <- get_evaluation(mice(data_missings, 
                             defaultMethod = c("cart", "logreg.boot", "polyreg",
                                               "polr"), print = FALSE))

complete_evaluation <- rbind(eval1, eval2, eval3, eval4, eval5, eval6, eval7, 
                             eval8, eval9, eval10, eval11, eval12)

# make one table
complete_evaluation <- mutate(as.data.frame(complete_evaluation),
               h_index = format(round(h_index, 2), nsmall = 2),
               impact_factor = format(round(impact_factor, 2), nsmall = 2),
               length = format(round(length, 2), nsmall = 2),
               gender = paste(as.character(format(round(gender, 2), 
                                                  nsmall = 2)), "%"),
               journal_subject = paste(as.character(format(round(
                 journal_subject, 2), nsmall = 2)), "%"))

rownames(complete_evaluation) <- c()
colnames(complete_evaluation) <- c("Correlation\n h-index", 
                                   "Correlation\n impact factor",
                                   "Correlation\n length",
                                   "Percentage\n gender", "Percentage\n journal subject")
complete_evaluation <- cbind("method" = c("pmm", "midastouch", "rf", "cart",
                                           "pmm, logreg, polyreg, polr",
                                           "pmm, logreg.boot, polyreg, polr",
                                           "midastouch, logreg, polyreg, polr",
                                           "midastouch, logreg.boot, polyreg, polr",
                                           "rf, logreg, polyreg, polr",
                                           "rf, logreg.boot, polyreg, polr",
                                           "cart, logreg, polyreg, polr",
                                           "cart, logreg.boot, polyreg, polr"),
                            "method numeric" = c("pmm", "midastouch", "rf", "cart",
                                                 "pmm",
                                                 "pmm",
                                                 "midastouch",
                                                 "midastouch",
                                                 "rf",
                                                 "rf",
                                                 "cart",
                                                 "cart"),
                            "method binary" = c("pmm", "midastouch", "rf", "cart",
                                                "logreg",
                                                "logreg.boot",
                                                "logreg",
                                                "logreg.boot",
                                                "logreg",
                                                "logreg.boot",
                                                "logreg",
                                                "logreg.boot"),
                            "method categorical" = c("pmm", "midastouch", "rf", "cart",
                                                     rep("polyreg", 8)),
                            complete_evaluation)
complete_evaluation <- select(complete_evaluation, -method)
  
pdf("./Plots/mice_comp.pdf", height = 8, width = 12)
grid.table(complete_evaluation)
dev.off()
