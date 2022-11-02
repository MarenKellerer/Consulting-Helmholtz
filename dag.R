# load packages
library(dagitty)
library(lavaan)
library(tidyverse)

# load data
data <- readRDS("./Data/data_clean.RDS")

# DAG ---------------------------------------------------------------------

# DAG with all variables, decided on previous studies
dag <- dagitty('dag {
                gender [pos="0.256,-0.888"]
                h_index [pos="-0.273,0.184"]
                impact_factor [pos="-0.336,-0.357"]
                citation_count [outcome,pos="0.683,-0.576"]
                journal_subject [pos="-0.301,-1.035"]
                length [pos="-0.325,-1.541"]
                oa_status [exposure,pos="-1.435,-0.497"]
                references_count [pos="0.397,-1.199"]
                gender -> citation_count
                gender -> journal_subject
                h_index -> citation_count
                h_index -> oa_status
                impact_factor -> h_index
                impact_factor -> citation_count
                impact_factor -> oa_status
                journal_subject -> h_index
                journal_subject -> impact_factor
                journal_subject -> citation_count
                journal_subject -> length
                journal_subject -> oa_status
                journal_subject -> references_count
                length -> oa_status
                length -> references_count
                oa_status -> citation_count
                references_count -> citation_count
                }')

# plot of the directed acyclic graph
# plot(dag)

#  minimal sets to condition on to render the pair independent
impliedConditionalIndependencies(dag)


# linear regressions for testing indepence assumption

lm_1 <- lm(impact_factor ~ gender + journal_subject, data = data)
lm_2 <- lm(length ~ gender + journal_subject, data = data)
lm_3 <- lm(references_count ~ gender + journal_subject, data = data) 
lm_4 <- lm(length ~ h_index + journal_subject, data = data) 
lm_5 <- lm(references_count ~ h_index + journal_subject, data = data) 
lm_6 <- lm(references_count ~ oa_status + journal_subject + length, data = data) 
lm_7 <- lm(length ~ citation_count + h_index + impact_factor + 
                         oa_status + journal_subject + references_count, 
           data = data)
lm_8 <- lm(h_index ~ gender + journal_subject, data = data)
lm_9 <- lm(length ~ impact_factor + journal_subject, data = data)
lm_10 <- lm(references_count ~ impact_factor + journal_subject, data = data)

# make df with all results

coef <- c(coef(lm_1)[2], coef(lm_2)[2], coef(lm_3)[2], coef(lm_4)[2], 
          coef(lm_5)[2], coef(lm_6)[2], coef(lm_6)[3], coef(lm_6)[4], 
          coef(lm_6)[5], coef(lm_7)[2], coef(lm_8)[2], coef(lm_9)[2], 
          coef(lm_10)[2])

dep_var <- c(lm_1$terms[[2]], lm_2$terms[[2]], lm_3$terms[[2]], lm_4$terms[[2]],
             lm_5$terms[[2]], rep(as.character(lm_6$terms[[2]]), 4), 
             lm_7$terms[[2]], lm_8$terms[[2]],
             lm_9$terms[[2]], lm_10$terms[[2]]) %>% as.character()

conf_int <- rbind(confint(lm_1)[2, ], confint(lm_2)[2, ], confint(lm_3)[2, ], 
                  confint(lm_4)[2, ], confint(lm_5)[2, ], confint(lm_6)[2, ],
                  confint(lm_6)[3, ], confint(lm_6)[4, ], confint(lm_6)[5, ], 
                  confint(lm_7)[2, ], confint(lm_8)[2, ],confint(lm_9)[2, ], 
                  confint(lm_10)[2, ])

tests_df <- data.frame(
  coef = coef,
  ci_l = conf_int[, 1],
  ci_u = conf_int[, 2],
  var = names(coef),
  model = c(1, 2, 3, 4, 5, 6, 6, 6, 6, 7, 8, 9, 10),
  dep_var = dep_var) %>% 
  mutate(sig = (ci_l < 0 & ci_u < 0) | (ci_l > 0 & ci_u > 0),
         call = paste(dep_var, "~", var))

tests_df$var <- factor(tests_df$call)


# function for getting a partial residual plot
# constant 0.001 for the log transformation (some variables contain 0 entries)
# input:  model:    fitted linear model 
#         variable: regressor of the fitted model 
#         xlab:     title of the x-axis
#         title:    main title of the plot
# output: ggplot of partial residuals
     
get_partial_residual_plot <- function(model, variable, xlab, title) {
  
  data <- na.omit(data[, all.vars(formula(model))])
  partial_residual <- resid(model) + data[[variable]] * coef(model)[variable]
  
  ggplot(data, aes(x = log(.data[[variable]] + 0.0001), y = partial_residual) ) +
    geom_point(alpha = 0.3) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(size = 14), 
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12)) +
    labs(title = title,
         x = paste("log(", xlab, ")", sep = " "),
         y = "Partial residuals") 
}

# plots for continuous variables - lm_4, lm_5, lm_7, lm_9, lm_10
plot_lm_4 <- get_partial_residual_plot(lm_4, "h_index", "h-index", 
                                       "length ~ h-index | .")
plot_lm_5 <- get_partial_residual_plot(lm_5, "h_index", "h-index", 
                                       "reference count ~ h-index | .")
plot_lm_7 <- get_partial_residual_plot(lm_7, "citation_count", 
                                       "citation count", 
                                       "length ~ citation count | .")
plot_lm_9 <- get_partial_residual_plot(lm_9, "impact_factor", "impact factor", 
                                       "length ~ impact factor | .")
plot_lm_10 <- get_partial_residual_plot(lm_10, "impact_factor", "impact factor", 
                                        "reference count ~ impact factor | .")











