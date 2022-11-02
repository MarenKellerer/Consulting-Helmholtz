library(dagitty)
library(ggdag)
library(collapsibleTree)
library(tidyverse)
library(ggstatsplot)
library(gridExtra) # to save table

# set theme for all of the plots
theme_set(theme_light() +
            theme(panel.background = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 14), 
                  axis.title.x = element_text(size = 12),
                  axis.title.y = element_text(size = 12)))

data <- readRDS("./Data/data_clean.RDS")

source("./dag.R")
source("./modelling.R")


# 1. Describing the Data --------------------------------------------------

# A) Frequency distributions

# for citation_count
frequency_dist_citation_count <- ggplot(data, aes(x = citation_count,
                                                  fill = is_oa)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
  labs(title = "Frequency distribution - citation count",
       y = "Frequency",
       x = "citation count",
       fill = "Is it OA?")
frequency_dist_citation_count

ggsave("./Plots/frequency_dist_citation_count.png",
       width = 11,
       height = 6,
       dpi = 1000)

frequency_dist_citation_count_zoom <- ggplot(data,
                                             aes(x = citation_count,
                                                 fill = is_oa)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  xlim(-1, 50) +
  scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
  labs(title = "Frequency distribution - citation count (up to 50)",
       y = "Frequency",
       x = "citation count",
       fill = "Is it OA?")
frequency_dist_citation_count_zoom

ggsave("./Plots/frequency_dist_citation_count_zoom.png",
       width = 11,
       height = 6,
       dpi = 1000)


# for impact factor
frequency_dist_impact_factor <- ggplot(data, aes(x = impact_factor,
                                                 fill = is_oa)) +
  geom_histogram(bins = 150, position = "dodge") +
  scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
  labs(title = "Frequency distribution - impact factor",
       y = "Frequency",
       x = "impact factor",
       fill = "Is it OA?")
frequency_dist_impact_factor

ggsave("./Plots/frequency_dist_impact_factor.png",
       width = 11,
       height = 6,
       dpi = 1000)

# for h-index
frequency_dist_h_index <- ggplot(data, aes(x = h_index, fill = is_oa)) +
  geom_histogram(bins = 150, position = "dodge") +
  scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
  labs(title = "Frequency distribution - h-index",
       y = "Frequency",
       x = "h-index",
       fill = "Is it OA?")
frequency_dist_h_index

ggsave("./Plots/frequency_dist_h_index.png",
       width = 11,
       height = 6,
       dpi = 1000)


# for references_count
frequency_dist_references_count <- ggplot(data, aes(x = references_count,
                                                    fill = is_oa)) +
  geom_histogram(bins = 150, position = "dodge") +
  scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
  labs(title = "Frequency distribution - references count",
       y = "Frequency",
       x = "references count",
       fill = "Is it OA?")
frequency_dist_references_count

ggsave("./Plots/frequency_dist_references_count.png",
       width = 11,
       height = 6,
       dpi = 1000)


# for length
frequency_dist_length <- ggplot(data, aes(x = length, fill = is_oa)) +
  geom_histogram(bins = 150, position = "dodge") +
  scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
  labs(title = "Frequency distribution - length",
       y = "Frequency",
       x = "length",
       fill = "Is it OA?")
frequency_dist_length

ggsave("./Plots/frequency_dist_length.png",
       width = 11,
       height = 6,
       dpi = 1000)


# for journal_subject
barplot_journal_subject <- ggplot(data, aes(x = journal_subject,
                                            fill = is_oa)) +
  geom_bar(position = "stack") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
  labs(title = "Frequency - journal subject",
       y = "Frequency",
       x = "journal subject",
       fill = "Is it OA?")
barplot_journal_subject

ggsave("./Plots/barplot_journal_subject.png",
       width = 11,
       height = 6,
       dpi = 1000)


# for gender
barplot_gender <- ggplot(data, aes(x = gender, fill = is_oa)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
  labs(title = "Frequency - gender",
       y = "Frequency",
       x = "gender",
       fill = "Is it OA?")
barplot_gender

ggsave("./Plots/barplot_gender.png",
       width = 11,
       height = 6,
       dpi = 1000)


# for time_since_pub
frequency_time_since_pub <- ggplot(data, aes(x = time_since_pub,
                                             fill = is_oa)) +
  geom_histogram(bins = 100, position = "dodge") +
  scale_fill_manual(values = c("#0072B2", "#56B4E9")) +
  scale_x_continuous(breaks = seq(365, 1825, 365),
                     labels = c("1 year", "2 years", 
                                "3 years", "4 years", "5 years")) +
  labs(title = "Frequency distribution - time since publication",
       y = "Frequency",
       x = "time since publication",
       fill = "Is it OA?")
frequency_time_since_pub

ggsave("./Plots/frequency_dist_time_since_pub.png",
       width = 11,
       height = 6,
       dpi = 1000)


# B) plotting zero and count part in Data

png("./Plots/zero_count.png",
    width = 1100,
    height = 600,
    pointsize = 17)

# plotting data
par(mfrow = c(1,2))
par(mar = c(5,4,4,4))

data$oa_status <- factor(data$oa_status,
                         levels = c("closed", "bronze", "hybrid", "gold",
                                    "green"))
# plot for zero vs. greater zero
plot(factor(citation_count == 0) ~ oa_status, data = data, 
     ylab = "citation count = 0", xlab = "OA status",  main = "Zero Part", 
     col = c("lightblue", "steelblue"))

par(mar = c(5,6,4,2))

# plot for neg. Bin. Model
plot(log(citation_count) ~ oa_status, data = data, 
     col = "steelblue", subset = citation_count > 0,
     ylab = "log of citation count > 0", xlab = "OA status", 
     main = "Count Part")

mtext("separated OA status frequency", line = -1.1, outer = TRUE, cex = 1.5)

dev.off()


# C) Visualizing how OA status is split 

tree_data <- data %>% 
  count(is_oa, oa_status) %>% 
  mutate("where" = c(NA, rep("Publisher website", 3), "Repository"),
         "journal" = c(NA, rep("Journal is not OA", 2), "Journal is OA", NA),
         "license" = c(NA, "no Open License", "Open License", NA, NA))

collapsibleTree(tree_data, hierarchy = c("is_oa", "where", "journal",
                                      "license", "oa_status", "n"), 
                                  fill = c("#56B4E9", "#0072B2", "#56B4E9", "grey",
                                           rep("#56B4E9", 2), "white",
                                           rep("#56B4E9", 2), "green", 
                                           rep("#56B4E9", 2), "orange", "white",
                                           "brown", "lightblue", 
                                           rep("white", 3)),
                                  root = "Is the paper OA?",
                                  nodeSize = "leafCount", 
                                  collapsed = FALSE, 
                                  fontSize = 18)

# D) missing values 

# only variables that are relevant for the DAG

missings_info <- data[,c("oa_status", "references_count", 
        "citation_count", "journal_subject",
        "impact_factor", "h_index", "time_since_pub", "gender",
        "length")] %>% 
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>% 
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>% # get n of NA per column
  mutate(pct = num.isna / total * 100) # get percentage per column
  
missings_plot <- missings_info %>% 
  mutate(pct_m = ifelse(isna == FALSE, pct-50, pct)) %>% 
  mutate(key = fct_rev(factor(key, levels = c('citation_count',
                                      'oa_status', 
                                      'impact_factor',
                                      'h_index',
                                      'journal_subject',
                                      'references_count',
                                      'time_since_pub',
                                      'gender',
                                      'length')))) %>% 
ggplot() +
  geom_bar(aes(x = key, y = pct_m, fill = isna), 
           stat = 'identity', alpha = 0.8) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'grey'), 
                    labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values (up to 50 %)", 
       x = 'Variable', y = "% of missing values") +
  scale_x_discrete(labels = c('citation_count' = "citation count",
                              'oa_status' = "OA status", 
                              'impact_factor' = "impact factor",
                              'h_index' = "h-index",
                              'journal_subject' = "journal subject",
                              'references_count' = "references count",
                              'time_since_pub' = "time since publication",
                              'gender' = "gender",
                              'length' = "length"))

missings_plot
ggsave("./Plots/missings_plot.png",
       width = 11,
       height = 6,
       dpi = 500)


# 2. DAG ------------------------------------------------------------------


# A) simple DAG 

# simple DAG for explaining the meaning of D-A-G
simple_dag <- dagitty('dag {
                      A [pos="-1.061,-0.998"]
                      B [pos="0.248,-0.998"]
                      C [exposure,pos="-1.061,0.457"]
                      D [outcome,pos="0.248,0.457"]
                      A -> B
                      A -> C
                      B -> C
                      C -> D
                      }')


simple_dag_plot <- ggdag(simple_dag,
                         edge_type = "link_arc",
                         text_size = 15,
                         node_size = 17, 
                         text_col = "black",
                         label_col = "white",
                         node = FALSE,
                         stylized = FALSE,
                         text = TRUE,
                         use_labels = NULL) + theme_dag_blank()

ggsave("./Plots/simple_dag.png", 
       plot = simple_dag_plot,
       device = "png",
       width = 11,
       height = 6,
       dpi = 500)


# B) DAG related to the data

data_dag <- dagitty('dag {
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


data_dag_plot <- ggdag(data_dag,
                       edge_type = "link_arc",
                       text_size = 11,
                       node_size = 17, 
                       text_col = "black",
                       label_col = "white",
                       node = FALSE,
                       stylized = FALSE,
                       text = TRUE,
                       use_labels = NULL) + theme_dag_blank()

ggsave("./Plots/data_dag.png", 
       plot = data_dag_plot,
       device = "png",
       width = 11,
       height = 6,
       dpi = 500)


# C) Visualizing the test results

dag_coef_plot <- ggplot(aes(coef, call, col = sig), data = tests_df) +
  geom_point() + 
  geom_vline(aes(xintercept = 0), size = .1, color = "grey") +
  geom_segment(aes(y = call,
                   yend = call,
                   x = ci_l, 
                   xend = ci_u)) +
  scale_y_discrete(labels = c("impact_factor ~ gendermale" = 
                                "impact factor ~ gender (male) |.", 
                              "length ~ gendermale" = 
                                "length ~ gender (male) |.", 
                              "references_count ~ gendermale" = 
                                "references count ~ gender (male) |.",
                              "length ~ h_index" = "length ~ h-index |.",
                              "references_count ~ h_index" = 
                                "references count ~ h-index |.",
                              "references_count ~ oa_statusbronze" = 
                                "OA status (bronze)",
                              "references_count ~ oa_statushybrid" = 
                                "references count ~ OA status (hybrid) |.",
                              "references_count ~ oa_statusgold" = 
                                "OA status (gold)",
                              "references_count ~ oa_statusgreen" = 
                                "OA status (green)", 
                              "length ~ citation_count" = 
                                "length ~ citation count |.",
                              "h_index ~ gendermale" = 
                                "h-index ~ gender (male) |.", 
                              "length ~ impact_factor" = 
                                "length ~ impact factor |.",
                              "references_count ~ impact_factor"  = 
                                "references count ~ impact factor |." )) +
  labs(title = "Coefficients of the linear models fitted to test independence",
       x = "Coefficient", y = "Model",
       col = "Significant at 5% level")

dag_coef_plot
ggsave("./Plots/dag_coef_plot.png",
       width = 11,
       height = 6,
       dpi = 500)


# D) Checking the linearity for testable implications

# partial residuals plots from dag.R
combined_plots <- combine_plots(
  plotlist = list(plot_lm_4, plot_lm_5, plot_lm_7, plot_lm_9, plot_lm_10),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "Checking linearity"
  )
)

combined_plots
ggsave("./Plots/linearity_plots.png", 
       plot = combined_plots,
       device = "png",
       width = 11,
       height = 6,
       dpi = 500)


# 3. Hurdle model ---------------------------------------------------------

# A) plotting the coefficients

compl_cases_count <- data.frame( # df with results for complete cases & count
  coef = summary(hurdle_negbin_complete_cases)$coefficients$count[,1],
  se = summary(hurdle_negbin_complete_cases)$coefficients$count[,2],
  model = "Complete cases",
  var = names(summary(hurdle_negbin_complete_cases)$coefficients$count[,1])) %>% 
  mutate(
    ci_l = coef - qnorm(0.025) * se,
    ci_u = coef + qnorm(0.025) * se,
    sig = (ci_l < 0 & ci_u < 0) | (ci_l > 0 & ci_u > 0))
mice_count <- data.frame( # df with results for imputation & count
  coef = pool_coef_count,
  se = sqrt(pool_var_count),
  model = "Imputation",
  var = names(pool_coef_count)) %>% 
  mutate(
    ci_l = coef-qnorm(0.025) * se,
    ci_u = coef+qnorm(0.025) * se,
    sig = (ci_l < 0 & ci_u < 0) | (ci_l > 0 & ci_u > 0))

# combine into one df with results of count model
count_results_df <- rbind(compl_cases_count, mice_count)
count_results_df$type <- "count"

# get exp coefficients
exp(hurdle_negbin_complete_cases$coefficients$count)


compl_cases_zero <- data.frame( # df with results for complete cases & zero
  coef = summary(hurdle_negbin_complete_cases)$coefficients$zero[,1],
  se = summary(hurdle_negbin_complete_cases)$coefficients$zero[,2],
  model = "Complete cases",
  var = names(summary(hurdle_negbin_complete_cases)$coefficients$zero[,1])) %>% 
  mutate(
    ci_l = coef - qnorm(0.025) * se,
    ci_u = coef + qnorm(0.025) * se,
    sig = (ci_l < 0 & ci_u < 0) | (ci_l > 0 & ci_u > 0))
mice_zero <- data.frame( # df with results for imputation & zero
  coef = pool_coef_zero,
  se = sqrt(pool_var_zero),
  model = "Imputation",
  var = names(pool_coef_zero)) %>% 
  mutate(
    ci_l = coef - qnorm(0.025) * se,
    ci_u = coef + qnorm(0.025) * se,
    sig = (ci_l < 0 & ci_u < 0) | (ci_l > 0 & ci_u > 0))

# combine into one df with results of zero model
zero_results_df <- rbind(compl_cases_zero, mice_zero)
zero_results_df$type <- "zero"

# get exp coefficients
exp(hurdle_negbin_complete_cases$coefficients$zero)

# get order of variables we want
levels_var <- c('(Intercept)', 'oa_statusbronze', 'oa_statushybrid', 
        'oa_statusgold', 'oa_statusgreen', 'impact_factor', 'h_index',
        'impact_factor:h_index', 'journal_subjectlife', 
        'journal_subjecthealth', 'journal_subjectphysical', 
        'journal_subjectsocial', 'references_count', 'time_since_pub',
        'Log(theta)')

# combine into one df with all results
results_df <- rbind(count_results_df, zero_results_df)
results_df$var <- factor(results_df$var, 
                              levels = levels_var)

# multiply numeric variables by one sd
for (i in 1:nrow(results_df)) { # go through all variables
  var <- results_df[i, "var"]
  coef <- results_df[i, "coef"]
  coef_final <- coef
  se <- results_df[i, "se"]
  se_final <- se
  if (var %in% # check if variable is numeric
     c("impact_factor", "h_index", "references_count", "time_since_pub")){
    sd <- sd(data[, as.character(var)], na.rm = TRUE) # get sd
    # multiply by sd
    coef_final <- coef * sd
    se_final <- se * sd
  }
  # save results
  results_df[i, "coef_final"] <- coef_final
  results_df[i, "se_final"] <- se_final
  
  # compute confidence interval
  results_df[i, "ci_l_final"] <- coef_final - qnorm(0.025) * se_final
  results_df[i, "ci_u_final"] <- coef_final + qnorm(0.025) * se_final
}

# plot

results_df %>% 
  subset(!var %in% c("(Intercept)", "Log(theta)")) %>% # coef not of interest
ggplot(aes(exp(coef_final), fct_rev(var), col = sig)) +
  geom_point() + 
  #xlim(c(0, 6.5)) +
  geom_vline(aes(xintercept = 1), size = .1, color = "black") +
  geom_segment(aes(y = fct_rev(var),
                   yend = fct_rev(var),
                   x = exp(ci_l_final), 
                   xend = exp(ci_u_final))) +
  facet_grid(rows = vars(model), cols = vars(type), scales = "free") +
  scale_y_discrete(labels = c('oa_statusbronze' = "OA status: bronze", 
                              'oa_statushybrid' = "OA status: hybrid", 
                              'oa_statusgold' = "OA status: gold",
                              'oa_statusgreen' = "OA status: green",
                              'impact_factor' = "impact factor",
                              'h_index' = "h-index",
                              'impact_factor:h_index'
                              = "impact factor * h-index",
                              'journal_subjectlife' = "journal subject: life",
                              'journal_subjecthealth' = 
                                "journal subject: health", 
                              'journal_subjectphysical' = 
                                "journal subject: physical",
                              'journal_subjectsocial' = 
                                "journal subject: social", 
                              'references_count' = "references count",
                              'time_since_pub' = "time since publication")) +
  labs(title = "Coefficients of NegBin Hurdle model:
       complete cases and imputation",
       x = "Coefficient", y = "Variable",
       col = "Significant at 5% level") -> coef_plot

coef_plot
ggsave("./Plots/coef_plot.png", width = 11, height = 6)

results_df %>% 
  subset(model == "Complete cases") %>% 
  subset(!var %in% c("(Intercept)", "Log(theta)")) %>% 
  ggplot(aes(exp(coef_final), fct_rev(var), col = sig)) +
  geom_point() + 
  xlim(c(0, 6.5)) +
  geom_vline(aes(xintercept = 1), size = .1, color = "black") +
  geom_segment(aes(y = fct_rev(var),
                   yend = fct_rev(var),
                   x = exp(ci_l_final), 
                   xend = exp(ci_u_final))) +
  facet_grid(cols = vars(type), scales = "free") +
  scale_y_discrete(labels = c('oa_statusbronze' = "OA status: bronze", 
                              'oa_statushybrid' = "OA status: hybrid", 
                              'oa_statusgold' = "OA status: gold",
                              'oa_statusgreen' = "OA status: green",
                              'impact_factor' = "impact factor",
                              'h_index' = "h-index",
                              'impact_factor:h_index'
                              = "impact factor * h-index",
                              'journal_subjectlife' = "journal subject: life",
                              'journal_subjecthealth' = 
                                "journal subject: health", 
                              'journal_subjectphysical' = 
                                "journal subject: physical",
                              'journal_subjectsocial' = 
                                "journal subject: social", 
                              'references_count' = "references count",
                              'time_since_pub' = "time since publication")) +
  labs(title = "Coefficients of NegBin Hurdle model: complete cases",
       x = "Coefficient", y = "Variable",
       col = "Significant at 5% level") -> coef_plot_compl

coef_plot_compl
ggsave("./Plots/coef_plot_compl.png", width = 11, height = 6)

results_df %>% 
  subset(model == "Imputation") %>% 
  subset(!var %in% c("(Intercept)", "Log(theta)")) %>% 
  ggplot(aes(exp(coef_final), fct_rev(var), col = sig)) +
  geom_point() +
  xlim(c(0, 6.5)) +
  geom_vline(aes(xintercept = 1), size = .1, color = "black") +
  geom_segment(aes(y = fct_rev(var),
                   yend = fct_rev(var),
                   x = exp(ci_l_final), 
                   xend = exp(ci_u_final))) +
  facet_grid(cols = vars(type), scales = "free") +
  scale_y_discrete(labels = c('oa_statusbronze' = "OA status: bronze", 
                              'oa_statushybrid' = "OA status: hybrid", 
                              'oa_statusgold' = "OA status: gold",
                              'oa_statusgreen' = "OA status: green",
                              'impact_factor' = "impact factor",
                              'h_index' = "h-index",
                              'impact_factor:h_index'
                              = "impact factor * h-index",
                              'journal_subjectlife' = "journal subject: life",
                              'journal_subjecthealth' = 
                                "journal subject: health", 
                              'journal_subjectphysical' = 
                                "journal subject: physical",
                              'journal_subjectsocial' = 
                                "journal subject: social", 
                              'references_count' = "references count",
                              'time_since_pub' = "time since publication")) +
  labs(title = "Coefficients of NegBin Hurdle model: imputation",
       x = "Coefficient", y = "Variable",
       col = "Significant at 5% level") -> coef_plot_imp

coef_plot_imp
ggsave("./Plots/coef_plot_imp.png", width = 11, height = 6)

# make a table of results

complete_table <- subset(results_df, model == "Complete cases") %>% 
  transmute("variable" = var,
            "model" = type,
            "coefficient" = as.character(format(round(coef_final, 2),
                                                      nsmall = 2)),
            "confidence interval" = paste("[", as.character(format(round(
              ci_l_final, 2), nsmall = 2)), ", ", as.character(format(round(
                ci_u_final, 2), nsmall = 2)), "]"),
            "exp(coefficient)" = as.character(format(round(exp(
              coef_final), 2), nsmall = 2)),
            "exp(confidence interval)" = paste("[", as.character(format(round(
              exp(ci_l_final), 2), nsmall = 2)), ", ", 
              as.character(format(round(exp(ci_u_final), 2), nsmall = 2)),"]"))
            
rownames(complete_table) <- c()

pdf("./Plots/coef_model_compl.pdf", height = 11, width = 11)
grid.table(complete_table)
dev.off()

imp_table <- subset(results_df, model == "Imputation") %>% 
  transmute("variable" = var,
            "model" = type,
            "coefficient" = as.character(format(round(coef_final, 2),
                                                nsmall = 2)),
            "confidence interval" = paste("[", as.character(format(round(
              ci_l_final, 2), nsmall = 2)), ", ", as.character(format(round(
                ci_u_final, 2), nsmall = 2)), "]"),
            "exp(coefficient)" = as.character(format(round(exp(
              coef_final), 2), nsmall = 2)),
            "exp(confidence interval)" = paste("[", as.character(format(round(
              exp(ci_l_final), 2), nsmall = 2)), ", ", 
              as.character(format(round(exp(ci_u_final), 2), nsmall = 2)),"]"))

rownames(imp_table) <- c()

pdf("./Plots/coef_model_imp.pdf", height = 11, width = 11)
grid.table(imp_table)
dev.off()

# B) rootogram 1: distribution comparison (observed vs. fitted values)

png("./Plots/rootogram_distribution.png",
    width = 1100,
    height = 600,
    pointsize = 20)

par(mfrow = c(1,2))
countreg::rootogram(hurdle_poi_complete_cases, 
                    main = "Poisson", 
                    xlab = "citation count", 
                    ylab = "root of frequency", 
                    col = "steelblue", 
                    ylim = c(-10, 25), 
                    max = 120)
countreg::rootogram(hurdle_negbin_complete_cases, 
                    main = "Negative Binomial", 
                    xlab = "citation count", 
                    ylab = "root of frequency", 
                    col = "steelblue", 
                    ylim = c(-10, 25), 
                    max = 120)
mtext("Empirical vs. fitted", line = -1.1, outer = TRUE, cex = 1.5)

dev.off()


# B) rootogram 2: complete cases vs. imputation

png("./Plots/rootogram_complete_imputation.png",
    width = 1100,
    height = 600,
    pointsize = 20)

par(mfrow = c(1,2))
countreg::rootogram(hurdle_negbin_imputation$analyses[[1]], 
                    main = "Imputation", 
                    xlab = "citation count", 
                    ylab = "root of frequency", 
                    col = "steelblue", 
                    ylim = c(0, 35), 
                    max = 120)
countreg::rootogram(hurdle_negbin_complete_cases, 
                    main = "Complete Cases", 
                    xlab = "citation count", 
                    ylab = "root of frequency", 
                    col = "steelblue", 
                    ylim = c(0, 35), 
                    max = 120)
mtext("Empirical vs. fitted", line = -1.1, outer = TRUE, cex = 1.5)

dev.off()
