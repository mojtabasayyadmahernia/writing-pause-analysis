# Study: Syntagmatic and Functional Context of Pauses during Writing: A Hallidayan Perspective
# Author: Mojtaba Sayyad Mahernia



data <- read.csv("final1.csv")

data_clean <- data %>%
  filter(!is.na(Pause), Pause > 0) %>%
  mutate(
    Pause_ms = Pause * 1000,
    Log_Pause_ms = log(Pause_ms),
    Syntactic_Context = factor(Syntactic.Context.of.the.pause),
    Functional_Context = factor(Functional.Context.of.the.pause),
    Process_Type = factor(Type.of.process),
    Participant_ID = factor(file_id),
    Syntactic_Major = case_when(
      str_detect(Syntactic.Context.of.the.pause, "end NP") ~ "End NP",
      str_detect(Syntactic.Context.of.the.pause, "mid NP") ~ "Mid NP", 
      str_detect(Syntactic.Context.of.the.pause, "end VP") ~ "End VP",
      str_detect(Syntactic.Context.of.the.pause, "mid word") ~ "Mid Word",
    ),
    
    Process_Major = case_when(
      str_detect(tolower(Type.of.process), "material") ~ "Material",
      str_detect(tolower(Type.of.process), "mental") ~ "Mental", 
      str_detect(tolower(Type.of.process), "relational") ~ "Relational",
      str_detect(tolower(Type.of.process), "verbal") ~ "Verbal",
      str_detect(tolower(Type.of.process), "existential") ~ "Existential",
      TRUE ~ "Other"
    )
  ) %>%
  filter(abs(scale(Log_Pause_ms)) < 3)

data_clean$Syntactic_Major <- factor(data_clean$Syntactic_Major)
data_clean$Process_Major <- factor(data_clean$Process_Major)
desc_overall <- data_clean %>%
  summarise(
    N = n(),
    Mean_ms = round(mean(Pause_ms), 1),
    Median_ms = round(median(Pause_ms), 1),
    SD_ms = round(sd(Pause_ms), 1),
    Min_ms = round(min(Pause_ms), 1),
    Max_ms = round(max(Pause_ms), 1),
    Q1_ms = round(quantile(Pause_ms, 0.25), 1),
    Q3_ms = round(quantile(Pause_ms, 0.75), 1)
  )

desc_syntactic <- data_clean %>%
  group_by(Syntactic_Major) %>%
  summarise(
    N = n(),
    Mean_ms = round(mean(Pause_ms), 1),
    Median_ms = round(median(Pause_ms), 1),
    SD_ms = round(sd(Pause_ms), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(N))

desc_process <- data_clean %>%
  filter(Process_Major != "Other") %>%
  group_by(Process_Major) %>%
  summarise(
    N = n(),
    Mean_ms = round(mean(Pause_ms), 1),
    Median_ms = round(median(Pause_ms), 1),
    SD_ms = round(sd(Pause_ms), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(N))

desc_participants <- data_clean %>%
  group_by(Participant_ID) %>%
  summarise(
    N_pauses = n(),
    Mean_duration = round(mean(Pause_ms), 1),
    SD_duration = round(sd(Pause_ms), 1),
    .groups = 'drop'
  )

write.csv(desc_overall, "output/descriptive_overall.csv", row.names = FALSE)
write.csv(desc_syntactic, "output/descriptive_syntactic.csv", row.names = FALSE)
write.csv(desc_process, "output/descriptive_process.csv", row.names = FALSE)
write.csv(desc_participants, "output/descriptive_participants.csv", row.names = FALSE)

lmm_syntactic <- lmer(Log_Pause_ms ~ Syntactic_Major + (1|Participant_ID), 
                      data = data_clean,
                      REML = TRUE)

summary(lmm_syntactic)
anova_syntactic <- anova(lmm_syntactic)

effect_sizes_syntactic <- calculate_effect_size(lmm_syntactic)
emmeans_syntactic <- emmeans(lmm_syntactic, ~ Syntactic_Major)
pairs_syntactic <- pairs(emmeans_syntactic, adjust = "bonferroni")

print(pairs_syntactic)
results_syntactic <- create_results_text(anova_syntactic, "Syntactic Context")
data_process <- data_clean %>%
  filter(Process_Major %in% c("Material", "Mental", "Relational", "Verbal"))

lmm_process <- lmer(Log_Pause_ms ~ Process_Major + (1|Participant_ID), 
                    data = data_process,
                    REML = TRUE)

summary(lmm_process)
anova_process <- anova(lmm_process)
print(anova_process)
effect_sizes_process <- calculate_effect_size(lmm_process)
emmeans_process <- emmeans(lmm_process, ~ Process_Major)
pairs_process <- pairs(emmeans_process, adjust = "bonferroni")

print(pairs_process)
results_process <- create_results_text(anova_process, "Process Type")
png("output/model_diagnostics_syntactic.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(lmm_syntactic, main = "Residuals vs Fitted")
qqnorm(residuals(lmm_syntactic), main = "Q-Q Plot of Residuals")
qqline(residuals(lmm_syntactic))
plot(fitted(lmm_syntactic), sqrt(abs(residuals(lmm_syntactic))), 
     main = "Scale-Location", xlab = "Fitted values", ylab = "√|Residuals|")
plot(hatvalues(lmm_syntactic), residuals(lmm_syntactic), 
     main = "Residuals vs Leverage", xlab = "Leverage", ylab = "Residuals")
dev.off()

syntactic_counts <- data_clean %>%
  count(Syntactic_Major, name = "observed") %>%
  arrange(desc(observed))

total_pauses <- sum(syntactic_counts$observed)
syntactic_counts$expected <- total_pauses / nrow(syntactic_counts)
syntactic_counts$sparse <- syntactic_counts$expected < 5

print(syntactic_counts)
chi_syntactic <- chisq.test(syntactic_counts$observed)
print(chi_syntactic)
mc_chi_syntactic <- chisq.test(syntactic_counts$observed, 
                               simulate.p.value = TRUE, B = 10000)
print(mc_chi_syntactic)
cramers_v_syntactic <- sqrt(chi_syntactic$statistic / (total_pauses * (nrow(syntactic_counts) - 1)))
process_counts <- data_clean %>%
  filter(Process_Major != "Other") %>%
  count(Process_Major, name = "observed") %>%
  arrange(desc(observed))

total_process_pauses <- sum(process_counts$observed)
process_counts$expected <- total_process_pauses / nrow(process_counts)
process_counts$sparse <- process_counts$expected < 5

print(process_counts)
chi_process <- chisq.test(process_counts$observed)
print(chi_process)
cramers_v_process <- sqrt(chi_process$statistic / (total_process_pauses * (nrow(process_counts) - 1)))
write.csv(syntactic_counts, "output/syntactic_frequency_analysis.csv", row.names = FALSE)
write.csv(process_counts, "output/process_frequency_analysis.csv", row.names = FALSE)

set.seed(42)

if (!dir.exists("output")) {
  dir.create("output")
}

calculate_effect_size <- function(model) {
  r2_marginal <- MuMIn::r.squaredGLMM(model)[1]
  r2_conditional <- MuMIn::r.squaredGLMM(model)[2]
  
  return(list(
    marginal_r2 = r2_marginal,
    conditional_r2 = r2_conditional
  ))
}

format_p <- function(p) {
  ifelse(p < 0.001, "< 0.001", 
         ifelse(p < 0.01, sprintf("= %.3f", p),
                sprintf("= %.2f", p)))
}

create_results_text <- function(model_anova, effect_name) {
  f_stat <- model_anova$`F value`[1]
  df1 <- model_anova$NumDF[1]
  df2 <- round(model_anova$DenDF[1], 1)
  p_val <- model_anova$`Pr(>F)`[1]
  
  sprintf("%s: F(%d, %.1f) = %.2f, p %s", 
          effect_name, df1, df2, f_stat, format_p(p_val))
}



bootstrap_mean <- function(data, indices) {
  sample_data <- data[indices, ]
  return(mean(sample_data$Pause_ms))
}

bootstrap_effect_size <- function(data, indices) {
  sample_data <- data[indices, ]
  
  means <- sample_data %>%
    filter(Syntactic_Major %in% c("End Sentence", "Mid Word")) %>%
    group_by(Syntactic_Major) %>%
    summarise(mean_duration = mean(Pause_ms), .groups = 'drop')
  
  if (nrow(means) == 2) {
    return(means$mean_duration[1] - means$mean_duration[2])
  } else {
    return(NA)
  }
}

boot_mean <- boot(data_clean, bootstrap_mean, R = 10000)
ci_mean <- boot.ci(boot_mean, type = "bca")

boot_effect <- boot(data_clean, bootstrap_effect_size, R = 10000)
boot_effect_clean <- boot_effect$t[!is.na(boot_effect$t)]
ci_effect <- quantile(boot_effect_clean, c(0.025, 0.975))

bootstrap_participant_mean <- function(data, indices) {
  unique_participants <- unique(data$Participant_ID)
  sampled_participants <- sample(unique_participants, replace = TRUE)
    sampled_data <- data[data$Participant_ID %in% sampled_participants, ]
  
  participant_means <- sampled_data %>%
    group_by(Participant_ID) %>%
    summarise(mean_duration = mean(Pause_ms), .groups = 'drop')
  
  return(mean(participant_means$mean_duration))
}

boot_participant <- boot(data_clean, bootstrap_participant_mean, R = 5000)
ci_participant <- boot.ci(boot_participant, type = "bca")

p1 <- ggplot(data_clean, aes(x = reorder(Syntactic_Major, Pause_ms, median), y = Pause_ms)) +
  geom_boxplot(aes(fill = Syntactic_Major), alpha = 0.7) +
  scale_y_log10() +
  labs(
    title = "Pause Duration by Syntactic Context",
    x = "Syntactic Context",
    y = "Pause Duration (ms, log scale)",
    fill = "Context"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave("output/pause_duration_by_syntactic_context.png", p1, 
       width = 10, height = 6, dpi = 300)

p2 <- ggplot(data_clean %>% filter(Process_Major != "Other"), 
             aes(x = reorder(Process_Major, Pause_ms, median), y = Pause_ms)) +
  geom_boxplot(aes(fill = Process_Major), alpha = 0.7) +
  scale_y_log10() +
  labs(
    title = "Pause Duration by Process Type",
    x = "Process Type",
    y = "Pause Duration (ms, log scale)",
    fill = "Process"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave("output/pause_duration_by_process_type.png", p2, 
       width = 8, height = 6, dpi = 300)

p3 <- ggplot(syntactic_counts, aes(x = reorder(Syntactic_Major, observed), y = observed)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = mean(syntactic_counts$observed), 
             linetype = "dashed", color = "red") +
  labs(
    title = "Frequency Distribution: Syntactic Contexts",
    x = "Syntactic Context",
    y = "Number of Pauses",
    caption = "Red line = expected frequency under equal distribution"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/frequency_distribution_syntactic.png", p3, 
       width = 10, height = 6, dpi = 300)

bootstrap_df <- data.frame(bootstrap_means = boot_mean$t[,1])

p4 <- ggplot(bootstrap_df, aes(x = bootstrap_means)) +
  geom_histogram(bins = 50, alpha = 0.7, fill = "steelblue", color = "white") +
  geom_vline(xintercept = boot_mean$t0, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = ci_mean$bca[4], color = "darkgreen", linetype = "dashed") +
  geom_vline(xintercept = ci_mean$bca[5], color = "darkgreen", linetype = "dashed") +
  labs(
    title = "Bootstrap Distribution of Mean Pause Duration",
    subtitle = "Red line = observed mean, Green lines = 95% CI bounds",
    x = "Mean Pause Duration (ms)",
    y = "Frequency"
  ) +
  theme_minimal()

library(ggplot2)
library(ggpattern)

context_counts <- c(163, 230, 190, 171, 38, 28, 14, 220, 8, 89, 69)
observed <- c(483, 481, 338, 112, 93, 0, 85, 229, 104, 1026, 100)
total_pauses <- sum(observed)
total_contexts <- sum(context_counts)
expected <- total_pauses * (context_counts / total_contexts)
context_labels <- c("End of NG", "Middle of NG", "End of VG", "Middle of VG", "End of AG", "Middle of AG", "End of PG", "Middle of PG", "End of Clause", "End of Sentence", "End of CG")
# Perform the permutation-based (Monte Carlo simulated) chi-square test
set.seed(123)  # For reproducibility
chi_sq_test <- chisq.test(x = observed, p = context_counts / total_contexts, simulate.p.value = TRUE, B = 9999)
print(chi_sq_test)

chi_sq_test$residuals
std_residuals <- chi_sq_test$residuals
print(std_residuals)
residuals <- chi_sq_test$residuals
print(residuals)
z_scores <- residuals
# Convert to p-values (two-tailed)
p_values_per_context <- 2 * (1 - pnorm(abs(z_scores)))
print(p_values_per_context)

p_values_with_labels <- data.frame(
  Context = context_labels,
  P_Value = p_values_per_context
)
# Print the p-values with context labels
print(p_values_with_labels)

p_values_with_labels <- data.frame(
  Context = context_labels,
  residuals = residuals
)
print(p_values_with_labels)

total_pauses <- sum(observed)
observed_proportions <- observed / total_pauses
expected_proportions <- (context_counts / sum(context_counts)) * total_pauses / total_pauses # Normalize to total pauses

df <- data.frame(
  Context = rep(context_labels, 2),
  Proportion = c(observed_proportions, expected_proportions),
  Type = rep(c("Observed", "Expected"), each = length(context_labels))
)

ggplot(df, aes(x = Context, y = Proportion, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Comparison of Observed vs. Expected Proportions of Pauses",
    y = "Proportion of Pauses",
    x = "Syntagmatic Context"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"), # Larger and bold x-axis labels
    axis.text.y = element_text(size = 14, face = "bold"), # Larger and bold y-axis labels
    axis.title.x = element_text(size = 16, face = "bold"), # Larger and bold x-axis title
    axis.title.y = element_text(size = 16, face = "bold"), # Larger and bold y-axis title
    plot.title = element_text(size = 18, face = "bold"), # Larger and bold plot title
    legend.title = element_blank(), # Remove the legend title
    legend.text = element_text(size = 12, face = "bold") # Adjust legend text size
  )


context_counts <- c(12, 65, 99, 28, 75, 5, 25, 16, 85, 70, 38, 181, 37, 28, 15, 5, 5, 77, 5, 30, 9, 5, 40, 118, 5, 22, 70, 5, 27, 84, 2, 19, 7, 42, 5, 68, 3, 11, 2, 3)
context_labels <- c("End of Actor", "End of Attribute", "End of Relational Process", "End of Beneficiary", "End of Circumstantial Adjunct", "End of Carrier", "End of Existent", "End of Existential Process", "End of Goal", "End of Hypotactic Conjunctive Adjunct", "End of Mood Adjunct", "End of Material Process", "End of Mental Process", "End of Paratactic Conjunctive Adjunct", "End of Phenomenon", "End of Receiver", "End of Sayer", "End of Scope", "End of Sensor", "End of Textual Adjunct", "End of Verbal Process", "End of Verbiage", "Middle of Actor", "Middle of Attribute", "Middle of Attributive Process", "Middle of Beneficiary", "Middle of Circumstantial Adjunct", "Middle of Carrier", "Middle of Existent", "Middle of Goal", "Middle of Mood Adjunct", "Middle of Material Process", "Middle of Mental Process", "Middle of Phenomenon", "Middle of Receiver", "Middle of Scope", "Middle of Sensor", "Middle of Textual Adjunct", "Middle of Verbal Process", "Middle of Verbiage")
observed <- c(53, 234, 22, 65, 290, 71, 38, 14, 260, 
                +               80, 95, 222, 83, 75, 72, 3, 0, 137, 21, 
                +               121, 30, 6, 42, 189, 22, 49, 125, 71, 60, 
                +               163, 20, 63, 20, 66, 0, 117, 12, 8, 12, 16)
total_contexts <- sum(context_counts)
expected <- total_pauses * (context_counts / total_contexts)


total_pauses <- sum(observed)
total_contexts <- sum(context_counts)
expected <- total_pauses * (context_counts / total_contexts)

# Perform the permutation-based (Monte Carlo simulated) chi-square test
set.seed(123)  # For reproducibility
chi_sq_test <- chisq.test(x = observed, 
                          p = context_counts / total_contexts, 
                          simulate.p.value = TRUE, 
                          B = 9999)

# Output the results (now with simulated/permutation p-value)
print(chi_sq_test)

chi_sq_test$residuals
std_residuals <- chi_sq_test$residuals
print(std_residuals)
residuals <- chi_sq_test$residuals
print(residuals)
z_scores <- residuals
# Convert to p-values (two-tailed)
p_values_per_context <- 2 * (1 - pnorm(abs(z_scores)))
print(p_values_per_context)

p_values_with_labels <- data.frame(
  Context = context_labels,
  P_Value = p_values_per_context
)
# Print the p-values with context labels
print(p_values_with_labels)

p_values_with_labels <- data.frame(
  Context = context_labels,
  residuals = residuals
)
print(p_values_with_labels)

total_pauses <- sum(observed)
observed_proportions <- observed / total_pauses
expected_proportions <- (context_counts / sum(context_counts)) * total_pauses / total_pauses # Normalize to total pauses

df <- data.frame(
  Context = rep(context_labels, 2),
  Proportion = c(observed_proportions, expected_proportions),
  Type = rep(c("Observed", "Expected"), each = length(context_labels))
)

ggplot(df, aes(x = Context, y = Proportion, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Comparison of Observed vs. Expected Proportions of Pauses",
    y = "Proportion of Pauses",
    x = "Functional Context"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"), # Larger and bold x-axis labels
    axis.text.y = element_text(size = 14, face = "bold"), # Larger and bold y-axis labels
    axis.title.x = element_text(size = 16, face = "bold"), # Larger and bold x-axis title
    axis.title.y = element_text(size = 16, face = "bold"), # Larger and bold y-axis title
    plot.title = element_text(size = 18, face = "bold"), # Larger and bold plot title
    legend.title = element_blank(), # Remove the legend title
    legend.text = element_text(size = 12, face = "bold") # Adjust legend text size
  )















