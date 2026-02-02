# Study: PAUSE FOR THOUGHT: SYSTEMIC FUNCTIONAL UNITS AND THE DYNAMICS OF WRITING
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

data <- read.csv("/home/ubuntu/upload/specificS2.csv", stringsAsFactors = FALSE)
data_with_pauses <- data[!is.na(data$Pause), ]

distances_corrected <- c()
pauses_corrected <- c()
positions_corrected <- c()
participants_corrected <- c()
row_indices <- c()

before_data <- data_with_pauses[!is.na(data_with_pauses$Distance.to.theme.before), ]
if(nrow(before_data) > 0) {
  distances_corrected <- c(distances_corrected, -before_data$Distance.to.theme.before)  # Negative for "before"
  pauses_corrected <- c(pauses_corrected, before_data$Pause)
  positions_corrected <- c(positions_corrected, rep("Before_TU", nrow(before_data)))
  participants_corrected <- c(participants_corrected, before_data$file_id)
  row_indices <- c(row_indices, which(!is.na(data_with_pauses$Distance.to.theme.before)))
}
for(i in 2:nrow(data_with_pauses)) {  # Start from row 2 since we need previous row
  if(!is.na(data_with_pauses$Distance.to.theme.after[i])) {
    # The distance in row i relates to the pause in row i-1
    distances_corrected <- c(distances_corrected, data_with_pauses$Distance.to.theme.after[i])
    pauses_corrected <- c(pauses_corrected, data_with_pauses$Pause[i-1])  # Previous row's pause
    positions_corrected <- c(positions_corrected, "After_TU")
    participants_corrected <- c(participants_corrected, data_with_pauses$file_id[i-1])  # Previous row's participant
    row_indices <- c(row_indices, i-1)  # Store the actual pause row index
  }
}
boundary_type_corrected <- ifelse(distances_corrected == 0 & positions_corrected == "Before_TU", "Boundary_Before_TU",
                         ifelse(distances_corrected == 0 & positions_corrected == "After_TU", "Boundary_After_TU",
                         ifelse(positions_corrected == "Before_TU", "NonBoundary_Before_TU", "NonBoundary_After_TU")))

tu_types <- c("topical", "evaluative", "thematized.CA", "marked", "enhanced", "textual")

theme_categories_corrected <- rep("Unknown", length(distances_corrected))
for(i in 1:length(distances_corrected)) {
  if(positions_corrected[i] == "Before_TU") {
    # For before TU, use the same row
    row_idx <- which(!is.na(data_with_pauses$Distance.to.theme.before))[sum(positions_corrected[1:i] == "Before_TU")]
  } else {
    # For after TU, use the row that contains the distance measure
    # Find which after_TU this is
    after_count <- sum(positions_corrected[1:i] == "After_TU")
    after_rows <- which(!is.na(data_with_pauses$Distance.to.theme.after))
    if(after_count <= length(after_rows)) {
      row_idx <- after_rows[after_count]
    } else {
      row_idx <- NA
    }
  }
  
  if(!is.na(row_idx) && row_idx <= nrow(data_with_pauses)) {
    present_types <- c()
    for(type in tu_types) {
      if(!is.na(data_with_pauses[row_idx, type]) && data_with_pauses[row_idx, type] > 0) {
        present_types <- c(present_types, type)
      }
    }
    
    if(length(present_types) > 0) {
      theme_categories_corrected[i] <- paste(present_types, collapse = "+")
    }
  }
}

theme_counts_corrected <- table(theme_categories_corrected)
print(theme_counts_corrected)

boundary_vs_nonboundary <- ifelse(distances_corrected == 0, "Boundary", "Non-boundary")
boundary_counts <- table(boundary_vs_nonboundary)
print(boundary_counts)

chi_boundary <- chisq.test(boundary_counts)
boundary_only <- boundary_type_corrected[boundary_type_corrected %in% c("Boundary_Before_TU", "Boundary_After_TU")]
boundary_preference_counts <- table(boundary_only)
cat("Observed boundary pause frequencies:\n")
print(boundary_preference_counts)

if(length(boundary_preference_counts) > 1) {
  chi_boundary_pref <- chisq.test(boundary_preference_counts)
  cat("\nChi-square test results:\n")
  cat("X-squared =", chi_boundary_pref$statistic, ", p-value =", chi_boundary_pref$p.value, "\n")
  cat("Interpretation: Tests if there's a preference for before vs after TU boundary pauses\n\n")
}

all_boundary_counts <- table(boundary_type_corrected)
cat("All boundary category frequencies:\n")
print(all_boundary_counts)

chi_all_boundaries <- chisq.test(all_boundary_counts)
ks_test <- ks.test(distances_corrected, "punif", min(distances_corrected), max(distances_corrected))
boundary_pauses <- pauses_corrected[distances_corrected == 0]
nonboundary_pauses <- pauses_corrected[distances_corrected != 0]

wilcox_boundary_duration <- wilcox.test(boundary_pauses, nonboundary_pauses)
before_tu_pauses <- pauses_corrected[positions_corrected == "Before_TU"]
after_tu_pauses <- pauses_corrected[positions_corrected == "After_TU"]

wilcox_position_duration <- wilcox.test(before_tu_pauses, after_tu_pauses)
before_boundary_pauses <- pauses_corrected[boundary_type_corrected == "Boundary_Before_TU"]
after_boundary_pauses <- pauses_corrected[boundary_type_corrected == "Boundary_After_TU"]

if(length(before_boundary_pauses) > 0 && length(after_boundary_pauses) > 0) {
  wilcox_boundary_position <- wilcox.test(before_boundary_pauses, after_boundary_pauses)
  cat("Before vs After TU boundary pause durations:\n")
  cat("W =", wilcox_boundary_position$statistic, ", p-value =", wilcox_boundary_position$p.value, "\n")
  cat("Medians: Before TU boundary =", median(before_boundary_pauses), ", After TU boundary =", median(after_boundary_pauses), "\n\n")
}

kruskal_all <- kruskal.test(pauses_corrected ~ boundary_type_corrected)
single_themes <- theme_categories_corrected[theme_categories_corrected %in% tu_types]
if(length(single_themes) > 0) {
  single_theme_counts <- table(single_themes)
  cat("Single theme type distribution:\n")
  print(single_theme_counts)
  
  if(length(single_theme_counts) > 1) {
    chi_themes <- chisq.test(single_theme_counts)
    cat("\nChi-square test (theme type distribution): X² =", chi_themes$statistic, ", p-value =", chi_themes$p.value, "\n")
  }
  
  # Test pause durations by theme type
  if(length(unique(single_themes)) > 1) {
    theme_pauses <- pauses_corrected[theme_categories_corrected %in% tu_types]
    kruskal_themes <- kruskal.test(theme_pauses ~ single_themes)
    cat("Kruskal-Wallis test (pause durations by theme type): H =", kruskal_themes$statistic, ", p-value =", kruskal_themes$p.value, "\n")
  }
}

cor_pearson <- cor.test(distances_corrected, pauses_corrected, method = "pearson")
cor_spearman <- cor.test(distances_corrected, pauses_corrected, method = "spearman")
cat("Spearman correlation:\n")
cat("rho =", cor_spearman$estimate, ", p-value =", cor_spearman$p.value, "\n\n")
before_distances <- distances_corrected[positions_corrected == "Before_TU"]
before_pauses <- pauses_corrected[positions_corrected == "Before_TU"]
after_distances <- distances_corrected[positions_corrected == "After_TU"]
after_pauses <- pauses_corrected[positions_corrected == "After_TU"]

cor_before <- cor.test(before_distances, before_pauses, method = "spearman")
cor_after <- cor.test(after_distances, after_pauses, method = "spearman")
distance_bins <- cut(distances_corrected, 
                    breaks = c(-Inf, -10, -1, 0.5, 10, Inf), 
                    labels = c("Far_Before", "Near_Before", "Boundary", "Near_After", "Far_After"),
                    include.lowest = TRUE, right = FALSE)

bin_counts <- table(distance_bins)
cat("Pause distribution by distance bins:\n")
print(bin_counts)

chi_bins <- chisq.test(bin_counts)
cat("\nChi-square test for distance bin distribution:\n")
cat("X-squared =", chi_bins$statistic, ", p-value =", chi_bins$p.value, "\n\n")

pooled_sd1 <- sqrt(((length(boundary_pauses)-1)*var(boundary_pauses) + 
                    (length(nonboundary_pauses)-1)*var(nonboundary_pauses)) / 
                   (length(boundary_pauses) + length(nonboundary_pauses) - 2))
cohens_d1 <- (mean(boundary_pauses) - mean(nonboundary_pauses)) / pooled_sd1
pooled_sd2 <- sqrt(((length(before_tu_pauses)-1)*var(before_tu_pauses) + 
                    (length(after_tu_pauses)-1)*var(after_tu_pauses)) / 
                   (length(before_tu_pauses) + length(after_tu_pauses) - 2))
cohens_d2 <- (mean(after_tu_pauses) - mean(before_tu_pauses)) / pooled_sd2
cat("Cohen's d (After vs Before TU):", cohens_d2, "\n")
(length(before_boundary_pauses) > 0 && length(after_boundary_pauses) > 0) {
  pooled_sd3 <- sqrt(((length(before_boundary_pauses)-1)*var(before_boundary_pauses) + 
                      (length(after_boundary_pauses)-1)*var(after_boundary_pauses)) / 
                     (length(before_boundary_pauses) + length(after_boundary_pauses) - 2))
  cohens_d3 <- (mean(after_boundary_pauses) - mean(before_boundary_pauses)) / pooled_sd3
  cat("Cohen's d (After vs Before TU boundaries):", cohens_d3, "\n")
}
position_counts <- table(positions_corrected)
for(position in names(position_counts)) {
  cat(position, ":", position_counts[position], 
      "(", round(position_counts[position]/sum(position_counts)*100, 1), "%)\n")
}

data <- read.csv("specificS2.csv", stringsAsFactors = FALSE)
data_with_pauses <- data[!is.na(data$Pause), ]

thematic_with_pause <- data$Thematic.element.with.pause
thematic_without_pause <- data$Thematic.element.without.pause
non_thematic_with_pause <- data$Non.Themetic.with.pause
non_thematic_without_pause <- data$Non.Thematic.without.pause

thematic_with <- sum(thematic_with_pause, na.rm = TRUE)
thematic_without <- sum(thematic_without_pause, na.rm = TRUE)
non_thematic_with <- sum(non_thematic_with_pause, na.rm = TRUE)
non_thematic_without <- sum(non_thematic_without_pause, na.rm = TRUE)

categories <- c("Thematic\nWith Pause", "Thematic\nWithout Pause", 
                "Non-Thematic\nWith Pause", "Non-Thematic\nWithout Pause")
counts <- c(thematic_with, thematic_without, non_thematic_with, non_thematic_without)
colors <- c("lightblue", "lightcoral", "lightgreen", "lightyellow")
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))
barplot(counts, names.arg = categories, 
        main = "Figure 4a: Raw Counts of TU vs Non-TU Elements",
        ylab = "Count",
        col = colors,
        las = 2,
        cex.names = 0.8)

for(i in 1:length(counts)) {
  text(i * 1.2 - 0.5, counts[i] + 20, counts[i], cex = 0.9, font = 2)
}
total_thematic <- thematic_with + thematic_without
total_non_thematic <- non_thematic_with + non_thematic_without
prop_thematic_with <- thematic_with / total_thematic * 100
prop_thematic_without <- thematic_without / total_thematic * 100
prop_non_thematic_with <- non_thematic_with / total_non_thematic * 100
prop_non_thematic_without <- non_thematic_without / total_non_thematic * 100
prop_matrix <- matrix(c(prop_thematic_with, prop_thematic_without,
                        prop_non_thematic_with, prop_non_thematic_without),
                      nrow = 2, byrow = TRUE)
colnames(prop_matrix) <- c("With Pause", "Without Pause")
rownames(prop_matrix) <- c("Thematic", "Non-Thematic")

barplot(t(prop_matrix), 
        main = "Figure 4b: Proportions of TU vs Non-TU Elements",
        ylab = "Percentage",
        col = c("lightblue", "lightcoral"),
        legend = TRUE,
        beside = TRUE)

text(1.5, prop_thematic_with/2, paste0(round(prop_thematic_with, 1), "%"), cex = 0.9, font = 2)
text(2.5, prop_thematic_without/2, paste0(round(prop_thematic_without, 1), "%"), cex = 0.9, font = 2)
text(4.5, prop_non_thematic_with/2, paste0(round(prop_non_thematic_with, 1), "%"), cex = 0.9, font = 2)
text(5.5, prop_non_thematic_without/2, paste0(round(prop_non_thematic_without, 1), "%"), cex = 0.9, font = 2)
text(3, 80, "X² = 33.14***", cex = 1.2, font = 2)
text(3, 75, "Cramer's V = 0.12", cex = 1, font = 2)

dev.off()
pause_categories <- c()
pause_row_indices <- c()
for(i in 1:nrow(data_with_pauses)) {
  current_row <- data_with_pauses[i, ]
  
  condition1 <- FALSE  # Distance.to.theme.after = 0 AND one row higher 'end'
  condition2 <- FALSE  # Distance.to.theme.before = 0 AND same row 'end'
  condition3 <- FALSE  # Distance.to.theme.before = 0 AND same row 'mid'
  
  if(!is.na(current_row$Distance.to.theme.after) && current_row$Distance.to.theme.after == 0) {
    if(i > 1) {
      previous_row <- data_with_pauses[i-1, ]
      if(!is.na(previous_row$Syntactic.Context.of.the.pause) && 
         grepl("end", previous_row$Syntactic.Context.of.the.pause, ignore.case = TRUE)) {
        condition1 <- TRUE
      }
    }
  }
  
  if(!is.na(current_row$Distance.to.theme.before) && current_row$Distance.to.theme.before == 0) {
    if(!is.na(current_row$Syntactic.Context.of.the.pause) && 
       grepl("end", current_row$Syntactic.Context.of.the.pause, ignore.case = TRUE)) {
      condition2 <- TRUE
    }
  }
  
  if(!is.na(current_row$Distance.to.theme.before) && current_row$Distance.to.theme.before == 0) {
    if(!is.na(current_row$Syntactic.Context.of.the.pause) && 
       grepl("mid", current_row$Syntactic.Context.of.the.pause, ignore.case = TRUE)) {
      condition3 <- TRUE
    }
  }
  
  category <- "Non_Boundary"
  if(condition1 && condition2) {
    category <- "Both_Before_After_Boundary"
  } else if(condition1) {
    category <- "Before_Boundary"
  } else if(condition2) {
    category <- "After_Boundary"
  } else if(condition3) {
    category <- "Middle_Thematic_Unit"
  } else if(!is.na(current_row$Distance.to.theme.after) || !is.na(current_row$Distance.to.theme.before)) {
    category <- "Non_Boundary"
  } else {
    next
  }
  
  pause_categories <- c(pause_categories, category)
  pause_row_indices <- c(pause_row_indices, i)
}

before_boundary_indices <- which(pause_categories %in% c("Before_Boundary", "Both_Before_After_Boundary"))
before_boundary_thematic_with <- 0
before_boundary_thematic_without <- 0
before_boundary_non_thematic_with <- 0
before_boundary_non_thematic_without <- 0

for(idx in before_boundary_indices) {
  row_idx <- pause_row_indices[idx]
  current_row <- data_with_pauses[row_idx, ]
  
  before_boundary_thematic_with <- before_boundary_thematic_with + 
    ifelse(is.na(current_row$Thematic.element.with.pause), 0, current_row$Thematic.element.with.pause)
  before_boundary_thematic_without <- before_boundary_thematic_without + 
    ifelse(is.na(current_row$Thematic.element.without.pause), 0, current_row$Thematic.element.without.pause)
  before_boundary_non_thematic_with <- before_boundary_non_thematic_with + 
    ifelse(is.na(current_row$Non.Themetic.with.pause), 0, current_row$Non.Themetic.with.pause)
  before_boundary_non_thematic_without <- before_boundary_non_thematic_without + 
    ifelse(is.na(current_row$Non.Thematic.without.pause), 0, current_row$Non.Thematic.without.pause)
}
after_boundary_indices <- which(pause_categories %in% c("After_Boundary", "Both_Before_After_Boundary"))
after_boundary_thematic_with <- 0
after_boundary_thematic_without <- 0
after_boundary_non_thematic_with <- 0
after_boundary_non_thematic_without <- 0

for(idx in after_boundary_indices) {
  row_idx <- pause_row_indices[idx]
  current_row <- data_with_pauses[row_idx, ]
  
  after_boundary_thematic_with <- after_boundary_thematic_with + 
    ifelse(is.na(current_row$Thematic.element.with.pause), 0, current_row$Thematic.element.with.pause)
  after_boundary_thematic_without <- after_boundary_thematic_without + 
    ifelse(is.na(current_row$Thematic.element.without.pause), 0, current_row$Thematic.element.without.pause)
  after_boundary_non_thematic_with <- after_boundary_non_thematic_with + 
    ifelse(is.na(current_row$Non.Themetic.with.pause), 0, current_row$Non.Themetic.with.pause)
  after_boundary_non_thematic_without <- after_boundary_non_thematic_without + 
    ifelse(is.na(current_row$Non.Thematic.without.pause), 0, current_row$Non.Thematic.without.pause)
}
png("figure5_boundary_analysis.png", width = 14, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
before_counts <- c(before_boundary_thematic_with, before_boundary_thematic_without,
                   before_boundary_non_thematic_with, before_boundary_non_thematic_without)
before_categories <- c("Thematic\nWith Pause", "Thematic\nWithout Pause",
                       "Non-Thematic\nWith Pause", "Non-Thematic\nWithout Pause")

barplot(before_counts, names.arg = before_categories,
        main = "Figure 5a: Before TU Boundaries - Raw Counts",
        ylab = "Count",
        col = colors,
        las = 2,
        cex.names = 0.8)

for(i in 1:length(before_counts)) {
  text(i * 1.2 - 0.5, before_counts[i] + 5, before_counts[i], cex = 0.9, font = 2)
}
total_before_thematic <- before_boundary_thematic_with + before_boundary_thematic_without
total_before_non_thematic <- before_boundary_non_thematic_with + before_boundary_non_thematic_without

prop_before_thematic_with <- before_boundary_thematic_with / total_before_thematic * 100
prop_before_non_thematic_with <- before_boundary_non_thematic_with / total_before_non_thematic * 100

before_prop_matrix <- matrix(c(prop_before_thematic_with, 100 - prop_before_thematic_with,
                               prop_before_non_thematic_with, 100 - prop_before_non_thematic_with),
                             nrow = 2, byrow = TRUE)
colnames(before_prop_matrix) <- c("With Pause", "Without Pause")
rownames(before_prop_matrix) <- c("Thematic", "Non-Thematic")

barplot(t(before_prop_matrix),
        main = "Figure 5b: Before TU Boundaries - Proportions",
        ylab = "Percentage",
        col = c("lightblue", "lightcoral"),
        legend = TRUE,
        beside = TRUE)

text(3, 50, "X² = 206.46***", cex = 1.1, font = 2)
text(3, 45, "V = 0.554", cex = 1, font = 2)
after_counts <- c(after_boundary_thematic_with, after_boundary_thematic_without,
                  after_boundary_non_thematic_with, after_boundary_non_thematic_without)

barplot(after_counts, names.arg = before_categories,
        main = "Figure 5c: After TU Boundaries - Raw Counts",
        ylab = "Count",
        col = colors,
        las = 2,
        cex.names = 0.8)

for(i in 1:length(after_counts)) {
  text(i * 1.2 - 0.5, after_counts[i] + 3, after_counts[i], cex = 0.9, font = 2)
}
total_after_thematic <- after_boundary_thematic_with + after_boundary_thematic_without
total_after_non_thematic <- after_boundary_non_thematic_with + after_boundary_non_thematic_without

prop_after_thematic_with <- after_boundary_thematic_with / total_after_thematic * 100
prop_after_non_thematic_with <- after_boundary_non_thematic_with / total_after_non_thematic * 100

after_prop_matrix <- matrix(c(prop_after_thematic_with, 100 - prop_after_thematic_with,
                              prop_after_non_thematic_with, 100 - prop_after_non_thematic_with),
                            nrow = 2, byrow = TRUE)
colnames(after_prop_matrix) <- c("With Pause", "Without Pause")
rownames(after_prop_matrix) <- c("Thematic", "Non-Thematic")

barplot(t(after_prop_matrix),
        main = "Figure 5d: After TU Boundaries - Proportions",
        ylab = "Percentage",
        col = c("lightblue", "lightcoral"),
        legend = TRUE,
        beside = TRUE)

text(3, 50, "X² = 114.37***", cex = 1.1, font = 2)
text(3, 45, "V = 0.598", cex = 1, font = 2)

dev.off()














