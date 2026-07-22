# ============================================================================
# 06_figures.R
# Main ggplot figures for the duration analysis. All figures are saved to
# output/figures/ with white backgrounds (readable in GitHub dark mode) and
# finding-oriented titles. Depends on data_clean (01), the frequency counts
# (04), the bootstrap objects (05), ggplot2.
# ============================================================================

# --- Pause duration by syntactic context (hero figure) -----------------------
p_syntactic <- ggplot(
  data_clean,
  aes(x = reorder(Syntactic_Major, Pause_ms, median), y = Pause_ms)
) +
  geom_boxplot(aes(fill = Syntactic_Major), alpha = 0.7) +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Pauses lengthen at major syntactic boundaries",
    subtitle = "Pause duration by syntactic context (log scale)",
    x = NULL, y = "Pause duration (ms, log scale)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "pause_by_syntactic_context.png"),
       p_syntactic, width = 9, height = 5.5, dpi = 150, bg = "white")

# --- Pause duration by process type ------------------------------------------
p_process <- ggplot(
  data_clean %>% filter(Process_Major != "Other"),
  aes(x = reorder(Process_Major, Pause_ms, median), y = Pause_ms)
) +
  geom_boxplot(aes(fill = Process_Major), alpha = 0.7) +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Pause duration by process type",
    x = NULL, y = "Pause duration (ms, log scale)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "pause_by_process_type.png"),
       p_process, width = 8, height = 5.5, dpi = 150, bg = "white")

# --- Frequency distribution of syntactic contexts ----------------------------
p_frequency <- ggplot(
  syntactic_counts,
  aes(x = reorder(Syntactic_Major, observed), y = observed)
) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_hline(yintercept = mean(syntactic_counts$observed),
             linetype = "dashed", colour = "red") +
  coord_flip() +
  labs(
    title = "Frequency of pauses by syntactic context",
    x = NULL, y = "Number of pauses",
    caption = "Dashed line = mean frequency across contexts"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "frequency_syntactic.png"),
       p_frequency, width = 9, height = 5.5, dpi = 150, bg = "white")

# --- Bootstrap distribution of the mean -------------------------------------
bootstrap_df <- data.frame(bootstrap_means = boot_mean$t[, 1])

p_bootstrap <- ggplot(bootstrap_df, aes(x = bootstrap_means)) +
  geom_histogram(bins = 50, fill = "steelblue", colour = "white", alpha = 0.8) +
  geom_vline(xintercept = boot_mean$t0, colour = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = ci_mean$bca[4], colour = "darkgreen", linetype = "dashed") +
  geom_vline(xintercept = ci_mean$bca[5], colour = "darkgreen", linetype = "dashed") +
  labs(
    title = "Bootstrap distribution of mean pause duration",
    subtitle = "Red = observed mean; green = 95% BCa CI bounds",
    x = "Mean pause duration (ms)", y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "bootstrap_mean_distribution.png"),
       p_bootstrap, width = 9, height = 5.5, dpi = 150, bg = "white")
