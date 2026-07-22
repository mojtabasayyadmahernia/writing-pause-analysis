# ============================================================================
# functions.R
# All helper functions for the pause analysis, defined in one place and
# sourced first by run_all.R so they exist before any script calls them.
# ============================================================================

# --- Effect size for a mixed model: marginal and conditional R^2 -------------
# Marginal R^2  = variance explained by fixed effects alone
# Conditional R^2 = variance explained by fixed + random effects
calculate_effect_size <- function(model) {
  r2 <- MuMIn::r.squaredGLMM(model)
  list(
    marginal_r2    = r2[1],
    conditional_r2 = r2[2]
  )
}

# --- Consistent p-value formatting for results strings -----------------------
format_p <- function(p) {
  ifelse(p < 0.001, "< 0.001",
         ifelse(p < 0.01, sprintf("= %.3f", p),
                sprintf("= %.2f", p)))
}

# --- Turn an lmerTest anova() into a readable results line -------------------
# NOTE: requires the model to have been fit with lmerTest attached, so that
# anova() returns NumDF / DenDF / Pr(>F). run_all.R loads lmerTest.
create_results_text <- function(model_anova, effect_name) {
  f_stat <- model_anova$`F value`[1]
  df1    <- model_anova$NumDF[1]
  df2    <- round(model_anova$DenDF[1], 1)
  p_val  <- model_anova$`Pr(>F)`[1]

  sprintf("%s: F(%d, %.1f) = %.2f, p %s",
          effect_name, df1, df2, f_stat, format_p(p_val))
}

# --- Cramer's V from a one-way chi-square goodness-of-fit test ---------------
cramers_v <- function(chi_stat, n, k) {
  sqrt(chi_stat / (n * (k - 1)))
}

# --- Bootstrap statistic: mean pause duration (for boot::boot) ---------------
bootstrap_mean <- function(data, indices) {
  mean(data[indices, ]$Pause_ms)
}

# --- Bootstrap statistic: difference in mean duration between two levels -----
# Compares the two syntactic levels named in `levels_to_compare`. Returns the
# difference in means, or NA if the resample does not contain both levels.
bootstrap_effect_size <- function(data, indices,
                                  levels_to_compare = c("End Sentence", "Mid Word")) {
  sample_data <- data[indices, ]

  means <- sample_data %>%
    filter(Syntactic_Major %in% levels_to_compare) %>%
    group_by(Syntactic_Major) %>%
    summarise(mean_duration = mean(Pause_ms), .groups = "drop")

  if (nrow(means) == 2) {
    means$mean_duration[1] - means$mean_duration[2]
  } else {
    NA_real_
  }
}

# --- Bootstrap statistic: cluster bootstrap by participant -------------------
# Resamples PARTICIPANTS (not individual pauses), so the resulting interval
# reflects that generalisation is across writers, not across keystrokes.
bootstrap_participant_mean <- function(data, indices) {
  # `indices` is supplied by boot() but ignored: we resample participants
  # ourselves rather than resampling rows.
  unique_participants  <- unique(data$Participant_ID)
  sampled_participants <- sample(unique_participants, replace = TRUE)

  # Build the resampled dataset by stacking each sampled participant's rows.
  sampled_data <- do.call(
    rbind,
    lapply(sampled_participants, function(p) data[data$Participant_ID == p, ])
  )

  participant_means <- sampled_data %>%
    group_by(Participant_ID) %>%
    summarise(mean_duration = mean(Pause_ms), .groups = "drop")

  mean(participant_means$mean_duration)
}

# --- Pooled-SD Cohen's d for two independent samples -------------------------
cohens_d <- function(x, y) {
  nx <- length(x); ny <- length(y)
  pooled_sd <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  (mean(x) - mean(y)) / pooled_sd
}

# --- One-way context frequency analysis (de-duplicated) ----------------------
# Runs a Monte Carlo chi-square goodness-of-fit test of `observed` counts
# against the proportions implied by `context_counts`, computes standardised
# residuals and per-context two-tailed p-values, and (optionally) saves a
# grouped observed-vs-expected proportion plot. Replaces the two near-identical
# copy-pasted blocks in the original script.
run_context_frequency <- function(observed, context_counts, context_labels,
                                   title, x_lab, out_png = NULL, seed = 123) {
  stopifnot(length(observed) == length(context_counts),
            length(observed) == length(context_labels))

  set.seed(seed)
  expected_p <- context_counts / sum(context_counts)
  chi <- chisq.test(x = observed, p = expected_p,
                    simulate.p.value = TRUE, B = 9999)

  chi_residuals <- chi$residuals
  p_per_context <- 2 * (1 - pnorm(abs(chi_residuals)))

  results <- data.frame(
    Context     = context_labels,
    Observed    = observed,
    Residual    = as.numeric(chi_residuals),
    P_Value     = p_per_context
  )

  total <- sum(observed)
  plot_df <- data.frame(
    Context    = rep(context_labels, 2),
    Proportion = c(observed / total, expected_p),
    Type       = rep(c("Observed", "Expected"), each = length(context_labels))
  )

  p <- ggplot(plot_df, aes(x = Context, y = Proportion, fill = Type)) +
    geom_col(position = "dodge") +
    labs(title = title, x = x_lab, y = "Proportion of pauses") +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
      legend.title = element_blank()
    )

  if (!is.null(out_png)) {
    ggsave(out_png, p, width = 11, height = 6, dpi = 150, bg = "white")
  }

  list(chi = chi, results = results, plot = p)
}
