# ============================================================================
# 03_models.R
# Linear mixed-effects models of log pause duration, with participant random
# intercepts. Pauses are nested within writers, so a random intercept per
# participant keeps the pause-level data while accounting for that clustering.
# ============================================================================

# --- Syntactic context -------------------------------------------------------
lmm_syntactic <- lmer(
  Log_Pause_ms ~ Syntactic_Major + (1 | Participant_ID),
  data = data_clean,
  REML = TRUE
)

anova_syntactic         <- anova(lmm_syntactic)          # needs lmerTest (loaded in run_all)
effect_sizes_syntactic  <- calculate_effect_size(lmm_syntactic)
emmeans_syntactic       <- emmeans(lmm_syntactic, ~ Syntactic_Major)
pairs_syntactic         <- pairs(emmeans_syntactic, adjust = "bonferroni")
results_syntactic       <- create_results_text(anova_syntactic, "Syntactic context")

# --- Process type ------------------------------------------------------------
# Restrict to the main transitivity process types for this model.
data_process <- data_clean %>%
  filter(Process_Major %in% c("Material", "Mental", "Relational",
                              "Attributive", "Verbal", "Existential"))

lmm_process <- lmer(
  Log_Pause_ms ~ Process_Major + (1 | Participant_ID),
  data = data_process,
  REML = TRUE
)

anova_process        <- anova(lmm_process)
effect_sizes_process <- calculate_effect_size(lmm_process)
emmeans_process      <- emmeans(lmm_process, ~ Process_Major)
pairs_process        <- pairs(emmeans_process, adjust = "bonferroni")
results_process      <- create_results_text(anova_process, "Process type")

# --- Diagnostics for the syntactic model ------------------------------------
# Checking assumptions explicitly, rather than trusting defaults: residuals
# should be roughly normal with constant variance after the log transform.
png(here("output", "figures", "model_diagnostics_syntactic.png"),
    width = 12, height = 8, units = "in", res = 150)
par(mfrow = c(2, 2))
plot(fitted(lmm_syntactic), resid(lmm_syntactic),
     main = "Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2)
qqnorm(resid(lmm_syntactic), main = "Q-Q Plot of Residuals"); qqline(resid(lmm_syntactic))
plot(fitted(lmm_syntactic), sqrt(abs(resid(lmm_syntactic))),
     main = "Scale-Location", xlab = "Fitted", ylab = "sqrt(|Residuals|)")
plot(hatvalues(lmm_syntactic), resid(lmm_syntactic),
     main = "Residuals vs Leverage", xlab = "Leverage", ylab = "Residuals")
dev.off()
