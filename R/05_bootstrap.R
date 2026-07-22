# ============================================================================
# 05_bootstrap.R
# Bootstrap estimates for the mean pause duration and for the difference
# between two syntactic contexts. Includes a cluster (participant-level)
# bootstrap so uncertainty reflects generalisation across writers.
# Depends on data_clean (01), functions.R, boot.
# Seed is set centrally in run_all.R.
# ============================================================================

# --- Bootstrap of the overall mean pause duration ----------------------------
boot_mean <- boot(data_clean, bootstrap_mean, R = 10000)
ci_mean   <- boot.ci(boot_mean, type = "bca")

# --- Bootstrap of the End Sentence vs Mid Word difference --------------------
# Both levels now exist in Syntactic_Major (the original filter looked for
# "End Sentence", which the old cleaning step never created, so every
# replicate returned NA). BCa needs a non-degenerate distribution, so we
# use a percentile interval on the non-NA replicates for robustness.
boot_effect       <- boot(data_clean, bootstrap_effect_size, R = 10000)
boot_effect_clean <- boot_effect$t[!is.na(boot_effect$t)]
n_valid_effect    <- length(boot_effect_clean)
ci_effect         <- if (n_valid_effect > 0) {
  quantile(boot_effect_clean, c(0.025, 0.975))
} else {
  c(NA_real_, NA_real_)
}
message(sprintf("bootstrap_effect_size: %d of %d replicates valid.",
                n_valid_effect, length(boot_effect$t)))

# --- Cluster bootstrap: mean of participant means ----------------------------
boot_participant <- boot(data_clean, bootstrap_participant_mean, R = 5000)
ci_participant   <- boot.ci(boot_participant, type = "bca")
