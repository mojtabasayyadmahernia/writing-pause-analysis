# ============================================================================
# 07_boundary.R
# Thematic-unit (TU) boundary analysis, using specificS2.csv. Tests whether
# pauses cluster at TU boundaries and whether their duration differs by
# position. Non-parametric tests (Wilcoxon, Kruskal-Wallis, KS) plus
# correlations and Cohen's d. Depends on functions.R.
#
# This is a self-contained analysis on a different file from Parts A; its
# objects are prefixed `tu_` to avoid clashing with the duration analysis.
# ============================================================================

tu_data <- read.csv(
  here("data", "specificS2.csv"),
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)
tu_pauses <- tu_data[!is.na(tu_data$Pause), ]

# --- Build aligned distance / pause / position vectors -----------------------
# "before" measures live on the same row as the pause; "after" measures live
# on the row *following* the pause, so they are matched back to row i-1.
distances    <- c()
pauses       <- c()
positions    <- c()
participants <- c()

before_rows <- tu_pauses[!is.na(tu_pauses$Distance.to.theme.before), ]
if (nrow(before_rows) > 0) {
  distances    <- c(distances, -before_rows$Distance.to.theme.before)  # negative = before
  pauses       <- c(pauses, before_rows$Pause)
  positions    <- c(positions, rep("Before_TU", nrow(before_rows)))
  participants <- c(participants, before_rows$file_id)
}

for (i in 2:nrow(tu_pauses)) {
  if (!is.na(tu_pauses$Distance.to.theme.after[i])) {
    distances    <- c(distances, tu_pauses$Distance.to.theme.after[i])
    pauses       <- c(pauses, tu_pauses$Pause[i - 1])       # pause is on previous row
    positions    <- c(positions, "After_TU")
    participants <- c(participants, tu_pauses$file_id[i - 1])
  }
}

boundary_type <- ifelse(distances == 0 & positions == "Before_TU", "Boundary_Before_TU",
                 ifelse(distances == 0 & positions == "After_TU",  "Boundary_After_TU",
                 ifelse(positions == "Before_TU", "NonBoundary_Before_TU", "NonBoundary_After_TU")))

# --- Boundary vs non-boundary frequency --------------------------------------
boundary_vs_non <- ifelse(distances == 0, "Boundary", "Non-boundary")
boundary_counts <- table(boundary_vs_non)
chi_boundary    <- chisq.test(boundary_counts)

boundary_only            <- boundary_type[boundary_type %in% c("Boundary_Before_TU", "Boundary_After_TU")]
boundary_preference      <- table(boundary_only)
chi_boundary_preference  <- if (length(boundary_preference) > 1) chisq.test(boundary_preference) else NULL

all_boundary_counts <- table(boundary_type)
chi_all_boundaries  <- chisq.test(all_boundary_counts)

# --- Distance distribution ---------------------------------------------------
ks_uniform <- ks.test(distances, "punif", min(distances), max(distances))

distance_bins <- cut(distances,
                     breaks = c(-Inf, -10, -1, 0.5, 10, Inf),
                     labels = c("Far_Before", "Near_Before", "Boundary",
                                "Near_After", "Far_After"),
                     include.lowest = TRUE, right = FALSE)
bin_counts <- table(distance_bins)
chi_bins   <- chisq.test(bin_counts)

# --- Duration comparisons (non-parametric) -----------------------------------
boundary_pauses    <- pauses[distances == 0]
nonboundary_pauses <- pauses[distances != 0]
wilcox_boundary    <- wilcox.test(boundary_pauses, nonboundary_pauses)

before_tu_pauses <- pauses[positions == "Before_TU"]
after_tu_pauses  <- pauses[positions == "After_TU"]
wilcox_position  <- wilcox.test(before_tu_pauses, after_tu_pauses)

before_boundary_pauses <- pauses[boundary_type == "Boundary_Before_TU"]
after_boundary_pauses  <- pauses[boundary_type == "Boundary_After_TU"]
wilcox_boundary_position <- if (length(before_boundary_pauses) > 0 &&
                                length(after_boundary_pauses) > 0) {
  wilcox.test(before_boundary_pauses, after_boundary_pauses)
} else NULL

kruskal_boundary_type <- kruskal.test(pauses ~ factor(boundary_type))

# --- Correlations between distance and pause duration ------------------------
cor_pearson  <- cor.test(distances, pauses, method = "pearson")
cor_spearman <- cor.test(distances, pauses, method = "spearman")

# --- Effect sizes (Cohen's d) ------------------------------------------------
# FIX: the original had `(length(...) > 0 && ...) {` with no `if` here,
# a syntax error that stopped the whole script parsing.
d_boundary_vs_non <- cohens_d(boundary_pauses, nonboundary_pauses)
d_after_vs_before <- cohens_d(after_tu_pauses, before_tu_pauses)
d_boundary_pos    <- if (length(before_boundary_pauses) > 1 &&
                         length(after_boundary_pauses) > 1) {
  cohens_d(after_boundary_pauses, before_boundary_pauses)
} else NA_real_

# --- Collect boundary results for reporting ----------------------------------
boundary_results <- list(
  boundary_counts       = boundary_counts,
  chi_boundary          = chi_boundary,
  chi_all_boundaries    = chi_all_boundaries,
  ks_uniform            = ks_uniform,
  bin_counts            = bin_counts,
  chi_bins              = chi_bins,
  wilcox_boundary       = wilcox_boundary,
  wilcox_position       = wilcox_position,
  kruskal_boundary_type = kruskal_boundary_type,
  cor_spearman          = cor_spearman,
  d_boundary_vs_non     = d_boundary_vs_non,
  d_after_vs_before     = d_after_vs_before,
  d_boundary_pos        = d_boundary_pos
)
