# ============================================================================
# 08_figures_boundary.R
# Figures for the thematic-unit analysis (thematic vs non-thematic elements,
# and before/after boundary counts). All chi-square / Cramer's V annotations
# are COMPUTED from the data, not typed in, so they can never drift from the
# figures. Depends on 07_boundary.R and its tu_data / tu_pauses.
# ============================================================================

# --- Aggregate thematic vs non-thematic element counts -----------------------
thematic_with     <- sum(tu_data$Thematic.element.with.pause,    na.rm = TRUE)
thematic_without  <- sum(tu_data$Thematic.element.without.pause, na.rm = TRUE)
non_thematic_with    <- sum(tu_data$Non.Themetic.with.pause,     na.rm = TRUE)
non_thematic_without <- sum(tu_data$Non.Thematic.without.pause,  na.rm = TRUE)

# Contingency table: element type x pause presence
tu_table <- matrix(
  c(thematic_with, thematic_without, non_thematic_with, non_thematic_without),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("Thematic", "Non-Thematic"), c("With Pause", "Without Pause"))
)

chi_tu <- chisq.test(tu_table)
v_tu   <- sqrt(as.numeric(chi_tu$statistic) / sum(tu_table))

# --- Figure 4: thematic vs non-thematic --------------------------------------
png(here("output", "figures", "figure4_thematic.png"),
    width = 12, height = 6, units = "in", res = 150)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

counts     <- c(thematic_with, thematic_without, non_thematic_with, non_thematic_without)
categories <- c("Thematic\nWith", "Thematic\nWithout", "Non-Thematic\nWith", "Non-Thematic\nWithout")
fig_colors <- c("lightblue", "lightcoral", "lightgreen", "lightyellow")

barplot(counts, names.arg = categories,
        main = "Figure 4a: Counts of TU vs non-TU elements",
        ylab = "Count", col = fig_colors, las = 2, cex.names = 0.8)

prop_matrix <- prop.table(tu_table, margin = 1) * 100
barplot(t(prop_matrix), beside = TRUE, legend = TRUE,
        main = "Figure 4b: Proportions with vs without pause",
        ylab = "Percentage", col = c("lightblue", "lightcoral"))

# Annotation computed from the test, not hardcoded:
annotation <- sprintf("X-sq = %.2f, p %s;  V = %.2f",
                      chi_tu$statistic, format_p(chi_tu$p.value), v_tu)
mtext(annotation, side = 3, line = -1.5, outer = TRUE, font = 2)
dev.off()

# --- Figure 5: before / after boundary counts --------------------------------
# Sum thematic/non-thematic element counts for rows at before- and after-TU
# boundaries, then plot counts and proportions with computed test annotations.
sum_col <- function(idx, col) {
  vals <- tu_pauses[idx, col]
  sum(ifelse(is.na(vals), 0, vals))
}

before_idx <- which(!is.na(tu_pauses$Distance.to.theme.before) &
                    tu_pauses$Distance.to.theme.before == 0)
after_idx  <- which(!is.na(tu_pauses$Distance.to.theme.after) &
                    tu_pauses$Distance.to.theme.after == 0)

make_boundary_table <- function(idx) {
  matrix(
    c(sum_col(idx, "Thematic.element.with.pause"),
      sum_col(idx, "Thematic.element.without.pause"),
      sum_col(idx, "Non.Themetic.with.pause"),
      sum_col(idx, "Non.Thematic.without.pause")),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("Thematic", "Non-Thematic"), c("With", "Without"))
  )
}

before_table <- make_boundary_table(before_idx)
after_table  <- make_boundary_table(after_idx)

safe_chi <- function(tab) tryCatch(chisq.test(tab), error = function(e) NULL)
chi_before <- safe_chi(before_table)
chi_after  <- safe_chi(after_table)

png(here("output", "figures", "figure5_boundary.png"),
    width = 14, height = 10, units = "in", res = 150)
par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))

barplot(as.vector(t(before_table)),
        names.arg = c("Them.\nWith", "Them.\nWithout", "Non.\nWith", "Non.\nWithout"),
        main = "Figure 5a: Before TU boundaries - counts",
        ylab = "Count", col = fig_colors, las = 2, cex.names = 0.8)

barplot(t(prop.table(before_table, 1) * 100), beside = TRUE, legend = TRUE,
        main = "Figure 5b: Before TU boundaries - proportions",
        ylab = "Percentage", col = c("lightblue", "lightcoral"))
if (!is.null(chi_before)) {
  v_before <- sqrt(as.numeric(chi_before$statistic) / sum(before_table))
  mtext(sprintf("X-sq = %.2f, p %s;  V = %.2f",
                chi_before$statistic, format_p(chi_before$p.value), v_before),
        side = 3, line = -1, font = 2)
}

barplot(as.vector(t(after_table)),
        names.arg = c("Them.\nWith", "Them.\nWithout", "Non.\nWith", "Non.\nWithout"),
        main = "Figure 5c: After TU boundaries - counts",
        ylab = "Count", col = fig_colors, las = 2, cex.names = 0.8)

barplot(t(prop.table(after_table, 1) * 100), beside = TRUE, legend = TRUE,
        main = "Figure 5d: After TU boundaries - proportions",
        ylab = "Percentage", col = c("lightblue", "lightcoral"))
if (!is.null(chi_after)) {
  v_after <- sqrt(as.numeric(chi_after$statistic) / sum(after_table))
  mtext(sprintf("X-sq = %.2f, p %s;  V = %.2f",
                chi_after$statistic, format_p(chi_after$p.value), v_after),
        side = 3, line = -1, font = 2)
}
dev.off()
