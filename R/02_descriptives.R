# ============================================================================
# 02_descriptives.R
# Summary tables of pause duration, overall and by category. Writes four CSVs
# to output/. Depends on data_clean (01_clean.R).
# ============================================================================

desc_overall <- data_clean %>%
  summarise(
    N         = n(),
    Mean_ms   = round(mean(Pause_ms), 1),
    Median_ms = round(median(Pause_ms), 1),
    SD_ms     = round(sd(Pause_ms), 1),
    Min_ms    = round(min(Pause_ms), 1),
    Max_ms    = round(max(Pause_ms), 1),
    Q1_ms     = round(quantile(Pause_ms, 0.25), 1),
    Q3_ms     = round(quantile(Pause_ms, 0.75), 1)
  )

desc_syntactic <- data_clean %>%
  group_by(Syntactic_Major) %>%
  summarise(
    N         = n(),
    Mean_ms   = round(mean(Pause_ms), 1),
    Median_ms = round(median(Pause_ms), 1),
    SD_ms     = round(sd(Pause_ms), 1),
    .groups   = "drop"
  ) %>%
  arrange(desc(N))

desc_process <- data_clean %>%
  filter(Process_Major != "Other") %>%
  group_by(Process_Major) %>%
  summarise(
    N         = n(),
    Mean_ms   = round(mean(Pause_ms), 1),
    Median_ms = round(median(Pause_ms), 1),
    SD_ms     = round(sd(Pause_ms), 1),
    .groups   = "drop"
  ) %>%
  arrange(desc(N))

desc_participants <- data_clean %>%
  group_by(Participant_ID) %>%
  summarise(
    N_pauses     = n(),
    Mean_duration = round(mean(Pause_ms), 1),
    SD_duration   = round(sd(Pause_ms), 1),
    .groups       = "drop"
  )

write.csv(desc_overall,      here("output", "descriptive_overall.csv"),      row.names = FALSE)
write.csv(desc_syntactic,    here("output", "descriptive_syntactic.csv"),    row.names = FALSE)
write.csv(desc_process,      here("output", "descriptive_process.csv"),      row.names = FALSE)
write.csv(desc_participants, here("output", "descriptive_participants.csv"), row.names = FALSE)
