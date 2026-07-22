# ============================================================================
# 01_clean.R
# Load final1.csv, derive pause measures and category variables, remove
# outliers. Produces `data_clean`, used by the descriptive, model, frequency,
# bootstrap, and figure scripts that follow.
# ============================================================================

# The file is Latin-1 encoded (not UTF-8) because the Burst column contains
# characters from the writers' texts. Declaring the encoding avoids a
# read error and mangled strings.
data_raw <- read.csv(
  here("data", "final1.csv"),
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)

data_clean <- data_raw %>%
  filter(!is.na(Pause), Pause > 0) %>%
  mutate(
    Pause_ms     = Pause * 1000,
    Log_Pause_ms = log(Pause_ms),

    Syntactic_Context  = factor(Syntactic.Context.of.the.pause),
    Functional_Context = factor(Functional.Context.of.the.pause),
    Process_Type       = factor(Type.of.process),
    Participant_ID     = factor(file_id),

    # Collapse the raw syntactic labels into major categories.
    # IMPORTANT: every level present in the data is matched here. The original
    # script matched only end NP / mid NP / end VP / mid word, which silently
    # dropped ~1,900 pauses (including all 1,197 "end sentence" pauses) to NA.
    # str_detect is order-sensitive, so more specific patterns come first:
    # "end sentence" and "end clause" are matched before the generic "end".
    Syntactic_Major = case_when(
      str_detect(Syntactic.Context.of.the.pause, "end sentence") ~ "End Sentence",
      str_detect(Syntactic.Context.of.the.pause, "end clause")   ~ "End Clause",
      str_detect(Syntactic.Context.of.the.pause, "end NP")       ~ "End NP",
      str_detect(Syntactic.Context.of.the.pause, "mid NP")       ~ "Mid NP",
      str_detect(Syntactic.Context.of.the.pause, "end VP")       ~ "End VP",
      str_detect(Syntactic.Context.of.the.pause, "mid VP")       ~ "Mid VP",
      str_detect(Syntactic.Context.of.the.pause, "end PP")       ~ "End PP",
      str_detect(Syntactic.Context.of.the.pause, "mid PP")       ~ "Mid PP",
      str_detect(Syntactic.Context.of.the.pause, "end CG")       ~ "End CG",
      str_detect(Syntactic.Context.of.the.pause, "end AG")       ~ "End AG",
      str_detect(Syntactic.Context.of.the.pause, "mid word")     ~ "Mid Word",
      TRUE                                                        ~ "Other"
    ),

    Process_Major = case_when(
      str_detect(tolower(Type.of.process), "material")    ~ "Material",
      str_detect(tolower(Type.of.process), "mental")      ~ "Mental",
      str_detect(tolower(Type.of.process), "relational")  ~ "Relational",
      str_detect(tolower(Type.of.process), "attributive") ~ "Attributive",
      str_detect(tolower(Type.of.process), "verbal")      ~ "Verbal",
      str_detect(tolower(Type.of.process), "existential") ~ "Existential",
      TRUE                                                 ~ "Other"
    )
  )

# Report how many rows fell through to "Other" before dropping/keeping them,
# so the decision is visible rather than silent.
n_other_syntactic <- sum(data_clean$Syntactic_Major == "Other")
message(sprintf("Syntactic_Major: %d of %d pauses did not match a defined category (-> 'Other').",
                n_other_syntactic, nrow(data_clean)))

# Outlier removal on the log scale (|z| < 3), computed as an explicit vector.
z_log <- as.numeric(scale(data_clean$Log_Pause_ms))
n_before <- nrow(data_clean)
data_clean <- data_clean[abs(z_log) < 3, ]
message(sprintf("Outlier removal: dropped %d pauses beyond 3 SD on the log scale (%d -> %d).",
                n_before - nrow(data_clean), n_before, nrow(data_clean)))

data_clean$Syntactic_Major <- factor(data_clean$Syntactic_Major)
data_clean$Process_Major   <- factor(data_clean$Process_Major)
