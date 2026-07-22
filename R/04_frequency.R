# ============================================================================
# 04_frequency.R
# Frequency (goodness-of-fit) analyses of pause counts across contexts.
#
# Two kinds of frequency test are run here:
# (a) data-derived counts for the major syntactic / process categories,
# computed directly from data_clean; and
# (b) the syntagmatic and functional context tables for expected observations,
# whose counts come from MANUAL annotation and are not derivable from the CSV
# (see note below). These are entered as data and analysed with the
# shared run_context_frequency() helper.
# ============================================================================

# --- (a) Data-derived major-category frequencies -----------------------------
syntactic_counts <- data_clean %>%
  count(Syntactic_Major, name = "observed") %>%
  arrange(desc(observed))

total_syntactic <- sum(syntactic_counts$observed)
chi_syntactic    <- chisq.test(syntactic_counts$observed)                    # uniform null
mc_chi_syntactic <- chisq.test(syntactic_counts$observed,
                               simulate.p.value = TRUE, B = 10000)
v_syntactic <- cramers_v(chi_syntactic$statistic, total_syntactic,
                         nrow(syntactic_counts))

process_counts <- data_clean %>%
  filter(Process_Major != "Other") %>%
  count(Process_Major, name = "observed") %>%
  arrange(desc(observed))

total_process <- sum(process_counts$observed)
chi_process   <- chisq.test(process_counts$observed)
v_process     <- cramers_v(chi_process$statistic, total_process,
                           nrow(process_counts))

write.csv(syntactic_counts, here("output", "syntactic_frequency_analysis.csv"), row.names = FALSE)
write.csv(process_counts,   here("output", "process_frequency_analysis.csv"),   row.names = FALSE)

# --- (b) Manually annotated fine-grained context tables ----------------------
# NOTE: these observed/expected counts come from hand annotation of the corpus
# (the mapping from raw text to fine syntagmatic/functional category was done
# manually and cannot be reproduced from the CSV columns). They are entered
# here verbatim as the study's annotated data.

# Syntagmatic context
syntagmatic_expected <- c(163, 230, 190, 171, 38, 28, 14, 220, 8, 89, 69)
syntagmatic_observed <- c(483, 481, 338, 112, 93, 0, 85, 229, 104, 1026, 100)
syntagmatic_labels   <- c("End of NG", "Middle of NG", "End of VG", "Middle of VG",
                          "End of AG", "Middle of AG", "End of PG", "Middle of PG",
                          "End of Clause", "End of Sentence", "End of CG")

freq_syntagmatic <- run_context_frequency(
  observed       = syntagmatic_observed,
  context_counts = syntagmatic_expected,
  context_labels = syntagmatic_labels,
  title          = "Observed vs expected pause proportions by syntagmatic context",
  x_lab          = "Syntagmatic context",
  out_png        = here("output", "figures", "frequency_syntagmatic.png")
)

# Functional context
functional_expected <- c(12, 65, 99, 28, 75, 5, 25, 16, 85, 70, 38, 181, 37, 28,
                         15, 5, 5, 77, 5, 30, 9, 5, 40, 118, 5, 22, 70, 5, 27, 84,
                         2, 19, 7, 42, 5, 68, 3, 11, 2, 3)
functional_observed <- c(53, 234, 22, 65, 290, 71, 38, 14, 260, 80, 95, 222, 83,
                         75, 72, 3, 0, 137, 21, 121, 30, 6, 42, 189, 22, 49, 125,
                         71, 60, 163, 20, 63, 20, 66, 0, 117, 12, 8, 12, 16)
functional_labels <- c("End of Actor", "End of Attribute", "End of Relational Process",
                       "End of Beneficiary", "End of Circumstantial Adjunct", "End of Carrier",
                       "End of Existent", "End of Existential Process", "End of Goal",
                       "End of Hypotactic Conjunctive Adjunct", "End of Mood Adjunct",
                       "End of Material Process", "End of Mental Process",
                       "End of Paratactic Conjunctive Adjunct", "End of Phenomenon",
                       "End of Receiver", "End of Sayer", "End of Scope", "End of Sensor",
                       "End of Textual Adjunct", "End of Verbal Process", "End of Verbiage",
                       "Middle of Actor", "Middle of Attribute", "Middle of Attributive Process",
                       "Middle of Beneficiary", "Middle of Circumstantial Adjunct",
                       "Middle of Carrier", "Middle of Existent", "Middle of Goal",
                       "Middle of Mood Adjunct", "Middle of Material Process",
                       "Middle of Mental Process", "Middle of Phenomenon", "Middle of Receiver",
                       "Middle of Scope", "Middle of Sensor", "Middle of Textual Adjunct",
                       "Middle of Verbal Process", "Middle of Verbiage")

freq_functional <- run_context_frequency(
  observed       = functional_observed,
  context_counts = functional_expected,
  context_labels = functional_labels,
  title          = "Observed vs expected pause proportions by functional context",
  x_lab          = "Functional context",
  out_png        = here("output", "figures", "frequency_functional.png")
)
