# ============================================================================
# run_all.R  -  entry point
# Study: Pause for Thought - Systemic Functional Units and the Dynamics of Writing
# Author: Mojtaba Sayyad Mahernia
#
# Run from the project root:   Rscript run_all.R
# or in RStudio with the project open:  source("run_all.R")
# ============================================================================

# --- Packages ----------------------------------------------------------------
# lmerTest must be loaded so that anova() on an lmer model returns
# NumDF / DenDF / Pr(>F), which create_results_text() relies on.
library(here)
library(dplyr)
library(stringr)
library(lme4)
library(lmerTest)
library(emmeans)
library(boot)
library(MuMIn)
library(ggplot2)

# --- Reproducibility & output dirs ------------------------------------------
set.seed(42)
dir.create(here("output"),            showWarnings = FALSE)
dir.create(here("output", "figures"), showWarnings = FALSE)

# --- Run pipeline in order ---------------------------------------------------
source(here("R", "functions.R"))          # helpers first
source(here("R", "01_clean.R"))           # -> data_clean
source(here("R", "02_descriptives.R"))    # -> descriptive CSVs
source(here("R", "03_models.R"))          # -> mixed models + diagnostics
source(here("R", "04_frequency.R"))       # -> frequency analyses
source(here("R", "05_bootstrap.R"))       # -> bootstrap CIs
source(here("R", "06_figures.R"))         # -> duration figures
source(here("R", "07_boundary.R"))        # -> boundary analysis (specificS2)
source(here("R", "08_figures_boundary.R"))# -> boundary figures

message("\nPipeline complete. Tables in output/, figures in output/figures/.")
