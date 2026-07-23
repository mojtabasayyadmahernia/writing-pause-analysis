# Pause Dynamics in Written Composition

Statistical analysis of keystroke-logged pauses, examining pause behaviour at structurally meaningful points from Systemic Functional Linguistics (SFL) perspective during written compostion.

*Part of my PhD project at Lund University. 
Sayyad Mahernia, M. (2026). Pause for thought: Systemic Functional units and the dynamics of writing [Doctoral dissertation, Lund University]. Lund University Publications. https://lup.lub.lu.se/search/publication/f454544b-a7ed-4770-8e82-4c4abc110de1





---

## Purpose

When writers pause mid-composition, are those pauses random, or do they cluster at meaningful points in the emerging text? This study treats pauses as a signal rather than noise. If their placement and duration track structural properties of lanugage, they can be read as evidence of relationship between cognitive processes and linguistic units generated during writing.

## Data

Writers recorded with keystroke logging. Each pause was hand-annotated for its syntactic position and its functional role in the clause.

- `final1.csv` — pauses with syntactic, functional, and process-type annotation
- `specificS2.csv` — the same pauses with thematic-unit boundary distances


## How it's analysed

**Mixed-effects models.** Pauses are nested within writers. Random intercepts per participant keep the pause-level data while accounting for that clustering. Duration is log-transformed because the raw distribution is heavily right-skewed.

**Bootstrap confidence intervals.** 10,000 resamples for the mean, plus a cluster bootstrap that resamples *participants* rather than individual pauses, so uncertainty reflects generalisation across writers.

**Frequency tests.** Monte Carlo chi-square, used because several context categories have expected counts below 5 where the standard approximation is unreliable. Cramér's V for effect size.

**Boundary analysis.** Non-parametric tests (Wilcoxon, Kruskal-Wallis, Kolmogorov-Smirnov) and Cohen's d on pause duration relative to thematic-unit boundaries.

All pairwise comparisons are Bonferroni-corrected.

## Main finding

Pause duration varies systematically with syntactic position. Pauses at sentence boundaries average **5,131 ms** against **1,883 ms** at word-internal positions.


## Running it

```bash
Rscript run_all.R
```

Writes tables to `output/` and figures to `output/figures/`. Seeded, so it reproduces identically.

## Structure

| Path | Contents |
|---|---|
| `run_all.R` | Entry point — loads packages, sets seed, runs everything in order |
| `R/functions.R` | Helper functions |
| `R/01`–`R/06` | Cleaning, descriptives, models, frequency tests, bootstrap, figures |
| `R/07`–`R/08` | Thematic-unit boundary analysis and its figures |
| `R/09_report_numbers.R` | Dumps all headline statistics to one file |
| `data/` | Annotated pause datasets |

**Note:** the frequency counts in `R/04_frequency.R` come from manual annotation of the corpus and can't be derived from the CSV columns, so they're entered in the script as data.