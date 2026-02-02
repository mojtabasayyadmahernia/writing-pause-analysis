
# Install and load necessary packages
if (!require("boot")) install.packages("boot", repos = "http://cran.us.r-project.org")
if (!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(boot)
library(dplyr)
file_path <- "final1.csv"
tryCatch({
  data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}, error = function(e) {
  data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "latin1")
})
data <- read.csv("final1.csv")
names(data) <- make.names(names(data), unique = TRUE)
data <- data %>%
  mutate(Pause = as.numeric(Pause)) %>%
  filter(!is.na(Pause), Pause > 0) # Remove NA or non-positive pause durations
mean_fun <- function(data, indices) {
  d <- data[indices, ] # Allows boot to select sample
  return(mean(d$Pause))
}
R_replicates <- 10000
set.seed(123) # for reproducibility
overall_bootstrap <- boot(
  data = data,
  statistic = mean_fun,
  R = R_replicates,
  strata = data$file_id # Stratify by participant to account for clustering
)
overall_ci <- boot.ci(
  boot.out = overall_bootstrap,
  type = "bca",
  conf = 0.95
)
syntactic_contexts <- unique(data$Syntactic_Context)
bootstrap_by_context <- lapply(syntactic_contexts, function(context) {
  set.seed(123) # for reproducibility
  context_data <- data %>%
    filter(Syntactic_Context == context)
  
  # Check if there is enough data to run the bootstrap
  if (nrow(context_data) > 1) {
    boot_result <- boot(
      data = context_data,
      statistic = mean_fun,
      R = R_replicates,
      strata = context_data$Participant_ID
    )
    return(boot_result)
  } else {
    return(NULL)
  }
})
ci_by_context <- lapply(bootstrap_by_context, function(boot_result) {
  if (!is.null(boot_result)) {
    boot.ci(boot.out = boot_result, type = "bca", conf = 0.95)
  } else {
    return(NULL)
  }
})
names(ci_by_context) <- syntactic_contexts


