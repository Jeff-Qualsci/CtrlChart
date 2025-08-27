# Generate test data for potency control charting application
library(tidyverse)

# Generate n random potency values consistent with true potency and MSR --------------------
# inputs and outputs are on the linear scale

pot_gen <- function(n, Pot, Msr) {
  10^(rnorm(n = n, mean = log10(Pot), sd = (log10(Msr) / 2)))
}

# Function to generate test data for FunCtrlCht.R -----------------
# inputs and outputs are on the linear scale
# truePot = expected mean potency value
# trueMsr = expected Minimum Significant Ratio
# numRuns the number of experiments simulated
# minReps the minimum number of replicates per run if varReps is TRUE
# maxReps the maximum number of replicates per run if varReps is TRUE, or the
# number of replicates per run if varReps is FALSE
# dates if TRUE generates a a sequence of dates for the Run identifier, if FALSE
# an integer sequence is generated
# varReps - if TRUE generates a random number of replicates within each run
# between minReps:maxReps, if FALSE all runs get the number of replicates
# specified in maxReps


tstdata <- function(truePot, trueMsr, numRuns, minReps, maxReps, varReps = TRUE,
                    dates = TRUE) {
  varFile <- ifelse(varReps, "V", "C")
  FileName <- paste("TestData/cc_data", truePot, trueMsr, numRuns, maxReps, varFile, sep = "_")
  FileName <- paste0(FileName, ".csv")

  Run <- if (dates) {
    seq(from = mdy("1/1/2025"), by = "1 week", length.out = numRuns)
  } else {
    c(1:NumRun)
  }

  Data <- vector("list", length = length(Run))

  for (i in seq_along(Run)) {
    Reps <- ifelse(varReps, sample(minReps:maxReps, 1), maxReps)
    Data[[i]] <- pot_gen(n = Reps, Pot = truePot, Msr = trueMsr)
  }

  Output <- tibble(Run, Data) %>%
    unnest(Data)

  write_csv(Output, file = FileName)

  Output
}

# Generate Data ----------------------------------------

TestData <- tstdata(truePot = 25, trueMsr = 3.5, numRuns = 75, minReps = 2, maxReps = 6, varReps = TRUE, dates = TRUE)
