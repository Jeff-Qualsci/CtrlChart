# Generate test data for potency control charting application
library(tidyverse)

# Function to generate test data with the same number of replicates per run -----------------

tstdata <- function(TrueMeas, TrueMSR, NumRun, NumRep, Dates = TRUE) {
  
  FileName <- paste('TestData/cc_data', TrueMeas, TrueMSR, NumRun, NumRep, sep = '_' )
  FileName <- paste0(FileName, ".csv")
  
  TrueMeas <- log10(TrueMeas)
  StdDev <- log10(TrueMSR) / (2 * sqrt(2))
  Run <- if(Dates){
    seq(from = mdy('1/1/2023'), by = '1 week', length.out = NumRun)
  } else {c(1:NumRun)}
  
  Data <- rnorm(n = NumRun * NumRep, mean = TrueMeas, sd = StdDev)
  Run <- rep(Run, each = NumRep)
  Replicate_ID <- rep(1:NumRep, times = NumRun)
  
  Output <- if (NumRep == 1)  {
    tibble(Run, Data) %>% 
      mutate(Data = 10 ^ Data)
  }  else {
    tibble(Run, Replicate_ID, Data) %>% 
      mutate(Data = 10 ^ Data)}
  
  write_csv(Output, file = FileName)
  
  Output
}

TestData <- tstdata(TrueMeas = 10, TrueMSR = 3, NumRun = 50, NumRep = 1,  Dates = TRUE)

