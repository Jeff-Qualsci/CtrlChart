# Test code to have run.signal be set to FALSE until a mean shift is detected.
library(tidyverse)
library(qicharts2)

usrdata <- read_csv("TestData/MeanShiftData.csv")

# Function to extract control chart values to be checked for flags. -----------
# endRun = last run for the extracted values
# usrdata = data frame containing the control chart data
# chart = type of control chart (e.g., "xbar", "s", "c", etc.) for qicharts2

extract_ccRunData <- function(endRun, usrdata, chart) {
  dataCols <- c(
    "x",
    "y",
    "y.length",
    "longest.run",
    "n.crossings",
    "n.crossings.min"
  )
  usrdata <- filter(usrdata, Run <= endRun)

  ccRunData <- qic(
    x = usrdata$Run,
    y = log10(usrdata$Data),
    chart = chart,
    return.data = TRUE
  ) %>%
    select(all_of(dataCols)) %>%
    mutate(y = 10^y) %>%
    summarize_all(last)

  return(ccRunData)
}

# Extract run summarized data from qic() for specified chart type ----------------

extract_ccData <- function(usrdata, chart) {
  runs <- unique(usrdata$Run)

  ccData <- runs %>%
    map(\(x) extract_ccRunData(
      endRun = x,
      usrdata = usrdata,
      chart = chart
    )) %>%
    list_rbind() %>%
    rename(Run = x, n = y.length) %>%
    mutate(
      longest.run = if_else(is.na(longest.run), 1, longest.run),
      n.crossings = if_else(is.na(n.crossings), 0, n.crossings)
    )

  return(ccData)
}

xbarData <- extract_ccData(usrdata = usrdata, chart = "xbar")

# Extract the test statistics as determined by qic_s
extract_qic_limits <- function(usrdata, chart) {
  limits <- c("cl", "lcl", "ucl", "longest.run.max", "n.crossings.min")

  cclimits <- qic(
    x = usrdata$Run,
    y = log10(usrdata$Data),
    chart = chart,
    return.data = TRUE
  ) %>%
    select(all_of(limits)) %>%
    mutate(across(contains("cl"), ~ 10^.x)) %>%
    summarize_all(last)
}

xbarlimits <- extract_qic_limits(usrdata, "xbar")

# Append run limits an check for out of control runs -----------------
check_rundata <- function(rundata, runlimits) {
  rundata <- rundata %>%
    mutate(
      cl = runlimits$cl,
      lcl = runlimits$lcl,
      ucl = runlimits$ucl,
      longest.run.max = runlimits$longest.run.max,
      sigma.signal = y > ucl | y < lcl,
      runs.signal = longest.run > longest.run.max |
        n.crossings < n.crossings.min
    )

  return(rundata)
}

xbarRunData <- check_rundata(rundata = xbarData, runlimits = xbarlimits)

# A qic() replacement which returns a df where the runs.signal flag is TRUE only
# when it violates n.crossings.min for that run or max.longest.run from the
# total number of runs.
qic_fix <- function(usrdata, chart) {
  rundata <- extract_ccData(usrdata, chart)
  runlimits <- extract_qic_limits(usrdata, chart)
  rundata <- check_rundata(rundata, runlimits)

  return(rundata)
}

xbar_qicfix <- qic_fix(usrdata, chart = "xbar")
