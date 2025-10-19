# Environment set up-------------------------------------
library(tidyverse)
library(qicharts2)
library(slider)

# Functions for Control Charts and MSR calculations --------------------
# Server side functions and constants for Shiny app

# Column order for exported Ctrl Chart Tables.
ctrlChtCols <- c(
  "Run", "n", "Data", "CL", "LCL", "UCL", "sigma.signal",
  "longest.run", "longest.run.max", "n.crossings",
  "n.crossings.min", "runs.signal"
)

# Function to extract run specific values from the last run in a group. ------
# endRun = last run for the extracted values
# usrdata = data frame containing all the control chart data
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

# Function to sequentially loop through the control chart data and create a df
# with the run specific values. -----------

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

# Extract the qic() test statistics from the for the entire chart.------------
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

  return(cclimits)
}

# Add run limits an check for out of control runs -----------------
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

# A qic() replacement which returns a df where the runs.signal flag is TRUE only
# when it violates n.crossings.min for that run or max.longest.run from the
# total number of runs.-------------
qic_fix <- function(usrdata, chart) {
  rundata <- extract_ccData(usrdata, chart)
  runlimits <- extract_qic_limits(usrdata, chart)
  rundata <- check_rundata(rundata, runlimits) %>%
    rename(
      Data = y,
      CL = cl,
      LCL = lcl,
      UCL = ucl
    )

  return(rundata)
}

# Check for out of control signals and return a message. -----------------------
check_run <- function(df) {
  noErrMsg <- "Pass, data is in control."
  Err1Msg <- "Data is out of control and should be investigated for cause (see chart data)."
  ClMsg <- "Run Data outside of control limits."
  RunMsg <- "A shift in the center line has occurred. Runs prior to detection are involved."

  clErr <- sum(df$sigma.signal, na.rm = TRUE)
  runErr <- sum(df$runs.signal, na.rm = TRUE)

  report <- if (clErr + runErr == 0) {
    noErrMsg
  } else if (clErr > 0 & runErr > 0) {
    paste(Err1Msg, ClMsg, RunMsg, sep = "\n")
  } else if (clErr > 0) {
    paste(Err1Msg, ClMsg, sep = "\n")
  } else {
    paste(Err1Msg, RunMsg, sep = "\n")
  }
}

# Prepare data frame for log_ctrl_cht() ------------------------------
plot_data <- function(df) {
  dataCols <- c("Data", "CL", "LCL", "UCL")

  df <- df %>%
    mutate(
      RunLabel = if_else(runs.signal == FALSE & sigma.signal == FALSE,
        "In Control",
        if_else(runs.signal & sigma.signal, "Both Flags",
          if_else(sigma.signal == TRUE, "Outside Limits", "Mean Shift")
        )
      ),
      shape = if_else(runs.signal == FALSE & sigma.signal == FALSE, 1,
        if_else(runs.signal & sigma.signal, 7,
          if_else(sigma.signal == TRUE, 4, 0)
        )
      )
    ) %>%
    select(!(ends_with("signal") | n)) %>%
    pivot_longer(
      cols = all_of(dataCols),
      names_to = "Line",
      values_to = "Data"
    ) %>%
    mutate(Lines = if_else(Line == "Data", "Data",
      if_else(Line == "CL", "Center Line", "Control Limit")
    ))
}

# Basic control chart with log scale for y axis -----------------
# Using geom_line for the reference lines as well as
# data allows the labels, linetytpes, ... to show up in legend instead of
# manually coding position in plot. This requires data for the plot to be in
# tidy format (tall) with all y values in the same column and a separate column
# to specify the groups. Specific axis and chart labels are applied within the
# data analysis functions.

log_ctrl_cht <- function(plotdata) {
  Report <- check_run(plotdata)

  plotdata <- plot_data(plotdata)

  chart <- ggplot(plotdata, aes(
    x = Run,
    y = Data,
    group = Line,
    color = Lines
  )) +
    geom_line() +
    geom_point(
      data = plotdata %>% filter(Line == "Data"),
      show.legend = FALSE
    ) +
    scale_colour_manual(values = c(
      "Data" = "black",
      "Center Line" = "mediumblue",
      "Control Limit" = "red"
    )) +
    scale_y_continuous(trans = "log10") +
    labs(caption = Report) +
    theme_linedraw() +
    theme(legend.position = "right")
}

# MSR Analysis and Charts -----------------------------------

msr_calc <- function(usrdata, usrtitle, msrWindow = 6) {
  usrdata <- usrdata %>%
    mutate(Log10Pot = log10(Data))

  MsrCum <- usrdata %>%
    mutate(
      sd_Cum = slide_dbl(Log10Pot, sd, .before = Inf),
      MSR_Cum = 10^(2 * sqrt(2) * sd_Cum)
    ) %>%
    group_by(Run) %>%
    summarise(MSR_Cum = last(MSR_Cum)) %>%
    ungroup()

  MsrWin <- usrdata %>%
    nest(.by = Run) %>%
    mutate(WindowData = slide(data, list_c, .before = (msrWindow - 1))) %>%
    unnest(WindowData) %>%
    group_by(Run) %>%
    summarise(
      sd_window = sd(Log10Pot),
      MSR_window = 10^(2 * sqrt(2) * sd_window)
    ) %>%
    ungroup()

  MsrData <- MsrCum %>%
    left_join(MsrWin) %>%
    select(-contains("sd")) %>%
    mutate(across(starts_with("MSR"), ~ signif(.x, digits = 3))) %>%
    filter(row_number() >= msrWindow)

  PlotData <- MsrData %>%
    pivot_longer(
      cols = starts_with("MSR"),
      names_to = "MSR_type", names_prefix = "MSR_",
      values_to = "MSR"
    ) %>%
    mutate(MSR_type = if_else(MSR_type == "Cum",
      "Cumulative",
      paste0("Last ", msrWindow, " Runs")
    ))

  MsrChart <- ggplot(
    PlotData,
    aes(
      x = Run,
      y = MSR,
      group = MSR_type,
      color = MSR_type
    )
  ) +
    geom_line() +
    labs(
      title = "MSR Chart",
      subtitle = usrtitle,
      y = "MSR",
      x = "Run"
    ) +
    theme_linedraw() +
    theme(legend.position = "right")

  MsrData <- MsrData %>%
    rename(
      `Cumulative MSR` = MSR_Cum,
      `Last 6 Runs MSR` = MSR_window
    )

  MsrChartReport <- list(MSRData = MsrData, MSRChart = MsrChart)
}

# Individual Data Analysis ---------------------------
ind_charts <- function(usrdata, usrtitle) {
  # Remove any n > 1 replicates, transform the data for control Chart analysis
  usrdata <- usrdata %>%
    group_by(Run) %>%
    summarise(Data = first(Data)) %>%
    ungroup()

  # Individual Chart (DataChart)

  IChartData <- as_tibble(qic_fix(
    usrdata = usrdata,
    chart = "i"
  )) %>%
    mutate(
      n = as.character(n),
      longest.run = as.character(longest.run),
      longest.run.max = as.character(longest.run.max),
      n.crossings = as.character(n.crossings),
      n.crossings.min = as.character(n.crossings.min)
    )

  IChart <- log_ctrl_cht(plotdata = IChartData)
  IChart <- IChart +
    labs(
      title = paste0("Individuals Chart - ", usrtitle),
      y = "Potency"
    )

  IChartData <- IChartData %>%
    relocate(all_of(ctrlChtCols)) %>%
    rename(
      Potency = Data,
      `Center Line` = CL,
      `Outside Limits` = sigma.signal,
      `Mean Shift` = runs.signal,
      `Actual Crossings` = n.crossings,
      `Minimum Crossings` = n.crossings.min,
      `Longest Run` = longest.run,
      `Maximum Run` = longest.run.max
    )

  # Moving Range Chart (VarChart)

  MRChartData <- as_tibble(qic_fix(
    usrdata = usrdata,
    chart = "mr"
  )) %>%
    mutate(
      n = as.character(n),
      longest.run = as.character(longest.run),
      longest.run.max = as.character(longest.run.max),
      n.crossings = as.character(n.crossings),
      n.crossings.min = as.character(n.crossings.min)
    )

  MRChart <- log_ctrl_cht(plotdata = MRChartData)
  MRChart <- MRChart +
    labs(
      title = paste0("Fold Moving Range Chart - ", usrtitle),
      y = "Fold Moving Range"
    )

  MRChartData <- MRChartData %>%
    relocate(all_of(ctrlChtCols)) %>%
    rename(
      `MR(fold)` = Data,
      `Center Line` = CL,
      `Outside Limits` = sigma.signal,
      `Mean Shift` = runs.signal,
      `Actual Crossings` = n.crossings,
      `Minimum Crossings` = n.crossings.min,
      `Longest Run` = longest.run,
      `Maximum Run` = longest.run.max
    )

  Output <- list(
    IChartData = IChartData,
    IChart = IChart,
    MRChart = MRChart,
    MRChartData = MRChartData
  )
}

# Replicate Standard Deviation Charts ------------------------

xbars_charts <- function(usrdata, usrtitle) {
  XbarChartData <- as_tibble(qic_fix(
    usrdata = usrdata,
    chart = "xbar"
  )) %>%
    mutate(
      n = as.character(n),
      longest.run = as.character(longest.run),
      longest.run.max = as.character(longest.run.max),
      n.crossings = as.character(n.crossings),
      n.crossings.min = as.character(n.crossings.min)
    )

  # Xbar Chart (DataChart)

  XbarChart <- log_ctrl_cht(plotdata = XbarChartData)
  XbarChart <- XbarChart +
    labs(
      title = paste0("Xbar Chart - ", usrtitle),
      y = "Potency"
    )

  XbarChartData <- XbarChartData %>%
    relocate(all_of(ctrlChtCols)) %>%
    rename(
      `Geo.Mean (Potency)` = Data,
      `Center Line` = CL,
      `Outside Limits` = sigma.signal,
      `Mean Shift` = runs.signal,
      `Actual Crossings` = n.crossings,
      `Minimum Crossings` = n.crossings.min,
      `Longest Run` = longest.run,
      `Maximum Run` = longest.run.max
    )

  # S Chart (VarChart)

  SChartData <- as_tibble(qic_fix(
    usrdata = usrdata,
    chart = "s"
  )) %>%
    mutate(
      n = as.character(n),
      longest.run = as.character(longest.run),
      longest.run.max = as.character(longest.run.max),
      n.crossings = as.character(n.crossings),
      n.crossings.min = as.character(n.crossings.min)
    )

  # number of singlet runs with missing FSD values
  singlets <- sum(is.na(SChartData$Data))

  SChart <- log_ctrl_cht(plotdata = SChartData)
  SChart <- SChart +
    labs(
      title = paste0("S Chart - ", usrtitle),
      y = "Fold Std. Dev.",
      subtitle = paste(
        "*",
        singlets,
        "missing values from runs with a single replicate."
      )
    )

  SChartData <- SChartData %>%
    relocate(all_of(ctrlChtCols)) %>%
    rename(
      `Std.Dev. (fold)` = Data,
      `Center Line` = CL,
      `Outside Limits` = sigma.signal,
      `Mean Shift` = runs.signal,
      `Actual Crossings` = n.crossings,
      `Minimum Crossings` = n.crossings.min,
      `Longest Run` = longest.run,
      `Maximum Run` = longest.run.max
    )

  Output <- list(
    XbarChartData = XbarChartData,
    XbarChart = XbarChart,
    SChart = SChart,
    SChartData = SChartData
  )
}
