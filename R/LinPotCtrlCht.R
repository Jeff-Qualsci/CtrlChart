# Deprecated: Use R/FunCtrlCht.R
# Base functionality for AGM Potency Control Charting App

# Server
# Environment set up-------------------------------------

library(tidyverse)
library(qicharts2)
library(slider)

pow10 <- function(x) {
  10^x
} # simplifies anitloging within other functions like across()

# Minimum number of runs for the Window MSR.  Could be changed to a larger number in UI if approved.

msrWindow <- 6 # could be changed or user input in future.

# Extract relevant columns from qic data frame. --------------------
qic_extract <- function(df) {
  columns <- c("x", "y", "y.length", "cl", "lcl", "ucl", "runs.signal", "sigma.signal")

  df <- df %>%
    select(all_of(columns)) %>%
    rename(
      Run = x,
      Data = y,
      n = y.length,
      CL = cl,
      LCL = lcl,
      UCL = ucl
    )
}

# Check for out of control signals and return a message. -----------------------
check_run <- function(df) {
  noErrMsg <- "Pass, data is in control."
  Err1Msg <- "Data is out of control and should be investigated for cause (see chart data)."
  ClMsg <- "TRUE values in the sigma.signal column indicate the run is outside of the control limits."
  RunMsg <- "TRUE values in the runs.signal column indicate other possible issues."

  clErr <- sum(df$sigma.signal)
  runErr <- sum(df$runs.signal)

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

# Generate data frame for log_ctrl_cht() ------------------------------
plot_data <- function(df) {
  dataCols <- c("Data", "CL", "LCL", "UCL")

  df <- df %>%
    mutate(
      RunLabel = if_else(runs.signal == FALSE & sigma.signal == FALSE, "In Control",
        if_else(runs.signal & sigma.signal, "Both Flags",
          if_else(sigma.signal == TRUE, "Control Limit Flag", "Run Flag")
        )
      ),
      shape = if_else(runs.signal == FALSE & sigma.signal == FALSE, 1,
        if_else(runs.signal & sigma.signal, 7,
          if_else(sigma.signal == TRUE, 4, 0)
        )
      )
    ) %>%
    select(!(ends_with("signal") | n)) %>%
    pivot_longer(cols = all_of(dataCols), names_to = "Line", values_to = "Data") %>%
    mutate(Lines = if_else(Line == "Data", "Data",
      if_else(Line == "CL", "Center Line", "Control Limit")
    ))
}


# Basic control chart with log scale for y axis with center line and control limits -----------------
# Using geom_line for the reference lines as well as data allows the labels, linetytpes, ... to show up in legend instead of manually coding position in plot.
# This requires data for the plot to be in tidy format (tall) with all y values in the same column and a separate column to specify the groups.
# Specific axis and chart labels are applied within the data analysis functions.

log_ctrl_cht <- function(plotdata) {
  Report <- check_run(plotdata)

  plotdata <- plot_data(plotdata)

  chart <- ggplot(plotdata, aes(x = Run, y = Data, group = Line, color = Lines)) +
    geom_line() +
    geom_point(data = plotdata %>% filter(Line == "Data"), show.legend = FALSE) +
    scale_colour_manual(values = c("Data" = "black", "Center Line" = "mediumblue", "Control Limit" = "red")) +
    scale_y_continuous(trans = "log10") +
    labs(caption = Report) +
    theme_linedraw() +
    theme(legend.position = "right")
}

# MSR Analysis and Charts -----------------------------------

msr_calc <- function(workingData, usrTitle, msrWindow = 6) {
  workingData <- workingData %>%
    mutate(Log10Pot = log10(Data))

  MsrCumm <- workingData %>%
    mutate(
      sd_Cumm = slide_dbl(Log10Pot, sd, .before = Inf),
      MSR_Cumm = 10^(2 * sqrt(2) * sd_Cumm)
    ) %>%
    group_by(Run) %>%
    summarise(MSR_cumm = last(MSR_Cumm)) %>%
    ungroup()

  MsrWin <- workingData %>%
    nest(.by = Run) %>%
    mutate(WindowData = slide(data, list_c, .before = (msrWindow - 1))) %>%
    unnest(WindowData) %>%
    group_by(Run) %>%
    summarise(
      sd_window = sd(Log10Pot),
      MSR_window = 10^(2 * sqrt(2) * sd_window)
    ) %>%
    ungroup()

  MsrData <- MsrCumm %>%
    left_join(MsrWin) %>%
    select(-contains("sd")) %>%
    mutate(across(starts_with("MSR"), ~ signif(.x, digits = 3))) %>%
    filter(row_number() >= msrWindow)

  PlotData <- MsrData %>%
    pivot_longer(cols = starts_with("MSR"), names_to = "MSR_type", names_prefix = "MSR_", values_to = "MSR") %>%
    mutate(MSR_type = if_else(MSR_type == "cumm", "Cummulative", paste0("Last ", msrWindow, " Runs")))

  MsrChart <- ggplot(PlotData, aes(x = Run, y = MSR, group = MSR_type, color = MSR_type)) +
    geom_line() +
    labs(
      title = "MSR Chart",
      subtitle = usrTitle,
      y = "MSR",
      x = "Run"
    ) +
    theme_linedraw() +
    theme(legend.position = "right")

  MsrChartReport <- list(MSRData = MsrData, MSRChart = MsrChart)
}


# Individual Data Analysis ---------------------------
ind_charts <- function(usrdata, usrtitle) {
  # Remove any n > 1 replicates, transform the data for control Chart analysis and moving range (MR) calculation
  usrdata <- usrdata %>%
    group_by(Run) %>%
    summarise(Data = first(Data)) %>%
    ungroup()

  # Individual Chart (DataChart)

  IChartData <- as_tibble(qic(x = usrdata$Run, y = log10(usrdata$Data), chart = "i", return.data = TRUE)) %>%
    qic_extract() %>%
    mutate(
      n = as.character(n),
      across(where(is.numeric), pow10)
    )

  IChart <- log_ctrl_cht(plotdata = IChartData)
  IChart <- IChart +
    labs(
      title = paste0("Individuals Chart - ", usrtitle),
      y = "Potency"
    )

  IChartData <- IChartData %>%
    rename(Potency = Data)

  # Moving Range Chart (VarChart)

  MRChartData <- as_tibble(qic(x = usrdata$Run, y = log10(usrdata$Data), chart = "mr", return.data = TRUE)) %>%
    qic_extract() %>%
    mutate(
      n = as.character(n),
      across(where(is.numeric), pow10)
    )

  MRChart <- log_ctrl_cht(plotdata = MRChartData)
  MRChart <- MRChart +
    labs(
      title = paste0("Fold Moving Range Chart - ", usrtitle),
      y = "Fold Moving Range"
    )

  MRChartData <- MRChartData %>%
    rename(`MR(fold)` = Data)


  Output <- list(IChartData = IChartData, IChart = IChart, MRChart = MRChart, MRChartData = MRChartData)
}

# Replicate Standard Deviation Charts ------------------------

xbars_charts <- function(usrdata, usrtitle) {
  XbarChartData <- as_tibble(qic(x = usrdata$Run, y = log10(usrdata$Data), chart = "xbar", return.data = TRUE)) %>%
    qic_extract() %>%
    mutate(
      n = as.character(n),
      across(where(is.numeric), pow10)
    )

  # Xbar Chart (DataChart)

  XbarChart <- log_ctrl_cht(plotdata = XbarChartData)
  XbarChart <- XbarChart +
    labs(
      title = paste0("Xbar Chart - ", usrtitle),
      y = "Potency"
    )

  XbarChartData <- XbarChartData %>%
    rename(`Geo.Mean(Potency)` = Data)

  # S Chart (VarChart)

  SChartData <- as_tibble(qic(x = usrdata$Run, y = log10(usrdata$Data), chart = "s", return.data = TRUE)) %>%
    qic_extract() %>%
    mutate(
      n = as.character(n),
      across(where(is.numeric), pow10)
    )

  singlets <- sum(is.na(SChartData$Data)) # number of singlet runs with missing FSD values


  SChart <- log_ctrl_cht(plotdata = SChartData)
  SChart <- SChart +
    labs(
      title = paste0("S Chart - ", usrtitle),
      y = "Fold Std. Dev.",
      subtitle = paste("*", singlets, "missing values from runs with a single replicate.")
    )

  SChartData <- SChartData %>%
    rename(`Std.Dev(fold)` = Data)

  message <- if (singlets > 0) {
    paste(singlets, "runs contained only 1 replicate. You may want to also run an Individuals analysis to assess the variability.")
  }

  Output <- list(XbarChartData = XbarChartData, XbarChart = XbarChart, SChart = SChart, SChartData = SChartData, Message = message)
}

# Client outline ----------------------------------------
# User specifies if runs are identified by dates and a title for the generated output.
# File must contain at least 2 columns.
# First column 'Run' identifies a specific experiment (Run) and should be a date(recommended) or a string.
# Second column 'Data' contains the potency values for the reference compound and should be a dbl
# Optional 3rd column to label the replicates within each run (user convenience only, not used in analysis)

# Specify label for output
usrTitle <- "New Mixed Data"

# Import data and check file
usrData <- read_csv(file = "TestData/cc_data_5_3_50_6_V.csv")

usrData <- if (is.Date(usrData$Run)) {
  arrange(usrData, Run)
}

# Determine number of replicates to select charts to create
repCount <- usrData %>%
  group_by(Run) %>%
  summarise(
    Reps = n(),
    Ind = Reps == 1
  ) %>%
  ungroup()

# Determine which types of charts to use.

chartType <- ifelse(max(repCount$Reps) == 1 | sum(repCount$Ind) / length(repCount$Run) > 0.49, "ind",
  ifelse(min(repCount$Reps) > 1, "rep", "both")
)

IndChtReport <- if (chartType %in% c("ind", "both")) {
  ind_charts(usrdata = usrData, usrtitle = usrTitle)
}

RepChtReport <- if (chartType %in% c("rep", "both")) {
  xbars_charts(usrData, usrTitle)
}

MsrChartReport <- if (length(repCount$Run) < msrWindow) {
  paste0("At least ", msrWindow, " runs are required to calculate MSR.")
} else {
  msr_calc(usrData, usrTitle, msrWindow)
}


# Write Report files - for development - Replace with code for usr display and download of charts and data ---------------------

# Create directory for report files Uncomment to save report locally
# ReportDir <- paste0('UsrReports/', usrTitle)
# dir.create(ReportDir)
#
# switch(chartType,
#        ind = {
#          write_csv(IndChtReport$IChartData, file = paste0(ReportDir, '/', 'IChartData.csv'))
#          write_csv(IndChtReport$MRChartData, file = paste0(ReportDir, '/', 'MRChartData.csv'))
#          write_csv(MsrChartReport$MSRData, file = paste0(ReportDir, '/', 'MSRData.csv'))
#          ggsave(filename = paste0(ReportDir, '/', 'IRChart.png'), plot = IndChtReport$IChart, height = 4, width = 6, units = "in")
#          ggsave(filename = paste0(ReportDir, '/', 'MRChart.png'), plot = IndChtReport$MRChart, height = 4, width = 6, units = "in")
#          ggsave(filename = paste0(ReportDir, '/', 'MSRChart.png'), plot = MsrChartReport$MSRChart, height = 4, width = 6, units = "in")
#        },
#        rep = {
#          write_csv(RepChtReport$XbarChartData, file = paste0(ReportDir, '/', 'XbarChartData.csv'))
#          write_csv(RepChtReport$SChartData, file = paste0(ReportDir, '/', 'SChartData.csv'))
#          write_csv(MsrChartReport$MSRData, file = paste0(ReportDir, '/', 'MSRData.csv'))
#          ggsave(filename = paste0(ReportDir, '/', 'XbarChart.png'), plot = RepChtReport$XbarChart, height = 4, width = 6, units = "in")
#          ggsave(filename = paste0(ReportDir, '/', 'SChart.png'), plot = RepChtReport$SChart, height = 4, width = 6, units = "in")
#          ggsave(filename = paste0(ReportDir, '/', 'MSRChart.png'), plot = MsrChartReport$MSRChart, height = 4, width = 6, units = "in")
#        },
#        both = {
#          write_csv(IndChtReport$IChartData, file = paste0(ReportDir, '/', 'IChartData.csv'))
#          write_csv(IndChtReport$MRChartData, file = paste0(ReportDir, '/', 'MRChartData.csv'))
#          ggsave(filename = paste0(ReportDir, '/', 'IRChart.png'), plot = IndChtReport$IChart, height = 4, width = 6, units = "in")
#          ggsave(filename = paste0(ReportDir, '/', 'MRChart.png'), plot = IndChtReport$MRChart, height = 4, width = 6, units = "in")
#          write_csv(RepChtReport$XbarChartData, file = paste0(ReportDir, '/', 'XbarChartData.csv'))
#          write_csv(RepChtReport$SChartData, file = paste0(ReportDir, '/', 'SChartData.csv'))
#          ggsave(filename = paste0(ReportDir, '/', 'XbarChart.png'), plot = RepChtReport$XbarChart, height = 4, width = 6, units = "in")
#          ggsave(filename = paste0(ReportDir, '/', 'SChart.png'), plot = RepChtReport$SChart, height = 4, width = 6, units = "in")
#          write_csv(MsrChartReport$MSRData, file = paste0(ReportDir, '/', 'MSRData.csv'))
#          ggsave(filename = paste0(ReportDir, '/', 'MSRChart.png'), plot = MsrChartReport$MSRChart, height = 4, width = 6, units = "in")
#        })
