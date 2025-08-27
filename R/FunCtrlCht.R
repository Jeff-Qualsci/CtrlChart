# Environment set up-------------------------------------
library(tidyverse)
library(qicharts2)
library(slider)

# Functions for Control Charts and MSR calculations --------------------
# Server side functions and constants for Shiny app

# Minimum number of runs for the Window MSR. Remove from Shiny server, if you
# want to allow user to set this.
msrWindow <- 6

# Function to calculate 10^x, used in mutate(across(...)) to simplify
# antilogging within other functions like across()
pow10 <- function(x) { 10^x }

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
      RunLabel = if_else(runs.signal == FALSE & sigma.signal == FALSE,
                         "In Control",
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
    pivot_longer(cols = all_of(dataCols),
                 names_to = "Line",
                 values_to = "Data") %>%
    mutate(Lines = if_else(Line == "Data", "Data",
      if_else(Line == "CL", "Center Line", "Control Limit")
    ))
}

# Basic control chart with center line and control limits -----------------
# Using geom_line for the reference lines as well as data allows the labels,
# linetytpes, ... to show up in legend instead of manually coding position in
# plot. This requires data for the plot to be in tidy format (tall) with all y
# values in the same column and a separate column to specify the groups.
# Specific axis and chart labels are applied within the data analysis functions.

ctrl_cht <- function(plotdata) {
  Report <- check_run(plotdata)

  plotdata <- plot_data(plotdata)

  ggplot(plotdata, aes(x = Run, y = Data, group = Line, color = Lines)) +
    geom_line() +
    geom_point(data = plotdata %>% filter(Line == "Data"),
               show.legend = FALSE) +
    scale_colour_manual(values = c("Data" = "black",
                                   "Center Line" = "mediumblue",
                                   "Control Limit" = "red")) +
    labs(caption = Report) +
    theme_light() +
    theme(legend.position = "right")
}

# Basic control chart with log scale for y axis with center line and control
# limits ----------------- Using geom_line for the reference lines as well as
# data allows the labels, linetytpes, ... to show up in legend instead of
# manually coding position in plot. This requires data for the plot to be in
# tidy format (tall) with all y values in the same column and a separate column
# to specify the groups. Specific axis and chart labels are applied within the
# data analysis functions.

log_ctrl_cht <- function(plotdata) {
  Report <- check_run(plotdata)

  plotdata <- plot_data(plotdata)

  chart <- ggplot(plotdata, aes(x = Run,
                                y = Data,
                                group = Line,
                                color = Lines)) +
    geom_line() +
    geom_point(data = plotdata %>% filter(Line == "Data"),
               show.legend = FALSE) +
    scale_colour_manual(values = c("Data" = "black",
                                   "Center Line" = "mediumblue",
                                   "Control Limit" = "red")) +
    scale_y_continuous(trans = "log10") +
    labs(caption = Report) +
    theme_linedraw() +
    theme(legend.position = "right")
}

# MSR Analysis and Charts -----------------------------------

msr_calc <- function(workingData, usrTitle, msrWindow = 6) {
  workingData <- workingData %>%
    mutate(Log10Pot = log10(Data))

  MsrCum <- workingData %>%
    mutate(
      sd_Cum = slide_dbl(Log10Pot, sd, .before = Inf),
      MSR_Cum = 10^(2 * sqrt(2) * sd_Cum)
    ) %>%
    group_by(Run) %>%
    summarise(MSR_Cum = last(MSR_Cum)) %>%
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

  MsrData <- MsrCum %>%
    left_join(MsrWin) %>%
    select(-contains("sd")) %>%
    mutate(across(starts_with("MSR"), ~ signif(.x, digits = 3))) %>%
    filter(row_number() >= msrWindow)

  PlotData <- MsrData %>%
    pivot_longer(cols = starts_with("MSR"),
                 names_to = "MSR_type", names_prefix = "MSR_",
                 values_to = "MSR") %>%
    mutate(MSR_type = if_else(MSR_type == "Cum",
                              "Cumulative",
                              paste0("Last ", msrWindow, " Runs")))

  MsrChart <- ggplot(PlotData,
                     aes(x = Run,
                         y = MSR,
                         group = MSR_type,
                         color = MSR_type)) +
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
  # Remove any n > 1 replicates, transform the data for control Chart analysis
  usrdata <- usrdata %>%
    group_by(Run) %>%
    summarise(Data = first(Data)) %>%
    ungroup()

  # Individual Chart (DataChart)

  IChartData <- as_tibble(qic(x = usrdata$Run,
                              y = log10(usrdata$Data),
                              chart = "i",
                              return.data = TRUE)) %>%
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

  MRChartData <- as_tibble(qic(x = usrdata$Run,
                               y = log10(usrdata$Data),
                               chart = "mr",
                               return.data = TRUE)) %>%
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


  Output <- list(IChartData = IChartData,
                 IChart = IChart,
                 MRChart = MRChart,
                 MRChartData = MRChartData)
}

# Replicate Standard Deviation Charts ------------------------

xbars_charts <- function(usrdata, usrtitle) {
  XbarChartData <- as_tibble(qic(x = usrdata$Run,
                                 y = log10(usrdata$Data), chart = "xbar",
                                 return.data = TRUE)) %>%
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

  SChartData <- as_tibble(qic(x = usrdata$Run,
                              y = log10(usrdata$Data),
                              chart = "s", return.data = TRUE)) %>%
    qic_extract() %>%
    mutate(
      n = as.character(n),
      across(where(is.numeric), pow10)
    )

  # number of singlet runs with missing FSD values
  singlets <- sum(is.na(SChartData$Data))

  SChart <- log_ctrl_cht(plotdata = SChartData)
  SChart <- SChart +
    labs(
      title = paste0("S Chart - ", usrtitle),
      y = "Fold Std. Dev.",
      subtitle = paste("*",
                       singlets,
                       "missing values from runs with a single replicate.")
    )

  SChartData <- SChartData %>%
    rename(`Std.Dev(fold)` = Data)
  }

  Output <- list(XbarChartData = XbarChartData,
                 XbarChart = XbarChart,
                 SChart = SChart,
                 SChartData = SChartData)
}
