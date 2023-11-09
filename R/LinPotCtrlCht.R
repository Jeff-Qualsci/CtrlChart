# Base functionality for AGM Potency Control Charting App

# Server 
# Environment set up-------------------------------------

library(tidyverse)
library(qicharts2)
library(slider)

# Control chart functions --------------------------------

pow10 <- function(x) { 10 ^ x } # simplifies anitloging within other functions like across()

# Basic control chart with log scale for y axis with center line and control limits -----------------
# Using geom_line for the reference lines as well as data allows the labels, linetytpes, ... to show up in legend instead of manually coding position in plot.
# This requires data for the plot to be in tidy format (tall) with all y values in the same column and a separate column to specify the groups.
# Specific axis and chart labels are applied within the data analysis functions.

log_ctrl_cht <- function(plotdata) {
  
  chart <- ggplot(plotdata, aes(x = Run, y = Data, group = Line, color = Lines)) +
    geom_line() +
    geom_point(data = plotdata %>% filter(Line == 'Data'), show.legend = FALSE) +
    scale_colour_manual(values = c('Data'= 'black', 'Center Line' = 'mediumblue', 'Control Limit' = 'red')) +
    scale_y_continuous(trans = "log10") +
    theme_linedraw() +
    theme(legend.position = 'right')
}

# MSR Analysis and Charts -----------------------------------
# Current version will be replaced in next version of code.

msrchart <- function(runs, data,msrtype, usrtitle) {
  
  MSRChart <- qic(x = runs,
                  y = data, 
                  chart = 'run',
                  xlab = 'Run Date',
                  ylab = msrtype,
                  title = usrtitle)
}

msr_ind <- function(df) {
  MSRData <- df %>% 
      mutate(Lst6_sd = slide_dbl(Log10Data, sd, .before = 5, .complete = TRUE),
             Lst6_MSR = 10 ^ (2 * sqrt(2) * Lst6_sd),
             Cumm_sd = slide_dbl(Log10Data, sd, .before = Inf, .complete = TRUE),
             Cumm_MSR = 10 ^ (2 * sqrt(2) * Cumm_sd)) %>% 
      filter(row_number() >5)
  }
  
msr_rep <- function(df) {
  startdate <- df$Run[5]
  
  MSRData <- df %>% 
    mutate(Log10Data = log10(Data),
           Cumm_sd = slide_dbl(Log10Data, sd, .before = Inf, .complete = TRUE),
           Cumm_MSR = 10 ^ (2 * sqrt(2) * Cumm_sd)) %>% 
    group_by(Run) %>% 
    summarise(Cumm_MSR = last(Cumm_MSR)) %>% 
    ungroup() %>% 
    filter(Run > startdate)
}

# Individual Data Analysis ---------------------------
ind_charts <- function(usrdata, usrtitle) {

# Remove any n > 1 replicates, transform the data for control Chart analysis and moving range (MR) calculation
    ChartData <- usrdata %>% 
    group_by(Run) %>% 
    summarise(Data = first(Data)) %>% 
    ungroup() %>% 
    mutate(Log10Data = log10(Data),
           MRLog = abs(slide_dbl(Log10Data, diff, .before = 1, .complete = TRUE)),
           FMR = pow10(MRLog))
  
  # Individual Chart (DataChart)
  IChartSumm <- summary(qic(ChartData$Run, ChartData$Log10Data, chart = 'i'))
  IChartStats <- as_tibble(IChartSumm) 
  IChartStats <- IChartStats %>% 
    mutate(across(contains('CL'), pow10),
           across(contains('CL'), ~ signif(.x, digits = 3)))
  
  PlotData <- ChartData %>% 
    select(Run, Data) %>% 
    mutate(CL = IChartStats$CL,
           UCL = IChartStats$aUCL,
           LCL = IChartStats$aLCL) %>% 
    pivot_longer(cols = !Run, names_to = 'Line', values_to = 'Data') %>% 
    mutate(Lines = if_else(Line == 'Data', 'Data',
                           if_else(Line == 'CL', 'Center Line', 'Control Limit')))
  
  IChart <- log_ctrl_cht(plotdata = PlotData)
  IChart <- IChart +
    labs(title = paste0('Individual Chart - ', usrtitle),
         y = 'Potency')
  
  # Moving Range Chart (VarChart)
  
  MRChartSumm <- summary(qic(ChartData$Run, ChartData$MRLog, chart = 'mr'))
  MRChartStats <- as_tibble(MRChartSumm) 
  MRChartStats <- MRChartStats %>% 
    mutate(across(contains('CL'), pow10),
           across(contains('CL'), ~ signif(.x, digits = 3)))
  
  PlotData <- ChartData %>% 
    select(Run, FMR) %>% 
    rename(Data = FMR) %>% 
    mutate(CL = MRChartStats$CL,
           UCL = MRChartStats$aUCL,
           LCL = MRChartStats$aLCL) %>% 
    pivot_longer(cols = !Run, names_to = 'Line', values_to = 'Data') %>% 
    mutate(Lines = if_else(Line == 'Data', 'Data',
                           if_else(Line == 'CL', 'Center Line', 'Control Limit')))
  
  MRChart <- log_ctrl_cht(plotdata = PlotData)
  MRChart <- MRChart +
    labs(title = paste0('Fold Moving Range Chart - ', usrtitle),
         y = 'Fold Moving Range')

  MSRData <- msr_ind(ChartData) 

  CummMSRChart <- msrchart(runs = MSRData$Run,
             data = MSRData$Cumm_MSR,
             msrtype = 'Cummulative MSR',
             usrtitle = paste0('Run Chart Cummulative MSR ', usrtitle))

  CummMSRSumm <-summary(CummMSRChart)

  Lst6MSRChart <-  msrchart(runs = MSRData$Run,
             data = MSRData$Lst6_MSR,
             msrtype = 'MSR(last 6 Runs)',
             usrtitle = paste0('Run Chart MSR(last 6 Runs) ', usrtitle))

  Lst6MSRSumm <- summary(Lst6MSRChart)
  
  Output <- list(ChartData = ChartData, DataChart = IChart, DataChartStats = IChartStats, VarChart = MRChart, VarChartStats = MRChartStats, MSRData = MSRData, Lst6MSRChart = Lst6MSRChart, Lst6MSRSumm = Lst6MSRSumm, CummMSRChart = CummMSRChart, CummMSRSumm = CummMSRSumm)
}

# Replicate Standard Deviation Charts ------------------------

xbars_charts <- function(usrdata, usrtitle) {
  
  usrdata <- usrdata %>% 
    mutate(Log10Data = log10(Data))
  
  ChartData <- usrdata %>% 
    group_by(Run) %>% 
    summarise(Data = mean(Log10Data),
              FSD = sd(Log10Data)) %>% 
    ungroup() %>% 
    mutate(across(!Run, pow10))
  
  # Xbar Chart (DataChart)
  XbarChartSumm <- summary(qic(usrdata$Run, usrdata$Log10Data, chart = 'xbar'))
  XbarChartStats <- as_tibble(XbarChartSumm) 
  XbarChartStats <- XbarChartStats %>% 
    mutate(across(contains('CL'), pow10),
           across(contains('CL'), ~ signif(.x, digits = 3)))
  
  PlotData <- ChartData %>% 
    select(Run, Data) %>% 
    mutate(CL = XbarChartStats$CL,
           UCL = XbarChartStats$aUCL,
           LCL = XbarChartStats$aLCL) %>% 
    pivot_longer(cols = !Run, names_to = 'Line', values_to = 'Data') %>% 
    mutate(Lines = if_else(Line == 'Data', 'Data',
                           if_else(Line == 'CL', 'Center Line', 'Control Limit')))
  
  XbarChart <- log_ctrl_cht(plotdata = PlotData)
  XbarChart <- XbarChart +
    labs(title = paste0('Xbar Chart - ', usrtitle),
         y = 'Potency')
  
  # S Chart (VarChart)
  
  SChartSumm <- summary(qic(usrdata$Run, usrdata$Log10Data, chart = 's'))
  SChartStats <- as_tibble(SChartSumm) 
  SChartStats <- SChartStats %>% 
    mutate(across(contains('CL'), pow10),
           across(contains('CL'), ~ signif(.x, digits = 3)))
  
  PlotData <- ChartData %>% 
    select(Run, FSD) %>% 
    rename(Data = FSD) %>% 
    mutate(CL = SChartStats$CL,
           UCL = SChartStats$aUCL,
           LCL = SChartStats$aLCL) %>% 
    pivot_longer(cols = !Run, names_to = 'Line', values_to = 'Data') %>% 
    mutate(Lines = if_else(Line == 'Data', 'Data',
                           if_else(Line == 'CL', 'Center Line', 'Control Limit')))
  
  SChart <- log_ctrl_cht(plotdata = PlotData)
  SChart <- SChart +
    labs(title = paste0('S Chart - ', usrtitle),
         y =
           
           )
  
  MSRData <- msr_rep(usrdata)
  
  CummMSRChart <- msrchart(runs = MSRData$Run,
                           data = MSRData$Cumm_MSR,
                           msrtype = 'Cummulative MSR',
                           usrtitle = paste0('Run Chart Cummulative MSR ', usrtitle))
  
  CummMSRSumm <- summary(CummMSRChart) 
  
  Output <- list(ChartData = ChartData, DataChart = XbarChart, DataChartStats = XbarChartStats, VarChart = SChart, VarChartStats = SChartStats, MSRData = MSRData, CummMSRChart = CummMSRChart, CummMSRSumm = CummMSRSumm)
}


# Client outline ----------------------------------------
# User specifies if runs are identified by dates and a title for the generated output
UsrTitle <- 'Potency 123456'

# Import data and check file
UsrData <- read_csv(file = 'TestData/cc_data_10_3_50_1.csv')

# Prepare data for charting
UsrData <- if (is.Date(UsrData$Run)){UsrData %>% 
    arrange(Run ) }

# Determine number of replicates to select charts to create
RepCount <- UsrData %>% 
  group_by(Run) %>% 
  summarise(Reps = n(),
            Ind = Reps == 1) %>% 
  ungroup()

# Determine which types of charts to use. 
# A string is assigned, in case > 2 options desired for future development (e.g. range vs SD charts for rep data)
# Decided 11/9/23 not to implement. Keeping ChartType to potentially give users options to chose either or both chart types when data is a mixture of replicates and individual.

ChartType <- if (max(RepCount$Reps) == 1 | sum(RepCount$Ind) / length(RepCount) > 0.4) {'ind'
} else {'rep'} 

CtrlChtReport <- if (ChartType == 'ind') {
  ind_charts(UsrData, UsrTitle)
} else{
  xbars_charts(UsrData, UsrTitle)
}






