# Base functionality for AGM Potency Control Charting App

# Environment set up-------------------------------------

library(tidyverse)
library(qicharts2)
library(slider)

# Control chart functions --------------------------------
# Charts and Summary stats for individual data

ind_charts <- function(runs, data,usrtitle) {
  IChart <- qic(x = runs,
                y = data, 
                chart = 'i',
                xlab = 'Run Date',
                ylab = 'Log10 Potency',
                title = paste0('I Chart ', usrtitle))
  
  IChartSumm <- summary(IChart)
  
  MRChart <- qic(x = runs,
                y = data, 
                chart = 'mr',
                xlab = 'Run Date',
                ylab = 'Moving Range (Log10 Potency)',
                title = paste0('MR Chart ', usrtitle))
  
  MRChartSumm <- summary(MRChart)
  
  Output <- list(IChart = IChart, IChartSumm = IChartSumm, MRChart = MRChart, MRChartSumm = MRChartSumm)
}

# Charts and data summary for replicate data, n >= 2

rep_charts <- function(runs,data, usrtitle) {
  XbarChart <- qic(x = runs,
                 y = data, 
                 chart = 'xbar',
                 xlab = 'Run Date',
                 ylab = 'Mean (Log10 Potency)',
                 title = paste0('Xbar Chart ', usrtitle))
  
  XbarSumm <- summary(XbarChart)

  SChart <- qic(x = runs,
                 y = data, 
                 chart = 's',
                 xlab = 'Run Date',
                 ylab = 'Std. Dev. (Log10 Potency)',
                 title = paste0('S Chart ', usrtitle))
  
  SSumm <- summary(SChart)
  
  Output <- list(XbarChart = XbarChart, XBarSumm = XbarSumm, SChart = SChart, SChartSumm = SSumm)
}

msrchart <- function(runs, data,msrtype, usrtitle) {
  
  MSRChart <- qic(x = runs,
                y = data, 
                chart = 'run',
                xlab = 'Run Date',
                ylab = msrtype,
                title = usrtitle)
}

msr <- function(df, startdate, usrtitle) {
  MSRData <- if (length(unique(df$Run)) == length(df)) {
    df %>% 
      mutate(Last6_sd = slide_dbl(Log10Pot, sd, .before = 5, .complete = TRUE),
             Last6_MSR = 10 ^ (2 * sqrt(2) * Last6_sd),
             Cumm_sd = slide_dbl(Log10Pot, sd, .before = Inf, .complete = TRUE),
             Cumm_MSR = 10 ^ (2 * sqrt(2) * Cumm_sd)) %>% 
      filter(!is.null(Last6_sd))
  } else {
        df %>% 
      mutate(Cumm_sd = slide_dbl(Log10Pot, sd, .before = Inf, .complete = TRUE),
             Cumm_MSR = 10 ^ (2 * sqrt(2) * Cumm_sd)) %>% 
      group_by(Run) %>% 
      summarise(Cumm_MSR = last(Cumm_MSR)) %>% 
      ungroup() %>% 
      filter(Run > startdate)
  }
  
  CummMSRChart <- msrchart(runs = MSRData$Run,
                           data = MSRData$Cumm_MSR,
                           msrtype = 'Cummulative MSR',
                           usrtitle = paste0('Run Chart Cummulative MSR ', usrtitle))
  
  CummMSRSumm <- summary(CummMSRChart)
  
  Last6MSRChart <- if (length(unique(df$Run)) == length(df)) {
    msrchart(runs = MSRData$Run,
             data = MSRData$Lst6_MSR,
             msrtype = 'MSR(last 6 Runs)',
             usrtitle = paste0('Run Chart MSR(last 6 Runs) ', usrtitle))
  }
  Last6MSRSumm <- if (length(unique(df$Run)) == length(df)) {summary(Last6MSRChart)}
  
  Output <- if (length(unique(df$Run)) == length(df)) {
    list(MSRData = MSRData, CummMSRChart = CummMSRChart, CummMSRSumm = CummMSRSumm, Last6MSRChart = Last6MSRChart, Last6MSRSumm = Last6MSRSumm)
  } else { list(MSRData = MSRData, CummMSRChart = CummMSRChart, CummMSRSumm = CummMSRSumm)}
  }
  
# App outline ----------------------------------------
# User specifies if runs are identified by dates and a title for the generated output
UsrTitle <- 'Potency 123456'

# Import data and check file
UsrData <- read_csv(file = 'TestData/cc_data_10_3_50_1.csv')


# Prepare data for charting

UsrData <- if (is.Date(UsrData$Run)){UsrData %>% 
    arrange(Run ) }

UsrData <- mutate(UsrData, Log10Pot = log10(Data))

# Determine number of replicates to select charts to create
RepCount <- UsrData %>% 
  group_by(Run) %>% 
  summarise(Reps = n()) %>% 
  ungroup()

CtrlCharts <- if (max(RepCount$Reps == 1)) {
  ind_charts(UsrData$Run, UsrData$Log10Pot, UsrTitle)
} else{
  rep_charts(UsrData$Run, UsrData$Log10Pot, UsrTitle)
}

MSRChart <- if (length(RepCount > 5)) {
  msr(UsrData, RepCount$Run[5], UsrTitle)
} else { 'A minimum of 6 runs are required to calculate MSR'}

# Generate Report------------------------------

# UsrData
# 
# CtrlCharts["XbarChart"]
# 
# CtrlCharts["XBarSumm"]
# 
# CtrlCharts["SChartSumm"]
# 
# MSRChart["MSRData"]
# 
# MSRChart["CummMSRChart"]
# 
# MSRChart["CummMSRSumm"]











