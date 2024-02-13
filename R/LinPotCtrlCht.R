# Base functionality for AGM Potency Control Charting App

# Server 
# Environment set up-------------------------------------

library(tidyverse)
library(qicharts2)
library(slider)

pow10 <- function(x) { 10 ^ x } # simplifies anitloging within other functions like across()

# Minimum number of runs for the Window MSR.  Could be changed to a larger number in UI if approved.

msrWindow <- 6

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

msr_calc <- function(workingData, usrTitle, msrWindow = 6) {
  
  workingData <- workingData %>% 
    select(Run, Log10Pot)
  
  MsrCumm <- workingData %>% 
    mutate(sd_Cumm = slide_dbl(Log10Pot, sd, .before = Inf),
           MSR_Cumm = 10 ^ (2 * sqrt(2) * sd_Cumm)) %>% 
    group_by(Run) %>% 
    summarise(MSR_cumm = last(MSR_Cumm)) %>% 
    ungroup()
  
  MsrWin <- workingData %>% 
    nest(.by = Run) %>% 
    mutate(WindowData = slide(data, list_c, .before = (msrWindow - 1))) %>% 
    unnest(WindowData) %>% 
    group_by(Run) %>% 
    summarise(sd_window = sd(Log10Pot),
              MSR_window = 10 ^ (2 * sqrt(2) * sd_window)) %>% 
    ungroup()
  
  MsrData <- MsrCumm %>% 
    left_join(MsrWin) %>% 
    select(-contains('sd')) %>% 
    mutate(across(starts_with('MSR'), ~signif(.x, digits = 3))) %>% 
    filter(row_number() >= msrWindow)
  
  PlotData <- MsrData %>% 
    pivot_longer(cols = starts_with('MSR'), names_to = 'MSR_type', names_prefix = 'MSR_', values_to = 'MSR') %>%
    mutate(MSR_type = if_else(MSR_type == 'cumm', 'Cummulative', paste0('Last ', msrWindow, ' Runs')))
  
  MsrChart <- ggplot(PlotData, aes(x = Run, y = MSR, group = MSR_type, color = MSR_type)) +
    geom_line() +
    labs(title = 'MSR Chart',
         subtitle = usrTitle,
         y = 'MSR',
         x = 'Run') +
    theme_linedraw() +
    theme(legend.position = 'right')

  MsrChartReport <- list(MSRData = MsrData, MSRChart = MsrChart)
}


# Individual Data Analysis ---------------------------
ind_charts <- function(usrdata, usrtitle) {

# Remove any n > 1 replicates, transform the data for control Chart analysis and moving range (MR) calculation
    ChartData <- usrdata %>% 
    group_by(Run) %>% 
    summarise(Data = first(Data),
              Log10Pot = first(Log10Pot)) %>% 
    ungroup() %>% 
    mutate(MRLog = abs(slide_dbl(Log10Pot, diff, .before = 1, .complete = TRUE)),
           FMR = pow10(MRLog))
  
  # Individual Chart (DataChart)
  IChartSumm <- summary(qic(ChartData$Run, ChartData$Log10Pot, chart = 'i'))
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
 
  Output <- list(IChartData = ChartData, IChart = IChart, IChartStats = IChartStats, MRChart = MRChart, MRChartStats = MRChartStats)
}

# Replicate Standard Deviation Charts ------------------------

xbars_charts <- function(usrdata, usrtitle) {
  
  ChartData <- usrdata %>% 
    group_by(Run) %>% 
    summarise(Data = mean(Log10Pot),
              FSD = sd(Log10Pot)) %>% 
    ungroup() %>% 
    mutate(across(!Run, pow10))
  
  # Xbar Chart (DataChart)
  XbarChartSumm <- summary(qic(usrdata$Run, usrdata$Log10Pot, chart = 'xbar'))
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
  
  SChartSumm <- summary(qic(usrdata$Run, usrdata$Log10Pot, chart = 's'))
  SChartStats <- as_tibble(SChartSumm) 
  SChartStats <- SChartStats %>% 
    mutate(across(contains('CL'), pow10),
           across(contains('CL'), ~ signif(.x, digits = 3)))
  
  singlets <- sum(is.na(ChartData$FSD)) # number of singlet runs with missing FSD values
  
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
         y = 'Fold Std. Dev.',
         caption = paste('*', singlets, 'missing values from runs with a single replicate.'))
  
  message <- if (singlets > 0) {
    
    paste(singlets, "runs contained only 1 replicate. You may want to also run an Individuals analysis to assess the variability.")
  }
 
  Output <- list(XbarChartData = ChartData, XbarChart = XbarChart, XbarChartStats = XbarChartStats, SChart = SChart, SChartStats = SChartStats, Message = message)
}

# Client outline ----------------------------------------
# User specifies if runs are identified by dates and a title for the generated output. 
# File must contain at least 2 columns.
# First column 'Run' identifies a specific experiment (Run) and should be a date(recommended) or a string.
# Second column 'Data' contains the potency values for the reference compound and should be a dbl
# Optional 3rd column to label the replicates within each run (user convenience only, not used in analysis)

# Specify label for output
usrTitle <- 'Replicate Data'



# Import data and check file
  usrData <- read_csv(file = 'TestData/cc_data_10_4_75_3_C.csv')

# Prepare data for charting
usrData <- usrData %>% 
  mutate(Log10Pot = log10(Data))


usrData <- if (is.Date(usrData$Run)) {
  arrange(usrData, Run)
}

# Determine number of replicates to select charts to create
repCount <- usrData %>% 
  group_by(Run) %>% 
  summarise(Reps = n(),
            Ind = Reps == 1) %>% 
  ungroup()

# Determine which types of charts to use. 

chartType <- ifelse(max(repCount$Reps) == 1 | sum(repCount$Ind) / length(repCount$Run) > 0.49, 'ind',
                    ifelse(min(repCount$Reps) >1, 'rep', 'both'))

IndChtReport <- if (chartType %in% c('ind', 'both')) {
  ind_charts(usrdata = usrData, usrtitle = usrTitle)
} 

RepChtReport <- if (chartType %in% c('rep', 'both')) {
  xbars_charts(usrData, usrTitle)
}

MsrChartReport <- if (length(repCount$Run) < msrWindow) {
  paste0('At least ', msrWindow, ' runs are required to calculate MSR.')
  } else {msr_calc(usrData, usrTitle, msrWindow)}


# Write Report files - for development - Replace with code for usr display and download of charts and data

# Create directory for report files
ReportDir <- paste0('UsrReports/', usrTitle)
dir.create(ReportDir)

switch(chartType,
       ind = {
         write_csv(IndChtReport$IChartData, file = paste0(ReportDir, '/', 'IChartData.csv'))
         write_csv(IndChtReport$IChartStats, file = paste0(ReportDir, '/', 'IChartStats.csv'))
         write_csv(IndChtReport$MRChartStats, file = paste0(ReportDir, '/', 'MRChartStats.csv'))
         write_csv(MsrChartReport$MSRData, file = paste0(ReportDir, '/', 'MSRData.csv'))
         ggsave(filename = paste0(ReportDir, '/', 'IRChart.png'), plot = IndChtReport$IChart, height = 4, width = 6, units = "in")
         ggsave(filename = paste0(ReportDir, '/', 'MRChart.png'), plot = IndChtReport$MRChart, height = 4, width = 6, units = "in")
         ggsave(filename = paste0(ReportDir, '/', 'MSRChart.png'), plot = MsrChartReport$MSRChart, height = 4, width = 6, units = "in")
       },
       rep = {
         write_csv(RepChtReport$XbarChartData, file = paste0(ReportDir, '/', 'XbarChartData.csv'))
         write_csv(RepChtReport$XbarChartStats, file = paste0(ReportDir, '/', 'XbarChartStats.csv'))
         write_csv(RepChtReport$SChartStats, file = paste0(ReportDir, '/', 'SChartStats.csv'))
         write_csv(MsrChartReport$MSRData, file = paste0(ReportDir, '/', 'MSRData.csv'))
         ggsave(filename = paste0(ReportDir, '/', 'XbarChart.png'), plot = RepChtReport$XbarChart, height = 4, width = 6, units = "in")
         ggsave(filename = paste0(ReportDir, '/', 'SChart.png'), plot = RepChtReport$SChart, height = 4, width = 6, units = "in")
         ggsave(filename = paste0(ReportDir, '/', 'MSRChart.png'), plot = MsrChartReport$MSRChart, height = 4, width = 6, units = "in")
       },
       both = {
         write_csv(IndChtReport$IChartData, file = paste0(ReportDir, '/', 'IChartData.csv'))
         write_csv(IndChtReport$IChartStats, file = paste0(ReportDir, '/', 'IChartStats.csv'))
         write_csv(IndChtReport$MRChartStats, file = paste0(ReportDir, '/', 'MRChartStats.csv'))
         ggsave(filename = paste0(ReportDir, '/', 'IRChart.png'), plot = IndChtReport$IChart, height = 4, width = 6, units = "in")
         ggsave(filename = paste0(ReportDir, '/', 'MRChart.png'), plot = IndChtReport$MRChart, height = 4, width = 6, units = "in")
         write_csv(RepChtReport$XbarChartData, file = paste0(ReportDir, '/', 'XbarChartData.csv'))
         write_csv(RepChtReport$XbarChartStats, file = paste0(ReportDir, '/', 'XbarChartStats.csv'))
         write_csv(RepChtReport$SChartStats, file = paste0(ReportDir, '/', 'SChartStats.csv'))
         ggsave(filename = paste0(ReportDir, '/', 'XbarChart.png'), plot = RepChtReport$XbarChart, height = 4, width = 6, units = "in")
         ggsave(filename = paste0(ReportDir, '/', 'SChart.png'), plot = RepChtReport$SChart, height = 4, width = 6, units = "in")
         write_csv(MsrChartReport$MSRData, file = paste0(ReportDir, '/', 'MSRData.csv'))
         ggsave(filename = paste0(ReportDir, '/', 'MSRChart.png'), plot = MsrChartReport$MSRChart, height = 4, width = 6, units = "in")
       })
