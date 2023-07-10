# Control charting for potency data AGM webtool development

# Set up environment
library(tidyverse)
library(slider)

# Function to generate test data

tstdata <- function(TrueMeas, TrueMSR, SmplSize, Dates = TRUE) {
  TrueMeas <- log10(TrueMeas)
  StdDev <- log10(TrueMSR) / (2 * sqrt(2))
  Run <- if(Dates){
    seq(from = mdy('1/1/2023'), by = '1 week', length.out = SmplSize)
  } else {c(1:SmplSize)}
  
  Measure <- rnorm(n = SmplSize, mean = TrueMeas, sd = StdDev)
  
  Output <- tibble(Run, Measure) %>% 
    mutate(Measure = 10 ^ Measure)
}

TestData <- tstdata(TrueMeas = 10, TrueMSR = 3, SmplSize = 50, Dates = TRUE)

write_csv(TestData,'TestData.csv')



#-----------------------------------------------------------------------------------------------------
# Functionality for web app

# numdiff subtracts the value from the previous run when called within slider
numdiff <- function(x) {
  x[2] - x[1]
}

# User specifies if runs are identified by dates and a title for the generated output

Dates = TRUE
UsrTitle <- 'Potency 123456'

# User uploads data - 2 columns Column 1 = Run number or date, Column 2 = Measurement

UsrData <- TestData %>% 
  arrange(Run) %>% 
  mutate(Measure = log10(Measure),
         CumMean = slide_dbl(Measure, mean, .before = Inf),
         CumSD = slide_dbl(Measure, sd, .before = Inf),
         CumUCL = CumMean + 3 * CumSD,
         CumLCL = CumMean - 3 * CumSD,
         CumMSR = 2 * sqrt(2) * CumSD,
         Lst6Mean = slide_dbl(Measure, mean, .before = 6, .complete = TRUE),
         Lst6SD = slide_dbl(Measure, sd, .before = 6, .complete = TRUE),
         Lst6UCL = Lst6Mean + 3 * Lst6SD,
         Lst6LCL = Lst6Mean - 3 * Lst6SD,
         Lst6MSR = 2 * sqrt(2) * Lst6SD,
         Ratio = slide_dbl(Measure, numdiff, .before = 1, .complete = TRUE),
         RatioUCL = 3 * CumSD,
         RatioLCL = 3 * CumSD * (-1),
         across(-Run, function(x) 10^x))

# Create Graphs
XLabel <- if_else(Dates, 'Date', 'Run')  

CumRunPlot <- ggplot(UsrData, aes(x = Run, y = Measure)) +
  geom_point() +
  geom_line(aes(y = UsrData$CumMean, color = 'blue')) +
  geom_hline(yintercept = tail(UsrData$CumMean, 1)) +
  geom_line(aes(y = CumUCL, color = 'red'), linetype = 'dashed') +
  geom_line(aes(y = CumLCL, color = 'red'), linetype ='dashed') +
  scale_y_continuous(trans = "log10") +
  labs(title = 'Cumulative Run Chart',
       subtitle = UsrTitle,
       y = 'Potency') +
  theme_linedraw() +
  theme(legend.position = 'none')

  
Lst6RunPlot <- ggplot(UsrData, aes(x = Run, y = Measure)) +
  geom_point() +
  geom_line(aes(y = UsrData$Lst6Mean, color = 'blue')) +
  geom_hline(yintercept = tail(UsrData$CumMean, 1)) +
  geom_line(aes(y = Lst6UCL, color = 'red'), linetype = 'dashed') +
  geom_line(aes(y = Lst6LCL, color = 'red'), linetype ='dashed') +
  scale_y_continuous(trans = "log10") +
  labs(title = '6 Previous Run Chart',
       subtitle = UsrTitle,
       y = 'Potency') +
  theme_linedraw() +
  theme(legend.position = 'none')  

CumMSRPlot <- ggplot(slice(UsrData, -(1:2)), aes(x = Run, y = CumMSR)) +
  geom_point() +
  geom_line(aes(color = 'blue')) +
  geom_hline(yintercept = tail(UsrData$CumMSR, 1)) +
  labs(title = 'Cumulative MSR Chart',
       subtitle = UsrTitle,
       y = 'MSR') +
  theme_linedraw() +
  theme(legend.position = 'none')  


Lst6MSRPlot <- ggplot(UsrData, aes(x = Run, y = Lst6MSR)) +
  geom_point() +
  geom_line(aes(color = 'blue')) +
  geom_hline(yintercept = tail(UsrData$CumMSR, 1)) +
  labs(title = 'MSR (last 6 Runs) MSR Chart',
       subtitle = UsrTitle,
       y = 'MSR') +
  theme_linedraw() +
  theme(legend.position = 'none')  


RatioPlot <- ggplot(UsrData, aes(x = Run, y = Ratio)) +
  geom_point() +
  geom_hline(yintercept = 1) +
  geom_line(aes(y = RatioUCL, color = 'red'), linetype = 'dashed') +
  geom_line(aes(y = RatioLCL, color = 'red'), linetype ='dashed') +
  scale_y_continuous(trans = "log10") +
  labs(title = 'Ratio (Run/Previous Run) Chart',
       subtitle = UsrTitle,
       y = 'Ratio') +
  theme_linedraw() +
  theme(legend.position = 'none')  

# Summary table

DataSummary <- UsrData %>% 
  summarise(across(where(is.numeric), list(Min = ~min(.x, na.rm = TRUE),
                                             Max = ~max(.x, na.rm = TRUE), 
                                             Last = ~last(.x)))) %>% 
  pivot_longer((everything()), names_to = c('Column', 'Statistic'), names_sep = '_', values_to = 'Value' ) %>% 
  pivot_wider(names_from = Column, values_from = Value) %>% 
  mutate_if(is.numeric, ~round(., 2)) %>% 
  select(-CumSD, -Lst6SD)


