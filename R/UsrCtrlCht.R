source("R/FunCtrlCht.R")
source("R/PotCCTestData.R")

# Use for testing of FunCtrlCht.R as well as functionality for Shiny client
# Client outline ----------------------------------------
# User specifies if runs are identified by dates and a title for the generated
# output. File must contain at least 2 columns. First column 'Run' identifies a
# specific experiment (Run) and should be a date(recommended) or a string.
# Second column 'Data' contains the potency values for the reference compound
# and should be a dbl Optional 3rd column to label the replicates within each
# run (user convenience only, not used in analysis)

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

chartType <- ifelse(
  max(repCount$Reps) == 1 | sum(repCount$Ind) / length(repCount$Run) > 0.49,
  "ind",
  ifelse(min(repCount$Reps) > 1,
    "rep",
    "both"
  )
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


# Write Report files - for development - Replace with code for usr display and
# download of charts and data ---------------------

# Create directory for report files Uncomment to save report locally
ReportDir <- paste0("UsrReports/", usrTitle)
dir.create(ReportDir)

switch(chartType,
  ind = {
    write_csv(IndChtReport$IChartData,
      file = paste0(ReportDir, "/", "IChartData.csv")
    )
    write_csv(IndChtReport$MRChartData,
      file = paste0(ReportDir, "/", "MRChartData.csv")
    )
    write_csv(MsrChartReport$MSRData,
      file = paste0(ReportDir, "/", "MSRData.csv")
    )
    ggsave(
      filename = paste0(ReportDir, "/", "IRChart.png"),
      plot = IndChtReport$IChart, height = 4, width = 6, units = "in"
    )
    ggsave(
      filename = paste0(ReportDir, "/", "MRChart.png"),
      plot = IndChtReport$MRChart, height = 4, width = 6, units = "in"
    )
    ggsave(
      filename = paste0(ReportDir, "/", "MSRChart.png"),
      plot = MsrChartReport$MSRChart, height = 4, width = 6, units = "in"
    )
  },
  rep = {
    write_csv(RepChtReport$XbarChartData,
      file = paste0(ReportDir, "/", "XbarChartData.csv")
    )
    write_csv(RepChtReport$SChartData,
      file = paste0(ReportDir, "/", "SChartData.csv")
    )
    write_csv(MsrChartReport$MSRData,
      file = paste0(ReportDir, "/", "MSRData.csv")
    )
    ggsave(
      filename = paste0(ReportDir, "/", "XbarChart.png"),
      plot = RepChtReport$XbarChart, height = 4, width = 6, units = "in"
    )
    ggsave(
      filename = paste0(ReportDir, "/", "SChart.png"),
      plot = RepChtReport$SChart, height = 4, width = 6, units = "in"
    )
    ggsave(
      filename = paste0(ReportDir, "/", "MSRChart.png"),
      plot = MsrChartReport$MSRChart, height = 4, width = 6, units = "in"
    )
  },
  both = {
    write_csv(IndChtReport$IChartData,
      file = paste0(ReportDir, "/", "IChartData.csv")
    )
    write_csv(IndChtReport$MRChartData,
      file = paste0(ReportDir, "/", "MRChartData.csv")
    )
    ggsave(
      filename = paste0(ReportDir, "/", "IRChart.png"),
      plot = IndChtReport$IChart, height = 4, width = 6, units = "in"
    )
    ggsave(
      filename = paste0(ReportDir, "/", "MRChart.png"),
      plot = IndChtReport$MRChart, height = 4, width = 6, units = "in"
    )
    write_csv(RepChtReport$XbarChartData,
      file = paste0(ReportDir, "/", "XbarChartData.csv")
    )
    write_csv(RepChtReport$SChartData,
      file = paste0(ReportDir, "/", "SChartData.csv")
    )
    ggsave(
      filename = paste0(ReportDir, "/", "XbarChart.png"),
      plot = RepChtReport$XbarChart, height = 4, width = 6, units = "in"
    )
    ggsave(
      filename = paste0(ReportDir, "/", "SChart.png"),
      plot = RepChtReport$SChart, height = 4, width = 6, units = "in"
    )
    write_csv(MsrChartReport$MSRData,
      file = paste0(ReportDir, "/", "MSRData.csv")
    )
    ggsave(
      filename = paste0(ReportDir, "/", "MSRChart.png"),
      plot = MsrChartReport$MSRChart, height = 4, width = 6, units = "in"
    )
  }
)
