# Test all standard use cases for FunCtrlCht.R

source("R/FunCtrlCht.R")

# Mean Shift Flag (new functionality from previos version) ==================

usrTitle <- "Mean Shift Data"

usrData <- read_csv(file = "TestData/MeanShiftData.csv")

meanShiftReps <- xbars_charts(usrData, usrTitle)

meanShiftInd <- ind_charts(usrData, usrTitle)

meanShiftMSR <- msr_calc(usrData, usrTitle)

# Constant Reps =========================

usrTitle <- "Constant Reps Data"

usrData <- read_csv(file = "TestData/CtrlChtConstReps.csv")

constantRepsXbarS <- xbars_charts(usrData, usrTitle)

constantRepsMSR <- msr_calc(usrData, usrTitle)

# Single Rep Data ========================

usrTitle <- "Single Rep Data"

usrData <- read_csv(file = "TestData/CtrlChtSingleRep.csv")

singleRepInd <- ind_charts(usrData, usrTitle)

singleRepMSR <- msr_calc(usrData, usrTitle)

# Variable Reps Data ====================

usrTitle <- "Variable Reps Data"

usrData <- read_csv(file = "TestData/CtrlChtVarReps.csv")

variableRepsXbarS <- xbars_charts(usrData, usrTitle)

variableRepsInd <- ind_charts(usrData, usrTitle)

variableRepsMSR <- msr_calc(usrData, usrTitle)
