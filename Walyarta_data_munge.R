rm(list = ls(all = T))

setwd("Z:\\DEC\\Eighty_Mile_Beach_and_Walyarta_Conservation_Program\\DATA\\analysis\\20140916\\raw_data")

FD <- read.csv("Walyarta_All_Field_Data_20140916_forAnalysis.csv", header = TRUE)
FD$FieldDate <- as.Date(FD$FieldDate, "%d/%m/%Y")

IND <- read.csv("Indices_l8ut11174m_160914_USG_utm51pre_stackR.csv", header = TRUE)
