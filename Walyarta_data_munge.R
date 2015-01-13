rm(list = ls(all = T))

library(dplyr)

setwd("Z:\\DEC\\Eighty_Mile_Beach_and_Walyarta_Conservation_Program\\DATA\\analysis\\20140916\\raw_data")

fd <- read.csv("Walyarta_All_Field_Data_20140916_forAnalysis.csv", header = TRUE)
fd$FieldDate <- as.Date(fd$FieldDate, "%d/%m/%Y")

ind <- read.csv("Indices_l8ut11174m_160914_USG_utm51pre_stackR.csv", header = TRUE)

glimpse(fd)
summarise(fd,avg=mean(FCNadirSelected))

a <- fd %>%
        group_by(SiteID) %>%
        summarise(
                avgFC=mean(FCNadirSelected),
                avgZN=mean(ZenithSelected),
                avgAE=mean(AerialCanopyCover))


