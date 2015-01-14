rm(list = ls(all = T))

library(dplyr)
library(lattice)
library(car)

setwd(paste0("Z:\\DEC\\Eighty_Mile_Beach_and_Walyarta_Conservation_",
      "Program\\DATA\\analysis\\20140916\\raw_data"))

fd <- read.csv("Walyarta_All_Field_Data_20140916_forAnalysis.csv",
               header = TRUE)
fd$FieldDate <- as.Date(fd$FieldDate, "%d/%m/%Y")

ind <- read.csv("Indices_l8ut11174m_160914_USG_utm51pre_stackR.csv",
                header = TRUE)

glimpse(fd)

#Field data munging
fd_out <- fd %>%
                group_by(SiteID) %>%
                summarise(
                        avgFC=mean(FCNadirSelected),
                        avgZN=mean(ZenithSelected),
                        avgAE=mean(AerialCanopyCover),
                        avgMR=mean(MuirVegetationClass),
                        avgTP=mean(Template, na.rm=TRUE))
#labels for MUIR
MRlab <- c("dtm", "lmw", "tm", "otm", "dlg", "tm", "tm", "otm", "tm", "votm",
  "dsc", "mdhg", "dhg", "otm", "oh", "lg", "tm", "volg", "h", "tm",
  "dsd", "otm", "dlg", "votm", "s", "mdhg", "otm", "dtm", "volg", "lhc")
fd_out <- cbind(fd_out, MRlab)


#Index from image munging
ind_out <- ind %>%
                  mutate(b234=(-4 * b2) - (b3) + (2 * b4))

#Combining datasets
all_data <- inner_join(fd_out, ind_out, by="SiteID")


#Analysis graphs
pairs(~avgFC+avgZN+avgAE+avgTP+i35+ndvi+b234, data=all_data,
      main="Pairs Scatterplot for Walyarta Data")

scatterplot.matrix(~avgFC+avgZN+avgAE+avgTP+i35+ndvi+b234|MRlab,
                   data=all_data)


