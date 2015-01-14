rm(list = ls(all = T))

library(dplyr)
library(lattice)
library(car)
library(ggplot2)
library(GGally)

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
                        avgSC=mean(Scode),
                        avgTP=mean(Template, na.rm=TRUE))
#labels for MUIR
MRlab <- c("dtm", "lmw", "tm", "otm", "dlg", "tm", "tm", "otm", "tm", "votm",
  "dsc", "mdhg", "dhg", "otm", "oh", "lg", "tm", "volg", "h", "tm",
  "dsd", "otm", "dlg", "votm", "s", "mdhg", "otm", "dtm", "volg", "lhc")
SClab <- c("m", "m", "m", "m", "g", "m", "m", "m", "m", "m", "o", "g", "g", 
           "m", "o", "g", "m", "g", "o", "m", "o", "m", "g", "m", "o", "g", 
           "m", "m", "g", "o")

cbind(fd_out$avgSC, SClab)

fd_out <- cbind(fd_out, MRlab, SClab)


#Index from image munging
ind_out <- ind %>%
                  mutate(b234=(-4 * b2) - (b3) + (2 * b4))

#Combining datasets
all_data <- inner_join(fd_out, ind_out, by="SiteID")
#pfc1 using zenith and aerial estimate where poss, otherwise 
all_data <- mutate(all_data, pfc1=ifelse(avgZN != 0, avgZN * avgAE,
                   avgFC))
#some models

#ndvi
a <- filter(all_data, SClab == "m")
badsites <- c("Wal_rs_004", "Wal_rs_006", "Wal_rs_014")
a1 <- filter(a, !(SiteID %in% badsites))

am <- summary(lm(pfc1 ~ ndvi, data = a))

am1 <- summary(lm(pfc1 ~ ndvi, data = a1))



b <- filter(all_data, SClab =="g")

bm <- summary(lm(pfc1 ~ ndvi, data = b))

c <- filter(all_data, SClab == "o")
om <- summary(lm(pfc1 ~ ndvi, data = c))

d <- filter(all_data, SClab != "m")
dm <- summary(lm(pfc1 ~ ndvi, data = d))


#i35
e <- filter(all_data, SClab == "m")
# badsites <- c("Wal_rs_004", "Wal_rs_006", "Wal_rs_014")
# e1 <- filter(a, !(SiteID %in% badsites))

em <- summary(lm(pfc1 ~ i35, data = e))

em1 <- summary(lm(pfc1 ~ i35, data = e1))



f <- filter(all_data, SClab =="g")

fm <- summary(lm(pfc1 ~ i35, data = f))

g <- filter(all_data, SClab == "o")
gm <- summary(lm(pfc1 ~ i35, data = g))

h <- filter(all_data, SClab != "m")
hm <- summary(lm(pfc1 ~ i35, data = h))

#b234
i <- filter(all_data, SClab == "m")
# badsites <- c("Wal_rs_004", "Wal_rs_006", "Wal_rs_014")
# e1 <- filter(a, !(SiteID %in% badsites))

im <- summary(lm(pfc1 ~ b234, data = i))

em1 <- summary(lm(pfc1 ~ b234, data = e1))



j <- filter(all_data, SClab =="g")

jm <- summary(lm(pfc1 ~ b234, data = j))

k <- filter(all_data, SClab == "o")
km <- summary(lm(pfc1 ~ b234, data = k))

n <- filter(all_data, SClab != "m")
nm <- summary(lm(pfc1 ~ b234, data = n))


table(SClab)








#Analysis graphs
pairs(~avgFC+avgZN+avgAE+avgTP+i35+ndvi+b234, data=all_data,
      main="Pairs Scatterplot for Walyarta Data")

scatterplot.matrix(~avgFC+avgZN+avgAE+avgTP+i35+ndvi+b234|MRlab,
                   data=all_data)

ggplot(all_data, aes(x= i35, y= avgZN, colour= MRlab)) +
        geom_point(shape=1) #+
        #geom_smooth(method=lm, se=FALSE)
ggplot(all_data, aes(x= ndvi, y= pfc1, label= SClab)) +
        geom_text()

ggplot(all_data, aes(x= ndvi, y= pfc1, label= SClab)) +
        geom_text()


ggplot(all_data, aes(x= ndvi, y= pfc1, label= SClab, colour = SClab)) +
        geom_text() +
        geom_smooth(method=lm, se=FALSE)

ggplot(all_data, aes(x= ndvi, y= pfc1, colour = SClab)) +
        geom_point(shape=1) +
        geom_smooth(method=lm, se=FALSE)
#ndvi
ggplot(all_data, aes(x= ndvi, y= pfc1, colour = SClab, label = SiteID)) +
        geom_point(shape=1) +
        geom_text()+
        geom_smooth(method=lm, se=FALSE)
#i35
ggplot(all_data, aes(x= i35, y= pfc1, colour = SClab, label = SiteID)) +
        geom_point(shape=1) +
        geom_text()+
        geom_smooth(method=lm, se=FALSE)

#b234
ggplot(all_data, aes(x= b234, y= pfc1, colour = SClab, label = SiteID)) +
        geom_point(shape=1) +
        geom_text()+
        geom_smooth(method=lm, se=FALSE)


#Graphs to keep 
setwd(paste0("Z:\\DEC\\Eighty_Mile_Beach_and_Walyarta_Conservation_",
                   "Program\\DATA\\analysis\\20140916\\Wal_git\\Walyarta\\graphs"))
png("Pairs Plot.png", width = 1024, height = 768)
ggpairs(all_data[,c("avgFC", "avgZN", "avgAE", "avgTP", "i35", "ndvi", "b234")])
dev.off()





