rm(list = ls(all = T))

library(dplyr)
library(lattice)
library(car)
library(ggplot2)
library(GGally)

setwd(paste0("Z:\\DEC\\Eighty_Mile_Beach_and_Walyarta_Conservation_",
      "Program\\DATA\\analysis\\20140916\\raw_data"))

# fd <- read.csv("Walyarta_All_Field_Data_20140916_MGA51_forAnalysis.csv",
#                header = TRUE)
# fd$FieldDate <- as.Date(fd$FieldDate, "%d/%m/%Y")

ind <- read.csv("Indices_l8ut11174m_160914_USG_utm51pre_stackR_plus_testsites.csv",
                header = TRUE)

#glimpse(fd)

#Field data munging
# fd_out <- fd %>%
#                 group_by(SiteID) %>%
#                 summarise(
#                         avgFC=mean(FCNadirSelected),
#                         avgZN=mean(ZenithSelected),
#                         avgAE=mean(AerialCanopyCover),
#                         avgCE=mean(EstimateCover),
#                         avgMR=mean(MuirVegetationClass),
#                         avgSC=mean(Scode),
#                         avgTP=mean(Template, na.rm=TRUE))
#labels for MUIR
# MRlab <- c("dtm", "lmw", "tm", "otm", "dlg", "tm", "tm", "otm", "tm", "votm",
#   "dsc", "sm", "dhg", "otm", "oh", "lg", "tm", "volg", "h", "tm",
#   "dsd", "otm", "dlg", "votm", "s", "mdhg", "otm", "dtm", "volg", "lhc", "test", 
#   "test", "test", "test")
# SClab <- c("m", "m", "m", "m", "g", "m", "m", "m", "m", "m", "o", "m", "g", 
#            "m", "o", "g", "m", "g", "o", "m", "o", "m", "g", "m", "o", "g", 
#            "m", "m", "g", "o", "t", "t", "t", "t")
soil <- c("n", "g", "g", "g", "g", "r", "g", "r", "g", "r", "g", "r", "r", 
          "r", "r", "g", "g", "g", "r", "g", "g", "g", "n", "g", "g", "r",
          "r", "g", "g", "r", "r", "r", "r", "r")



#fd_out <- cbind(fd_out, MRlab, SClab, soil)


#Index from image munging
ind_out <- ind %>%
                  mutate(b234=(-4 * b2) - (b3) + (2 * b4),
                         redI=(-2 * b1) + (b4) + (b6),
                         greyI=(-2 * b1) +(b3) - (b5))

ind_out <- cbind(ind_out, soil)

#Combining datasets
#all_data <- inner_join(fd_out, ind_out, by="SiteID")
#pfc1 using zenith and aerial estimate where poss, otherwise 
# all_data <- mutate(all_data, pfc1=ifelse(avgZN != 0, avgZN * avgAE,
#                    avgFC))
# 
# all_data <- mutate(all_data, pfc2=ifelse(avgZN != 0, avgZN * avgCE,
#                                          avgCE))

#some models

#red
a <- filter(ind_out, soil != "g")

am <- lm(cover ~ redI, data = a)

#grey
b <- filter(ind_out, soil != "r")

bm <- lm(cover ~ greyI, data = b)

bm2 <- lm(cover ~ b3, data = b) #band 3


lm_eqn = function(m) {
        
        l <- list(a = format(coef(m)[1], digits = 2),
                  b = format(abs(coef(m)[2]), digits = 2),
                  r2 = format(summary(m)$r.squared, digits = 3));
        
        if (coef(m)[2] >= 0)  {
                eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
        } else {
                eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
        }
        
        as.character(as.expression(eq));                 
}

#Analysis graphs

#RED
ggplot(filter(ind_out, soil != "g"), aes(x= redI, y= cover, label = siteID)) +
        geom_point(shape=1) +
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 100, y = 75, label = lm_eqn(am), colour="black", 
                 size = 5, parse=TRUE)#+
        #geom_text()

#GREY
ggplot(filter(ind_out, soil != "r"), aes(x= greyI, y= cover, label = siteID)) +
        geom_point(shape=1) +
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = -275, y = 75, label = lm_eqn(bm), colour="black", 
                 size = 5, parse=TRUE) +
        geom_text()

#B3
ggplot(filter(ind_out, soil != "r"), aes(x= b3, y= cover, label = siteID)) +
        geom_point(shape=1) +
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 60, y = 75, label = lm_eqn(bm2), colour="black", 
                 size = 5, parse=TRUE) #+
        #geom_text()







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

ggplot(all_data, aes(x= ndvi, y= pfc2, colour = SClab, label = SiteID)) +
        geom_point(shape=1) +
        geom_text()+
        geom_smooth(method=lm, se=FALSE)
#i35
ggplot(all_data, aes(x= i35, y= pfc1, colour = SClab, label = SiteID)) +
        geom_point(shape=1) +
        geom_text()+
        geom_smooth(method=lm, se=FALSE)

ggplot(all_data, aes(x= i35, y= pfc2, colour = SClab, label = SiteID)) +
        geom_point(shape=1) +
        geom_text()+
        geom_smooth(method=lm, se=FALSE)

#b234
ggplot(all_data, aes(x= b234, y= pfc1, colour = SClab, label = SiteID)) +
        geom_point(shape=1) +
        geom_text()+
        geom_smooth(method=lm, se=FALSE)

ggplot(all_data, aes(x= b234, y= pfc2, colour = SClab, label = SiteID)) +
        geom_point(shape=1) +
        geom_text()+
        geom_smooth(method=lm, se=FALSE)

#Graphs to keep 
setwd(paste0("Z:\\DEC\\Eighty_Mile_Beach_and_Walyarta_Conservation_",
                   "Program\\DATA\\analysis\\20140916\\Wal_git\\Walyarta\\graphs"))
png("Pairs Plot.png", width = 1024, height = 768)
ggpairs(all_data[,c("avgFC", "avgZN", "avgAE", "avgTP", "i35", "ndvi", "b234")])
dev.off()





