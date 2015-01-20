rm(list = ls(all = T))

library(dplyr)
library(lattice)
library(car)
library(ggplot2)
library(GGally)

data_loc <- paste0("Z:\\DEC\\Eighty_Mile_Beach_and_Walyarta_Conservation_",
                   "Program\\DATA\\analysis\\20140916\\raw_data")

graph_loc <- paste0("Z:\\DEC\\Eighty_Mile_Beach_and_Walyarta_Conservation_Program\\",
                    "DATA\\analysis\\20140916\\Wal_git\\Walyarta\\graphs")

setwd(data_loc)

fd <- read.csv("Walyarta_All_Field_Data_20140916_MGA51_forAnalysis.csv",
               header = TRUE)
fd$FieldDate <- as.Date(fd$FieldDate, "%d/%m/%Y")

ind <- read.csv("Indices_l8ut11174m_160914_USG_utm51pre_stackR_plus_testsites.csv",
                header = TRUE)

ind2 <- read.csv("Indices_l8ut11174m_160914_USG_utm51pre_stackR.csv",
                   header = TRUE)

#glimpse(fd)

#Field data munging
fd_out <- fd %>%
                group_by(SiteID) %>%
                summarise(
                        avgFC=mean(FCNadirSelected),
                        avgZN=mean(ZenithSelected),
                        avgAE=mean(AerialCanopyCover),
                        avgCE=mean(EstimateCover),
                        avgMR=mean(MuirVegetationClass),
                        avgSC=mean(Scode),
                        avgTP=mean(Template, na.rm=TRUE))
#labels for MUIR
MRlab <- c("dtm", "lmw", "tm", "otm", "dlg", "tm", "tm", "otm", "tm", "votm",
  "dsc", "sm", "dhg", "otm", "oh", "lg", "tm", "volg", "h", "tm",
  "dsd", "otm", "dlg", "votm", "s", "mdhg", "otm", "dtm", "volg", "lhc")
SClab <- c("m", "m", "m", "m", "g", "m", "m", "m", "m", "m", "o", "m", "g", 
           "m", "o", "g", "m", "g", "o", "m", "o", "m", "g", "m", "o", "g", 
           "m", "m", "g", "o")
soil2 <-  c("n", "g", "g", "g", "g", "r", "g", "r", "g", "r", "g", "r", "r", 
            "r", "r", "g", "g", "g", "r", "g", "g", "g", "n", "g", "g", "r",
            "r", "g", "g", "r")

soil <- c("n", "g", "g", "g", "g", "r", "g", "r", "g", "r", "g", "r", "r", 
          "r", "r", "g", "g", "g", "r", "g", "g", "g", "n", "g", "g", "r",
          "r", "g", "g", "r", "r", "r", "r", "r")



fd_out <- cbind(fd_out, MRlab, SClab, soil2)


#Index from image munging
ind_out <- ind %>%
                  mutate(b234=(-4 * b2) - (b3) + (2 * b4),
                         redI=(-2 * b1) + (b4) + (b6),
                         greyI=(-2 * b1) +(b3) - (b5),
                         b15=(b1+b5),
                         b1m35=(b1-b3+b5),
                         b135=(b1+b3+b5))

ind_out <- cbind(ind_out, soil)

#Combining datasets just for comparison WITHOUT test sites
all_data <- inner_join(fd_out, ind2, by="SiteID")
#pfc1 using zenith and aerial estimate where poss, otherwise 
all_data <- mutate(all_data, pfc1=ifelse(avgZN != 0, avgZN * avgAE,
                   avgFC))

all_data <- mutate(all_data, pfc2=ifelse(avgZN != 0, avgZN * avgCE,
                                         avgCE))

all_data <- mutate(all_data, pfc3=ifelse(avgZN != 0, (1-avgZN) * avgCE,
                                         avgCE))

#some models

#red
a <- filter(ind_out, soil != "g")


am <- lm(cover ~ redI, data = a)


#all sites
am2 <- lm(cover ~ redI, data = ind_out)
am3 <- lm(cover ~ i35, data = ind_out)

#grey
b <- filter(ind_out, soil != "r")

bm <- lm(cover ~ greyI, data = b) # 0.589

bm2 <- lm(cover ~ b3, data = b) # 0.6319

bm3 <- lm(cover ~ i35, data = b) # 0.6668

bm4 <- lm(cover ~ b15, data = b) # 0.6364

bm5 <- lm(cover ~ b1, data=b) # 0.4854

bm6 <- lm(cover ~ b1m35, data=b) # 0.626

bm7 <- lm(cover ~ b135, data=b) # 0.639

#models for NO TEST sites
cm <- lm(pfc1 ~ i35, data = all_data)
cm2 <- lm(pfc2 ~ i35, data = all_data)
cm3 <- lm(avgCE ~ i35, data = all_data)
cm4 <- lm(pfc3 ~ i35, data = all_data)


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


#graphs for NO SITES
ggplot(all_data, aes(x= i35, y= pfc1)) +
        geom_point(shape=1) +
        xlab("i35 index")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 100, y = 30, label = lm_eqn(cm), colour="black", 
                 size = 5, parse=TRUE)

ggplot(all_data, aes(x= i35, y= pfc2)) +
        geom_point(shape=1) +
        xlab("i35 index")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 100, y = 50, label = lm_eqn(cm2), colour="black", 
                 size = 5, parse=TRUE)

ggplot(all_data, aes(x= i35, y= avgCE)) +
        geom_point(shape=1) +
        xlab("i35 index")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 100, y = 75, label = lm_eqn(cm3), colour="black", 
                 size = 5, parse=TRUE)


#Analysis graphs

#RED
setwd(graph_loc)

#red Index
ggplot(filter(ind_out, soil != "g"), aes(x= redI, y= cover, label = siteID)) +
        geom_point(shape=1) +
        xlab("red index")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 100, y = 75, label = lm_eqn(am), colour="black", 
                 size = 3, parse=TRUE) +
        ggsave("red_index.png", width=6, height=4, dpi=400)
        #geom_text()

#red index all sites
ggplot(ind_out, aes(x= redI, y= cover, label = siteID)) +
        geom_point(shape=1) +
        xlab("red index")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 100, y = 75, label = lm_eqn(am2), colour="black", 
                 size = 3, parse=TRUE)

#grey i35 all sites
ggplot(ind_out, aes(x= i35, y= cover, label = siteID)) +
        geom_point(shape=1) +
        xlab("i35 index")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 100, y = 75, label = lm_eqn(am2), colour="black", 
                 size = 3, parse=TRUE)

#GREY
ggplot(filter(ind_out, soil != "r"), aes(x= greyI, y= cover, label = siteID)) +
        geom_point(shape=1) +
        xlab("grey index")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = -250, y = 75, label = lm_eqn(bm), colour="black", 
                 size = 3, parse=TRUE) +
        ggsave("grey_index.png", width=6, height=4, dpi=400)

#B3
ggplot(filter(ind_out, soil != "r"), aes(x= b3, y= cover, label = siteID)) +
        geom_point(shape=1) +
        xlab("band 3")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 60, y = 75, label = lm_eqn(bm2), colour="black", 
                 size = 3, parse=TRUE) +
        ggsave("band_3.png", width=6, height=4, dpi=400)
#i35
ggplot(filter(ind_out, soil != "r"), aes(x= i35, y= cover, label = siteID)) +
        geom_point(shape=1) +
        xlab("band 3 + band 5")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 100, y = 75, label = lm_eqn(bm3), colour="black", 
                 size = 3, parse=TRUE) +
        ggsave("i35.png", width=6, height=4, dpi=400)

#b1 + b5
ggplot(filter(ind_out, soil != "r"), aes(x= b15, y= cover, label = siteID)) +
        geom_point(shape=1) +
        xlab("band 1 + band 5")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 225, y = 75, label = lm_eqn(bm4), colour="black", 
                 size = 3, parse=TRUE) +
        ggsave("b15.png", width=6, height=4, dpi=400)

#b1
ggplot(filter(ind_out, soil != "r"), aes(x= b1, y= cover, label = siteID)) +
        geom_point(shape=1) +
        xlab("band 1")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 75, y = 75, label = lm_eqn(bm5), colour="black", 
                 size = 3, parse=TRUE) +
        ggsave("b1.png", width=6, height=4, dpi=400)

#b1m35
ggplot(filter(ind_out, soil != "r"), aes(x= b1m35, y= cover, label = siteID)) +
        geom_point(shape=1) +
        xlab("band 1 - band 3 + band 5")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 160, y = 75, label = lm_eqn(bm6), colour="black", 
                 size = 3, parse=TRUE) +
        ggsave("b1m35.png", width=6, height=4, dpi=400)

#b135
ggplot(filter(ind_out, soil != "r"), aes(x= b135, y= cover, label = siteID)) +
        geom_point(shape=1) +
        xlab("band 1 + band 3 + band 5")+
        geom_smooth(method=lm, se=FALSE) +
        annotate("text", x = 275, y = 75, label = lm_eqn(bm7), colour="black", 
                 size = 3, parse=TRUE) +
        ggsave("b135.png", width=6, height=4, dpi=400)






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





