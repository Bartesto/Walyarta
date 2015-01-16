rm(list = ls())

setwd(paste0("Z:\\DEC\\Eighty_Mile_Beach_and_Walyarta_Conservation_",
             "Program\\DATA\\analysis\\20140916\\CVA"))

# Read .cvm file from working directory...
classes <- 34 #update from lis file
nbands <- 6
filename <- "l8ut11174m_160914_USG_utm51pre_2.cvm"
filename

# Load file and extract labels
raw = scan(filename, skip=classes+2)
cooked = matrix(raw, nrow=classes, ncol=nbands+1, byrow=T)
raw2 = read.table(skip=2, nrows=classes, filename) #changed classes to 326
cv=cooked
labels=(raw2[,9])
labelno=cooked[,1]
labeltext=as.character(labelno)
density<-substr(labels,7,9)
labelnum<-(raw2[,3])
substrate<-substr(labels,1,1)


# Display subsequent plots CV1 vs CV2

plot(cv[,2], cv[,3], xlab="CV1", ylab="CV2", type="n", main = paste(filename, "CV1 vs CV2", sep=" "), asp=1)
text(cv[,2], cv[,3], labels=labels, cex=0.7)

plot(cv[,3], cv[,4], xlab="CV2", ylab="CV3", type="n", main = paste(filename, "CV2 vs CV3 1st_pass", sep=" "), asp=1)
text(cv[,3], cv[,4], labels=labels, cex=0.7)

plot(cv[,4], cv[,5], xlab="CV3", ylab="CV4", type="n", main = paste(filename, "CV3 vs CV4 1st_pass", sep=" "), asp=1)
text(cv[,4], cv[,5], labels=labels, cex=0.7)

plot(cv[,5], cv[,6], xlab="CV4", ylab="CV5", type="n", main = paste(filename, "CV4 vs CV5 1st_pass", sep=" "), asp=1)
text(cv[,5], cv[,6], labels=labels, cex=0.7)

plot(cv[,6], cv[,7], xlab="CV5", ylab="CV6", type="n", main = paste(filename, "CV5 vs CV6 1st_pass", sep=" "), asp=1)
text(cv[,6], cv[,7], labels=labels, cex=0.7)

plot(cv[,2], cv[,4], xlab="CV1", ylab="CV3", type="n", main = paste(filename, "CV1 vs CV3 1st_pass", sep=" "), asp=1)
text(cv[,2], cv[,4], labels=labels, cex=0.7)


#Plot zoomed in
xmin <- -1
xmax <- 20
ymin <- 6
ymax <- 25
plot(cv[,2], cv[,3], xlab="CV1", ylab="CV2", type="n", main = paste(filename, "CV1 vs CV2", sep=" "), xlim=c(xmin,xmax), ylim=c(ymin,ymax))
text(cv[,2], cv[,3], labels=labels, cex=0.7)

#Plot with class number only USING ABOVE ZOOM
plot(cv[,2], cv[,3], xlab="CV1", ylab="CV2", type="n", main = paste(filename, "CV1", " vs CV2 ", sep=" "), xlim=c(xmin,xmax), ylim=c(ymin,ymax))
text(cv[,2], cv[,3], labels=labeltext, cex=0.7)

#test between CV's from 1 site to another
opt1=16
opt2=17

a1=cv[opt1,2]
b1=cv[opt1,3]
c1=cv[opt1,4]

a2=cv[opt2,2]
b2=cv[opt2,3]
c2=cv[opt2,4]

output=c(a1-a2,b1-b2,c1-c2)
output



# Save plot as jpeg file in working directory...

jpeg(file=jpegfile, quality=100, width=800, height=800)
plot(cv[,1], cv[,2], type="n", main = paste(sensor, "_", year, "_", map, quadrant, "_pre90plant_z", zone, ".jpg", sep=""), asp=1)
text(cv[,1], cv[,2], col=colours, labels=labels, cex=0.5)
dev.off()


#-----------------------------------------------------------------------------------------------------------
# Define function giving separation between points in CV-space metric...

pair.sep <- function(i,j){
   ind <- c(i,j)
   res1 <- cbind(ind,cv[ind,])
   res2 <- sqrt(sum((cv[i,] - cv[j,])^2))
   list(CV_coords = res1,Separation = res2)
}

# Apply separation function to pairs of interest (use their index labels)...

pair.sep(91,87)
pair.sep(11,47)
pair.sep(11,26)
pair.sep(9,26)
pair.sep(9,25)


#-----------------------------------------------------------------------------------------------------------
# Zoom in on plot to better read the labels if necessary...
# simply change the following limits to suit... 

xmin <- 25
xmax <- 35
ymin <- 15
ymax <- 30

plot(cv[,1], cv[,2], type="n", main = paste(sensor, "_", year, "_", map, quadrant, "_pre90plant_z", zone, ".jpg", sep=""), asp=1,
     xlim=c(xmin,xmax), ylim=c(ymin,ymax))
text(cv[,1], cv[,2], col=colours, labels=labels, cex=0.5)



