
# read in data set
data <- read.table("https://www.ndbc.noaa.gov/view_text_file.php?filename=41004h2015.txt.gz&dir=data/historical/stdmet/", header = F)
indx <- sapply(data, is.factor)
data[indx] <- lapply(data[indx], function(x) as.numeric(as.character(x)))

# take out unnecessary rows and columns
data <- data[-1,]
data <- data[,-c(17,18)]
rownames(data) <- seq(length=nrow(data))
colnames(data) <- c("YY", "MM", "DD", "hh", "mm", "WDIR", "WSPD", "GST" , "WVHT", "DPD" , "APD" , "MWD" , "PRES", "ATMP", "WTMP", "DEWP")
for (j in 1:length(data$WVHT))
{
  if (data$WVHT[j]>20) {
    data <- data[-j,]
  }
}
attach(data)

# we can see a non-linear pattern between WSPD vs WVHT, so try polynomial fitting:
WSPD.2 <- WSPD*WSPD
summary(lm(WVHT ~ WSPD + WSPD.2))
plot(WSPD, WVHT, cex=.6, main='Exponential Relationship Between Wind Speed and Wave Height')
curve(-0.0521536*x+0.0130451*x^2+0.9535438,add=T,col='red',lwd=2)

# plot various relationships
par(mfrow=c(2,2))
plot(MM, ATMP, pch=15, cex=.7)
plot(PRES, WSPD, pch=20, cex=.4); abline(v=1013.25, lty=2, col='blue')
plot(DPD, WVHT, pch=20, cex=.4)
plot(WTMP[-c(1846,1847)], WVHT[-c(1846,1847)], pch=20, cex=.4)
dev.off()

# plot a wave period vs direction chart, non-polar
szn.col <- c()
s.col <- c("deepskyblue","chartreuse1","gold","darkorange2")
for (j in 1:length(MM))
{
  if (MM[j] == 12 | MM[j] == 1 | MM[j] == 2) {szn.col[j] <- "deepskyblue"}
  else if (MM[j] == 3 | MM[j] == 4 | MM[j] == 5)  {szn.col[j] <- "chartreuse1"}
  else if (MM[j] == 6 | MM[j] == 7 | MM[j] == 8)  {szn.col[j] <- "gold"}
  else {szn.col[j] <- "darkorange2"}
}
plot(DPD, MWD, col=szn.col, pch=20, main='Relationship Between Wave Period and Direction', cex=.5)
legend('topright', legend=c("Winter", "Spring", "Summer", "Fall"), pch=20, col=s.col)

# plot the same data on a polar graph
library(plotrix)
polar.plot(DPD,MWD,main="Dominant Wave Period by Direction (Buoy 41004 - 2015)",lwd=1,line.col=szn.col,start=90,clockwise=TRUE,show.grid.labels=2)
legend('bottomleft', legend=c("Winter", "Spring", "Summer", "Fall"), lty=1, col=s.col)
# polar.plot(DPD,MWD,main="Dominant Wave Period by Direction (Buoy 41004 - 2015)",lwd=.1,line.col=4,start=90,clockwise=TRUE,rp.type='s',point.symbols=20,show.grid.labels=2)

# overlay a simplified polar plot over a map of the region
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-84, -74), ylim = c(26, 39), asp = 1)
par(new=TRUE)
polar.plot(DPD,MWD,main="Dominant Wave Period by Direction (Buoy 41004 - 2015)",lwd=1,line.col=4,start=90,clockwise=TRUE,show.grid.labels=2)
