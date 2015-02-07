#################################################
## read data
mlb.df <- read.table("http://www.amstat.org/publications/jse/datasets/MLBattend.dat.txt")    

## clean data
colnames(mlb.df) <- c("team","league","div","season","attendance","runs.scored","runs.allowed","wins","losses","games.back")
mlb.df$season <- as.integer(mlb.df$season) + 1900
mlb.df[mlb.df$season == 1900,'season'] <- 2000

#################################################
## take the subset of data pertaining to the Boston Red Sox (BOS)
bos.df <- mlb.df[mlb.df$team=="BOS",]

#################################################
## Figure 1
## plot the attendance
plot(bos.df$attendance)

#################################################
## Figure 2
## plot attendance as a function of year
plot(bos.df$season,bos.df$attendance)

#################################################
## Figure 3
## make the labels nicer
plot(bos.df$season,bos.df$attendance,
  main="Red Sox attendance as a function of year",
  ylab="Attendance",xlab="Year",
     yaxt='n'
)
axis(side=2,at=c(1e6,1.5e6,2e6),labels=c("1mil","2mil","3mil"))
#################################################
## Figure 4
## same, but as a line plot
plot(bos.df$season,bos.df$attendance,
  main="Red Sox attendance as a function of year",
  ylab="Attendance",xlab="Year",type='l'
)

#################################################
## Figure 5
## same, but as a line-point hybrid
plot(bos.df$season,bos.df$attendance,
  main="Red Sox attendance as a function of year",
  ylab="Attendance",xlab="Year",type='b'
)

#################################################
## Figure 6
## points, attendance by year. But this time
## add the mean and 95% confidence interval
attendance.mean <- mean(bos.df$attendance)
attendance.sd <- sd(bos.df$attendance)
attendance.standard.error <- attendance.sd / sqrt(nrow(bos.df))
plot(bos.df$season,bos.df$attendance,
  main="Red Sox attendance as a function of year",
  ylab="Attendance",xlab="Year"
)
abline(h=attendance.mean)
abline(h=c(attendance.mean+1.96*attendance.standard.error,
  attendance.mean-1.96*attendance.standard.error),lty=2)


#################################################
## Figure 7
## points, attendance by year. 
## Add a linear regression
plot(bos.df$season,bos.df$attendance,
  main="Red Sox attendance as a function of year",
  ylab="Attendance",xlab="Year")
linear.model.A <- lm(attendance ~ season, data=bos.df)
abline(linear.model.A,col='red')

#################################################
## Figure 7B
## Same, but add text to the plot
plot(bos.df$season,bos.df$attendance,main="Red Sox attendance as a function of year",ylab="Attendance",xlab="Year")
linear.model.A <- lm(attendance ~ season, data=bos.df)
abline(linear.model.A,col='red')
r2 <- summary(linear.model.A)$r.squared
r2Text <- sprintf("R-squared = %.2f", r2)
text(1990,1.95e6,r2Text)


#################################################
## Figure 8
## points, attendance by year, but make point size be proportional to 
## the difference between runs scored and runs allowed.

#- Define a reusable normalization function 
#- Takes a vector, normalizes it to range 0.5-5.5 
#- modified from David Weisman
normalizeFunc <- function(vec) {
  .5  + (  5 * (vec - min(vec)) / (max(vec) - min(vec)) )
}

run.delta <- bos.df$runs.scored - bos.df$runs.allowed
pointsize <- normalizeFunc(run.delta)

plot(bos.df$season,bos.df$attendance,main="Red Sox attendance as a function of year",ylab="Attendance",xlab="Year",cex=pointsize)

#################################################
## Figure 9
hist(bos.df$runs.scored,freq=FALSE,main="BOS Runs Scored Histogram",xlab="Runs Scored")

#################################################
## Figure 10
par(mfrow=c(1,3))
hist(bos.df$runs.scored,main="BOS Runs Scored Histogram, 5 bins",xlab="Runs Scored",breaks=5)
hist(bos.df$runs.scored,main="BOS Runs Scored Histogram, 10 bins",xlab="Runs Scored",breaks=10,col='red')
hist(bos.df$runs.scored,main="BOS Runs Scored Histogram, 15 bins",xlab="Runs Scored",breaks=15,col='blue')
par(mfrow=c(1,1))

#################################################
## Figure 11
## All of major league baseball (back to the mlb.df data frame).
## How many were in attendance vs. year
plot(mlb.df$season,mlb.df$attendance,main="MLB attedance as a function of year",
  ylab="Attendance",xlab="Year"
)

#################################################
## Figure 12
mlb.df$clr <- "blue"  ## initialize a color column in DF
mlb.df[mlb.df$team=="BOS",'clr'] <- 'red'
mlb.df[mlb.df$team=="NYA",'clr'] <- "black"
mlb.df$pch <- 1 ## intialize a point type column
mlb.df[mlb.df$team=="BOS" | mlb.df$team=="NYA",'pch'] <- 16
plot(mlb.df$season,mlb.df$attendance,main="MLB attedance as a function of year",
  ylab="Attendance",xlab="Year",col=mlb.df$clr,pch=mlb.df$pch
)
legend('topleft',fill=c("black","red","blue"),
  legend=c("Yankees","Red Sox","Everyone else")
)

#################################################
## Figure 13
## First box and whisker plot
boxplot(mlb.df$attendance,ylab="Attendance",main="All attendance boxplot")

#################################################
## Figure 14
## Box plot broken out by year
boxplot(attendance ~ season, data=mlb.df,
  ylab="Attendance",main="Attendance by year boxplot",las=2
)

#################################################
## Figure 15
## Boxplot broken out by year and league
boxplot(runs.scored ~ league + season, data=mlb.df,las=2,cex.axis=.5,
  col=c("red","blue"),main="Runs scored by year and league",ylab="Runs Scored"
)
abline(v=8.5,lty=2)
text(4,1000,"No DH")
text(12,1000,"DH")
legend('bottomright',fill=c("red","blue"),
  legend=c("American League","National League")
)

#################################################
## save Figure 15
pdf(file="boxplot_AL_vs_NL_runs.pdf",height=6,width=14)
boxplot(runs.scored ~ league + season, data=mlb.df,las=2,cex.axis=.5,
  col=c("red","blue"),main="Runs scored by year and league",ylab="Runs Scored"
)
abline(v=8.5,lty=2)
text(4,1000,"No DH")
text(12,1000,"DH")
legend('bottomright',fill=c("red","blue"),
  legend=c("American League","National League")
)
dev.off()

