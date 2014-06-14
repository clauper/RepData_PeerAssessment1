
#rm(list = ls())


#install.packages("knitr")
#library(knitr)



getwd()
setwd("\\\\SEAMS/DFS/HomeDirs/CLauper/R/Courseara/Course4ReproducibleResearch/Week2/repdata-data-activity")
getwd()
dir()


d.activity <- read.csv(file = "activity.csv", na.strings="NA", header=T)


#-------------------------------------------------------------------------------------------------------------

TotalSteps <- sapply(split(d.activity$steps,d.activity$date),sum)

hist(TotalSteps, breaks=100, xlab="Number of Steps", main="Total Steps per day", col="darkred")
summary(TotalSteps)

#-------------------------------------------------------------------------------------------------------------

AvSteps <- sapply(split(d.activity$steps,d.activity$interval),mean,na.rm=T)

d.stepsinterval <- data.frame(Interval=unique(d.activity$interval),Av.Steps=AvSteps )

d.stepsinterval[d.stepsinterval$Av.Steps==max(AvSteps),]

plot(AvSteps ~ unique(d.activity$interval), type="l", xlab="Interval", ylab="Steps", col="darkgreen")
abline(h=max(AvSteps), col="red")
abline(v=d.stepsinterval[d.stepsinterval$Av.Steps==max(AvSteps),1], col="purple")
text(c(d.stepsinterval[d.stepsinterval$Av.Steps==max(AvSteps),1],d.stepsinterval[d.stepsinterval$Av.Steps==max(AvSteps),1]),c(190,10),labels=c(paste("max.steps =",round(max(AvSteps),0)),paste("interval =",d.stepsinterval[d.stepsinterval$Av.Steps==max(AvSteps),1])),col=c("red","purple"),pos=c(2,2))



#-------------------------------------------------------------------------------------------------------------

length(is.na(d.activity$steps)[is.na(d.activity$steps)=="TRUE"])

#copy the dataset
d.infilled <- d.activity

#if step is NA, replace it with the mean across the appropriate 5-minute interval;
#leave it as it is otherwise.
for(i in 1:nrow(d.activity)){
  if(is.na(d.infilled$steps[i])) d.infilled$steps[i] <- round(d.stepsinterval[d.stepsinterval$Interval==d.infilled$interval[i],2],0)
  else d.infilled$steps[i]<-d.infilled$steps[i]
}

TotalStepsI <- sapply(split(d.infilled$steps,d.infilled$date),sum)


hist(TotalStepsI, breaks=100, xlab="Number of Steps", main="Total Steps per day (infilled)", col="darkred")
summary(TotalStepsI)


#-------------------------------------------------------------------------------------------------------------
#install.packages("ggplot2")
#library(ggplot2)
#install.packages("reshape")
#library(reshape)

d.infilled$day <- weekdays(strptime(d.infilled$date, format="%Y-%m-%d"))
for(i in 1:length(d.infilled$day)){
  if((d.infilled$day[i]=="Sunday")||(d.infilled$day[i]=="Saturday")) d.infilled$weeksplit[i] <-"weekend" else d.infilled$weeksplit[i] <-"weekday"
}


d.agg <- aggregate(d.infilled[,c(1,3)],by=list("interval"=d.infilled$interval,"weeksplit"=d.infilled$weeksplit), mean, na.rm=T)


g.plot3 <- qplot(interval, steps,data=d.agg,facets=weeksplit~.,  geom="line", color=weeksplit) 
g.plot3 <-g.plot3 + labs(title = "Average Number of Steps Taken")
g.plot3 <-g.plot3 + labs(x= "Interval")
g.plot3 <-g.plot3 + labs(y= "Average Steps")
g.plot3

