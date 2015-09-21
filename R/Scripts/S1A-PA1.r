###################################################
# Name: S1A-PA1.r
# To: Develop R code for Reproducible Research: Peer Assessment 1
#     When code is validated and works, transfer to PA1_template.Rmd
# To Knitr:
# - library(knitr)
# - setwd()
# - knit2htmlt('Doc.Rmd')
# - browseURL('Doc.html')
# To include plot
#    ```{r showplot, fig.height=4}
#       par(...)
#       plot(...)
#    ```
# To include table
#    ```{r shwotable, results='asis'
#       library(xtable)
#       fit <- lm(...)
#       xt <- xtable(summary(fit))
#       print(xt, type='html')
#    ```
#################################################
options(stringsAsFactors=F)
options('stringsAsFactors')

zz <- function(n='') source(paste("Scripts/t", n, sep=''))
PLOT <- function() {OLDPNG <- PNG;PNG <<- 'png';zz();PNG <<- 'pdf';zz();PNG <<- OLDPNG}
pdfPLOT <- function() { OLDPNG <- PNG; PNG <<- 'pdf'; zz(); PNG <<- OLDPNG}
pngPLOT <- function() { OLDPNG <- PNG; PNG <<- 'png'; zz(); PNG <<- OLDPNG}

setwd('/Users/Jill/Documents/Fcollin_Admin/Courses/ReproducibleResearch/RepData_PeerAssessment1/R')
WRKDIR <- getwd()
WRKDIR

###########################################################
GRAPH_DIR <- "Graphs/S1A"
dir.create(GRAPH_DIR)

GRAPH_PNG <- "Graphs/S1A/png/"
dir.create(GRAPH_PNG)

###########################################################
# Read Data and Preprocess
###########################################################
list.files('..')
#[1] "activity.csv"      "activity.zip"      "doc"               "instructions_fig" 
#[5] "PA1_template.html" "PA1_template.md"   "PA1_template.Rmd"  "R"                
#[9] "README.md"        

activity.frm <- read.table('../activity.csv', header=T, sep=',', as.is=T)
head(activity.frm)
 #steps       date interval
#1    NA 2012-10-01        0
#2    NA 2012-10-01        5
#3    NA 2012-10-01       10
#4    NA 2012-10-01       15
#5    NA 2012-10-01       20
#6    NA 2012-10-01       25

Hr.vec  <- activity.frm$interval %/% 100
Min.vec <- activity.frm$interval %% 100
sum(100*Hr.vec+Min.vec == activity.frm$interval)
#[1] 17568

Hr.vec = formatC(Hr.vec, width=2, flag='0')
Min.vec = formatC(Min.vec, width=2, flag='0')

DayofWeek.vec <- weekdays(as.Date(activity.frm$date))

DayType.vec <- ifelse(is.element(DayofWeek.vec, c('Saturday', 'Sunday')), 'WeekEnd', 'WeekDay')
table(DayType.vec, DayofWeek.vec)
          #DayofWeek.vec
#DayType.vec Friday Monday Saturday Sunday Thursday Tuesday Wednesday
    #WeekDay   2592   2592        0      0     2592    2592      2592
    #WeekEnd      0      0     2304   2304        0       0         0

Act.frm <- data.frame(
   Date = activity.frm$date,
   DayofWeek = DayofWeek.vec,
   DayType = DayType.vec,
   Hr = Hr.vec, 
   Min = Min.vec, 
   HrMin = paste(Hr.vec, Min.vec, sep=':'),
   Interval = activity.frm$interval,
   Steps = activity.frm$steps )

str(Act.frm)
#data.frame':	17568 obs. of  8 variables:
 #$ Date     : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
 #$ DayofWeek: chr  "Monday" "Monday" "Monday" "Monday" ...
 #$ DayType  : chr  "WeekDay" "WeekDay" "WeekDay" "WeekDay" ...
 #$ Hr       : chr  "00" "00" "00" "00" ...
 #$ Min      : chr  "00" "05" "10" "15" ...
 #$ HrMin    : chr  "00:00" "00:05" "00:10" "00:15" ...
 #$ Interval : int  0 5 10 15 20 25 30 35 40 45 ...
 #$ Steps    : int  NA NA NA NA NA NA NA NA NA NA ...

table(Act.frm$Hr)
 #00  01  02  03  04  05  06  07  08  09  10  11  12  13  14  15  16  17  18  19 
#732 732 732 732 732 732 732 732 732 732 732 732 732 732 732 732 732 732 732 732 
 #20  21  22  23 
#732 732 732 732 

table(Act.frm$Min)
  #00   05   10   15   20   25   30   35   40   45   50   55 
#1464 1464 1464 1464 1464 1464 1464 1464 1464 1464 1464 1464 

with(Act.frm, table(Date))
#Date
#2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 2012-10-08 2012-10-09 
       #288        288        288        288        288        288        288        288        288 
#2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
       #288        288        288        288        288        288        288        288        288 
#2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 
       #288        288        288        288        288        288        288        288        288 
#2012-10-28 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
       #288        288        288        288        288        288        288        288        288 
#2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 2012-11-12 2012-11-13 2012-11-14 
       #288        288        288        288        288        288        288        288        288 
#2012-11-15 2012-11-16 2012-11-17 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
       #288        288        288        288        288        288        288        288        288 
#2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
       #288        288        288        288        288        288        288 

#####################################################
## What is mean total number of Steps taken per day?
#####################################################
# 1. Calculate the total number of steps taken per day
with(Act.frm, 
   Steps.ByDay.lst <<- split(Steps, Date)
   )
Steps.PerDay.vec <- sapply(Steps.ByDay.lst, sum, narm=T)

summary(Steps.PerDay.vec)
   #Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     #42    8842   10770   10770   13300   21200       8 

sum(sapply(Steps.ByDay.lst, function(x) sum(is.na(x)))==288)
#[1] 8
# Check!

# 2. Make a histogram of the total number of steps taken each day
if(PNG=='x11') quartz(width=8, height=5)
if(PNG == 'png')
    png(file.path(GRAPH_PNG,
       paste("S1A-HistStepsPerDay.png", sep='')),
         height=480, width=480*11/8)
if(PNG == 'pdf')
    pdf(file.path(GRAPH_PDF,
       paste("S1A-HistStepsPerDay.pdf", sep='')),
         height=8, width=11)

hist(Steps.PerDay.vec, xlab='Steps per Day', main='Total Number of Steps per Day')

if(PNG != 'x11') dev.off()


mean(Steps.PerDay.vec, na.rm=T)
#[1] 10767.19

median(Steps.PerDay.vec, na.rm=T)
#[1] 10766

#####################################################
## What is the average daily activity pattern?
#####################################################
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#    and the average number of steps taken, averaged across all days (y-axis)

with(Act.frm,
   Steps.ByInt.lst <<- split(Steps, HrMin)
   )
Steps.ByInt.vec <- sapply(Steps.ByInt.lst, mean, na.rm=T)
Steps.ByInt.vec[1:5]
    #00:00     00:05     00:10     00:15     00:20 
#1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 

# Compare this with aggregate 
StepsByInt.frm <- aggregate(Steps ~ HrMin, data=Act.frm, FUN=mean, na.rm=T)

str(StepsByInt.frm)
#'data.frame':	288 obs. of  2 variables:
 #$ HrMin: chr  "00:00" "00:05" "00:10" "00:15" ...
 #$ Steps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
 
cor(StepsByInt.frm$Steps, Steps.ByInt.vec)
# Check!


if(PNG=='x11') quartz(width=8, height=5)
if(PNG == 'png')
    png(file.path(GRAPH_PNG,
       paste("S1A-AveDailyActPatt.png", sep='')),
         height=480, width=480*11/8)
if(PNG == 'pdf')
    pdf(file.path(GRAPH_PDF,
       paste("S1A-AveDailyActPatt.pdf", sep='')),
         height=8, width=11)

plot(StepsByInt.frm$Steps, xlab='Hour of Day', ylab='Steps per 5 Min Interval', type='l', xaxt='n')

Hour.vec <- sapply(strsplit(StepsByInt.frm$HrMin, split=':'), function(x) x[1])
axis(side=1, at=match(unique(Hour.vec), Hour.vec), Hour.vec[match(unique(Hour.vec), Hour.vec)],
     las=1)
title("Average Daily Activity Pattern")

if(PNG != 'x11') dev.off()

# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
names(Steps.ByInt.vec)[which.max(Steps.ByInt.vec)]
#[1] "08:35"


#####################################################
## Imputing missing values
#####################################################
# 1. Calculate and report the total number of missing values in the dataset 
#  (i.e. the total number of rows with NAs)
sum(is.na(Act.frm$Steps))
#[1] 2034

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
# Assume NAs are due to monitoring faults.
# Use other similar days to impute missing step counts
Act.frm[1:10,]
         #Date DayofWeek DayType Hr Min HrMin Interval Steps
#1  2012-10-01    Monday WeekDay 00  00 00:00        0    NA
#2  2012-10-01    Monday WeekDay 00  05 00:05        5    NA
#3  2012-10-01    Monday WeekDay 00  10 00:10       10    NA
#4  2012-10-01    Monday WeekDay 00  15 00:15       15    NA
#5  2012-10-01    Monday WeekDay 00  20 00:20       20    NA
#6  2012-10-01    Monday WeekDay 00  25 00:25       25    NA
#7  2012-10-01    Monday WeekDay 00  30 00:30       30    NA
#8  2012-10-01    Monday WeekDay 00  35 00:35       35    NA
#9  2012-10-01    Monday WeekDay 00  40 00:40       40    NA
#10 2012-10-01    Monday WeekDay 00  45 00:45       45    NA
 

Day_HrMin.vec <- paste(Act.frm$DayofWeek, Act.frm$HrMin, sep='_')
Day_HrMin.Step.lst <- split(Act.frm$Steps, Day_HrMin.vec)
Day_HrMin.medianStep.vec <- sapply(Day_HrMin.Step.lst, median, na.rm=T)[Day_HrMin.vec]
     
# 3. Create a new dataset with the missing data filled in.

Act.frm$Steps <- ifelse(is.na(Act.frm$Steps), Day_HrMin.medianStep.vec, Act.frm$Steps)
Act.frm[1:10,]
         #Date DayofWeek DayType Hr Min HrMin Interval Steps
#1  2012-10-01    Monday WeekDay 00  00 00:00        0     0
#2  2012-10-01    Monday WeekDay 00  05 00:05        5     0
#3  2012-10-01    Monday WeekDay 00  10 00:10       10     0
#4  2012-10-01    Monday WeekDay 00  15 00:15       15     0
#5  2012-10-01    Monday WeekDay 00  20 00:20       20     0
#6  2012-10-01    Monday WeekDay 00  25 00:25       25     0
#7  2012-10-01    Monday WeekDay 00  30 00:30       30     0
#8  2012-10-01    Monday WeekDay 00  35 00:35       35     0
#9  2012-10-01    Monday WeekDay 00  40 00:40       40     0
#10 2012-10-01    Monday WeekDay 00  45 00:45       45     0

with(Act.frm, 
   Steps.ByDay.lst <<- split(Steps, Date)
   )
Steps.PerDay.vec <- sapply(Steps.ByDay.lst, sum, narm=T)

summary(Steps.PerDay.vec)
   ##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     #42    6779   10400    9706   12810   21200 
 
summary(Act.frm$Steps)
   #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    #0.0     0.0     0.0    33.7     9.0   806.0 

# 4.Make a histogram of the total number of steps taken each day 
# and Calculate and report the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?

if(PNG=='x11') quartz(width=8, height=5)
if(PNG == 'png')
    png(file.path(GRAPH_PNG,
       paste("S1A-HistStepsPerDay2.png", sep='')),
         height=480, width=480*11/8)
if(PNG == 'pdf')
    pdf(file.path(GRAPH_PDF,
       paste("S1A-HistStepsPerDay2.pdf", sep='')),
         height=8, width=11)

hist(Steps.PerDay.vec, xlab='Steps per Day', main='Total Number of Steps per Day (Imputed)')

if(PNG != 'x11') dev.off()


mean(Steps.PerDay.vec, na.rm=T)
#[1] 9706.238

median(Steps.PerDay.vec, na.rm=T)
#[1] 10396

#####################################################
## Are there differences in activity patterns between weekdays and weekends?
# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
#    indicating whether a given date is a weekday or weekend day.
# 
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#    and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
#    See the README file in the GitHub repository to see an example of 
#    what this plot should look like using simulated data.
#####################################################

if(PNG=='x11') quartz(width=8, height=5)
if(PNG == 'png')
    png(file.path(GRAPH_PNG,
       paste("S1A-AveDailyActPattByDayType.png", sep='')),
         height=480, width=480*11/8)
if(PNG == 'pdf')
    pdf(file.path(GRAPH_PDF,
       paste("S1A-AveDailyActPattByDayType.pdf", sep='')),
         height=8, width=11)
#par(mfrow=c(1,1), mar=c(2,2,4,1), oma=c(2,2,2,0))

Steps.ByInt.vec.lst <- lapply(unique(Act.frm$DayType),
   function(DT) {
   DT.rows <- which(Act.frm$DayType == DT)
   DT.Act.frm <- Act.frm[DT.rows,]
   with(DT.Act.frm, DT.Steps.ByInt.lst <<- split(Steps, HrMin))
   sapply(DT.Steps.ByInt.lst, mean, na.rm=T)
  })
names(Steps.ByInt.vec.lst) <- unique(Act.frm$DayType)

sapply(Steps.ByInt.vec.lst, length)
#WeekDay WeekEnd 
    #288     288 

sum(names(Steps.ByInt.vec.lst[[1]]) == names(Steps.ByInt.vec.lst[[2]]))
#[1] 288

StepsPerIntByDayType.vec <- sapply(Steps.ByInt.vec.lst, mean, na.rm=T)
StepsPerIntByDayType.vec
 #WeekDay  WeekEnd 
#31.78476 39.08181 
 
plot(x=1:length(Steps.ByInt.vec.lst[["WeekDay"]]),  xlab='Hour of Day',
        y=Steps.ByInt.vec.lst[["WeekDay"]], ylab='Steps (Imputed) per 5 Min Interval', 
        type='l', col='blue', ylim=range(unlist(Steps.ByInt.vec.lst)), xaxt='n')

lines(x=1:length(Steps.ByInt.vec.lst[["WeekEnd"]]),  xlab='Hour of Day',
        y=Steps.ByInt.vec.lst[["WeekEnd"]], ylab='Steps (Imputed) per 5 Min Interval', 
        type='l', col='red', xaxt='n')

Hour.vec <- sapply(strsplit(names(Steps.ByInt.vec.lst[["WeekEnd"]]), split=':'), function(x) x[1])
axis(side=1, at=match(unique(Hour.vec), Hour.vec),
     formatC(Hour.vec,width=2, flag='0')[match(unique(Hour.vec), Hour.vec)],
     las=1)
title("Average Daily Activity Pattern by Day Type")


legend('topleft',
   legend=c(paste("WeekDay (Mean=", round(StepsPerIntByDayType.vec["WeekDay"]),")", sep=''),
            paste("WeekEnd (Mean=", round(StepsPerIntByDayType.vec["WeekEnd"]),")", sep='')),
   lty=1, col=c('blue', 'red'),bty='n', cex=0.8)


if(PNG != 'x11') dev.off()

############################################################
# Use lattice or gglplot instead
############################################################
library(lattice)

StepsByIntDayType.frm <- aggregate(Steps ~ HrMin * DayType, data=Act.frm, FUN=mean, na.rm=T)
str(StepsByIntDayType.frm)
#'data.frame':	576 obs. of  3 variables:
 #$ HrMin  : chr  "00:00" "00:05" "00:10" "00:15" ...
 #$ DayType: chr  "WeekDay" "WeekDay" "WeekDay" "WeekDay" ...
 #$ Steps  : num  2.0222 0.4 0.1556 0.1778 0.0889 ...

Steps.ByInt.vec.lst[["WeekDay"]][1:5]
     #00:00      00:05      00:10      00:15      00:20 
#2.02222222 0.40000000 0.15555556 0.17777778 0.08888889 
cor(Steps.ByInt.vec.lst[["WeekDay"]], StepsByIntDayType.frm$Steps[StepsByIntDayType.frm$DayType=="WeekDay"])
# Check!

StepsByIntDayType.frm$HrMin <- as.factor(StepsByIntDayType.frm$HrMin)
StepsByIntDayType.frm$DayType <- as.factor(StepsByIntDayType.frm$DayType)
at.vec <- pretty(1:length(unique(StepsByIntDayType.frm$HrMin)))[-1]

if(PNG=='x11') quartz(width=8, height=5)
if(PNG == 'png')
    png(file.path(GRAPH_PNG,
       paste("S1A-AveDailyActPattByDayType2.png", sep='')),
         height=480, width=480*11/8)
if(PNG == 'pdf')
    pdf(file.path(GRAPH_PDF,
       paste("S1A-AveDailyActPattByDayType2.pdf", sep='')),
         height=8, width=11)
#par(mfrow=c(1,1), mar=c(2,2,4,1), oma=c(2,2,2,0))


print(plot(xyplot(Steps ~ HrMin | DayType, data=StepsByIntDayType.frm, type='l', layout=c(1,2),
    scales=list(x=list(at=at.vec, labels=unique(StepsByIntDayType.frm$HrMin)[at.vec])),
    main="Average Daily Activity Pattern by Day Type")))

if(PNG != 'x11') dev.off()

