# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
## URL of the dataset
sourceDataURL <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
## File names for data
fileList <- c('activity.csv')
## Temporary File for storing the downloaded zip then extract the files we want into R
temp <- tempfile()
download.file(sourceDataURL, temp, method = "libcurl")
## Read the file from the source
activity.data <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
## Make real dates
activity.data[, "date"] <- as.Date(activity.data[, "date"], "%Y-%m-%d")
## summarize data
total.daily.steps <- aggregate(activity.data[, "steps"], by = list(activity.data[, "date"]), FUN = sum, na.rm = TRUE)
colnames(total.daily.steps) <- c("date", "daily.steps")
```
### Summary of the data read in

```r
summary(activity.data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?
### Histogram of the number of steps daily

```r
hist( total.daily.steps[, "daily.steps"]
     ,xlab = "Steps Per Day"
     ,ylab = "Frequency"
     ,breaks = 10
     ,main = "Histogram of Total Daily Steps (na removed)"
    )
```

![](PA1_template_files/figure-html/mean steps per day histogram-1.png) 

### Mean and Median daily steps

```r
mean(total.daily.steps[, "daily.steps"])
```

```
## [1] 9354.23
```

```r
median(total.daily.steps[, "daily.steps"])
```

```
## [1] 10395
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
