# Reproducible Research: Peer Assessment 1


```
## Loading required package: grid
```

## Loading and preprocessing the data
The input file is downloaded automatically only if an existing copy is unavailable, and assumed to be static and independent of the download date/time. This eliminates the need to keep timestamped versions of the data for every run. Only csv files are kept with all other intermediate (i.e. zip files) discarded immediately.

```r
ZIPFILE <- "data.zip"
RAWFILE <- "activity.csv"
FILEURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if (! file.exists(RAWFILE)) {
  download.file(url = FILEURL, destfile = ZIPFILE)
  unzip(ZIPFILE)
  file.remove(ZIPFILE)
}
DATA <- read.csv(RAWFILE)
```

## What is mean total number of steps taken per day?
First we build summary with the total number of steps for each recorded date, ignoring NA values; then plot using ggplot2 to build the required histogram

```r
SUMD <- ddply(DATA, .(date), summarise, steps = mean(steps, na.rm = T))
rownames(SUMD) <- SUMD$date
ggplot(data = SUMD, aes(x = steps)) + 
  geom_histogram() +
  ggtitle("Steps per day") + 
  xlab("steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The mean and median are ``37.3825996`` and ``37.3784722`` respectively

## What is the average daily activity pattern?
For the average daily pattern we proceed similarly as before indexing by interval instead of date. We then plot in the same way we proceeded before 

```r
SUMI <- ddply(DATA, .(interval), summarise, steps = mean(steps, na.rm = T))
rownames(SUMI) <- SUMI$interval
ggplot(data = SUMI, aes(interval, steps)) + 
  geom_line() +
  ggtitle("Average steps per interval") +
  xlab("interval") +
  ylab("steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

The interval with the highest average is ``835``

## Imputing missing values

We can easily find the number of missing values to be ``2304`` out of ``17568``. The original mean was calculated to be ``37.3825996``, we'll use this value as a simple fill strategy


```r
omean <- mean(DATA$steps[! is.na(DATA$steps)])
fillfn <- function(steps, date, interval) { ifelse(is.na(steps), omean, steps) }
FDATA <- mdply(DATA, fillfn) %>% mutate(steps = V1)
SUMF <- ddply(FDATA, .(interval), summarise, steps = mean(steps, na.rm = T))
p1 <- ggplot(data = SUMF, aes(interval, steps)) + 
  geom_line() +
  ggtitle("Steps per interval (with filled data)") + 
  xlab("steps")
SUMS <- merge(SUMI, SUMF, by = "interval") %>% 
  mutate(steps.d = steps.y - steps.x)
```

The new mean is now ``37.3825996``. We've introduced a bias of ``0`` into our set with the selected fill strategy.
