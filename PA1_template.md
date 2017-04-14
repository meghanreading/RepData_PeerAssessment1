```{r}
setwd("~/Desktop/Data Science Specialization/data")

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

unzip(zipfile="./data/Dataset.zip",exdir="./data")

activitydata<- read.csv("~/Desktop/Data Science Specialization/data/activity.csv") 

library(dplyr)
library(ggplot2)

head(activitydata)
dim(activitydata)
glimpse(activitydata)
summary(activitydata)

activitydata$date<- as.Date(activitydata$date)
```

#Mean total number of steps taken per day
##### 1.Calculate the total number of steps taken per day:
```{r}
Total_Steps<- activitydata%>%
    group_by(date)%>%
    filter(!is.na(steps))%>%
    summarise(total_steps = sum(steps, na.rm=TRUE))
Total_Steps
```

##### 2. Plot using ggplot:
```{r}
ggplot(Total_Steps, aes(x = total_steps)) +
    geom_histogram(fill = "blue", binwidth = 1000) +
    labs(title = "Daily Steps", x = "Total Steps", y = "Frequency")
```

##### 3. Calculate and report the mean and median of the total number of steps taken per day:
```{r}
Mean_Steps<- mean(Total_Steps$total_steps, na.rm=TRUE)
Mean_Steps
Median_Steps<- median(Total_Steps$total_steps, na.rm=TRUE)
Median_Steps
```

#Average daily activity pattern
##### 1. Calculating Avg. Steps:
```{r}
Interval<- activitydata%>%
    group_by(interval)%>%
    filter(!is.na(steps))%>%
    summarise(avg_steps = mean(steps, na.rm=TRUE))
Interval
```

##### 1.a Plotting Avg. Steps:
```{r}
ggplot(Interval, aes(x =interval , y=avg_steps)) +
    geom_line(color="blue", size=1) +
    labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
```

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
Interval[which.max(Interval$avg_steps),]
```

#Imputing missing values
##### 1. Calculate total number of missing values in the dataset:
```{r}
sum(is.na(activitydata$steps))
```

##### 2. Imputing missing values using mean for each day and 3. Create a new dataset that is equal to the original dataset but with the missing data filled in:
```{r}
activitydata2<- activitydata
nas<- is.na(activitydata2$steps)
avg_interval<- tapply(activitydata2$steps, activitydata2$interval, mean, na.rm=TRUE, simplify = TRUE)
activitydata2$steps[nas] <- avg_interval[as.character(activitydata2$interval[nas])]
names(activitydata2)
```

##### 4. Check if no missing value is appearing:
```{r}
sum(is.na(activitydata2))
```

##### 5. Reorder columns (for better understanding of the data):
```{r}
activitydata2<- activitydata2[, c("date", "interval", "steps")]
head(activitydata2)
```

#Create histogram of the total number of steps taken each day and calculate mean and median total number of steps taken per day:
```{r}
Total_Steps2<- activitydata2%>%
    group_by(date)%>%
    summarise(total_steps = sum(steps, na.rm=TRUE))
Total_Steps2
```
```{r echo=FALSE}
ggplot(Total_Steps2, aes(x = total_steps)) +
    geom_histogram(fill = "blue", binwidth = 1000) +
    labs(title = "Daily Steps including Missing values", x = "Interval", y = "No. of Steps")
```

##### 4.2 Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
Mean_Steps2<- mean(Total_Steps2$total_steps, na.rm=TRUE)
Mean_Steps2
Median_Steps2<- median(Total_Steps2$total_steps, na.rm=TRUE)
Median_Steps2
```
##### Answer: The impact of imputing missing data with the average number of steps in the same 5-min interval is that both the mean and the median are same : 10766.19

#Differences in activity patterns between weekdays and weekends
```{r}
head(activitydata2)
```
##### 5.1 Create new varibale called WeekType for Weekday & Weekend:
```{r}
activitydata2<- activitydata2%>%
    mutate(weektype= ifelse(weekdays(activitydata2$date)=="Saturday" | weekdays(activitydata2$date)=="Sunday", "Weekend", "Weekday"))

head(activitydata2)
```

###### Plotting:
```{r}
Interval2<- activitydata2%>%
    group_by(interval, weektype)%>%
    summarise(avg_steps2 = mean(steps, na.rm=TRUE))
head(Interval2)
```
```{r}
plot<- ggplot(Interval2, aes(x =interval , y=avg_steps2, color=weektype)) +
    geom_line() +
    labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") +
    facet_wrap(~weektype, ncol = 1, nrow=2)
print(plot)
```

#####Answer: Yes there are some differences. During weekdays activity is greatest in the morning, however overall there is more activity on weekends than weekdays. 
