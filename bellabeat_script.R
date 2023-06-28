### first install all the packages we need:

install.packages('tidyverse') # to handle dataframes
install.packages('janitor') # to better clean data
install.packages('lubridate') # to better handle datetime values
install.packages('ggplot2') # to create visualizations
install.packages('scales') # to better handle visalizations
install.packages('ggridges') # to create ridgeplots
install.packages('patchwork') # to display several plots
install.packages('ggpubr') # to add customized details to plots
library('tidyverse')
library('janitor')
library('lubridate')
library('ggplot2')
library('ggridges')
library('scales')
library('patchwork')
library('ggpubr')

### read the files and make sure column names are consistent:

daily_activity <- read_csv('data/dailyActivity_merged.csv') %>%
  clean_names()
  
daily_sleep <- read_csv('data/sleepDay_merged.csv') %>%
  clean_names()

hourly_intensity <- read_csv('data/hourlyIntensities_merged.csv') %>%
  clean_names()

hourly_steps <- read_csv('data/hourlySteps_merged.csv') %>%
  clean_names()
 
### let's take a look at datasets:

glimpse(daily_activity)
glimpse(daily_sleep)
glimpse(hourly_intensity)
glimpse(hourly_steps)

### let's clean up and transform the data based on our needs
### (rename date columns, change their data type, transform and select data):

daily_activity <- daily_activity %>%
  mutate(id=as.character(id),
         date = mdy(activity_date),
         day = weekdays(date),
         active_minutes=rowSums(across(c(very_active_minutes, 
                            fairly_active_minutes,
                            lightly_active_minutes))),
         total_minutes=rowSums(across(c(active_minutes, sedentary_minutes)))) %>%
  select(id, date, day, total_steps, total_distance,
         active_minutes, sedentary_minutes, total_minutes, calories)

daily_sleep <- daily_sleep %>%
  separate(sleep_day, into=c('sleep_date','time'), sep=' ') %>%
  mutate(id=as.character(id),
         date = mdy(sleep_date),
         day = weekdays(date)) %>%
  select(id, date, day, total_minutes_asleep)

hourly_intensity <- hourly_intensity %>%
  mutate(id=as.character(id),
         datetime = mdy_hms(activity_hour),
         date = as.Date(datetime),
         day = weekdays(date),
         hour = as.numeric(format(as.POSIXct(datetime),format = '%H'))) %>%
  select(id, date, day, hour, total_intensity, average_intensity)

hourly_steps <- hourly_steps %>%
  mutate(id=as.character(id),
         datetime = mdy_hms(activity_hour),
         date = as.Date(datetime),
         day = weekdays(date),
         hour = as.numeric(format(as.POSIXct(datetime),format = '%H'))) %>%
  select(id, date, day, hour, step_total)

### let's get days of week ordered correctly:

daily_activity$day <- ordered(daily_activity$day, 
                              levels = c("Monday", "Tuesday", "Wednesday", 
                                         "Thursday", "Friday", "Saturday", 
                                         "Sunday"))

daily_sleep$day <- ordered(daily_sleep$day, 
                              levels = c("Monday", "Tuesday", "Wednesday", 
                                         "Thursday", "Friday", "Saturday", 
                                         "Sunday"))

hourly_intensity$day <- ordered(hourly_intensity$day, 
                              levels = c("Monday", "Tuesday", "Wednesday", 
                                         "Thursday", "Friday", "Saturday", 
                                         "Sunday"))

hourly_steps$day <- ordered(hourly_steps$day, 
                                levels = c("Monday", "Tuesday", "Wednesday", 
                                           "Thursday", "Friday", "Saturday", 
                                           "Sunday"))

### check for any missing values:

any(is.na(daily_activity))
any(is.na(daily_sleep))
any(is.na(hourly_intensity))
any(is.na(hourly_steps))

## check for duplicates:

get_dupes(daily_activity) # no duplicates
get_dupes(daily_sleep) # 3 duplicates
get_dupes(hourly_intensity) # no duplicates
get_dupes(hourly_steps) # no duplicates

## get rid of found duplicates:

daily_sleep <- daily_sleep %>% distinct()

### check for zero values:

daily_activity %>%
  filter(total_steps==0 & active_minutes==0) %>%
  group_by(id) %>%
  summarize(count=n_distinct(date)) # 15 users have 1-14 days with zero activity

daily_sleep %>%
  filter(total_minutes_asleep==0) %>%
  group_by(id) %>%
  summarize(count=n_distinct(date)) # no zero values

hourly_intensity %>%
  group_by(id,date) %>%
  summarize(sum=sum(total_intensity)) %>%
  filter(sum==0) %>%
  group_by(id) %>%
  summarize(count=n_distinct(date)) # 15 users have 1-14 days with zero intensity

hourly_steps %>%
  group_by(id,date) %>%
  summarize(sum=sum(step_total)) %>%
  filter(sum==0) %>%
  group_by(id) %>%
  summarize(count=n_distinct(date)) # 15 users have 1-14 days with zero steps

### because of some zero values present, let's check the correctness of day-to-day data:

daily_activity %>%
  group_by(date) %>%
  summarize(avg=mean(active_minutes)) %>%
  ggplot() +
  aes(x=date, y=avg, fill='#F8766D') +
  geom_col() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_date(breaks = '1 day', date_labels="%d.%m",
               expand=c(0,0)) +
  ylab('active minutes on average') +
  theme(legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.background=element_blank())
# let's remove all the data from the last day to not skew the results, as it is 
# two times smaller than the other days data:

daily_activity <- daily_activity %>%
  filter(date!='2016-05-12')

daily_sleep <- daily_sleep %>%
  filter(date!='2016-05-12')

hourly_intensity <- hourly_intensity %>%
  filter(date!='2016-05-12')

hourly_steps <- hourly_steps %>%
  filter(date!='2016-05-12')

### let's see how comprehensive and correct our data is:

length(unique(daily_activity$id)) # all 33 people
length(unique(daily_sleep$id)) # 24 out of 33
length(unique(hourly_intensity$id)) # all 33 people
length(unique(hourly_steps$id)) # all 33 people

length(unique(daily_activity$date)) # all 30 days
length(unique(daily_sleep$date)) # all 30 days
length(unique(hourly_intensity$date)) # all 30 days
length(unique(hourly_steps$date)) # all 30 days

min(daily_activity$total_minutes) # 6 mins
max(daily_activity$total_minutes) # 1440 mins = 24 hours

############################### VISUALIZATIONS ###############################

### to make consistent plots, let's create own theme:

bellabeat_theme <- function(..., base_size = 10) {
  
  theme(
    text = element_text(family = "Roboto", size = base_size),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    plot.title.position = "plot",
    plot.title = element_text(size = 16,
                              face = "bold",
                              color = "black",
                              vjust = 1,
                              hjust = 0.5),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "#d0d0d0", linewidth = 0.3),
    panel.background = element_blank(), panel.border = element_blank(),
    plot.background = element_blank(), legend.position = 'none')
}


############################## ACTIVITY TRACKING ##############################

### let's see how often users use devices for daily activities:

count_activity <- daily_activity %>%
  group_by(id) %>%
  summarize(count = n_distinct(date))
  
a1 <- ggplot(count_activity) +
  aes(y=count, x=id, fill=id) + 
  geom_col() +
  geom_hline(yintercept=mean(count_activity$count), alpha=0.5) +
  scale_y_continuous(breaks = seq(0, 32, by = 2),expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  ylab('days registered') +
  xlab('users') +
  labs(title='Daily Activity Tracking Usage by User') +
  bellabeat_theme() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        panel.grid.major=element_blank())

### let's see how often users use devices for activity during the day:

hours_activity <- daily_activity %>%
  filter(total_steps!=0 & active_minutes!=0) %>%
  group_by(id) %>%
  summarize(avg_hours = mean(total_minutes)/60)

a2 <- ggplot(hours_activity) +
  aes(y=avg_hours, x=id, fill=id) + 
  geom_col() +
  geom_hline(yintercept=mean(hours_activity$avg_hours), alpha=0.5) +
  scale_y_continuous(breaks = seq(0, 24, by = 2),expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  ylab('hours registered on average') +
  xlab('users') +
  labs(title='Hourly Activity Tracking Usage by User') +
  bellabeat_theme() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        panel.grid.major=element_blank())

### how active are users throughout the week:

wraps <- as_labeller(
  c('sedentary_minutes'='sedentary minutes', 'active_minutes'='active minutes'))

a3 <- daily_activity %>%
  pivot_longer(active_minutes:sedentary_minutes, 
               names_to='minutes', values_to='result') %>%
  ggplot(aes(y=fct_rev(day), x=result, fill=day)) +
  geom_density_ridges(alpha=0.7,
                      quantile_lines=TRUE, quantile_fun=mean) +
  ylab('day of the week') +
  xlab('minutes registered') +
  labs(title='Active/Sedentary Minutes Registered by Day of the Week') +
  scale_x_continuous(breaks=seq(0,1500, by=200), expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  bellabeat_theme() +
  scale_fill_hue() +
  facet_wrap(vars(minutes), ncol = 2, labeller=wraps)


(a1 | a2) / a3


############################## STEPS TRACKING ##############################

### check for correlation between number of steps and calories burnt:

st1 <- daily_activity %>%
  ggplot(aes(x=total_steps, y=calories)) + 
  geom_point(position = 'jitter', alpha=0.5) +
  geom_smooth(method="lm",se=FALSE, color='#F8766D') +
  xlab('number of steps') +
  ylab('calories burnt') +
  labs(title='Steps-Calories Relationship') +
  scale_y_continuous(breaks=seq(0,5000, by=500), labels = number_format(),
                     expand = c(0,0)) +
  scale_x_continuous(breaks=seq(0,25000, by=5000), labels = number_format(),
                     expand=c(0,0)) +
  coord_cartesian(xlim = c(0, 25000)) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01) +
  bellabeat_theme()

### average number of steps by hour:

st2 <- hourly_steps %>%
  group_by(hour) %>%
  summarize(average = mean(step_total)) %>%
  ggplot() +
  aes(y=average, x=hour) + 
  geom_line(color='#F8766D') +
  ylab('steps on average') +
  xlab('hour') +
  labs(title='Average Number of Steps by Hour') +
  scale_x_continuous(breaks = seq(0,23, by=1), expand=c(0,0)) +
  scale_y_continuous(breaks = seq(0, 700, by = 100), expand=c(0,0)) +
  coord_cartesian(ylim = c(0, 700)) +
  bellabeat_theme()

### how different are the average steps taken throughout the week:

average_steps <- daily_activity %>%
  group_by(day) %>%
  summarize(avg = mean(total_steps)) 

st3 <- ggplot(average_steps) +
  aes(y=avg, x=day, fill=day) +
  geom_col(width=0.8) +
  geom_hline(yintercept=mean(average_steps$avg), alpha=0.5) +
  scale_y_continuous(breaks=seq(0, 10000, by=1000), labels = number_format(),
                     expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  xlab('day of the week') +
  ylab('steps on average') +
  labs(title='Average Number of Steps by Day of the Week') +
  coord_cartesian(ylim = c(0, 10000)) +
  bellabeat_theme() +
  scale_fill_hue()


st1 / (st2 | st3)

############################ INTENSITY TRACKING ##############################

### what hours people are mostly active in terms of intensity:

i1 <- hourly_intensity %>%
  group_by(hour) %>%
  summarize(average = mean(total_intensity)) %>%
  ggplot() +
  aes(y=average, x=hour) + 
  geom_line(color='#F8766D') +
  ylab('average intensity') +
  xlab('hour') +
  labs(title='Average Level of Intensity by Hour') +
  scale_x_continuous(breaks = seq(0,23, by=1), expand=c(0,0)) +
  scale_y_continuous(breaks = seq(0, 22, by = 2), expand=c(0,0)) +
  coord_cartesian(ylim = c(0, 23)) +
  bellabeat_theme()

### intensity throughout the week:

daily_intensity <- hourly_intensity %>%
  group_by(date, day, id) %>%
  summarize(daily_intensity = sum(total_intensity)) 

i2 <- daily_intensity %>%
  group_by(day) %>%
  summarize(avg = mean(daily_intensity)) %>%
  ggplot() +
  aes(y=avg, x=day, fill=day) +
  geom_col(width=0.8) +
  scale_y_continuous(breaks=seq(0, 350, by=50), labels = number_format(),
                     expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  xlab('day of the week') +
  ylab('average daily intensity') +
  labs(title='Average Intensity by Day of the Week') +
  coord_cartesian(ylim = c(0, 350)) +
  bellabeat_theme() +
  scale_fill_hue()


i1 | i2

### looks a lot like steps activity. Let's check for correlation between 
### intensity of steps and calories burnt:

merged <- merge(daily_intensity, daily_activity, on=id)

i3 <- merged %>%
  ggplot(aes(x=daily_intensity, y=calories)) + 
  geom_point(position = 'jitter', alpha=0.5) +
  geom_smooth(color='#F8766D') +
  xlab('daily intensity') +
  ylab('calories burnt') +
  labs(title='Intensity-Calories Relationship') +
  scale_y_continuous(breaks=seq(0,5000, by=500), labels = number_format(),
                     expand = c(0,0)) +
  scale_x_continuous(breaks=seq(0,1000, by=100), labels = number_format(),
                     expand=c(0,0)) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01) +
  bellabeat_theme()

### compare two correlations:

st1 | i3

############################### SLEEP TRACKING ##############################

### let's see how often users use devices for tracking sleep:

count_sleep <- daily_sleep %>%
  group_by(id) %>%
  summarize(count = n_distinct(date))

sl1 <- ggplot(count_sleep) +
  aes(y=count, x=id, fill=id) + 
  geom_col() +
  geom_hline(yintercept=mean(count_sleep$count), alpha=0.5) +
  scale_y_continuous(breaks = seq(0, 30, by = 2),expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  ylab('nights registered') +
  xlab('users') +
  labs(title='Sleep Tracking Device Usage by User') +
  bellabeat_theme() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        panel.grid.major=element_blank())

### daily number of hours spent asleep by user:

average_sleep <- daily_sleep %>%
  group_by(id) %>%
  summarize(average_minutes=mean(total_minutes_asleep)) %>%
  mutate(average_hours = average_minutes / 60)
  
sl2 <- ggplot(average_sleep) +
  aes(y=average_hours, x=id, fill=id) + 
  geom_col() +
  geom_hline(yintercept=mean(average_sleep$average_hours), alpha=0.5) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,11, by=1), expand=c(0,0)) +
  coord_cartesian(ylim = c(0, 11)) +
  xlab('users') +
  ylab('average number of hours of sleep') +
  labs(title='Hours of Sleep on Average by User') +
  bellabeat_theme() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        panel.grid.major=element_blank())

### how well users sleep throughout the week:

sl3 <- ggplot(daily_sleep, aes(y=fct_rev(day), x=total_minutes_asleep /60, fill=day)) +
  geom_density_ridges(alpha=0.7, 
                      quantile_lines=TRUE, quantile_fun=mean) +
  xlab('number of hours of sleep') +
  ylab('day of the week') +
  labs(title='Hours of Sleep by Day of the Week') +
  scale_x_continuous(breaks=seq(0,15, by=1), expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  bellabeat_theme() +
  scale_fill_hue()


sl1 / sl2 | sl3