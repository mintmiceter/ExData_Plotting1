# load tidyverse & lubridate packages
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(lubridate)) install.packages('lubridate')

# if power file doesn't exist then download and unzip else print 'file exists'
if(!'household_power_consumption.txt' %in% list.files('~/Coursera/Exploratory Data Analysis/Project 1')){
  download.file('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip',
                path.expand('~/Coursera/Exploratory Data Analysis/Project 1/household_power.zip'))
  unzip('~/Coursera/Exploratory Data Analysis/Project 1/household_power.zip',
        exdir = '~/Coursera/Exploratory Data Analysis/Project 1')
} else {
  print('file exists')
}

# set new working directory
setwd('~/Coursera/Exploratory Data Analysis/Project 1')

# read household_power_consumption.txt file
# read_csv2 for semicolon separated file
power <- read_csv2('household_power_consumption.txt') %>% 
  #convert Date column to date format
  mutate(Date = dmy(Date)) %>% 
  # filter 2007-02-01 and 2007-02-02
  filter(between(Date, ymd('2007-02-01'), ymd('2007-02-02'))) %>% 
  gather(key = Sub_metering, value = Value,
         Sub_metering_1, Sub_metering_2, Sub_metering_3)


# Plot 3
# line plot
ggplot(power, aes(x = ymd_hms(paste(Date, Time)), 
                  y = as.numeric(Value), 
                  group = Sub_metering, 
                  colour = Sub_metering)) +
  geom_line() + 
  # ticks as single day of the week
  scale_x_datetime(date_breaks = '1 day', date_labels = '%a') +
  # change Sub_metering line colors
  scale_color_manual(values = c('black', 'red', 'blue')) + 
  # remove x axis label and rename y axis label
  labs(x = NULL, y = 'Energy sub metering') +
  # classic theme (white background, no axis lines)
  theme_classic() +
  # legend top right position and black border
  theme(legend.background = element_rect(colour = 'black', size = 0.5),
        # remove legent title
        legend.title = element_blank(),
        # legend.position = c(.888, .837),
        legend.position = c(.806, .871),
        panel.border = element_rect(colour = 'black', fill = NA))
# the legend position is aligned on the .png
  
# save plot3 to a 4"x4" (480 pixel x 480 pixel) png file
ggsave('./plot3.png', width = 4, height = 4, dpi = 120)
