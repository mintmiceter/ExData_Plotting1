# load tidyverse packages
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
  filter(between(Date, ymd('2007-02-01'), ymd('2007-02-02')))

# Plot the histogram
ggplot(power, aes(as.numeric(Global_active_power))) +
  # histogram bins by 0.5, with black border color and #ff2600 body color
  geom_histogram(breaks = seq(0, 
                              max(as.numeric(power$Global_active_power)), 
                              by = 0.5), 
                 col = 'black',
                 fill = '#ff2600') + 
  # change the y axis tickmarks by 200
  scale_y_continuous(breaks = seq(0, 
                                  sum(as.numeric(power$Global_active_power) <= 0.5),
                                  by = 200)) +
  # chart and axis labels
  labs(title = 'Global Active Power', 
       x = 'Global Active Power (kilowatts)', 
       y = 'Frequency') +
  # classic theme (white background, no axis lines)
  theme_classic() +
  # center the axis title
  theme(plot.title = element_text(hjust = 0.5)) 

# save plot1 to a 4"x4" (480 pixel x 480 pixel) png file
ggsave('./plot1.png', width = 4, height = 4, dpi = 120)
