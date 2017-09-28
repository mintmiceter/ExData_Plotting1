# load tidyverse & lubridate packages
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(lubridate)) install.packages('lubridate')
if (!require(gridExtra)) install.packages('gridExtra')


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


# Plot 1
plot1 <- ggplot(power, aes(ymd_hms(paste(Date, Time)), 
                           as.numeric(Global_active_power))) +
  geom_line() + 
  # ticks as single day of the week
  scale_x_datetime(date_breaks = '1 day', date_labels = '%a') +
  # remove x axis label and rename y axis label
  labs(x = NULL, y = 'Global Active Power') +
  # classic theme (white background, no axis lines)
  theme_classic() +
  # add border
  theme(panel.border = element_rect(colour = 'black', fill = NA),
        text = element_text(size = rel(3)))

# Plot 2
plot2 <- ggplot(power, aes(ymd_hms(paste(Date, Time)), as.numeric(Voltage))) +
  geom_line() + 
  # x ticks as single day of the week
  scale_x_datetime(date_breaks = '1 day', date_labels = '%a') +
  # y ticks in thousands
  scale_y_continuous(label = function(x)x/1000, 
                     breaks = seq(234000, 246000, by = 4000)) + 
  # rename x axis label and y axis label
  labs(x = 'datetime', y = 'Voltage') +
  # classic theme (white background, no axis lines)
  theme_classic() +
  # add border
  theme(panel.border = element_rect(colour = 'black', fill = NA),
        text = element_text(size = rel(3)))

# Plot 4
plot4 <- ggplot(power, aes(ymd_hms(paste(Date, Time)), 
                           as.numeric(Global_reactive_power))) +
  geom_line() + 
  # x ticks as single day of the week
  scale_x_datetime(date_breaks = '1 day', date_labels = '%a') +
  # rename x axis as datetime
  labs(x = 'datetime', y = 'Global_reactive_power') +
  # classic theme (white background, no axis lines)
  theme_classic() +
  # add border
  theme(panel.border = element_rect(colour = 'black', fill = NA),
        text = element_text(size = rel(3)))


power_long <- power %>% gather(key = Sub_metering, value = Value,
                               Sub_metering_1, Sub_metering_2, Sub_metering_3)

# Plot 3
plot3 <- ggplot(power_long, aes(x = ymd_hms(paste(Date, Time)), 
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
  theme(legend.position = c(.65, .8),
        # remove legend title
        legend.title = element_blank(),
        # resize legend font size
        legend.text = element_text(size = 5), 
        # decrease legend spacing
        legend.key.height = unit(0.5, 'line'), 
        # border around plot
        panel.border = element_rect(colour = 'black', fill = NA),
        # resize text in plot
        text = element_text(size = rel(3)))
  
plot5 <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# save plot1-4 to a 4"x4" (480 pixel x 480 pixel) png file
ggsave(plot5, file = 'plot4.png', width = 4, height = 4, dpi = 120)
