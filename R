#' ---
#' title: "Reproducible Reasearch Project 2"
#' author: "Dennis Oriaifo"
#' date: "June 25th, 2017"
#' ---

# Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.
# In this analysis, data is preprocessed, aggregrated and graphed to show the top 10 events that affect health and the top 25 events that cause the most damange ($)


rm(list=ls())
cat('\014')
library(knitr)
library(plyr)
library(ggplot2)

dataset = read.csv('repdata%2Fdata%2FStormData.csv')

## DATA PREPROCESSING

# Rename column header
colnames(dataset)[colnames(dataset)=='EVTYPE'] = 'Event_Type'

# Harmonizing Event Type - per category/ sorted in alphabetical order 
  # To better answer the question "Does the analysis address the question of 
   # which types of events are most harmful to population health and greatest economic impact"

# Astronomical terms
dataset$Event_Type = gsub('ASTRONOMICAL HIGH TIDE', 'ASTRONOMICAL TIDE', dataset$Event_Type)
dataset$Event_Type = gsub('ASTRONOMICAL LOW TIDE', 'ASTRONOMICAL TIDE', dataset$Event_Type)

# Cold terms
dataset$Event_Type = gsub('Cold', 'COLD TEMP', dataset$Event_Type)
dataset$Event_Type = gsub('COLD/WIND CHILL', 'COLD TEMP', dataset$Event_Type)
dataset$Event_Type = gsub('EXTREME COLD TEMPERATURES', 'COLD TEMP', dataset$Event_Type)
dataset$Event_Type = gsub('EXTREME COLD', 'COLD TEMP', dataset$Event_Type)

# Drought terms
dataset$Event_Type = gsub('DROUGHT', 'DRY CONDITIONS', dataset$Event_Type)
dataset$Event_Type = gsub('DRY MICROBURST', 'DRY CONDITIONS', dataset$Event_Type)

# Freeze terms
dataset$Event_Type = gsub('FREEZE', 'FREEZING CONDITIONS', dataset$Event_Type)
dataset$Event_Type = gsub('FREEZING DRIZZLE', 'FREEZING CONDITIONS', dataset$Event_Type)
dataset$Event_Type = gsub('FREEZING FOG', 'FREEZING CONDITIONS', dataset$Event_Type)
dataset$Event_Type = gsub('FREEZING RAIN', 'FREEZING CONDITIONS', dataset$Event_Type)
dataset$Event_Type = gsub('FREEZING RAIN AND SLEET', 'FREEZING CONDITIONS', dataset$Event_Type)
dataset$Event_Type = gsub('FREEZING CONDITIONS AND SLEET', 'FREEZING CONDITIONS', dataset$Event_Type)
dataset$Event_Type = gsub('FROST/FREEZE', 'FREEZING CONDITIONS', dataset$Event_Type)
dataset$Event_Type = gsub('FROST/FREEZING CONDITIONS', 'FREEZING CONDITIONS', dataset$Event_Type)

# Hail terms
dataset$Event_Type = gsub('HAIL 75', 'HAIL', dataset$Event_Type)
dataset$Event_Type = gsub('NON SEVERE HAIL', 'HAIL', dataset$Event_Type)

# Heat terms
dataset$Event_Type = gsub('EXCESSIVE HEAT', 'HIGH HEAT',dataset$Event_Type)
dataset$Event_Type = gsub('EXTREME HEAT', 'HIGH HEAT', dataset$Event_Type)
dataset$Event_Type = gsub('HEAT', 'HIGH HEAT', dataset$Event_Type)
dataset$Event_Type = gsub('RECORD HEAT', 'HIGH HEAT', dataset$Event_Type)
dataset$Event_Type = gsub('RECORD WARMTH', 'HIGH HEAT', dataset$Event_Type)
dataset$Event_Type = gsub('Temperature record', 'HIGH HEAT',dataset$Event_Type)

# Flooding terms
dataset$Event_Type = gsub('Coastal Flood', 'COASTAL FLOOD', dataset$Event_Type)
dataset$Event_Type = gsub('COASTAL FLOODINGing', 'COASTAL FLOOD', dataset$Event_Type)
dataset$Event_Type = gsub('COASTAL FLOOD', 'COASTAL FLOOD', dataset$Event_Type)
dataset$Event_Type = gsub('COASTAL FLOODINGING', 'COASTAL FLOOD',dataset$Event_Type)
dataset$Event_Type = gsub('CSTL FLOODING/EROSION', 'COASTAL FLOOD', dataset$Event_Type)

dataset$Event_Type = gsub('URBAN FLOODING', 'URBAN FLOOD', dataset$Event_Type)
dataset$Event_Type = gsub('URBAN/SML STREAM FLD', 'URBAN FLOOD', dataset$Event_Type)

dataset$Event_Type[dataset$Event_Type == 'FLASH FLOOD'] = 'FLOOD/FLASH FLOOD'
dataset$Event_Type = gsub('FLASH FLOODING', 'FLOOD/FLASH FLOOD', dataset$Event_Type)
dataset$Event_Type[dataset$Event_Type == 'FLOOD'] = 'FLOOD/FLASH FLOOD'

# Snow terms
dataset$Event_Type = gsub('BLOWING SNOW- EXTREME WIND CHI', 'SNOWFALL', dataset$Event_Type)
dataset$Event_Type = gsub('HEAVY SNOW', 'SNOWFALL', dataset$Event_Type)
dataset$Event_Type = gsub('HEAVY SNOW/SQUALLS', 'SNOWFALL', dataset$Event_Type)
dataset$Event_Type = gsub('HEAVY WET SNOW', 'SNOWFALL', dataset$Event_Type)
dataset$Event_Type = gsub('HEAVY LAKE SNOW', 'SNOWFALL', dataset$Event_Type)
dataset$Event_Type = gsub('LAKE-EFFECT SNOW', 'SNOWFALL', dataset$Event_Type)
dataset$Event_Type = gsub('LIGHT SNOW', 'SNOWFALL', dataset$Event_Type)
dataset$Event_Type = gsub('Light Snow', 'SNOWFALL', dataset$Event_Type)
dataset$Event_Type = gsub('MODERATE SNOWFALL', 'SNOWFALL', dataset$Event_Type)
dataset$Event_Type = gsub('SNOW/SQUALLS', 'SNOWFALL', dataset$Event_Type)
dataset$Event_Type = gsub('SNOW AND ICE', 'SNOWFALL', dataset$Event_Type)
dataset$Event_Type = gsub('SNOW/BLOWING SNOW', 'SNOWFALL', dataset$Event_Type)

# Rain terms
dataset$Event_Type = gsub('EXCESSIVE RAINFALL', 'HEAVY RAIN', dataset$Event_Type)
dataset$Event_Type = gsub('Heavy rain', 'HEAVY RAIN', dataset$Event_Type)

# Surf terms
dataset$Event_Type = gsub('HEAVY SURF/HIGH SURF', 'HEAVY/HIGH SURF', dataset$Event_Type)
dataset$Event_Type = gsub('HIGH SURF', 'HEAVY/HIGH SURF', dataset$Event_Type)

# Thunderstorm terms
dataset$Event_Type = gsub('THUNDERSTORMSS', 'THUNDERSTORMS', dataset$Event_Type)
dataset$Event_Type = gsub('THUNDERSTORM/LIGHTNING', 'THUNDERSTORMS',dataset$Event_Type)
dataset$Event_Type = gsub('THUNDERSTORMS WIND', 'THUNDERSTORMS',dataset$Event_Type)
dataset$Event_Type = gsub('THUNDERSTORM WIND', 'THUNDERSTORMS',dataset$Event_Type)
dataset$Event_Type = gsub('THUNDERSTORM WINDS', 'THUNDERSTORMS',dataset$Event_Type)
dataset$Event_Type = gsub('THUNDERSTORM WIND HAIL', 'THUNDERSTORMS',dataset$Event_Type)
dataset$Event_Type = gsub('THUNDERSTORM WIND/LIGHTNING' , 'THUNDERSTORMS',dataset$Event_Type)
dataset$Event_Type = gsub('THUNDERSTORMLIGHTNING', 'THUNDERSTORMS', dataset$Event_Type)

# Unseasonable weather terms
dataset$Event_Type = gsub('UNSEASONABLY WET', 'UNSEASONABLE TEMP', dataset$Event_Type)
dataset$Event_Type = gsub('UNSEASONABLY WARM', 'UNSEASONABLE TEMP', dataset$Event_Type)

# Wild/forest fires terms
dataset$Event_Type = gsub('WILD FIRES', 'WILDFIRE', dataset$Event_Type)
dataset$Event_Type = gsub('WILDFIRES', 'WILDFIRE', dataset$Event_Type)

# Wind condition terms
dataset$Event_Type = gsub('EXTREME WINDCHILL', 'WIND', dataset$Event_Type)
dataset$Event_Type = gsub('DRY MICROBURST WIND', 'WIND', dataset$Event_Type)
dataset$Event_Type = gsub('HIGH WIND', 'WIND', dataset$Event_Type) 
dataset$Event_Type = gsub('HIGH WINDSS', 'WIND',dataset$Event_Type) 
dataset$Event_Type = gsub('STRONG WIND', 'WIND', dataset$Event_Type)
dataset$Event_Type = gsub('TSTM WIND', 'WIND', dataset$Event_Type)
dataset$Event_Type = gsub('WIND DAMAGE', 'WIND',dataset$Event_Type)
dataset$Event_Type = gsub('WIND/HAIL', 'WIND', dataset$Event_Type)
dataset$Event_Type = gsub('WINDS', 'WIND', dataset$Event_Type) 
dataset$Event_Type = gsub('WIND 73', 'WIND', dataset$Event_Type)

# Winter weather terms
dataset$Event_Type = gsub('WINTER STORM', 'WINTER WEATHER', dataset$Event_Type)
dataset$Event_Type = gsub('WINTER WEATHER/MIX', 'WINTER WEATHER', dataset$Event_Type)
dataset$Event_Type = gsub('WINTRY MIX', 'WINTER WEATHER', dataset$Event_Type)

# OTHER terms
dataset$Event_Type = gsub('DENSE FOG', 'FOG', dataset$Event_Type)
dataset$Event_Type = gsub(' LIGHTNING', 'LIGHTNING',dataset$Event_Type)
dataset$Event_Type = gsub('Mudslide', 'MUD SLIDES', dataset$Event_Type)
dataset$Event_Type = gsub('RIP CURRENT', 'RIP CURRENTS',dataset$Event_Type)
dataset$Event_Type = gsub('STORM SURGE/TIDE', 'STORM SURGE',dataset$Event_Type)
dataset$Event_Type = gsub('THUNDERSTORMLIGHTNING', 'THUNDERSTORM',dataset$Event_Type)
dataset$Event_Type = gsub('TORNADO F0', 'TORNADO', dataset$Event_Type)
dataset$Event_Type = gsub('TROPICAL DEPRESSION', 'TROPICAL STORM', dataset$Event_Type)
dataset$Event_Type = gsub('WATER SPOUT', 'WATERSPOUT', dataset$Event_Type)

###==================================

# Subset data for Health and Economic effects by removing unnecessary columns
datasetHealth = dataset[,c(8,23:24)]
datasetEcon = dataset[,c(8,25,27)]

# Aggregate subsets to get total values by event type
datasetHealthAggr = ddply(datasetHealth, .(Event_Type), colwise(sum))
datasetHealthAggr = arrange(datasetHealthAggr,-FATALITIES,-INJURIES)

# Aggregate subsets by event type
datasetEconAggr = ddply(datasetEcon,.(Event_Type),colwise(sum))
datasetEconAggr = arrange(datasetEconAggr, -PROPDMG, -CROPDMG)

# Get the top 10 health and top 25 economic impacts
datasetHealthAggrTop10 = head(datasetHealthAggr,10)
datasetEconAggrTop25 = head(datasetEconAggr,25)

# Reshape data to distinguish between fatality and injury
library(reshape2)
datasetHealthAggrTop10Reshaped = melt(datasetHealthAggrTop10,id.vars = 'Event_Type')
datasetHealthAggrTop10Reshaped$Event_Type = factor(datasetHealthAggrTop10Reshaped$Event_Type, 
                                        levels = datasetHealthAggrTop10Reshaped$Event_Type[order(c(1:10))])

datasetEconAggrTop25Reshaped = melt(datasetEconAggrTop25,id.vars = 'Event_Type')
datasetEconAggrTop25Reshaped$Event_Type = factor(datasetEconAggrTop25Reshaped$Event_Type, 
                                      levels = datasetEconAggrTop25Reshaped$Event_Type[order(c(1:25))])
# Map data
HealthPlot = ggplot(datasetHealthAggrTop10Reshaped, 
              aes(x = Event_Type, y = value,fill=variable)) + geom_bar(stat='identity')
HealthPlot + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title='Weather Events w/ Highest Fatalities/Injuries',x='Event Type', y = '# of Occurences')

EconPlot = ggplot(datasetEconAggrTop25Reshaped, 
              aes(x = Event_Type, y = value,fill=variable)) + geom_bar(stat='identity')
EconPlot + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title='Weather Events w/ Highest Economic Impact', x='Event Type', y = 'Damage ($)')

