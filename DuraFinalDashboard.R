###AUTHOR: BECCA DURA
###DATE: 5/15/2020
###STAT190 PROJECT 2
###COVID-19 EXPLORATION & DASHBOARD 


###LOAD PACKAGES###
library(ggplot2)
library(lubridate)
library(cowplot)
library(maps)
library(plotly)
library(mapproj)
library(viridis)
library(RColorBrewer) #for custom color palettes
library(dplyr) #for manipulating, aggregating, piping
library(tidytext) #for (tidy) text mining
library(topicmodels) #for LDA (topic modeling)
library(reshape2) #for reshaping data (long to wide or wide to long)
library(SnowballC)
library(stringr)
library(textstem)
library(wordcloud)
library(shiny)
library(shinydashboard)
library(shinythemes)


#######COUNTRY DATA VISUALIZATION#######
country_data <- read.csv("who-situation-reports-covid-19.csv")

country_data$reported_date2 <- as.Date(country_data$reported_date, 
                                       format = "%Y-%m-%d")
country_data$confirmed_cases2 <- as.numeric(as.character(country_data$confirmed_cases))
country_data$new_confirmed_cases2 <- as.numeric(as.character(country_data$new_confirmed_cases))
country_data$total_deaths2 <- as.numeric(as.character(country_data$total_deaths))
country_data$new_total_deaths2 <- as.numeric(as.character(country_data$new_total_deaths))
#country_data$month <- (month(country_data$reported_date2, label=TRUE))
country_data$total_death_rate <- country_data$total_deaths2 / 
  (country_data$total_deaths2 + country_data$confirmed_cases2)
country_data$daily_death_rate <- country_data$new_total_deaths2 / 
  (country_data$new_total_deaths2 + country_data$new_confirmed_cases2)

#####DATA CLEANING FOR MAIN COUNTRIES######
#####UNITED STATES#####
us_data <- country_data[which(country_data$reporting_country_territory == "United States of America"),]

us_data <- us_data[which(us_data$province_city == ""),]

#Fix data reporting issue at beginning of April
us_data$confirmed_cases2[which(us_data$reported_date == "2020-04-02")] <- us_data$confirmed_cases2[which(us_data$reported_date == "2020-04-03")] - us_data$new_confirmed_cases2[which(us_data$reported_date == "2020-04-03")]
us_data$new_confirmed_cases2[which(us_data$reported_date == "2020-04-02")] <- us_data$confirmed_cases2[which(us_data$reported_date == "2020-04-02")] - us_data$confirmed_cases2[which(us_data$reported_date == "2020-04-01")]
us_data$total_deaths2[which(us_data$reported_date == "2020-04-02")] <- us_data$total_deaths2[which(us_data$reported_date == "2020-04-03")] - us_data$new_total_deaths2[which(us_data$reported_date == "2020-04-03")]
us_data$new_total_deaths2[which(us_data$reported_date == "2020-04-02")] <- us_data$total_deaths2[which(us_data$reported_date == "2020-04-02")] - us_data$total_deaths2[which(us_data$reported_date == "2020-04-01")]

#Create death rate columns
us_data$total_death_rate <- us_data$total_deaths2 / 
  (us_data$confirmed_cases2)
us_data$daily_death_rate <- us_data$new_total_deaths2 / 
  (us_data$confirmed_cases2)

us_data$total_death_rate[is.nan(us_data$total_death_rate)] <- 0
us_data$daily_death_rate[is.nan(us_data$daily_death_rate)] <- 0


#####CHINA#####
china_data <- country_data[which(country_data$reporting_country_territory == "China"),]
china_data <- china_data[which(china_data$province_city == ""),]

#Fix data reporting issue at beginning of April
china_data$new_total_deaths2[which(china_data$reported_date == "2020-04-03")] <- china_data$total_deaths2[which(china_data$reported_date == "2020-04-03")]
china_data$total_deaths2[which(china_data$reported_date == "2020-04-03")] <- china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-04-03")]
china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-04-03")] <- substr(china_data$confirmed_cases2[which(china_data$reported_date == "2020-04-03")], 6, 7)
china_data$confirmed_cases2[which(china_data$reported_date == "2020-04-03")] <- substr(china_data$confirmed_cases2[which(china_data$reported_date == "2020-04-03")], 1, 5)

china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-04-30")] <- china_data$total_deaths2[which(china_data$reported_date == "2020-04-30")]
china_data$total_deaths2[which(china_data$reported_date == "2020-04-30")] <- china_data$new_total_deaths2[which(china_data$reported_date == "2020-04-30")]

china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-04-29")] <- substr(china_data$total_deaths2[which(china_data$reported_date == "2020-04-29")], 1, 1)
china_data$total_deaths2[which(china_data$reported_date == "2020-04-29")] <- substr(china_data$total_deaths2[which(china_data$reported_date == "2020-04-29")], 2, 5)

china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-04-28")] <- china_data$total_deaths2[which(china_data$reported_date == "2020-04-28")]
china_data$total_deaths2[which(china_data$reported_date == "2020-04-28")] <- china_data$new_total_deaths2[which(china_data$reported_date == "2020-04-28")]

china_data$new_total_deaths2[which(china_data$reported_date == "2020-04-28")] <- 0
china_data$new_total_deaths2[which(china_data$reported_date == "2020-04-30")] <- 0

china_data$new_confirmed_cases2 <- as.numeric(as.character(china_data$new_confirmed_cases2))
china_data$confirmed_cases2 <- as.numeric(as.character(china_data$confirmed_cases2))
china_data$total_deaths2 <- as.numeric(as.character(china_data$total_deaths2))

china_data$confirmed_cases2[which(china_data$reported_date == "2020-04-02")] <- china_data$confirmed_cases2[which(china_data$reported_date == "2020-04-03")] - china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-04-03")]
china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-04-02")] <- china_data$confirmed_cases2[which(china_data$reported_date == "2020-04-02")] - china_data$confirmed_cases2[which(china_data$reported_date == "2020-04-01")]
china_data$total_deaths2[which(china_data$reported_date == "2020-04-02")] <- china_data$total_deaths2[which(china_data$reported_date == "2020-04-03")] - china_data$new_total_deaths2[which(china_data$reported_date == "2020-04-03")]
china_data$new_total_deaths2[which(china_data$reported_date == "2020-04-02")] <- china_data$total_deaths2[which(china_data$reported_date == "2020-04-02")] - china_data$total_deaths2[which(china_data$reported_date == "2020-04-01")]

china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-13")] <- china_data$confirmed_cases2[which(china_data$reported_date == "2020-03-16")] - china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-03-16")]
china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-12")] <- china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-13")] - china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-02-13")]
china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-11")] <- china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-12")] - china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-02-12")]
china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-10")] <- china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-11")] - china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-02-11")]
china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-09")] <- china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-10")] - china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-02-10")]
china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-08")] <- china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-09")] - china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-02-09")]
china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-07")] <- china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-08")] - china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-02-08")]
china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-06")] <- china_data$confirmed_cases2[which(china_data$reported_date == "2020-02-07")] - china_data$new_confirmed_cases2[which(china_data$reported_date == "2020-02-07")]

china_data$new_confirmed_cases2[is.na(china_data$new_confirmed_cases2)] <- 0
china_data$confirmed_cases2[is.na(china_data$confirmed_cases2)] <- 0
china_data$new_total_deaths2[is.na(china_data$new_total_deaths2)] <- 0
china_data$total_deaths2[is.na(china_data$total_deaths2)] <- 0

#Create death rate columns
china_data$total_death_rate <- china_data$total_deaths2 / 
  (china_data$confirmed_cases2)
china_data$daily_death_rate <- china_data$new_total_deaths2 / 
  (china_data$confirmed_cases2)

china_data$total_death_rate[is.na(china_data$total_death_rate)] <- 0
china_data$daily_death_rate[is.na(china_data$daily_death_rate)] <- 0


#####SPAIN#####
spain_data <- country_data[which(country_data$reporting_country_territory == "Spain"),]
spain_data <- spain_data[which(spain_data$province_city == ""),]

#Fix data reporting issue at beginning of April
spain_data$new_total_deaths2[which(spain_data$reported_date == "2020-04-03")] <- spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-03")]
spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-03")] <- spain_data$new_confirmed_cases2[which(spain_data$reported_date == "2020-04-03")]
spain_data$new_confirmed_cases2[which(spain_data$reported_date == "2020-04-03")] <- substr(spain_data$confirmed_cases2[which(spain_data$reported_date == "2020-04-03")], 7, 10)
spain_data$confirmed_cases2[which(spain_data$reported_date == "2020-04-03")] <- substr(spain_data$confirmed_cases2[which(spain_data$reported_date == "2020-04-03")], 1, 6)

spain_data$new_confirmed_cases2[which(spain_data$reported_date == "2020-04-30")] <- spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-30")]
spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-30")] <- spain_data$new_total_deaths2[which(spain_data$reported_date == "2020-04-30")]

spain_data$new_confirmed_cases2[which(spain_data$reported_date == "2020-04-29")] <- substr(spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-29")], 1, 4)
spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-29")] <- substr(spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-29")], 5, 9)

spain_data$new_confirmed_cases2[which(spain_data$reported_date == "2020-04-28")] <- spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-28")]
spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-28")] <- spain_data$new_total_deaths2[which(spain_data$reported_date == "2020-04-28")]


spain_data$new_confirmed_cases2 <- as.numeric(as.character(spain_data$new_confirmed_cases2))
spain_data$confirmed_cases2 <- as.numeric(as.character(spain_data$confirmed_cases2))
spain_data$total_deaths2 <- as.numeric(as.character(spain_data$total_deaths2))

spain_data$confirmed_cases2[which(spain_data$reported_date == "2020-04-02")] <- spain_data$confirmed_cases2[which(spain_data$reported_date == "2020-04-03")] - spain_data$new_confirmed_cases2[which(spain_data$reported_date == "2020-04-03")]
spain_data$new_confirmed_cases2[which(spain_data$reported_date == "2020-04-02")] <- spain_data$confirmed_cases2[which(spain_data$reported_date == "2020-04-02")] - spain_data$confirmed_cases2[which(spain_data$reported_date == "2020-04-01")]
spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-02")] <- spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-03")] - spain_data$new_total_deaths2[which(spain_data$reported_date == "2020-04-03")]
spain_data$new_total_deaths2[which(spain_data$reported_date == "2020-04-02")] <- spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-02")] - spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-01")]

#Create death rate columns
spain_data$total_death_rate <- spain_data$total_deaths2 / 
  (spain_data$confirmed_cases2)
spain_data$daily_death_rate <- spain_data$new_total_deaths2 / 
  (spain_data$confirmed_cases2)

spain_data$new_total_deaths2[which(spain_data$reported_date == "2020-04-30")] <- spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-30")] - spain_data$total_deaths2[which(spain_data$reported_date2 == "2020-04-29")]
spain_data$new_total_deaths2[which(spain_data$reported_date == "2020-04-28")] <- spain_data$total_deaths2[which(spain_data$reported_date == "2020-04-28")] - spain_data$total_deaths2[which(spain_data$reported_date2 == "2020-04-25")]


spain_data$new_confirmed_cases2[is.na(spain_data$new_confirmed_cases2)] <- 0
spain_data$confirmed_cases2[is.na(spain_data$confirmed_cases2)] <- 0
spain_data$new_total_deaths2[is.na(spain_data$new_total_deaths2)] <- 0
spain_data$total_deaths2[is.na(spain_data$total_deaths2)] <- 0

spain_data$total_death_rate <- spain_data$total_deaths2 / 
  (spain_data$confirmed_cases2)
spain_data$daily_death_rate <- spain_data$new_total_deaths2 / 
  (spain_data$confirmed_cases2)

spain_data$total_death_rate[is.na(spain_data$total_death_rate)] <- 0
spain_data$daily_death_rate[is.na(spain_data$daily_death_rate)] <- 0


#####ITALY#####
italy_data <- country_data[which(country_data$reporting_country_territory == "Italy"),]
italy_data <- italy_data[which(italy_data$province_city == ""),]

#Fix data reporting issues in April
italy_data$new_total_deaths2[which(italy_data$reported_date == "2020-04-03")] <- italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-03")]
italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-03")] <- italy_data$new_confirmed_cases2[which(italy_data$reported_date == "2020-04-03")]
italy_data$new_confirmed_cases2[which(italy_data$reported_date == "2020-04-03")] <- substr(italy_data$confirmed_cases2[which(italy_data$reported_date == "2020-04-03")], 7, 10)
italy_data$confirmed_cases2[which(italy_data$reported_date == "2020-04-03")] <- substr(italy_data$confirmed_cases2[which(italy_data$reported_date == "2020-04-03")], 1, 6)

italy_data$new_confirmed_cases2[which(italy_data$reported_date == "2020-04-30")] <- italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-30")]
italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-30")] <- italy_data$new_total_deaths2[which(italy_data$reported_date == "2020-04-30")]

italy_data$new_confirmed_cases2[which(italy_data$reported_date == "2020-04-29")] <- substr(italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-29")], 1, 4)
italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-29")] <- substr(italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-29")], 5, 9)

italy_data$new_confirmed_cases2[which(italy_data$reported_date == "2020-04-28")] <- italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-28")]
italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-28")] <- italy_data$new_total_deaths2[which(italy_data$reported_date == "2020-04-28")]


italy_data$new_confirmed_cases2 <- as.numeric(as.character(italy_data$new_confirmed_cases2))
italy_data$confirmed_cases2 <- as.numeric(as.character(italy_data$confirmed_cases2))
italy_data$total_deaths2 <- as.numeric(as.character(italy_data$total_deaths2))

italy_data$confirmed_cases2[which(italy_data$reported_date == "2020-04-02")] <- italy_data$confirmed_cases2[which(italy_data$reported_date == "2020-04-03")] - italy_data$new_confirmed_cases2[which(italy_data$reported_date == "2020-04-03")]
italy_data$new_confirmed_cases2[which(italy_data$reported_date == "2020-04-02")] <- italy_data$confirmed_cases2[which(italy_data$reported_date == "2020-04-02")] - italy_data$confirmed_cases2[which(italy_data$reported_date == "2020-04-01")]
italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-02")] <- italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-03")] - italy_data$new_total_deaths2[which(italy_data$reported_date == "2020-04-03")]
italy_data$new_total_deaths2[which(italy_data$reported_date == "2020-04-02")] <- italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-02")] - italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-01")]

italy_data$new_total_deaths2[which(italy_data$reported_date == "2020-04-30")] <- italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-30")] - italy_data$total_deaths2[which(italy_data$reported_date2 == "2020-04-29")]
italy_data$new_total_deaths2[which(italy_data$reported_date == "2020-04-28")] <- italy_data$total_deaths2[which(italy_data$reported_date == "2020-04-28")] - italy_data$total_deaths2[which(italy_data$reported_date2 == "2020-04-25")]

#Create death rate columns
italy_data$new_confirmed_cases2[is.na(italy_data$new_confirmed_cases2)] <- 0
italy_data$confirmed_cases2[is.na(italy_data$confirmed_cases2)] <- 0
italy_data$new_total_deaths2[is.na(italy_data$new_total_deaths2)] <- 0
italy_data$total_deaths2[is.na(italy_data$total_deaths2)] <- 0

italy_data$total_death_rate <- italy_data$total_deaths2 / 
  (italy_data$confirmed_cases2)
italy_data$daily_death_rate <- italy_data$new_total_deaths2 / 
  (italy_data$confirmed_cases2)

italy_data$total_death_rate[is.na(italy_data$total_death_rate)] <- 0
italy_data$daily_death_rate[is.na(italy_data$daily_death_rate)] <- 0


#####SWEDEN#####
sweden_data <- country_data[which(country_data$reporting_country_territory == "Sweden"),]
sweden_data <- sweden_data[which(sweden_data$province_city == ""),]

#Fix data reporting issue at beginning of April
sweden_data$new_total_deaths2[which(sweden_data$reported_date == "2020-04-03")] <- sweden_data$total_deaths2[which(sweden_data$reported_date == "2020-04-03")]
sweden_data$total_deaths2[which(sweden_data$reported_date == "2020-04-03")] <- sweden_data$new_confirmed_cases2[which(sweden_data$reported_date == "2020-04-03")]
sweden_data$new_confirmed_cases2[which(sweden_data$reported_date == "2020-04-03")] <- substr(sweden_data$confirmed_cases2[which(sweden_data$reported_date == "2020-04-03")], 5, 7)
sweden_data$confirmed_cases2[which(sweden_data$reported_date == "2020-04-03")] <- substr(sweden_data$confirmed_cases2[which(sweden_data$reported_date == "2020-04-03")], 1, 4)

sweden_data$new_confirmed_cases2 <- as.numeric(as.character(sweden_data$new_confirmed_cases2))
sweden_data$confirmed_cases2 <- as.numeric(as.character(sweden_data$confirmed_cases2))

sweden_data$confirmed_cases2[which(sweden_data$reported_date == "2020-04-02")] <- sweden_data$confirmed_cases2[which(sweden_data$reported_date == "2020-04-03")] - sweden_data$new_confirmed_cases2[which(sweden_data$reported_date == "2020-04-03")]
sweden_data$new_confirmed_cases2[which(sweden_data$reported_date == "2020-04-02")] <- sweden_data$confirmed_cases2[which(sweden_data$reported_date == "2020-04-02")] - sweden_data$confirmed_cases2[which(sweden_data$reported_date == "2020-04-01")]
sweden_data$total_deaths2[which(sweden_data$reported_date == "2020-04-02")] <- sweden_data$total_deaths2[which(sweden_data$reported_date == "2020-04-03")] - sweden_data$new_total_deaths2[which(sweden_data$reported_date == "2020-04-03")]
sweden_data$new_total_deaths2[which(sweden_data$reported_date == "2020-04-02")] <- sweden_data$total_deaths2[which(sweden_data$reported_date == "2020-04-02")] - sweden_data$total_deaths2[which(sweden_data$reported_date == "2020-04-01")]

#Create death rate columns
sweden_data$total_death_rate <- sweden_data$total_deaths2 / 
  (sweden_data$confirmed_cases2)
sweden_data$daily_death_rate <- sweden_data$new_total_deaths2 / 
  (sweden_data$confirmed_cases2)

sweden_data$total_death_rate[is.nan(sweden_data$total_death_rate)] <- 0
sweden_data$daily_death_rate[is.nan(sweden_data$daily_death_rate)] <- 0



#######POPULATION DENSITY VISUALIZATION#######
population_density_data <- read.csv("Population_Density_by_Country.csv", skip=4)

#Dataframe with data on countries that has been cleaned
main_countries_data <- population_density_data[which(population_density_data$Country.Name == "United States"|population_density_data$Country.Name == "China"|population_density_data$Country.Name == "Spain"|population_density_data$Country.Name == "Italy"|population_density_data$Country.Name == "Sweden"),]

#Dataframe with countries with population density > 500
population_density_data2 <- population_density_data[which(population_density_data$X2018 > 500),]



#######WEATHER EXPLORATION & VISUALIZATION#######
weather_data <- read.csv("temperature_dataframe.csv")

weather_data$date2 <- as.Date(weather_data$date, 
                              format = "%m/%d/%Y")

# Get the world polygon
world <- map_data("world") 

# Reorder data + Add a new column with tooltip text
weather_data_lim <- weather_data %>%
  arrange(tempC) %>%
  mutate( country=factor(country, unique(country))) %>%
  mutate( mytext=paste(
    "Country: ", country, "\n", 
    "Temperature: ", tempC, sep="")
  )

# Make the map (static)
# p <- weather_data_lim %>%
#   ggplot() +
#   geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
#   geom_point(aes(x=long, y=lat, size=tempC, color=tempC, text=mytext, alpha=0.3) ) +
#   scale_size_continuous(range=c(0.5, 4)) +
#   scale_color_viridis(option="inferno", trans="log" ) +
#   scale_alpha_continuous(trans="log") +
#   theme_void() +
#   coord_map() +
#   theme(plot.title = element_text(face = "bold")) +
#   labs(title = "Temperature (Celsius) on 3-9-2020")
# 
# p <- ggplotly(p, tooltip="text") 
# p



#############FACTORS-WEATHER, TRENDS & POP DENSITY##########333
#Trends
country_factors_data <- country_data[which(country_data$reported_date2 >= "2020-01-22" & country_data$reported_date2 <= "2020-03-09"),]
country_factors_data$new_confirmed_cases2[is.na(country_factors_data$new_confirmed_cases2)] <- 0
country_factors_data$confirmed_cases2[is.na(country_factors_data$confirmed_cases2)] <- 0
country_factors_data$new_total_deaths2[is.na(country_factors_data$new_total_deaths2)] <- 0
country_factors_data$total_deaths2[is.na(country_factors_data$total_deaths2)] <- 0
country_factors_data$total_death_rate[is.na(country_factors_data$total_death_rate)] <- 0
country_factors_data$daily_death_rate[is.na(country_factors_data$daily_death_rate)] <- 0

country_factors_data2 <- mutate_if(country_factors_data, 
                                   is.factor, 
                                   str_replace_all, pattern = "United States of America", replacement = "USA")

weather_factors_data2 <- weather_data[which(weather_data$date == "3/9/2020"),]
weather_factors_data2<- weather_factors_data2[c('country','lat','long')]

names(country_factors_data2)[names(country_factors_data2)=="reporting_country_territory"] <- "country"
factors_data_merged <- merge(country_factors_data2,weather_factors_data2,by="country")

##Population Density Map
population_density_data <- mutate_if(population_density_data, 
                                     is.factor, 
                                     str_replace_all, pattern = "United States", replacement = "USA")

names(population_density_data)[names(population_density_data)=="Country.Name"] <- "country"
factors_pop_data_merged <- merge(population_density_data,weather_factors_data2,by="country")




############LDA- TOPIC MODELING##############
data = read.csv("metadata.csv")

data2 <- data[which(data$abstract != ""),]
data3 <- data2[c('cord_uid', 'title', 'abstract', 'publish_time', 'journal','url')]
#str(data3)
data3$publish_time <- as.Date(data3$publish_time, 
                              format = "%Y-%m-%d")
data3$title <- as.character(data3$title)
data3$abstract <- as.character(data3$abstract)
data3$journal <- as.character(data3$journal)
data3$cord_uid <- as.character(data3$cord_uid)
#str(data3)

#Subset dataset to a reasonable side for LDA
data4 <- data3[0:5000,]

data5 <- mutate_if(data4, 
                   is.character, 
                   tolower)
#Replace common phrases with their acronyms, so the words were not split 
#apart into their individual meanings
data6 <- mutate_if(data5, 
                   is.character, 
                   str_replace_all, pattern = "severe acute respiratory syndrome", replacement = "sars")
data6 <- mutate_if(data6, 
                   is.character, 
                   str_replace_all, pattern = "human leukocyte antigen", replacement = "hla") 
data6 <- mutate_if(data6, 
                   is.character, 
                   str_replace_all, pattern = "geographic information systems", replacement = "gis") 
data6 <- mutate_if(data6, 
                   is.character, 
                   str_replace_all, pattern = "locked nucleic acid", replacement = "lna") 
data6 <- mutate_if(data6, 
                   is.character, 
                   str_replace_all, pattern = "herpes simplex virus", replacement = "hsv")
data6 <- mutate_if(data6, 
                   is.character, 
                   str_replace_all, pattern = "background", replacement = "")
data6 <- mutate_if(data6, 
                   is.character, 
                   str_replace_all, pattern = "aim", replacement = "")
data6 <- mutate_if(data6, 
                   is.character, 
                   str_replace_all, pattern = "objective", replacement = "")

#Get root of words with various endings
data6$abstract <- data6$abstract %>%
  lemmatize_strings()

#tokens <- data3 %>% unnest_tokens(word, abstract)
tokens <- data6 %>% unnest_tokens(word, abstract)
#head(tokens)

data(stop_words)
tokens_clean <- tokens %>% anti_join(stop_words)
#head(tokens_clean)

tokens_clean2 <- tokens_clean[c('word')]

tokens_count <- tokens_clean2 %>%
  #sorts from most frequent to least
  count(word, sort = TRUE) %>%
  #reorders the factor levels for the plot
  mutate(word = reorder(word,n))

#ggplot(data = tokens_count[1:20,]) +
 # geom_col(aes(x=word, y=n)) +
  #labs(x = "Word", y = "Count")+
  #coord_flip()

tokens_clean4 <- tokens_clean[c('cord_uid', 'word')]


tokens_count4 <- tokens_clean4 %>%
  #include id so it counts within unique id
  count(cord_uid, word, sort = TRUE)%>%
  ungroup()

dtm <- tokens_count4 %>%
  cast_dtm(cord_uid, word, n)

#Use LDA to find 9 topics
lda <- LDA(dtm, k = 9, control = list(seed = 1234))

topics <- tidy(lda, matrix = "beta")

names(topics)[names(topics)=="term"] <- "word"

#get a small data frame of the top 10 words for each topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#top_terms

top_terms_topics <- top_terms

#Label topics
top_terms_topics$topic_title[which(top_terms_topics$topic==1)] <- "Gene Sequences in Viruses"
top_terms_topics$topic_title[which(top_terms_topics$topic==2)] <- "Cell Responses"
top_terms_topics$topic_title[which(top_terms_topics$topic==3)] <- "Reproduction of Viruses"
top_terms_topics$topic_title[which(top_terms_topics$topic==4)] <- "Public Health System"
top_terms_topics$topic_title[which(top_terms_topics$topic==5)] <- "Treatments for Respiratory Issues"
top_terms_topics$topic_title[which(top_terms_topics$topic==6)] <- "Influenza Viruses"
top_terms_topics$topic_title[which(top_terms_topics$topic==7)] <- "Health Pandemics"
top_terms_topics$topic_title[which(top_terms_topics$topic==8)] <- "Respiratory Syndromes/Illnesses"
top_terms_topics$topic_title[which(top_terms_topics$topic==9)] <- "Detecting Respiratory Viruses"

#top_terms %>%
 # mutate(term = reorder(term, beta)) %>%
  #ggplot(aes(term, beta, fill = factor(topic))) +
  #geom_col(show.legend = FALSE) +
#  facet_wrap(~ topic, scales = "free") +
 # coord_flip() +
  #labs(title = "LDA Topic Modeling- 9 Topics")


#Utilize more of top words for each topic for wordclouds
top_terms2 <- topics %>%
  group_by(topic) %>%
  top_n(50, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Get documents ready to be ordered in applicability to each topic
topic_merged <- merge(top_terms2,tokens_count4,by="word")
topic_merged$product <- topic_merged$beta * topic_merged$n
topic_merged2 <- topic_merged %>% group_by(cord_uid, topic) %>%
  summarize(sum_product = sum(product))
topic_merged3 <- merge(topic_merged2,data4,by="cord_uid")
topic_merged3 <- topic_merged3[order(-topic_merged3$sum_product),]

#wordcloud(top_terms_topic1$term, top_terms_topic1$beta, 
 #         min.freq=1, scale=c(3,.5), random.order=FALSE, 
  #        color=brewer.pal(8, "Dark2"))

#lda_gamma <- tidy(lda, matrix = "gamma")
#lda_gamma

#assignments <- augment(lda, data = dtm)
#assignments




##FORECASTING-- Nothing below seemed to work very well, so this was not included in the dashboard
# library(tidyverse)
# library(fpp2)
# #library(xts)
# #library(TSstudio)
# 
# 
# us_data2 <- us_data[order(us_data$reported_date2),]
# us_data2$new_confirmed_cases2[is.na(us_data2$new_total_deaths2)] <- 0
# #us_data2$new_confirmed_cases2[which(us_data2$new_total_deaths2 == 0)] <- 0.01
# us_data_ts <- ts(us_data2$new_confirmed_cases2)
# 
# 
# us_data_train <- window(us_data_ts, end = 92)
# us_data_test <- window(us_data_ts, start = 93)
# 
# ses_us_data <- ses(us_data_train, alpha = .2, h = 12)
# autoplot(ses_us_data)
# 
# us_data_diff <- diff(us_data_train)
# autoplot(us_data_diff)
# 
# ses_us_data_diff <- ses(us_data_diff, alpha = .2, h = 12)
# autoplot(ses_us_data_diff)
# 
# us_data_diff_test <- diff(us_data_test)
# accuracy(ses_us_data_diff, us_data_diff_test)
# 
# # identify optimal alpha parameter
# alpha <- seq(.01, .99, by = .01)
# RMSE <- NA
# for(i in seq_along(alpha)) {
#   fit <- ses(us_data_diff, alpha = alpha[i], h = 100)
#   RMSE[i] <- accuracy(fit, us_data_diff_test)[2,2]
# }
# 
# # convert to a data frame and idenitify min alpha value
# alpha.fit <- data_frame(alpha, RMSE)
# alpha.min <- filter(alpha.fit, RMSE == min(RMSE))
# 
# # plot RMSE vs. alpha
# ggplot(alpha.fit, aes(alpha, RMSE)) +
#   geom_line() +
#   geom_point(data = alpha.min, aes(alpha, RMSE), size = 2, color = "blue") 
# 
# RMSE
# 
# #refit model with optimal alpha
# ses_us_data_opt <- ses(us_data_diff, alpha = .82, h = 12)
# 
# # performance eval
# accuracy(ses_us_data_opt, us_data_diff_test)
# 
# # plotting results
# p1 <- autoplot(ses_us_data_opt) +
#   theme(legend.position = "bottom")
# p2 <- autoplot(us_data_diff_test) +
#   autolayer(ses_us_data_opt, alpha = .5) +
#   ggtitle("Predicted vs. actuals for the test data set")
# 
# gridExtra::grid.arrange(p1, p2, nrow = 1)
# 
# 
# 
# #Holt's Method
# 
# holt_us_data <- holt(us_data_train, h = 12)
# autoplot(holt_us_data)
# 
# holt_us_data$model
# 
# accuracy(holt_us_data, us_data_test)
# 
# # identify optimal alpha parameter
# beta <- seq(.0001, .5, by = .001)
# RMSE <- NA
# for(i in seq_along(beta)) {
#   fit <- holt(us_data_train, beta = beta[i], h = 100)
#   RMSE[i] <- accuracy(fit, us_data_test)[2,2]
# }
# 
# # convert to a data frame and idenitify min alpha value
# beta.fit <- data_frame(beta, RMSE)
# beta.min <- filter(beta.fit, RMSE == min(RMSE))
# 
# # plot RMSE vs. alpha
# ggplot(beta.fit, aes(beta, RMSE)) +
#   geom_line() +
#   geom_point(data = beta.min, aes(beta, RMSE), size = 2, color = "blue")
# RMSE
# 
# # new model with optimal beta
# holt_us_data_opt <- holt(us_data_train, h = 12, beta = 0.5)
# 
# # accuracy of first model
# accuracy(holt_us_data, us_data_test)
# 
# # accuracy of new optimal model
# accuracy(holt_us_data_opt, us_data_test)
# 
# p1 <- autoplot(holt_us_data) +
#   ggtitle("Original Holt's Model") #+
# #coord_cartesian(ylim = c(400, 1000))
# 
# p2 <- autoplot(holt_us_data_opt) +
#   ggtitle("Optimal Holt's Model") #+
# # coord_cartesian(ylim = c(400, 1000))
# 
# gridExtra::grid.arrange(p1, p2, nrow = 1)
# 
# 
# # plotting results
# p1 <- autoplot(holt_us_data_opt) +
#   theme(legend.position = "bottom")
# p2 <- autoplot(us_data_test) +
#   autolayer(holt_us_data_opt, alpha = .5) +
#   ggtitle("Predicted vs. actuals for the test data set")
# 
# gridExtra::grid.arrange(p1, p2, nrow = 1)
# 
# 
# #Holt-Winters Seasonal Method- no seasonal trend, so doesn't work
# autoplot(decompose(us_data_ts))
# us_data_hw <- ets(us_data_train, model = "AAA")
# autoplot(forecast(us_data_hw))
# 
# 

# 
#####LSTM
# #library(keras)
# 
# install.packages("devtools")
# 
# devtools::install_github("rstudio/keras")
# library(keras)
# #install_keras()
# install_keras(method = c("auto", "virtualenv", "conda"), conda = "auto",
#               tensorflow = "default", extra_packages = NULL)
# #library(tensorflow)
# 
# 
# 
# 
# Series = us_data2$new_confirmed_cases  # your time series 
# 
# # transform data to stationarity
# diffed = diff(Series, differences = 1)
# 
# 
# # create a lagged dataset, i.e to be supervised learning
# 
# lags <- function(x, k){
#   
#   lagged =  c(rep(NA, k), x[1:(length(x)-k)])
#   DF = as.data.frame(cbind(lagged, x))
#   colnames(DF) <- c( paste0('x-', k), 'x')
#   DF[is.na(DF)] <- 0
#   return(DF)
# }
# supervised = lags(diffed, 1)
# head(supervised)
# 
# 
# ## split into train and test sets
# 
# N = nrow(supervised)
# n = round(N *0.66, digits = 0)
# train = supervised[1:n, ]
# test  = supervised[(n+1):N,  ]
# 
# 
# ## scale data
# normalize <- function(train, test, feature_range = c(0, 1)) {
#   x = train
#   fr_min = feature_range[1]
#   fr_max = feature_range[2]
#   std_train = ((x - min(x) ) / (max(x) - min(x)  ))
#   std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
#   
#   scaled_train = std_train *(fr_max -fr_min) + fr_min
#   scaled_test = std_test *(fr_max -fr_min) + fr_min
#   
#   return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
#   
# }
# 
# 
# ## inverse-transform
# inverter = function(scaled, scaler, feature_range = c(0, 1)){
#   min = scaler[1]
#   max = scaler[2]
#   n = length(scaled)
#   mins = feature_range[1]
#   maxs = feature_range[2]
#   inverted_dfs = numeric(n)
#   
#   for( i in 1:n){
#     X = (scaled[i]- mins)/(maxs - mins)
#     rawValues = X *(max - min) + min
#     inverted_dfs[i] <- rawValues
#   }
#   return(inverted_dfs)
# }
# 
# 
# Scaled = normalize(train, test, c(-1, 1))
# 
# y_train = Scaled$scaled_train[, 2]
# x_train = Scaled$scaled_train[, 1]
# 
# y_test = Scaled$scaled_test[, 2]
# x_test = Scaled$scaled_test[, 1]
# 
# ## fit the model
# 
# dim(x_train) <- c(length(x_train), 1, 1)
# dim(x_train)
# X_shape2 = dim(x_train)[2]
# X_shape3 = dim(x_train)[3]
# batch_size = 1
# units = 1
# 
# model <- keras_model_sequential() 
# model%>%
#   layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
#   layer_dense(units = 1)
# 
# 
# 
# model %>% compile(
#   loss = 'mean_squared_error',
#   optimizer = optimizer_adam( lr= 0.02 , decay = 1e-6 ),  
#   metrics = c('accuracy')
# )
# 
# 
# 
# summary(model)
# 
# nb_epoch = 20   
# for(i in 1:nb_epoch ){
#   model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
#   model %>% reset_states()
# }
# 
# 
# L = length(x_test)
# dim(x_test) = c(length(x_test), 1, 1)
# 
# scaler = Scaled$scaler
# 
# predictions = numeric(L)
# for(i in 1:L){
#   X = x_test[i , , ]
#   dim(X) = c(1,1,1)
#   # forecast
#   yhat = model %>% predict(X, batch_size=batch_size)
#   
#   # invert scaling
#   yhat = inverter(yhat, scaler,  c(-1, 1))
#   
#   # invert differencing
#   yhat  = yhat + Series[(n+i)] 
#   
#   # save prediction
#   predictions[i] <- yhat
# }
# 
# plot(predictions)
# #lines(stats::lowess(predictions))
# lines(x=predictions, y=NULL, col="blue")
# #plot(y_test)
# lines(x=Series2, y=NULL, col="red")
# 
# #ggplot(data=NULL)
# 
# ggplot(data=NULL) +
#   #geom_line(aes(x=predictions, y=NULL)) + 
#   geom_line(aes(x=Series, y=us_data2$reported_date))
# 
# 
# Series2 = Series[70:104]
# us_data2$pred_new_confirmed_cases <- NULL
# us_data2$pred_new_confirmed_cases[70:104] <- predictions 
# 
# ggplot(data=us_data2) +
#   geom_line(aes(y=new_confirmed_cases, x=reported_date2), colour="black") +
#   geom_line(aes(y=pred_new_confirmed_cases, x=reported_date2), colour="red") +
#   labs(title = 'LSTM Predictions- New Confirmed Cases', x = 'Date', y = 'New Confirmed Cases')
# 
# 
# 
# ##Forecasting using lm
# linear_model <-lm(us_data2$new_confirmed_cases2 ~ us_data2$reported_date2)
# plot(us_data2$reported_date2, us_data2$new_confirmed_cases2, pch=16, ylab = "Counts ", cex.lab = 1.3, col = "red" )
# abline(lm(us_data2$new_confirmed_cases2 ~ us_data2$reported_date2), col = "blue")
# 
# 
# us_data2 <- us_data[order(us_data$reported_date2),]
# us_data2$new_confirmed_cases2[is.na(us_data2$new_confirmed_cases2)] <- 0
# us_data_ts <- ts(us_data2$new_confirmed_cases2)
# t <- time(us_data_ts)
# linear_model <-lm(us_data2$new_confirmed_cases2 ~ t + I(t^2))
# plot(us_data2$new_confirmed_cases2, main = "Time Series with Quadratic Trend")
# points(t,predict.lm(linear_model),type='l',col='red')
# 
# 
# 
# #fit first degree polynomial equation:
# fit  <- lm(us_data2$new_confirmed_cases2~t)
# #second degree
# fit2 <- lm(us_data2$new_confirmed_cases2~poly(t,2,raw=TRUE))
# #third degree
# fit3 <- lm(us_data2$new_confirmed_cases2~poly(t,3,raw=TRUE))
# #fourth degree
# fit4 <- lm(us_data2$new_confirmed_cases2~poly(t,4,raw=TRUE))
# 
# plot(us_data2$new_confirmed_cases2, main = "Time Series with Quadratic Trend")
# points(t,predict.lm(fit),type='l',col='red')
# points(t,predict.lm(fit2),type='l',col='blue')
# points(t,predict.lm(fit3),type='l',col='green')
# points(t,predict.lm(fit4),type='l',col='purple')
# #library(fpp2)
# x <- 1:150
# pred = ts(x, start=1,end=150, frequency = 1)
# 
# fit4_pred <- forecast(fit4, newdata=pred)
# 
# plot(us_data2$new_confirmed_cases2, main = "Time Series with Quadratic Trend")
# points(t,predict.lm(fit4),type='l',col='blue')
# points(pred,predict.lm(fit4),type='l',col='red')
# 
# ggplot(data=NULL) +
#   #geom_line(aes(x=t, y=predict.lm(fit4)))+ 
#   geom_line(aes(x=pred, y=fit4_pred))





#####DASHBOARD#####

#Create layouts for plots on pages
frow1 <- fluidRow(
  box(plotOutput("plot1", height = 250), width = 12)
)
frow2 <- fluidRow(
  box(plotOutput("plot2", height = 400), width = 12)
)
frow3 <- fluidRow(
  box(plotOutput("plot3", height = 500), width = 36)
)

frow4 <- fluidRow(
  box(plotOutput("plot4", height = 350), width = 12)
)

frow5 <- fluidRow(
  box(plotOutput("plot5", height = 300), width = 12),
)

frow6 <- fluidRow(
  box(plotOutput("plot6", height = 500), width = 36)
)

frow7 <- fluidRow(
  box(plotOutput("plot7", height = 250), width = 12)
)

frow8 <-  fluidRow(column(12,
                          DT::dataTableOutput('table'))
)

frow9 <- fluidRow(
  box(plotOutput("plot8", height = 250), width = 12)
)

frow10 <- fluidRow(
  box(plotOutput("plot9", height = 250), width = 12)
)



# Define UI for app ----
ui <- ({
  #shinyUI(navbarPage(title = span( "COVID-19 Exploration", style = "background-color: #DEEBF7; color:black") ,
  shinyUI(navbarPage(title = "COVID-19 Exploration" ,                   
                     #theme = shinytheme("slate"),
                     theme = shinytheme("cerulean"),
                     tabPanel("Introduction",
                              h3("Project Introduction"),
                              br(),
                              h4("Project Background"),
                              "Due to the recent spreading of COVID-19, this dashboard has been created to help people better explore current trends for COVID-19 cases and deaths.  This dashboard also allows people to explore factors that may be playing a role in the spread of COVID-19, such as population density and weather patterns.  Finally, this dashboard provides people the opportunity to find articles surrounding various aspects of the coronaviruses through LDA topic modeling.  My hope in creating this dashboard is for it to be an educational tool for anyone to learn more about the spread of COVID-19 as well as a tool for people researching various aspects of the coronaviruses.",
                              #img(src='https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.nature.com%2Fcollections%2Fhajgidghjb&psig=AOvVaw2VPqRGF2exA8pdNAPdjbqd&ust=1587963097889000&source=images&cd=vfe&ved=0CAIQjRxqFwoTCNCjx9WlhekCFQAAAAAdAAAAABAD', align = "right"),
                              #htmlOutput('picture'),
                              uiOutput("myList"),
                              h4("Disclaimers & Future Work"),
                              "Please note that not all the data may be completely reflective of current COVID-19 trends as there is always room for user error when inputting data and not all cases have been reported.  In addition to that, please note that the COVID-19 projections may change over time, so these projections are only reflective as of the date the dashboard was created.  Finally, in the future, a goal for this project would be to collect more data on the factors affecting the spread of COVID-19 since the datasets used did not have data reported past March.",
                              br(),
                              br(),
                              a(href="https://www.cdc.gov/coronavirus/2019-ncov/index.html", "Click Here to View Information on CDC Guidelines Surrounding COVID-19"),
                              br(),
                              br()
                              ),
                     tabPanel("Trends",
                              h3("Visualizing the Spread of COVID-19"),
                              br(),
                              sidebarPanel(
                                selectInput('country', 'What country do you want to view COVID-19 trends for? (Figure 1)', 
                                            c("United States" = "us_data",
                                              "China" = "china_data",
                                              "Spain" = "spain_data",
                                              "Italy" = "italy_data",
                                              "Sweden" = "sweden_data")),
                                br(),
                                br(),
                                selectInput('country_graph', 'What measure do you want to compare for countries? (Figure 2)',
                                            c('New Positive Cases' = 'new_confirmed_cases2',
                                              'New Deaths' = 'new_total_deaths2',
                                              'Daily Death Rate' = 'daily_death_rate',
                                              'Total Positive Cases' = 'confirmed_cases2',
                                              'Total Deaths' = 'total_deaths2',
                                              'Total Death Rate' = 'total_death_rate'
                                            ))
                              ),
                              mainPanel(frow6,
                                        br(),
                                        br(),
                                        frow5,
                                        br(),
                                        br())
                              
                              #splitLayout(cellWidths = c("50%", "50%"), frow5, frow6)
                              #frow6)
                     ),
                     tabPanel("Factors",
                              h3("Factors Affecting the Spread of COVID-19"),
                              br(),
                              #"**Note: Maps may take a few moments to load, and not all measures for countries have data for all dates.",
                              sidebarPanel(
                                
                                radioButtons("type", "Type of Weather (Figure 1): ",
                                             c("Temperature" = "tempC",
                                               "Humidity" = "humidity",
                                               "Windspeed" = "windspeedKmph")),
                                br(),
                                br(),
                                sliderInput("animation", "Date (Figures 1 & 2):",
                                            min = as.Date('2020-01-22', format="%Y-%m-%d"), max = as.Date('2020-03-21', format="%Y-%m-%d"),
                                            value = as.Date("2020-01-22", format="%Y-%m-%d"), step = 1),
                                br(),
                                br(),
                                selectInput('country_factors', 'What measure do you want to view for countries (Figure 2)?',
                                            c('New Positive Cases' = 'new_confirmed_cases2',
                                              'New Deaths' = 'new_total_deaths2',
                                              'Daily Death Rate' = 'daily_death_rate',
                                              'Total Positive Cases' = 'confirmed_cases2',
                                              'Total Deaths' = 'total_deaths2',
                                              'Total Death Rate' = 'total_death_rate'
                                            ))
                              ),
                              mainPanel("**Note: Maps may take a few moments to load, and not all measures for countries have data for all dates.",
                                        h4("Weather Factors"),
                                        frow1,
                                        br(),
                                        br(),
                                        h4("COVID-19 Measure"),
                                        frow9,
                                        br(),
                                        br(),
                                        h4("Population Density in 2018"),
                                        frow10,
                                        br(),
                                        br())
                                        #frow4,
                                        #br(),
                                        #br(),
                                        #frow7
                     ),
                     tabPanel("LDA",
                              h3("LDA"),
                              ("LDA, or Latent Dirichlet Allocation, is a method frequently used to model topics within a set of documents.  Since the documents do not previously have topic labels, this is a method of unsupervised classification.  LDA is a very commonly used method for modeling topics, because it allows documents to be classified under multiple topics since each topic is made up of a mixture of words, some of which may appear in multiple topics.  For the coronavirus documents, I chose to utilize LDA to identify nine main topics within the documents (as seen in Figure 1 below), which I labeled myself based on the most common words within each topic.  I also utilized the probability of each word being found in each topic, also known as beta, as well as the count of how many times each word appeared in each document, to create a recommendation system for the documents.  If you select one of the topics below, you can view a word cloud of the fifty most common words in that topic as well as a list of documents related to that topic (in order of applicability)."),
                              br(),
                              br(),
                              br(),
                              sidebarPanel(width=3.5,
                                selectInput('recommendations', 'What topic do you want to learn more about?',
                                            c('Gene Sequences in Viruses' = 1,
                                              'Cell Responses' = 2,
                                              'Reproduction of Viruses' = 3,
                                              'Public Health System' = 4,
                                              'Treatments for Respiratory Issues' = 5,
                                              'Influenza Viruses' = 6,
                                              'Health Pandemics' = 7,
                                              'Respiratory Syndromes/Illnesses' = 8,
                                              'Detecting Respiratory Viruses' = 9
                                            ))
                              ),
                              #sidebarPanel([inputs for the second tab]),
                              mainPanel(width=8.5,
                                        br(),
                                        h4("9 Common Topics within Coronavirus Documents"),
                                        frow3,
                                        br(),
                                        br(),
                                        h4("Common Words within the Chosen Topic"),
                                        frow2,
                                        br(),
                                        br(),
                                        h4("Documents Related to the Chosen Topic"),
                                        br(),
                                        frow8,
                                        br(),
                                        br())
                     ),
                     tabPanel("Conclusions",
                              uiOutput("myConclusions")),
                     tabPanel("Data Citations",
                              uiOutput("myCitations")),
                     tabPanel("Contact Information",
                              h3("Contact Information"),
                              "Name: Becca Dura",
                              br(),
                              "Email: rebecca.dura@drake.edu"
                     )
  ))
})


server <- function(input, output) {
  weather_data2 <- reactive({
    weather_data[which(weather_data$date2 == input$animation),]
  })
  
  
  output$plot1 <- renderPlot({
    ggplot(data=weather_data2()) +
      geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
      geom_point(aes_string(x='long', y='lat', size=input$type, color=input$type), alpha=0.4) +
      scale_size_continuous(range=c(0.5,8)) +
      scale_color_viridis(trans="log") +
      theme_void() + 
      coord_map()
  })
  
  top_terms_wordcloud <- reactive({top_terms2[which
                                 (top_terms2$topic == input$recommendations),]})
  
  output$plot2 <- renderPlot({
    wordcloud(top_terms_wordcloud()$word, top_terms_wordcloud()$beta, 
              min.freq=1, scale=c(4.5,0.5), random.order=FALSE, 
              color=brewer.pal(8, "Dark2"))
    #par(mar = rep(0, 4))
  })
  output$plot3 <- renderPlot({
    top_terms_topics %>%
      mutate(word = reorder(word, beta)) %>%
      ggplot(aes(word, beta, fill = factor(topic_title))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic_title, scales = "free") +
      coord_flip() +
      #labs(title = "9 Common Topics within Coronavirus Documents") +
      theme(plot.title = element_text(face = "bold"))
  })
  output$myList <- renderUI(HTML("<ul><h4>List of Project Goals </h4>
                                  <li>Goal 1: Visualize the spread of COVID-19.</li>
                                  <li>Goal 2: Visualize factors that may be affecting the spread of COVID-19.</li>
                                  <li>Goal 3: Utilize LDA topic modeling on abstracts from documents about the coronaviruses to identify common topics within the documents.</li>
                                  <li>Goal 4: Create a recommendation system that displays documents about various coronavirus topics in order of applicability.</li></ul>"))
  
  output$plot4 <- renderPlot({
    ggplot(data = population_density_data2, aes(reorder(x = Country.Name, -X2018) , y = X2018)) +
      geom_bar(stat="identity", fill="light blue", colour="black")+
      labs(x = "Country", y = "Population Density in 2018", title = "Population Density by Country") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(face = "bold"))
  })
  
  
  
  country_graph_y <- reactive({
    if (input$country_graph == "new_confirmed_cases2")
      "New Confirmed Cases"
    else if (input$country_graph == "new_total_deaths2")
      "New Deaths"
    else if (input$country_graph == "daily_death_rate")
      "Daily Death Rate"
    else if (input$country_graph == "confirmed_cases2")
      "Total Confirmed Cases"
    else if (input$country_graph == "total_deaths2")
      "Total Deaths"
    else if (input$country_graph == "total_death_rate")
      "Total Death Rate"
  })
  
  output$plot5 <- renderPlot({
    ggplot(data = NULL) +
      geom_smooth(data=china_data, aes(x = reported_date2, y = !!as.name(input$country_graph), colour = "red")) + 
      geom_smooth(data=us_data, aes(x = reported_date2, y = !!as.name(input$country_graph), colour = "blue")) + 
      geom_smooth(data=spain_data, aes(x = reported_date2, y = !!as.name(input$country_graph), colour = "black")) + 
      geom_smooth(data=italy_data, aes(x = reported_date2, y = !!as.name(input$country_graph), colour = "green")) + 
      geom_smooth(data=sweden_data, aes(x = reported_date2, y = !!as.name(input$country_graph), colour = "purple")) + 
      scale_color_identity(name = "Key",
                           breaks = c("red", "blue", "black", "green", "purple"),
                           labels = c("China", "United States", "Spain", "Italy", "Sweden"),
                           guide = "legend") +
      labs(x = "Date", y = country_graph_y(), title = paste(country_graph_y(),"by Country")) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  country_data2 <- reactive({
    if (input$country == "us_data")
      us_data
    else if (input$country == "china_data")
      china_data
    else if (input$country == "spain_data")
      spain_data
    else if (input$country == "italy_data")
      italy_data
    else if (input$country == "sweden_data")
      sweden_data
  })
  
  country_title <- reactive({
    if (input$country == "us_data")
      "Confirmed COVID-19 Cases, Deaths, & Death Rate Over Time in US"
    else if (input$country == "china_data")
      "Confirmed COVID-19 Cases, Deaths, & Death Rate Over Time in China"
    else if (input$country == "spain_data")
      "Confirmed COVID-19 Cases, Deaths, & Death Rate Over Time in Spain"
    else if (input$country == "italy_data")
      "Confirmed COVID-19 Cases, Deaths, & Death Rate Over Time in Italy"
    else if (input$country == "sweden_data")
      "Confirmed COVID-19 Cases, Deaths, & Death Rate Over Time in Sweden"
  })
  
  output$plot6 <- renderPlot({
    plot1 <- ggplot(data = country_data2()) +
      geom_smooth(aes(x = reported_date2, y = confirmed_cases2)) +
      geom_line(aes(x = reported_date2, y = confirmed_cases2)) + 
      labs(x = "Date", y = "Total Confirmed Cases")
    plot2 <- ggplot(data = country_data2()) +
      geom_smooth(aes(x = reported_date2, y = new_confirmed_cases2)) + 
      geom_line(aes(x = reported_date2, y = new_confirmed_cases2)) +
      labs(x = "Date", y = "New Confirmed Cases")
    plot3 <- ggplot(data = country_data2()) +
      geom_smooth(aes(x = reported_date2, y = total_deaths2)) + 
      geom_line(aes(x = reported_date2, y = total_deaths2)) +
      labs(x = "Date", y = "Total Confirmed Deaths")
    plot4 <- ggplot(data = country_data2()) +
      geom_smooth(aes(x = reported_date2, y = new_total_deaths2)) + 
      geom_line(aes(x = reported_date2, y = new_total_deaths2)) +
      labs(x = "Date", y = "New Confirmed Deaths")
    plot5 <- ggplot(data = country_data2()) +
      geom_smooth(aes(x = reported_date2, y = total_death_rate)) +
      geom_line(aes(x = reported_date2, y = total_death_rate)) + 
      labs(x = "Date", y = "Total Death Rate")
    plot6 <- ggplot(data = country_data2()) +
      geom_smooth(aes(x = reported_date2, y = daily_death_rate)) +
      geom_line(aes(x = reported_date2, y = daily_death_rate)) + 
      labs(x = "Date", y = "Daily Death Rate")
    
    title <- ggdraw() + 
      draw_label(country_title(), fontface='bold')
    row1 <- plot_grid(plot1, plot2, ncol = 2)
    row2 <- plot_grid(plot3, plot4, ncol = 2)
    row3 <- plot_grid(plot5, plot6, ncol = 2)
    plot_grid(title, row1, row2, row3, nrow = 4,
              rel_heights = c(0.2, 1, 1, 1))
  })
  
  output$plot7 <- renderPlot({
    ggplot(data = main_countries_data, aes(reorder(x = Country.Name, -X2018), y = X2018)) +
      geom_bar(stat="identity", fill="light blue", colour="black") +
      labs(x = "Country", y = "Population Density in 2018", title = "Population Density by Country") +
      theme(plot.title = element_text(face = "bold"))
  })
  
  topic_merged4 <- reactive({
    topic_merged3[which(topic_merged3$topic == input$recommendations),]
  })
  
  topic_merged5 <- reactive({topic_merged4()[c('title', 'publish_time', 'journal','url')]})
  
  #topic_merged4 <- reactive({
   # topic_merged3[c('title', 'publish_time', 'journal','url')]})
  
  output$table <- DT::renderDataTable(topic_merged5())
  
  output$myCitations <- renderUI(HTML("<ul><h3>Data Citations </h3>
                                  <li>Namara (2020). WHO Situation Reports: COVID-19. Retrieved from https://app.namara.io/#/data_sets/284444a6-86b5-495e-9657-99bdad85ea7a</li>
                                  <li>Pierre Winter, Kaggle (2020). COVID19_Global_Weather_Data. Retrieved from https://www.kaggle.com/winterpierre91/covid19-global-weather-data</li>
                                  <li>World Bank Group (2020). Population density (people per sq. km of land area). Retrieved from https://data.worldbank.org/indicator/en.pop.dnst</li>
                                  <li>Allen Institute for AI, Kaggle (2020). COVID-19 Open Research Dataset Challenge (CORD-19). Retrieved from https://www.kaggle.com/allen-institute-for-ai/CORD-19-research-challenge/#metadata.csv</li></ul>"))
  
  factors_data_merged_date <- reactive({factors_data_merged[which(factors_data_merged$reported_date2 == input$animation),]})
  
  output$plot8 <- renderPlot({
    ggplot(data=factors_data_merged_date()) +
      geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
      geom_point(aes_string(x='long', y='lat', size=input$country_factors, color=input$country_factors), alpha=0.4) +
      scale_size_continuous(range=c(0.5,8)) +
      scale_color_viridis(trans="log") +
      theme_void() + 
      coord_map()
  })
  
  output$plot9 <- renderPlot({
    ggplot(data=factors_pop_data_merged) +
      geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
      geom_point(aes_string(x='long', y='lat', size='X2018', color='X2018'), alpha=0.4) +
      scale_size_continuous(range=c(0.5,8)) +
      scale_color_viridis(trans="log") +
      theme_void() + 
      coord_map()
  })
  
  output$myConclusions <- renderUI(HTML("<ul><h3>Project Conclusions </h3>
                                  <li>Documents on the coronaviruses can be split into the following nine categories: cell responses, detecting respiratory viruses, gene sequences in viruses, health pandemics, influenza viruses, the public health system, reproduction of viruses, respiratory syndromes and illnesses, and treatments for respiratory issues.</li>
                                  <li>LDA does seem to be an effective method when creating a recommendation system, as my coronavirus document recommender performs quite well, in my opinion.</li>
                                  <li>China does not regularly report their COVID-19 cases and deaths, so even with manually cleaning their data, it is not very easy to visualize China's COVID-19 trends.</li>
                                  <li>Italy, Spain, and China have all had significantly decreasing numbers in COVID-19 cases and deaths recently, while the United States and Sweden are just now starting to come down from their peak numbers.</li>
                                  <li>Out of the five countries analyzed, Italy, by far, had the highest death rates.  Italy also has a much higher population density than the other four countries, signifying that areas with a higher population density may have higher death rates.</li>
                                  <li>Areas with higher humidities tend to have higher death rates. </li></ul>"))
  
  #output$picture<-renderText({c('<img src="https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.nature.com%2Fcollections%2Fhajgidghjb&psig=AOvVaw2VPqRGF2exA8pdNAPdjbqd&ust=1587963097889000&source=images&cd=vfe&ved=0CAIQjRxqFwoTCNCjx9WlhekCFQAAAAAdAAAAABAD">')})
  
}

# Create Shiny app ----
shinyApp(ui, server)
