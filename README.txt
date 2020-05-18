# COVID-19 EXPLORATION


## AUTHOR
------------
- Becca Dura - rebecca.dura@drake.edu


## INTRODUCTION
------------

This project was created as an interactive tool to help people better visualize COVID-19 trends, determine factors that play a role in the spread of COVID-19, and find documents related to various coronavirus topics through LDA topic modeling. 

## REQUIREMENTS
------------

In order to run this code, R must be installed.

For Macs, R can be installed using this link: https://cran.r-project.org/bin/macosx/

For Windows, use this link: 
https://cran.r-project.org/bin/windows/

Then install free RStudio Desktop using the following link:

https://rstudio.com/products/rstudio/download/

Open RStudio. The following packages must be installed:

ggplot2,
lubridate,
cowplot,
maps,
plotly,
mapproj,
viridis,
RColorBrewer,
dplyr,
tidytext,
topicmodels,
reshape2,
SnowballC,
stringr,
textstem,
wordcloud,
shiny,
shinydashboard,
shinythemes

To install a package, follow this example:
```
install.packages("dplyr")
```

## EXECUTING PROGRAM  
-----------------

To run this code, the following files must be downloaded and saved to your workspace

1. 'metadata.csv' 

This file contains data on many documents regarding the coronaviruses.. 

2. 'Population_Density_by_Country.csv' 

This file contains data on the population density for each country in 2018. 

3. 'temperature_dataframe.csv' 

This file contains data on the weather patterns of various countries from the beginning of 2020. 

4. 'who-situation-reports-covid-19.csv' 

This file contains daily data on COVID-19 cases and deaths in 2020 for each country. 


Following this, open 'DuraFinalDashboard.R' and click 'Run'.





