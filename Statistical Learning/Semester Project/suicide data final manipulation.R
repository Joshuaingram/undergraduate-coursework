# Data Manipulation for Statistical Learning Project Suicide Data

# loading in the dataset
library(readr)
suicide.data <- read.csv("C:/Users/joshi/Downloads/suicide-rates-overview-1985-to-2016 (3)/master.csv")

library(tidyverse)

head(suicide.data)
ls(suicide.data)
nrow(suicide.data)
ncol(suicide.data)

suicide.data$sex <- as.factor(suicide.data$sex)
suicide.data$country <- as.factor(suicide.data$ï..country)
suicide.data$generation <- as.factor(suicide.data$generation)
suicide.data$age <- as.factor(suicide.data$age)
suicide.data$suicides_no <- as.numeric(suicide.data$suicides_no)
suicide.data$population <- as.numeric(suicide.data$population)
suicide.data$suicides.100k.pop <- as.numeric(suicide.data$suicides.100k.pop)
suicide.data$HDI.for.year <- as.numeric(suicide.data$HDI.for.year)
suicide.data$gdp_per_capita <- as.numeric(suicide.data$gdp_per_capita....)
suicide.data$gdp_for_year <- suicide.data$gdp_for_year....

suicide.data$country.year<- NULL
suicide.data$ï..country <- NULL
suicide.data$gdp_for_year.... <- NULL
suicide.data$gdp_per_capita.... <- NULL

# Create four categories for HDI index: very high development (.8-1), high 
# development (.6-.799), medium development (.4 - .599), low development (0 - .4)

# new development variable created with 4 factors (shown above)
suicide.data.mutated <- mutate(suicide.data, Development = factor(case_when(HDI.for.year >= .8 ~ "high",
                                                                            HDI.for.year >= .6 & HDI.for.year <= .799 ~ "medium",
                                                                            HDI.for.year <= .599 ~ "low or very low",
                                                                            is.na(HDI.for.year) == T ~ "unknown",
                                                                            TRUE ~ NA_character_)))
head(suicide.data.mutated)

levels(suicide.data$country)

# creating a variable to break countries up into 8 regions (as defined by US Department of Homeland Security)
# first I'll make the lists for each region of the world
Africa <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon",
            "Cape Verde", "Cental African Republic", "Chad", "Comoros", "Cote d'lvoire", "Democratic Republic of the Congo",
            "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon",
            "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia",
            "Libya", "Madagascar", "Malawi", "Mali", "Mali", "Mauritania", "Mauritius", "Morocco",
            "Mozambique", "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Reunion", "Rwanda",
            "Saint Helena", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan",
            "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara", "Zambia", "Zimbabwe")
Asia <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei", "Burma",
          "Cambodia", "China", "Cyprus", "East Timor", "Georgia", "Hong Kong", "India", "Indonesia",
          "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwwait", "Kyrgyzstan", "Laos",
          "Lebanon", "Macau", "Malaysia", "Maldives", "Mongolia", "Nepal", "North Korea", "Oman",
          "Pakistan", "Philippines", "Qatar", "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka", "Syria",
          "Taiwan", "Tajikistan", "Thailand", "Turkey", "Turkmenistan", "United Arab Emirates",
          "Uzbekistan", "Yemen")
Caribbean <- c("Anguilla", "Antigua and Barbuda", "Aruba", "Bahamas", "Barbados",
               "Bermuda", "British Virgin Islands", "Cayman Islands", "Cuba", "Dominica",
               "Dominican Republic", "Grenada", "Guadeloupe", "Haiti", "Jamaica", "Martinique",
               "Montserrat", "Netherlands Antilles", "Puerto Rico", "Saint Kitts and Nevis", "Saint Lucia",
               "Saint Vincent and the Grenadines", "Trinidad and Tobago", "Turks and Caicos Islands",
               "U.S. Virgin Islands")
Central.America <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua",
                     "Panama")
Europe <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia",
            "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Gibraltar", "Greece",
            "Holy See", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein", 
            "Lithuania", "Luxembourg", "Macedonia", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands",
            "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Slovak Republic", "Slovenia",
            "Spain", "Serbia", "Serbia and Montenegro", "Sweden", "Switzerland", "Ukraine", "United Kingdom")
North.America <- c("Canada", "Greenland", "Mexico", "Saint Pierre and Miquelon", "United States")
Oceania <- c("American Samoa", "Australia", "Christmas Island", "Cocos Islands", "Cook Islands",
             "Federated States of Micronesia", "Fiji", "French Polynesia", "Guam", "Kiribati",
             "Marshall Islands", "Nauru", "New Caledonia", "New Zealand", "Niue", "Northern Mariana Islands",
             "Palau", "Papua New Guinea", "Pitcairn Islands", "Samoa", "Solomon Islands", "Tokelau", "Tonga",
             "Tuvalu", "Vanuatu", "Wallis and Futuna Islands")
South.America <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Falkland Islands",
                   "French Guiana", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")

suicide.data.mutated2 <- mutate(suicide.data.mutated, Region = factor(case_when(country %in% Africa ~ "Africa",
                                                                                country %in% Asia ~ "Asia",
                                                                                country %in% Caribbean ~ "Caribbean",
                                                                                country %in% Central.America ~ "Central America",
                                                                                country %in% Europe ~ "Europe",
                                                                                country %in% North.America ~ "North America",
                                                                                country %in% Oceania ~ "Oceania",
                                                                                country %in% South.America ~ "South America",
                                                                                TRUE ~ NA_character_)))

tail(suicide.data.mutated2)

# removed all years before 1990 since the hdi was not created yet
suicide.data.mutated3 <- suicide.data.mutated2[which(suicide.data.mutated2$year >= 1990),]

mean(is.na(suicide.data.mutated3))

mean(is.na(suicide.data.mutated3$HDI.for.year))
# 68% of our data in HDI is missing


# creating a new subset of data that gets the total number of suicides and removes all the demographic information
years.list <- seq(1990, 2016, 1)
country.list <- levels(suicide.data.mutated3$country)

colnames(suicide.data.mutated3)

aggregate.suicides <- c()
aggregate.population <- c()
aggregate.per100 <- c()
new.years <- c()
new.countries <- c()
new.development <- c()
new.region <- c()
new.gdp.capita <- c()

for (j in country.list){
  for (i in years.list){
    selected.rows <- suicide.data.mutated3[suicide.data.mutated3$year == i & suicide.data.mutated3$country == j,]
    
    if (nrow(selected.rows) != 0){
      total.suicides <- sum(selected.rows$suicides_no)

      aggregate.suicides <- append(aggregate.suicides, total.suicides)
      
      total.population <- sum(selected.rows$population)
      
      aggregate.population <- append(aggregate.population, total.population)
      
      per100 <- total.suicides / (total.population/100000)
      aggregate.per100 <- append(aggregate.per100, per100)
      
      new.years <- append(new.years, i)
      new.countries <- append(new.countries, j)
      new.development <- append(new.development, selected.rows$Development[1])
      new.region <- append(new.region, selected.rows$Region[1])
      new.gdp.capita <- append(new.gdp.capita, selected.rows$gdp_per_capita[1])
    }
    selected.rows <- NULL
  }
}

new.suicide.data <- data.frame(new.years, new.countries, new.region, new.development, 
                               new.gdp.capita, aggregate.population, aggregate.suicides,  aggregate.per100)

colnames(new.suicide.data) <- c("Year", "Country", "Region", "Development", "GDP.per.capita", "Population", "Suicides", "Suicides.per100")

# new development variable created with 4 factors (shown above)
new.suicide.data <- mutate(new.suicide.data, Development = factor(case_when(Development == 1 ~ "high",
                                                                            Development == 3 ~ "medium",
                                                                            Development == 2 ~ "low or very low",
                                                                            Development == 4 ~ "unknown",
                                                                            TRUE ~ NA_character_)))

new.suicide.data <- mutate(new.suicide.data, Region = factor(case_when(Region == 1 ~ "Africa",
                                                                            Region == 3 ~ "Asia",
                                                                            Region == 2 ~ "Caribbean",
                                                                            Region == 4 ~ "Central America",
                                                                            Region == 5 ~ "Europe",
                                                                            Region == 6 ~ "North America",
                                                                            Region == 7 ~ "Oceania",
                                                                            Region == 8 ~ "South America",
                                                                            TRUE ~ NA_character_)))
head(new.suicide.data)

demographic.suicides <- suicide.data.mutated3
aggregate.suicides <- new.suicide.data

head(demographic.suicides)
head(aggregate.suicides)
