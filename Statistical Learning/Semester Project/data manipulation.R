tail(master, 20)

suicides <- master

suicides$sex <- as.factor(suicides$sex)
suicides$country <- as.factor(suicides$country)
suicides$generation <- as.factor(suicides$generation)
suicides$age <- as.factor(suicides$age)

lapply(suicides, levels)

'''

Things to do to manipulate data:


Create four categories for HDI index: very high development (.8-1), high development (.6-.799), medium development (.4 - .599), low development (0 - .4)
determine which levels of HDI are best
see which HDI is used (inequality adjusted?)
understand what is used to findHDI and what it means
select year to use for HDI level (if the country will have constant HDI throughout)

perhaps I can combine the data for each year so I only have population, gdp_for_year, gdp_per_capita, population, country, HDI, total suicides, and suicides per 100k.
This may be a lot of work, but could prove useful for focusing on the interaction between GDP and HDI affecting suicides

Figure out what to do for NA values for countries without HDI (remove from dataset?)

what if I also create a region/continent variable, rather than just country? This could be added to looking at HDI, etc.

too much missing data for the HDI... will have to break countries up into regions or continents and look at interaction between region and GDP



'''


