library(here)
gapminder=read.csv(here::here("data","gapminder5.csv"))
str(gapminder)
# change country and continent into character
gapminder$country=as.character(gapminder$country)
gapminder$continent=as.character(gapminder$continent)
str(gapminder)

# create a vector of values that you want to repeat the function for
obs = 1:nrow(gapminder)

# initialize the for loop with `for (i in vector)` 
for(i in obs){
    gapminder[i,"gap"]=gapminder[i,"pop"]*gapminder[i,"gdpPercap"]
}

#Create a new variable that finds that natural log (log) of the GDP per capita 
#and of population - call them log_gdpPercap and log_pop
for(i in obs){
    gapminder[i,"log_gdpPercap"]=log(gapminder$gdpPercap[i])
    gapminder[i,"log_pop"]=log(gapminder$pop[i])
}

#Avoid when possible, especially when there is a vectorized function you can use
gapminder$vec_log_gdpPercap = log(gapminder$gdpPercap)
all(gapminder$vec_log_gdpPercap == gapminder$log_gdpPercap)

# Has life expectancy increased over time?
# Find the mean life expectancy by year
years = unique(gapminder$year)

for (i in years) {
    mean_le <- mean(gapminder$lifeExp[gapminder$year == i], 
                    na.rm = T)
    print(paste0(i, ": ", mean_le))
}

# Try the same thing for continents, find the one with highest mean life expectancy
continent=unique(gapminder$continent)
meanforcontinent=c()
for(i in continent){
    mean_tem=mean(gapminder$lifeExp[gapminder$continent==i],na.rm=T)
    print(paste0(i,": ",mean_tem))
}
# an alternative way
data=aggregate(gapminder$lifeExp~gapminder$continent,FUN=mean)
sort(data$`gapminder$continent`,decreasing = T)[1]

#What is the mean life expectancy for each continent for each year?
for (i in continent) {
    print(paste0("Continent: ", i))
    for (j in years) {
        mean_le <- mean(gapminder$lifeExp[gapminder$continent == i & 
                                              gapminder$year == j], 
                        na.rm = T)
        print(paste0(j, ": ", mean_le))
    }
}
#alternative
aggregate(gapminder$lifeExp~gapminder$continent+gapminder$year,FUN=mean)

#What is the standard deviation (sd) for life expectancy 
#for each continent for each year?
for (i in continent) {
    print(paste0("Continent: ", i))
    for (j in years) {
        sd_le <- sd(gapminder$lifeExp[gapminder$continent == i & 
                                          gapminder$year == j], 
                    na.rm = T)
        print(paste0(j, ": ", sd_le))
    }
}

#alternative
aggregate(gapminder$lifeExp~gapminder$continent+gapminder$year,FUN=sd)

#What is the standard deviation for life expectancy for 
#each year between 1987 and 2002 (inclusive)?
i=1987
while(i <=2002) {
    sd_le=sd(gapminder$lifeExp[gapminder$year==i])
    print(paste0(i,": ",sd_le))
    i=i+5
}

#Write a for loop that reports the mean population for years 
#greater than or equal to 1987
#paste will automatically add empty space between each element
for (j in years){
    if(j>=1987){
        tem=mean(gapminder$pop[gapminder$year==j])
        print(paste("mean population for year", j, " is ",tem ))
    }
    else{
        print("sorry, year is less than 1987")
    }
}

#Write a function that reports the mean, median, minimum, 
#and maximum for life expectancy for a continent in gapminder

basicCal=function(data,var,continent){
    temdata=data[,var][data$continent==continent]
    print(mean(temdata))
    print(median(temdata))
    print(max(temdata))
    print(min(temdata))
}
basicCal(gapminder,"lifeExp","Asia")

