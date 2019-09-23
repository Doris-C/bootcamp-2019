library(tidyverse)
source(here::here("data/day3_objects.R"))
ggplot(data=gapminder07)+geom_point(mapping = aes(x=gdpPercap,y=lifeExp))

#Exercise1
ggplot(data=gapminder07)+geom_point(mapping = aes(x=log(pop),y=log(gdpPercap)))+
    labs(title="Relationship between GDP per capita and population in 2007",
         x="Logged GDP per capita", y="Logged life expectancy")

#Exercise2
long_gen%>%
    filter(source=="large_hydro"|source=="small_hydro")%>%
    group_by(datetime)%>%
    summarise(output=sum(output))%>%
    ggplot() + geom_col(mapping = aes(x=datetime,y=output))+
    labs(title="Total hydro power",x="Hour",y="Outpus")

#Exercise3
long_merged_energy%>%
    group_by(source)%>%
    summarise(output=sum(output))%>%
    ggplot()+
    geom_col(aes(x=source,y=output),fill="darkred")+
    geom_abline(aes(intercept = mean(output),slope=0))

#Exercise4
long_merged_energy %>%
    filter(source=="wind"|source=="solar"|source=="geothermal")%>%
    ggplot()+
    geom_line(aes(x=datetime,y=output,group=source,col=source),size=1.5)




