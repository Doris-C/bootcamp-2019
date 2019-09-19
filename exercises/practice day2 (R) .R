library(here)
generation=read.csv(here::here("data/ca_energy_generation.csv"),stringsAsFactors = F)
imports=read.csv(here::here("data/ca_energy_imports.csv"),stringsAsFactors = F)
str(generation)
install.packages("lubridate")
library(lubridate)
generation$datetime=as_datetime(generation$datetime)
imports$datetime=as_datetime(imports$datetime)


library(reshape2)
long_gen=melt(generation,id.vars="datetime",variable.name="source",
              value.name = "usage")

merged_energy=merge(generation,imports,by="datetime")
dim(merged_energy)
long_merged_energy=melt(merged_energy,id.vars="datetime",variable.name = "source",
                        value.name = "usage")

# data manipulation
library(dplyr)
library(data.table) #efficient and fast for large data
library(tidyverse)  #for dplyr

select(generation,datetime)
select(generation,datetime:biomass)



#Practice
merged_energy%>%
    select(contains("hydro"))%>%
    mutate(total_hydro=large_hydro+small_hydro)%>%
    summarize(mean_hydro=mean(total_hydro,na.rm=T))

long_merged_energy%>%
    filter(source %in% c("large_hydro","small_hydro","biogas","biomass"))%>%
    group_by(source)%>%
    summarize(mean_usage=mean(usage,na.rm=T))

merged_energy%>%
    select(small_hydro,large_hydro,biogas,biomass)%>%
    summarize(meansamllh=mean(small_hydro),meanlargeh=mean(large_hydro),
              meanbiogas=mean(biogas),meanbiomass=mean(biomass))

merged_energy %>% 
    select(datetime, contains("hydro"), contains("bio")) %>% 
    melt(id.vars = "datetime",
         variable.name = "source",
         value.name = "usage") %>% 
    group_by(source) %>% 
    summarize(mean_usage = mean(usage, na.rm = T))




#### data table
library(data.table)
data_file=here::here("data","ca_energy_generation.csv")
generation_df=read.csv(data_file,stringsAsFactors = F) #datafile
generation_dt=fread(data_file) #datatable

view(generation_df)
view(generation_dt)
generation_df
generation_dt
str(generation_df)
str(generation_dt)


generation_dt[wind>4400&mday(datetime)==7]

#Exercise
generation_dt[natural_gas<=5000&large_hydro>2000]
generation_dt[coal>10&solar>median(solar)]

generation_dt[,newcol := 3*wind + solar*biogas/2]
generation_dt[,.(newcol = 3*wind + solar*biogas/2)]

#Exercise
generation_dt[,total_hydro:=small_hydro+large_hydro]
generation_dt[,.(nuclear_m=mean(nuclear),biogas_m=mean(biogas))]

generation_dt[solar==0,.(datetime,total_thermal=natural_gas+coal)]

#Exercise
generation_dt[,.(median_solargeneration=median(solar)),by=hour(datetime)]
generation_dt[solar>0,.(maxgasgeneration=max(natural_gas)),by=day(datetime)]

#Exercise













