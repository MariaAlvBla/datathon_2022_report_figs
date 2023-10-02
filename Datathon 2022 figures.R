#Required libraries----

library(stringr)
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
require(readxl)

#Set working directory
setwd("name of working directory")

#Figure 1a----

#Read the data file
Output<-read.table("Output.txt", sep="\t", header=TRUE, fill=TRUE)
Output$WB.Classification=as.factor(Output$WB.Classification)

#Set the WB$Classification levels
levels(Output$WB.Classification)=c("Low income", "Lower-middle income" ,"Upper-middle income", "High income")

#Create the plot
A=ggplot(Output, aes(x=WB.Classification, color=RD.investments, y=Percentage))+
  geom_point(size=4, alpha=0.7)+
  theme_classic()+
  scale_color_viridis()+
  ylab("% of ecological meta analysis (p/author, p/country)")+
  theme(axis.title.y =element_blank())+
  coord_flip()+
  guides(color=guide_legend(title="% GERD of GDP"))

#Read the data file
EMP_coverage_by_WB_income<-read.table("EMP_coverage_by_WB_income.txt", sep="\t", header=TRUE, fill=TRUE)

#Set the world map
world=map_data("world")

#Create frequency table of the EMP_coverage_by_WB_income per region and income
Counts=EMP_coverage_by_WB_income %>%  
  group_by(region, income) %>%    
  tally()

countries=inner_join(world, Counts, by = "region")
countries$income=as.factor(countries$income)

#Create frequency table of countries per region and income
Counts=countries %>%  
  group_by(region, income) %>%        
  summarize(income=first(income))%>%
  tally()

#Set the level names of the income column
countries$income <- factor(countries$income, levels = c("High income", "Upper-middle income", "Lower-middle income"))

#Create the plot
B=ggplot(data = world) +
  geom_polygon(data = world, fill = "gray", mapping = aes(x = long, y = lat, group=group))+
  geom_polygon(data = countries, aes(x = long, y = lat, group=group, fill=income), alpha=0.3)+
  geom_point(data = EMP_coverage_by_WB_income, aes(x = Longitude, y = Latitude))+
  theme_minimal()+
  scale_fill_viridis(discrete=TRUE)

#Present figures A and B alongside
cowplot::plot_grid(A, B, ncol=2)

#Figure 2b----

#information
#https://www.geeksforgeeks.org/how-to-make-world-map-with-ggplot2-in-r/
#https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/


#Read data file
for_map=read_excel("Dataset_Datathon_FINAL.xlsx", sheet = "Dataset_Datathon")
colnames(for_map)

#Create freq table of BioProject rows (BioSamples) per site
freq_coord_bioproj <- for_map %>% 
  group_by(BioProject,latitude,longitude, geo_loc_name_country, env_broad_scale) %>%   # grouping, drag country names into this, you will use inner join on country names later on
  tally()

freq_coord_bioproj<-freq_coord_bioproj[-c(1:2),]#erase the first two rows that have inconclusive information

#Set the column names
colnames(freq_coord_bioproj)
colnames(freq_coord_bioproj)= c("BioProject", "lat", "long", "region", "Realm", "Samples")

#Specify the columns to be used as latitude and longitud
freq_coord_bioproj$lat=as.numeric(freq_coord_bioproj$lat)
freq_coord_bioproj$long=as.numeric(freq_coord_bioproj$long)

#Set the world map
world_map <- map_data("world")

#Set map with only Argentina and Uruguay
datathon_countries = c("Uruguay","Argentina","Antarctica")

#Set map with other South American countries
SouthAmerica=c("Brazil", "Chile", "Paraguay", "Bolivia")


# Retrieve the map with the South American countries
datathon_countries <- map_data("world", region = datathon_countries)
SouthAmerica=map_data("world", region= SouthAmerica)

#Plot the BioSample's coordinates on the created maps
ggplot() +  
  geom_polygon(data = datathon_countries, fill ="gray", mapping = aes(x = long, y = lat, group=group))+
  geom_polygon(data = SouthAmerica, fill = "lightgray", mapping = aes(x = long, y = lat, group=group))+
  geom_point(data = freq_coord_bioproj, aes(x = long, y = lat,  size=Samples, color=Realm, alpha=0.7))+
  coord_map(projection = "mercator") +
  lims(x = c(-80, -40), y = c(-65, -20))+
  theme_minimal()+
  scale_color_viridis(discrete=TRUE)

# Compute the centroid as the mean longitude and latitude
# Used as label coordinate for country's names
datathon_countries = datathon_countries %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

countries=inner_join(world, Counts, by = "region")

counts<-for_map %>%
  group_by(latitude,longitude,BioProject) %>%
  tally()

