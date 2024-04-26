################ CUMULATIVE GROWING DEGREE DAY ##########

library(cowplot)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tibble)
library(beepr)
library(tidyr)


#####
#Years:
#2000-2004
#2005-2012
#2013-2017
#2018-2022

####### 
######## Cumulative Growing Degree Days (CGDD)
######
# Step 1) Calcuate 2000-2004, 2005-2012, 2013-2017, 2018-2022 cumulative growing degree days
# Step 2) Add the 2000-2004 and 2005-2012 together - > this is 2012 CGDD
# Step 3) Add the 2012 and 2013-2017 together - > this is 2017 CGDD
# Step 4) Add the 2017 and 2018-2022 together - > this is 2022 CGDD



############# CUMALTIVE GROWING DEGREE DAYS (2004-2022) #####################
#Yearly sum of daily average degrees above 5 degrees (as per Korner & Hilburner)
#Growing degree days (GDD) are a measure of this heat accumulation.

# Step 1) Load data
Summit_climate <- read.csv("data/Summit environmental data (temp & prec).csv") 
Summit_avg <- Summit_climate[,c("Site", "Year", "Month", "Day", "max_temp", "min_temp")]

# Step 2) Select the growing season temps only
Summit_avg2 <- Summit_avg %>% 
  filter(Month %in% c(10:12,1:3))

# Step 3) Calcluate daily average temperature by getting the mid point between tempmax and tempmin
Summit_avgtemp <- Summit_avg %>%
  group_by(Site, Year, Month, Day) %>%
  mutate(avg_temp = (max_temp + min_temp)/2) %>% 
  ungroup()

# Step 4) Select for growing degree days (days averging above 5 degrees)
Summit_tempmax_GDD <- Summit_avgtemp %>% 
  filter(avg_temp > "0")

# Step 5) Get the yearly sum of daily average degrees of days >5 degrees
Summit_tempmax_GDD_2 <- Summit_tempmax_GDD %>% 
  group_by(Site, Year) %>% 
  summarise(cum.gdd = sum(avg_temp)) %>% 
  ungroup()

# Step 6) Get the sum cum.gdd between each survey period
Summit_tempmax_GDD_04 <- Summit_tempmax_GDD_2 %>% 
  filter(Year %in% c(2000:2004))
Summit_tempmax_GDD_12 <- Summit_tempmax_GDD_2 %>% 
  filter(Year %in% c(2005:2012))
Summit_tempmax_GDD_17 <- Summit_tempmax_GDD_2 %>%
  filter(Year %in% c(2013:2017))
Summit_tempmax_GDD_22 <- Summit_tempmax_GDD_2 %>%
  filter(Year %in% c(2018:2022))

# Step 7) Get the sum of cum.gdd for each survey period
Summit_tempmax_GDD_04 <- Summit_tempmax_GDD_04 %>% 
  group_by(Site) %>% 
  summarise(cum.gdd2 = sum(cum.gdd)) %>% 
  ungroup()
Summit_tempmax_GDD_12 <- Summit_tempmax_GDD_12 %>% 
  group_by(Site) %>% 
  summarise(cum.gdd2 = sum(cum.gdd)) %>% 
  ungroup()
Summit_tempmax_GDD_17 <- Summit_tempmax_GDD_17 %>% 
  group_by(Site) %>% 
  summarise(cum.gdd2 = sum(cum.gdd)) %>% 
  ungroup()
Summit_tempmax_GDD_22 <- Summit_tempmax_GDD_22 %>% 
  group_by(Site) %>% 
  summarise(cum.gdd2 = sum(cum.gdd)) %>% 
  ungroup()

# Step 8) Add the year to the dataset
Summit_tempmax_GDD_04$Year04 <- 2004
Summit_tempmax_GDD_12$Year12 <- 2012
Summit_tempmax_GDD_17$Year17 <- 2017
Summit_tempmax_GDD_22$Year22 <- 2022

# Step 9) Add the GDD values together to get cumulative GG over the survey period
GGD_2004 <- Summit_tempmax_GDD_04 %>% 
  select(Site, cum.gdd04 = cum.gdd2, Year04)

GGD_2012 <- left_join(Summit_tempmax_GDD_04, Summit_tempmax_GDD_12, by = "Site") %>% 
  mutate(cum.gdd12 = cum.gdd2.x + cum.gdd2.y) %>% 
  #remove columns not used
  select(-cum.gdd2.x, -cum.gdd2.y, -Year04)

GGD_2017 <- full_join(GGD_2012, Summit_tempmax_GDD_17, by = "Site") %>% 
  mutate(cum.gdd17 = cum.gdd12 + cum.gdd2) %>% 
  #remove columns not used
  select(-cum.gdd2, -cum.gdd12, -Year12)

GGD_2022 <- full_join(GGD_2017, Summit_tempmax_GDD_22, by = "Site") %>% 
  mutate(cum.gdd22 = cum.gdd17 + cum.gdd2) %>% 
  #remove columns not used
  select(-cum.gdd2, -cum.gdd17, -Year17)

# Step 10) change the Year columns name to "Year" and the cum.gdd to "cum.gdd"
GGD_2004 <- GGD_2004 %>% 
  rename(Year = Year04) %>% 
  rename(cum.gdd = cum.gdd04)

GGD_2012 <- GGD_2012 %>% 
  rename(Year = Year12) %>% 
  rename(cum.gdd = cum.gdd12)

GGD_2017 <- GGD_2017 %>% 
  rename(Year = Year17) %>% 
  rename(cum.gdd = cum.gdd17)

GGD_2022 <- GGD_2022 %>% 
  rename(Year = Year22) %>% 
  rename(cum.gdd = cum.gdd22)


# Step 11) Join dataset together 
summit_cum.gdd <- full_join(GGD_2004, GGD_2012)
summit_cum.gdd <- full_join(summit_cum.gdd, GGD_2017)
summit_cum.gdd <- full_join(summit_cum.gdd, GGD_2022)

# Step 12) Check data is good
view(summit_cum.gdd)
unique(summit_cum.gdd$Year)
unique(summit_cum.gdd$Site)
anyNA(summit_cum.gdd)
hist(summit_cum.gdd$cum.gdd, main = "Histogram of Cumulative Growing Degree Days", xlab = "Cumulative Growing Degree Days")

# Step 13) Write the dataset to a csv file
write.csv(summit_cum.gdd, "data/Summit CGDD.csv", row.names = FALSE)







##############
############### OLD CODE BELOW ############
################


### 2.Get the average cum.gdd for each site between each survey year
Summit_tempmax_GDD_2 <- Summit_tempmax_GDD_2 %>%
  group_by(Site) %>%
  summarise(mean_cumgdd = mean(cum.gdd)) %>%
  ungroup()

write.csv(Summit_tempmax_GDD_2, "data/Summit_GDD (avg cum dgg per site).csv", row.names = FALSE)






############
######### OLD CODE BELOW ##################
############

#### Elevation vs GDD
summit_GGD <- read.csv("data/Summit_GDD (avg cum dgg per site).csv")
elevation <- read.csv("data/Elevation.csv")

new <- left_join(summit_GGD, elevation, by = "Site")


ggplot(new, aes(x=ele, y=mean_cumgdd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  labs(x="Elevation (m)", y="Cumulative growing degree days (¬∞C)") +
  #ylim(0,NA) +
  theme_cowplot() +
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12)) 

#Save plot
ggsave("output/Elevation_vs_GDD_change2.png", width = 6, height = 4, dpi = 300)




















############ opld code below #################






#Calcuate the mean yearly sum of degrees between 2004-2011, 2012-2016 and 2017-2022 for all 14 sites
Summit_tempmax_GDD_means <- Summit_tempmax_GDD_2 %>%
  group_by(Site) %>%
  summarise(
    mean_2004 = mean(cum.gdd[Year == 2004]),
    mean_2004_2012 = mean(cum.gdd[Year >= 2004 & Year <= 2012]),
    mean_2012_2017 = mean(cum.gdd[Year >= 2012 & Year <= 2017]),
    mean_2017_2022 = mean(cum.gdd[Year >= 2017 & Year <= 2022]))




############# CUMALTIVE GROWING DEGREE DAYS (BASELINE) #####################
Baseline <- read.csv("data/All summit environmental data (baseline).csv")

#Relabel column "Day" as "Year", and "Year" as "Day"
Baseline <- rename(Baseline, Year = Day, Day = Year)

#Growing season temps only
Baseline2 <- filter(Baseline, Month %in% c(10:12,1:3))

##### Calcluate daily average temperature by getting the mid point between tempmax and tempmin
Baseline_avgtemp <- Baseline2 %>%
  group_by(Site, Year, Month, Day) %>%
  mutate(avg_temp = (max_temp + min_temp)/2) %>% 
  ungroup()

#Select for growing degree days (days averging above 5 degrees)
Baseline_GGD <- Baseline_avgtemp %>% 
  filter(avg_temp > "5")

#Get the yearly sum of daily average degrees of days >5 degrees
Baseline_GGD_2 <- Baseline_GGD %>% 
  group_by(Site, Year) %>% 
  summarise(cum.gdd = sum(avg_temp)) %>% 
  ungroup()

#Calcuate the mean yearly sum of daily average degrees between 2004-2011, 2012-2016 and 2017-2022 for all 14 sites
Baseline_GGD_means <- Baseline_GGD_2 %>%
  group_by(Site) %>%
  summarise(
    baseline_1960_1990 = mean(cum.gdd[Year >= 1960 & Year <= 1990]))



######### Calcuate the change in from baseline (1960-1990) #######
#Change from baseline
#Yearly sum of daily average degrees above 5 degrees
#Growing degree days (GDD) are a measure of this heat accumulation.

#Join the Baseline_GGD_means to Summit_tempmax_GDD_means using full_join by Site
Summit_GDD <- left_join(Baseline_GGD_means, Summit_tempmax_GDD_means, by = "Site")

# Calculate the change from baseline (1960-1990) for each time period
Summit_GDD_change <- Summit_GDD %>%
  group_by(Site) %>%
  mutate(
    change_2004 = mean_2004 - baseline_1960_1990,
    change_2012 = mean_2004_2012 - baseline_1960_1990,
    change_2017 = mean_2012_2017 - baseline_1960_1990,
    change_2022 = mean_2017_2022 - baseline_1960_1990)

#Select column for analaysis
Summit_GDD_change2 <- Summit_GDD_change[,c("Site", "change_2004", "change_2012", "change_2017", "change_2022")]

#Add in 2004-2012 as baseline
#Summit_GDD_change2$change_baseline <- "0"

#Gather data and make a column for change and the values
Summit_GDD_change_long <- Summit_GDD_change2 %>%
  group_by(Site) %>%
  gather(key = "period", value = "change", change_2004, change_2012, change_2017, change_2022)

#Remove the word "change" from the value column name
Summit_GDD_change_long2 <- Summit_GDD_change_long %>%
  separate(`period`, into = c("delete", "Year"), sep = "_")
#remove the "delete" column
Summit_GDD_change_long2 <- Summit_GDD_change_long2[,-2]
view(Summit_GDD_change_long2)

#Save as csv
write.csv(Summit_GDD_change_long2, "data/Summit_GDD3.csv", row.names = FALSE)


######### Calcuate the change in from baseline (2004) #######
#Change from baseline
#Yearly sum of daily average degrees above 5 degrees
#Growing degree days (GDD) are a measure of this heat accumulation.

#Join the Baseline_GGD_means to Summit_tempmax_GDD_means using full_join by Site
Summit_GDD <- left_join(Baseline_GGD_means, Summit_tempmax_GDD_means, by = "Site")

# Calculate the change from baseline (1960-1990) for each time period
Summit_GDD_change <- Summit_GDD %>%
  group_by(Site) %>%
  mutate(
    change_2004 = mean_2004 - mean_2004,
    change_2012 = mean_2004_2012 - mean_2004,
    change_2017 = mean_2012_2017 - mean_2004,
    change_2022 = mean_2017_2022 - mean_2004)

#Select column for analaysis
Summit_GDD_change2 <- Summit_GDD_change[,c("Site", "change_2004", "change_2012", "change_2017", "change_2022")]

#Add in 2004-2012 as baseline
#Summit_GDD_change2$change_baseline <- "0"

#Gather data and make a column for change and the values
Summit_GDD_change_long <- Summit_GDD_change2 %>%
  group_by(Site) %>%
  gather(key = "period", value = "change", change_2004, change_2012, change_2017, change_2022)

#Remove the word "change" from the value column name
Summit_GDD_change_long2 <- Summit_GDD_change_long %>%
  separate(`period`, into = c("delete", "Year"), sep = "_")
#remove the "delete" column
Summit_GDD_change_long2 <- Summit_GDD_change_long2[,-2]
view(Summit_GDD_change_long2)

#Save as csv
write.csv(Summit_GDD_change_long2, "data/Summit_GDD (baseline 2004).csv", row.names = FALSE)





######## PRODUCE A PLOT OF THE YEARLY CHANGE IN GGD #######

#Add baseline column
Summit_tempmax_GDD_3 <- left_join(Summit_tempmax_GDD_2, Baseline_GGD_means, by = "Site")

#Calcuate the yearly change in GDD from 2004 to 2022 from baseline 
Summit_tempmax_GDD_4 <- Summit_tempmax_GDD_3 %>%
  group_by(Site) %>%
  mutate(change = cum.gdd - baseline_1960_1990)

#Get the mean change per year
Summit_tempmax_GDD_5 <- Summit_tempmax_GDD_4 %>%
  group_by(Year) %>%
  summarise(mean_change = mean(change), sd_change = sd(change))

#Create a new column for la nina and el nino data
#Data retireved from BOM - http://www.bom.gov.au/climate/history/enso/
nina <- data.frame(
  Year = c(2004: 2022),
  Climate = c("Neutral", "Neutral", "El Nino", "Neutral", "La Nina", "Neutral", "Neutral", 
              "La Nina", "La Nina", "Neutral", "Neutral", "El Nino", "El Nino", "Neutral", "Neutral", "Neutral","La Nina", "La Nina", "La Nina")
)

#Join data
Summit_tempmax_GDD_5 <- left_join(Summit_tempmax_GDD_5, nina, by = "Year")

###Bar graph of the mean cumulative changes in gdd over time with error bars
ggplot(Summit_tempmax_GDD_5, aes(x=Year, y=mean_change, group = Climate)) +
  geom_bar(stat="identity", aes(fill = Climate)) +
  geom_errorbar(aes(ymin=mean_change-sd_change, ymax=mean_change+sd_change), width=.2, colour="black") +
  labs(x="Year", y="Annual mean change in cumulative growing degrees (¬∞C)") +
  ylim(0,NA) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("darkred", "darkblue", "grey")) +
  theme_cowplot()

#Save plot
ggsave("output/Summit_GDD_change.png", width = 10, height = 6, dpi = 300)






