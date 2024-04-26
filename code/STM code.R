#############################
##############################
#Summit vegetation State and Transition Model 
##############################
##############################
##############################

library(tidyverse)
library(dplyr)

##### Step 1) upload fire data #####
df1_l <- read.csv("data/Raw line intercept data.csv")

#### Step 2) Fix data #####

#Fix NA's
anyNA(df1_l$Lifeform) 
df1_l <- group_by(df1_l, Site, Aspect, Distance)

#make 2 new df with 2004 and 2022 data
DF_04_22 <- df1_l %>% 
  filter(Year %in%  c("2004", "2022")) #filters for 2004 and 2022
DF_04_22_2 <- DF_04_22[,-1] #removed the column
DF_04_22_3 <- spread(DF_04_22_2, Year, Lifeform) #spreads year and lifeform

#group Site, Aspect, Distance
DF_04_22_3 <- group_by(DF_04_22_3, Site, Aspect, Distance)
anyNA(DF_04_22_3) 
unique(DF_04_22_3$Site)

#Filter sites that werent surveyed in 2004
DF_04_22_4 <- DF_04_22_3 %>% 
  filter(!Site %in% c("F", "TT", "MH", "LSK", "MHW", "MF"))
unique(DF_04_22_4$Site) #there should be 8 sites
#Check for NAs
anyNA(DF_04_22_4)
#omit any NAs - 
#some of the transects in 2022 were slightly longer than the 2004 due to placement of peg
#This includes BF, H, MM, MSp, 
DF_04_22_4 = DF_04_22_4 %>% na.omit()

#relabel 
names(DF_04_22_4)[8] <- "X2004" #changing column name so its not a number
names(DF_04_22_4)[9] <- "X2022" #chanigng column name so its not a number


##### Step 3) Upload fire data #####
Fire_history <- read.csv("data/Fire data (all sites).csv") #fine scale fire history


#### Step 4) Fix fire data ####

#make row 1 your top row
colnames(Fire_history) <-  Fire_history[1,] 
#convert to long form
Transect_fire_L <- Fire_history %>% 
  group_by(Site, Aspect) %>% 
  gather(key = "Distance", value = "B/UB", 3:502) %>% 
  ungroup()
#change this column to an integer
Transect_fire_L$`B/UB` <- as.integer(Transect_fire_L$`B/UB`) 
glimpse(Transect_fire_L) #check its changed to an integer

unique(Transect_fire_L$Aspect)
unique(Transect_fire_L$Distance)
Transect_fire_L <-  Transect_fire_L[-1,] #remove the top row that is wrong

##change binary code to burnt and unburnt 
Transect_fire_L$`B/UB` <- as.character(Transect_fire_L$`B/UB`) #changes this column to character not numerical
glimpse(Transect_fire_L) #check its changed to an integer

Transect_fire_L_sel <- Transect_fire_L %>% 
  mutate(`B/UB` = ifelse(`B/UB` == 0, "Unburnt", `B/UB`)) %>% 
  mutate(`B/UB` = ifelse(`B/UB` == 1, "Burnt", `B/UB`)) %>% 
  group_by(Site, Aspect, Distance, `B/UB`)

unique(Transect_fire_L_sel$Site)

#### Step 5) Join datasets #####

#unite the lifeform and fire data and filter NAs
DF_04_22_4$Distance <- as.character(DF_04_22_4$Distance)
glimpse(DF_04_22_4)

#Join
df_complete_fire <- full_join(Transect_fire_L_sel, DF_04_22_4)

df_complete_fire = df_complete_fire %>% na.omit() #filter NAs
unique(df_complete_fire$`B/UB`) #checking data is all good
unique(df_complete_fire$Site) #make sure they joined properly
head(df_complete_fire)

#find any other NA
anyNA(df_complete_fire) 


#### Step 4) Calcuate transitions based of unburnt and burnt transects ####

df_complete_fire <- df_complete_fire %>% 
  ungroup()

#Group by site and transition to get number of those transitions at each site
df_s2 <- df_complete_fire %>% 
  unite("transition", 9:10, sep = "_") %>%
  group_by(`B/UB`, transition) %>% 
  summarise(n = n()) %>% 
  separate(transition, c("X2004","X2022"), remove = FALSE, sep = "_")

#summarise total number of transition
df_3 <- df_complete_fire %>% 
  group_by(`B/UB`, X2004) %>% 
  summarise(lftotal = n()) 

#convert to proportion 
df_j <- left_join(df_s2, df_3, by = c("B/UB", "X2004")) %>% 
  mutate(pct = n/lftotal*100)

view(df_j)

##### Step 5) Save output #####
write.csv(df_j, "output/STM data.csv", row.names = FALSE)






