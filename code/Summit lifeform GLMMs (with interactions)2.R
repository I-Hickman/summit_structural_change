##################################################
# CHANGES IN LIFEFORM COVER ON ALPINE SUMMITS OVER TIME 
# IN RELATION TO ENVIRONMENTAL FACTORS
##################################################

library(Rcpp)
library(brms)
library(bayesplot)
library(cowplot)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tibble)
library(beepr)
library(bayestestR)
library(GGally)


####### Step 1: Read in csv file #####
Lifeform <- read.csv("data/Lifeform intercept data.csv")
anyNA(Lifeform)
GDD <- read.csv("data/Summit CGDD.csv")

#Join Growing Degree Days and Precip data
Lifeform <- left_join(Lifeform, GDD, by = c("Site", "Year"), relationship = "many-to-many")

#Change year to a factor
Lifeform$Year.F = as.factor(Lifeform$Year)

#Explore the data
hist(Lifeform$count)
hist(Lifeform$cum.gdd2)
hist(Lifeform$Time_since_burnt)
hist(Lifeform$ele)


#Separate into lifeforms for analysis
Shrubs2 <- Lifeform %>% 
  filter(Lifeform == "shrub")
Forbs2 <- Lifeform %>% 
  filter(Lifeform == "herb")
Graminoid2 <- Lifeform %>% 
  filter(Lifeform == "grass")



#########################################
####### FORBS #####

glimpse(Forbs2)

### Step 1: Check for correlation between variables (<0.7) #######
Forbs2 %>%
  dplyr::select(-c(Site, Year, Year.F, Plot.size..m2., Aspect, SR.w.m2, 
  Lifeform, count, total, Cover, lat, lon, Grazing.ceased)) %>%
  GGally::ggpairs() 

### Step 2: Standardise variables ######
Forbs2$Year.F= as.factor(Forbs2$Year)
Forbs2$TSF.std= as.vector(scale(Forbs2$Time_since_burnt)) 
Forbs2$gdd.std = as.vector(scale(Forbs2$cum.gdd))
Forbs2$ele.std = as.vector(scale(Forbs2$ele))


### Step 3: Create model ####
mForbGLM <- brms::brm(count | trials(total) ~ 
                       TSF.std*gdd.std + ele.std +
                       (1 | Site) + (1|Year.F),
                       family = binomial("logit"),
                       iter = 2000,  
                       future = TRUE,
                       cores = 4,
                       chains = 4,
                       control = list(adapt_delta = 0.999, max_treedepth=15), 
                       data = Forbs2)

### Step 5: Validate model ####
#Check model
summary(mForbGLM) #summary stats - look at Rhat and make sure <1.05
plot(mForbGLM) #check Convergence of the model by looking at the Markov chains
pp_check(mForbGLM) #check model predictions to observations
bayes_R2(mForbGLM) # check the R2

### Step 6: Fixed effects coefficient plot ####
mForb2_fixed <- as.data.frame(fixef(mForbGLM))
mForb2_fixed2 <- rownames_to_column(mForb2_fixed, var = "Variable")
mForb2_fixed2$Variable <- factor(mForb2_fixed2$Variable,
                               levels = c("TSF.std:gdd.std",
                                          "gdd.std", "TSF.std",
                                          "ele.std", "Intercept"))

custom_labels <- c( "ele.std" = "Elevation",
                    "gdd.std" = "CGD",
                    "TSF.std:gdd.std" = "TSF:CGD",
                   "TSF.std" = "TSF",
                   "Intercept" = "Intercept")

#Get the 80% credible intervals from model
ci_eti_fixed_89 <- ci(mForbGLM, method = "ETI", ci = 0.89, effects = "fixed")
ci_eti_fixed_95 <- ci(mForbGLM, method = "ETI", ci = 0.95, effects = "fixed")

#Rename CI low and CI high columns 
ci_eti_fixed_89 <- ci_eti_fixed_89 %>%
  rename(Q5.5 = CI_low, Q94.5 = CI_high) #Calcuates the 89% CI
ci_eti_fixed_95 <- ci_eti_fixed_95 %>%
  rename(Q2.5 = CI_low, Q97.5 = CI_high) #Calculates the 95% CI

#join the 80% and 95% credible intervals
Forb_fixed_ef <- left_join(ci_eti_fixed_89, ci_eti_fixed_95, by = "Parameter")

# add in estimate column 
Forb_fixed_ef$Estimate <- mForb2_fixed2$Estimate
Forb_fixed_ef$Variable <- mForb2_fixed2$Variable


#Plot
Forb2_fixed <- ggplot(Forb_fixed_ef, aes(x = Estimate, y = Variable)) +
  geom_point(shape = 16, size = 2.5) +         # Point for the estimate
  geom_errorbarh(aes(xmin = Q5.5, xmax = Q94.5, height = 0), linewidth = 0.7) +  # 89% Confidence intervals)
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5, height = 0), linewidth = 0.3) +  # 95% Confidence intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    # Add a dashed line at x = 0
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Coefficients")

Forb2_fixed <- Forb2_fixed +
  annotate("text", x = Inf, y = Inf, label = "(a)", vjust = 1, hjust = 1, size = 6)
  
Forb2_fixed

### Step 7: Random effects ####

# 1. Plot estimates of the standard deviation and CI of random effects 
#to see which one explains the most in the unexplained variation 
summary(mForbGLM) #pull out the estimate and CI's from the model summary and create a new DF
forb_ran_DF <- data.frame(
  variable = c("Site", "Year"),
  estimate = c(0.85, 1.00),
  lci = c(0.56, 0.05),
  uci = c(1.32, 3.05)
)

Forb_rand_all <- ggplot(forb_ran_DF, aes(x = estimate, y = variable)) +
  geom_point(shape = 16, size = 2.5) +        
  geom_errorbarh(aes(xmin = lci, xmax = uci, height = 0), linewidth = 0.3) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Estimated standard deviation")

Forb_rand_all <- Forb_rand_all +
annotate("text", x = Inf, y = Inf, label = "(b)", vjust = 1, hjust = 1, size = 6)

ggsave("output/Forb/Forb random effects plot.png", 
       plot = Forb_rand_all, width = 7, height = 3.5) 

#### Random effects: site and year (appendix)
#SITE
Forb_rand <- as.data.frame(ranef(mForbGLM, groups = "Site"))
Forb_rand <- rownames_to_column(Forb_rand, var = "Variable")
Forb_rand <- Forb_rand[order(Forb_rand$Site.Estimate.Intercept), ]

Forb_rand_site <- ggplot(Forb_rand, aes(x = Site.Estimate.Intercept, y = reorder(Variable, Site.Estimate.Intercept))) +
  geom_point(shape = 16, size = 2.5) +        
  geom_errorbarh(aes(xmin = Site.Q2.5.Intercept, xmax = Site.Q97.5.Intercept, height = 0)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Estimated standard deviation")


#YEAR
Forb_rand2 <- as.data.frame(ranef(mForbGLM, groups = "Year.F"))
Forb_rand2 <- rownames_to_column(Forb_rand2, var = "Variable")
Forb_rand2 <- Forb_rand2[order(Forb_rand2$Year.F.Estimate.Intercept), ]

Forb_rand_year <- ggplot(Forb_rand2, aes(x = Year.F.Estimate.Intercept, y = reorder(Variable, Year.F.Estimate.Intercept))) +
  geom_point(shape = 16, size = 2.5) +        
  geom_errorbarh(aes(xmin = Year.F.Q2.5.Intercept, xmax = Year.F.Q97.5.Intercept, height = 0)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Estimated standard deviation")


#Save both plots
plot <- grid.arrange(Forb_rand_site, Forb_rand_year,
                                         ncol = 2)
ggsave("output/Forb/Forb model output (random effects).png", 
       plot = plot, width = 10, height = 3.5) 



### Step 8: Plot marginal predictions ####

#### Step 8.1: Time since fire X CGD ####

#Fire values - high and low
mean(Forbs2$Time_since_burnt) #28.29064
sd(Forbs2$Time_since_burnt) #28.25379
range(Forbs2$Time_since_burnt) #4 - 83
##LOW fire frequency (at 83 years unburnt - transform (83 -mean)/SD = 1.936355)
(83 - 28.29064)/28.25379 
#HIGH fire frequency (at 4 years unburnt - transform (4 -mean)/SD = -0.8597303)
(4 -28.29064)/28.25379 

#CGD values
mean(Forbs2$cum.gdd) #46967.4
sd(Forbs2$cum.gdd) #19446.54
range(Forbs2$cum.gdd) #13384.1 77193.0

### LOW FIRE PREDICTIONS = 83 years##
#Create new dataframe
newdat_forb_F_CGD_Low <- data.frame(Site = "average site",
                           Year.F = "average year",
                           cum.gdd = seq(13300, 77200, by = 100), 
                           TSF.std = 1.936355, #Predicting at low fire freq
                           ele.std = 0,
                           total = 1) %>% 
  dplyr::mutate(gdd.std = (cum.gdd - 46967.4)/19446.54)
#Predictions
predictions<- posterior_epred(mForbGLM, newdat= newdat_forb_F_CGD_Low, re_formula = NULL, allow_new_levels = TRUE)
#Mean
mean <- apply(predictions, 2, mean)
#Confidence limits from predictions (the 95th credible intervals)
lci <- apply(predictions, 2, quantile, 0.025)
uci <- apply(predictions, 2, quantile, 0.975)
#Combine 
newdat_forb_F_CGD_Low$mean <- mean
newdat_forb_F_CGD_Low$lci <- lci
newdat_forb_F_CGD_Low$uci <- uci


#### HIGH FIRE FREG PREDICTIONS = 4 years ##
newdat_forb_F_CGD_High <- data.frame(Site = "average site",
                                    Year.F = "average year",
                                    cum.gdd = seq(13300, 77200, by = 100), 
                                    TSF.std = -0.8597303, #Predicting at high fire freq
                                    ele.std = 0,
                                    total = 1) %>% 
  dplyr::mutate(gdd.std = (cum.gdd - 46967.4)/19446.54)
#Predictions
predictions<- posterior_epred(mForbGLM, newdat= newdat_forb_F_CGD_High, re_formula = NULL, allow_new_levels = TRUE)
#Mean
mean <- apply(predictions, 2, mean)
#Confidence limits from predictions (the 95th credible intervals)
lci <- apply(predictions, 2, quantile, 0.025)
uci <- apply(predictions, 2, quantile, 0.975)
#Combine 
newdat_forb_F_CGD_High$mean <- mean
newdat_forb_F_CGD_High$lci <- lci
newdat_forb_F_CGD_High$uci <- uci


##Plot predictions
Forb2_plot_Fire_CGD <- ggplot(newdat_forb_F_CGD_High, aes(x = cum.gdd, y = mean)) + 
  geom_path(color = "red") +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2, fill = "red") +
  geom_path(data = newdat_forb_F_CGD_Low, aes(x = cum.gdd, y = mean), color = "blue") +
  geom_ribbon(data = newdat_forb_F_CGD_Low, aes(ymin = lci, ymax = uci), alpha = 0.2, fill = "blue") +
  #Put in data points 
  #geom_point(data = Forbs2, aes(x = Time_since_burnt, y = Cover), 
             #shape = 21, fill = "lightgrey", alpha = 0.2, colour = "black")+
  theme_cowplot()+
  ylim(0,1) +
  ylab(expression("Forb proportional cover"))+
  xlab(expression("Cumulative summer growing degrees (°C)")) +
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12)) 

Forb2_plot_Fire_CGD <- Forb2_plot_Fire_CGD +
  annotate("text", x = Inf, y = Inf, label = "(c)", vjust = 1, hjust = 1, size = 6)

Forb2_plot_Fire_CGD



#### Step 8.2: Elevation ####

mean(Forbs2$ele) #1752.115
sd(Forbs2$ele) #96.6135
range(Forbs2$ele) #1574.000 1982.326
#Create new dataframe
newdat_forb4 <- data.frame(Site = "average site",
                           Year.F = "average year",
                           ele = seq(1500, 2000, by = 10), 
                           gdd.std = 0,
                           TSF.std = 0, 
                           total = 1) %>% 
  dplyr::mutate(ele.std = (ele - 1752.115)/96.6135)
#Predictions
predictions<- posterior_epred(mForbGLM, newdat= newdat_forb4, allow_new_levels = TRUE)
#Mean
mean <- apply(predictions, 2, mean)
#Confidence limits from predictions (the 95th credible intervals)
lci <- apply(predictions, 2, quantile, 0.025)
uci <- apply(predictions, 2, quantile, 0.975)
#Combine 
newdat_forb4$mean <- mean
newdat_forb4$lci <- lci
newdat_forb4$uci <- uci


##Plot predictions
Forb2_plot_ele <- ggplot(newdat_forb4, aes(x = ele, y = mean)) + 
  geom_path() +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2) +
  #geom_point(data = Forbs2, aes(x = ele, y = Cover), 
             #shape = 21, fill = "lightgrey", alpha = 0.2, colour = "black")+
  #geom_line(size = 0.75) +
  theme_cowplot()+
  ylim(0,1) +
  ylab(expression(" "))+
  xlab(expression("Elevation (m)")) +
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12)) 

Forb2_plot_ele <- Forb2_plot_ele+
annotate("text", x = Inf, y = Inf, label = "(d)", vjust = 1, hjust = 1, size = 6)

Forb2_plot_ele


#### Step 8.3: Plot all together #####
plot_forb2 <- grid.arrange(Forb2_fixed, Forb_rand_all,
                           Forb2_plot_Fire_CGD,
                           Forb2_plot_ele,
                           ncol = 2)
#Save
ggsave("output/Forb/Forb GLM.png", 
       plot = plot_forb2, width = 11, height = 9)




#########################################
## SHRUBS #####
###

glimpse(Shrubs2)

### Step 1: Check for correlation between variables (<0.7) #######
Shrubs2 %>%
  dplyr::select(-c(Site,  Year, Year.F, Plot.size..m2.,
                   Aspect, SR.w.m2, Lifeform, count, total, Cover, 
                   lat, lon, Grazing.ceased)) %>%
  GGally::ggpairs()

### Step 2: Standardise variables ######
Shrubs2$Year.F= as.factor(Shrubs2$Year)
Shrubs2$TSF.std= as.vector(scale(Shrubs2$Time_since_burnt)) 
Shrubs2$gdd.std = as.vector(scale(Shrubs2$cum.gdd))
Shrubs2$ele.std = as.vector(scale(Shrubs2$ele))


### Step 3: Create model ####
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE, threads_per_chain = 1)

mShrubGLM <- brms::brm(count | trials(total) ~ 
                      TSF.std * gdd.std + ele.std + 
                      ( 1 | Site) + (1|Year.F),
                      family = binomial("logit"),
                      future = TRUE,
                      iter = 2000, 
                      cores = 4,
                      chains = 4,
                      control = list(adapt_delta = 0.99, max_treedepth=15), 
                      data = Shrubs2)

### Step 5: Validate model ####
#Check model
summary(mShrubGLM) 
plot(mShrubGLM) 
pp_check(mShrubGLM)
bayes_R2(mShrubGLM)


### Step 6: Fixed effects coefficient plot ####
mShrub2_fixed <- as.data.frame(fixef(mShrubGLM))
mShrub2_fixed <- rownames_to_column(mShrub2_fixed, var = "Variable")
mShrub2_fixed$Variable <- factor(mShrub2_fixed$Variable,
                                 levels = c("TSF.std:gdd.std",
                                            "gdd.std", "TSF.std",
                                            "ele.std", "Intercept"))

#Get the 80% credible intervals from model
ci_eti_fixed_89 <- ci(mShrubGLM, method = "ETI", ci = 0.89, effects = "fixed")
ci_eti_fixed_95 <- ci(mShrubGLM, method = "ETI", ci = 0.95, effects = "fixed")

#Rename CI low and CI high columns 
ci_eti_fixed_89 <- ci_eti_fixed_89 %>%
  rename(Q5.5 = CI_low, Q94.5 = CI_high)
ci_eti_fixed_95 <- ci_eti_fixed_95 %>%
  rename(Q2.5 = CI_low, Q97.5 = CI_high)

#join the 80% and 95% credible intervals
Shrub_fixed_ef <- left_join(ci_eti_fixed_89, ci_eti_fixed_95, by = "Parameter")

# add in estimate column 
Shrub_fixed_ef$Estimate <- mShrub2_fixed$Estimate
Shrub_fixed_ef$Variable <- mShrub2_fixed$Variable
Shrub_fixed_ef$Q5.5

#Plot
Shrub2_fixed <- ggplot(Shrub_fixed_ef, aes(x = Estimate, y = Variable)) +
  geom_point(shape = 16, size = 2.5) +         # Point for the estimate
  geom_errorbarh(aes(xmin = Q5.5, xmax = Q94.5, height = 0), linewidth = 0.7) +  # 89% Confidence intervals)
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5, height = 0), linewidth = 0.3) +  # 95% Confidence intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    # Add a dashed line at x = 0
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Coefficients")

Shrub2_fixed <- Shrub2_fixed+
  annotate("text", x = Inf, y = Inf, label = "(a)", vjust = 1, hjust = 1, size = 6)

### Step 7: Random effects ####

# 1. Plot estimates of the standard deviation and CI of random effects to see which one explains the most in the unexplained variation 
summary(mShrubGLM) #pull out the estimate and CI's from the model summary and create a new DF
Shrub_ran_DF <- data.frame(
  variable = c("Site", "Year"),
  estimate = c(0.80, 0.55),
  lci = c(0.53, 0.13),
  uci = c(1.27, 1.76 )
)

Shrub_rand_all <- ggplot(Shrub_ran_DF, aes(x = estimate, y = variable)) +
  geom_point(shape = 16, size = 2.5) +        
  geom_errorbarh(aes(xmin = lci, xmax = uci, height = 0), size = 0.3) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Estimated standard deviation")

Shrub_rand_all <- Shrub_rand_all +
  annotate("text", x = Inf, y = Inf, label = "(b)", vjust = 1, hjust = 1, size = 6)

ggsave("output/Shrub/Shrub random effects plot.png", 
       plot = Shrub_rand_all, width = 7, height = 3.5) 

#### Random effects: site and year (appendix)
#SITE
Shrub_rand_site <- as.data.frame(ranef(mShrubGLM, groups = "Site"))
Shrub_rand_site <- rownames_to_column(Shrub_rand_site, var = "Variable")
Shrub_rand_site <- Shrub_rand_site[order(Shrub_rand_site$Site.Estimate.Intercept), ]

Shrub_rand_site <- ggplot(Shrub_rand_site, aes(x = Site.Estimate.Intercept, y = reorder(Variable, Site.Estimate.Intercept))) +
  geom_point(shape = 16, size = 2.5) +        
  geom_errorbarh(aes(xmin = Site.Q2.5.Intercept, xmax = Site.Q97.5.Intercept, height = 0)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Estimated standard deviation")


#YEAR
Shrub_rand2 <- as.data.frame(ranef(mShrubGLM, groups = "Year.F"))
Shrub_rand2 <- rownames_to_column(Shrub_rand2, var = "Variable")
Shrub_rand2 <- Shrub_rand2[order(Shrub_rand2$Year.F.Estimate.Intercept), ]

Shrub_rand_year <- ggplot(Shrub_rand2, aes(x = Year.F.Estimate.Intercept, y = reorder(Variable, Year.F.Estimate.Intercept))) +
  geom_point(shape = 16, size = 2.5) +        
  geom_errorbarh(aes(xmin = Year.F.Q2.5.Intercept, xmax = Year.F.Q97.5.Intercept, height = 0)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Estimated standard deviation")


#Save both plots
plot <- grid.arrange(Shrub_rand_site, Shrub_rand_year,
                     ncol = 2)

ggsave("output/Shrub/Shrub model output (random effects).png", 
       plot = plot, width = 10, height = 3.5) 


### Step 8: Plot marginal predictions ####

#### Step 8.1: Time since fire X CGD ####
mean(Shrubs2$Time_since_burnt) #46967.4
sd(Shrubs2$Time_since_burnt)
#Fire values - high and low
##LOW fire frequency (at 83 years unburnt - transform (83 -mean)/SD = 1.890691)
(83 - 29.33)/28.38644 
#HIGH fire frequency (at 4 years unburnt - transform (4 -mean)/SD = -0.8923275)
(4 -29.33)/28.38644 

#CGD values
mean(Shrubs2$cum.gdd) #46967.4
sd(Shrubs2$cum.gdd) #19446.54
range(Shrubs2$cum.gdd) #13384.1 77193.0

### LOW FIRE PREDICTIONS = 83 years##
#Create new dataframe
newdat_shrub_F_CGD_Low <- data.frame(Site = "average site",
                                     Year.F = "average year",
                                     cum.gdd = seq(13300, 77200, by = 100), 
                                     TSF.std = 1.890691, #Predicting at low fire freq
                                     ele.std = 0,
                                     total = 1) %>% 
  dplyr::mutate(gdd.std = (cum.gdd - 46967.4)/19446.54)
#Predictions
predictions<- posterior_epred(mShrubGLM, newdat= newdat_shrub_F_CGD_Low, allow_new_levels = TRUE)
#Mean
mean <- apply(predictions, 2, mean)
#Confidence limits from predictions (the 95th credible intervals)
lci <- apply(predictions, 2, quantile, 0.025)
uci <- apply(predictions, 2, quantile, 0.975)
#Combine 
newdat_shrub_F_CGD_Low$mean <- mean
newdat_shrub_F_CGD_Low$lci <- lci
newdat_shrub_F_CGD_Low$uci <- uci


#### HIGH FIRE FREG PREDICTIONS = 4 years ##
newdat_shrub_F_CGD_High <- data.frame(Site = "average site",
                                      Year.F = "average year",
                                      cum.gdd = seq(13300, 77200, by = 100), 
                                      TSF.std = -0.8923275, #Predicting at high fire freq
                                      ele.std = 0,
                                      total = 1) %>% 
  dplyr::mutate(gdd.std = (cum.gdd - 46967.4)/19446.54)
#Predictions
predictions<- posterior_epred(mShrubGLM, newdat= newdat_shrub_F_CGD_High, allow_new_levels = TRUE)
#Mean
mean <- apply(predictions, 2, mean)
#Confidence limits from predictions (the 95th credible intervals)
lci <- apply(predictions, 2, quantile, 0.025)
uci <- apply(predictions, 2, quantile, 0.975)
#Combine 
newdat_shrub_F_CGD_High$mean <- mean
newdat_shrub_F_CGD_High$lci <- lci
newdat_shrub_F_CGD_High$uci <- uci


##Plot predictions
Shrub2_plot_Fire_CGD <- ggplot(newdat_shrub_F_CGD_High, aes(x = cum.gdd, y = mean)) + 
  geom_path(color = "red") +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2, fill = "red") +
  geom_path(data = newdat_shrub_F_CGD_Low, aes(x = cum.gdd, y = mean), color = "blue") +
  geom_ribbon(data = newdat_shrub_F_CGD_Low, aes(ymin = lci, ymax = uci), alpha = 0.2, fill = "blue") +
  #Put data points 
  #geom_point(data = Shrubs2, aes(x = cum.gdd, y = Cover), 
  #shape = 21, fill = "lightgrey", alpha = 0.2, colour = "black")+
  theme_cowplot()+
  ylim(0,1) +
  ylab(expression("Shrub proportional cover"))+
  xlab(expression("Cumulative summer growing degrees (°C)")) +
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12)) 

Shrub2_plot_Fire_CGD <- Shrub2_plot_Fire_CGD +
  annotate("text", x = Inf, y = Inf, label = "(c)", vjust = 1, hjust = 1, size = 6)


#### Step 8.2: Elevation ####

mean(Shrubs2$ele) #1749.595
sd(Shrubs2$ele) #97.67031
range(Shrubs2$ele) # 1574.000 1982.326
#Create new dataframe
newdat_Shrub4 <- data.frame(Site = "average site",
                            Year.F = "average year",
                            ele = seq(1500, 2000, by = 10), 
                            TSF.std = 0, 
                            gdd.std = 0,
                            total = 1) %>% 
  dplyr::mutate(ele.std = (ele - 1749.595)/97.67031)
#Predictions
predictions<- posterior_epred(mShrubGLM, newdat= newdat_Shrub4, allow_new_levels = TRUE)
#Mean
mean <- apply(predictions, 2, mean)
#Confidence limits from predictions (the 95th credible intervals)
lci <- apply(predictions, 2, quantile, 0.025)
uci <- apply(predictions, 2, quantile, 0.975)
#Combine 
newdat_Shrub4$mean <- mean
newdat_Shrub4$lci <- lci
newdat_Shrub4$uci <- uci


##Plot predictions
Shrub2_plot_ele <- ggplot(newdat_Shrub4, aes(x = ele, y = mean)) + 
  geom_path() +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2) +
  #geom_point(data = Shrubs2, aes(x = ele, y = Cover), 
             #shape = 21, fill = "lightgrey", alpha = 0.2, colour = "black")+
  #geom_line(size = 0.75) +
  theme_cowplot()+
  ylim(0,1) +
  ylab(expression(" "))+
  xlab(expression("Elevation (m)")) +
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12))

Shrub2_plot_ele <- Shrub2_plot_ele + 
  annotate("text", x = Inf, y = Inf, label = "(d)", vjust = 1, hjust = 1, size = 6)




#### Step 8.3: Plot all together #####

plot_Shrub2 <- grid.arrange(Shrub2_fixed, Shrub_rand_all,
                            Shrub2_plot_Fire_CGD,
                           Shrub2_plot_ele,
                           #layout_matrix = layout_matrix,
                           ncol = 2)
#Save
ggsave("output/Shrub/Shrub GLM.png", 
       plot = plot_Shrub2, width = 11, height = 9)



#########################################
## GRAMINOIDS #####
##

glimpse(Graminoid2)
### Step 1: Check for correlation between variables (<0.7) #######
Graminoid2 %>%
  dplyr::select(-c(Site,  Year, Year.F, Plot.size..m2., Aspect, 
          SR.w.m2, Lifeform, count, total, Cover, lat, lon, 
          Grazing.ceased)) %>%
  GGally::ggpairs()

### Step 2: Standardise variables ######
Graminoid2$Year.F= as.factor(Graminoid2$Year)
Graminoid2$TSF.std= as.vector(scale(Graminoid2$Time_since_burnt)) 
Graminoid2$gdd.std = as.vector(scale(Graminoid2$cum.gdd))
Graminoid2$ele.std = as.vector(scale(Graminoid2$ele))

### Step 3: Create model ####
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE, threads_per_chain = 1)

mGramGLM <- brms::brm(count | trials(total) ~ 
                       TSF.std * gdd.std + ele.std +
                       ( 1 | Site) + (1|Year.F),
                       family = binomial("logit"),
                       iter = 2000, 
                       cores = 4,
                       chains = 4,
                       future = TRUE,
                       control = list(adapt_delta = 0.99, max_treedepth=15), 
                       data = Graminoid2)

### Step 5: Validate model ####
#Check model
summary(mGramGLM) 
plot(mGramGLM) 
pp_check(mGramGLM) 
bayes_R2(mGramGLM) 

### Step 6: Fixed effects coefficient plot ####
mgram2_fixed <- as.data.frame(fixef(mGramGLM))
mgram2_fixed <- rownames_to_column(mgram2_fixed, var = "Variable")
mgram2_fixed$Variable <- factor(mgram2_fixed$Variable,
                                levels = c("TSF.std:gdd.std",
                                           "gdd.std", "TSF.std",
                                           "ele.std", "Intercept"))

#Get the 80% credible intervals from model
ci_eti_fixed_89 <- ci(mGramGLM, method = "ETI", ci = 0.89, effects = "fixed")
ci_eti_fixed_95 <- ci(mGramGLM, method = "ETI", ci = 0.95, effects = "fixed")

#Rename CI low and CI high columns 
ci_eti_fixed_89 <- ci_eti_fixed_89 %>%
  rename(Q5.5 = CI_low, Q94.5 = CI_high)
ci_eti_fixed_95 <- ci_eti_fixed_95 %>%
  rename(Q2.5 = CI_low, Q97.5 = CI_high)

#join the 80% and 95% credible intervals
Gram_fixed_ef <- left_join(ci_eti_fixed_89, ci_eti_fixed_95, by = "Parameter")

# add in estimate column 
Gram_fixed_ef$Estimate <- mgram2_fixed$Estimate
Gram_fixed_ef$Variable <- mgram2_fixed$Variable


#Plot
Gram2_fixed <- ggplot(Gram_fixed_ef, aes(x = Estimate, y = Variable)) +
  geom_point(shape = 16, size = 2.5) +         # Point for the estimate
  geom_errorbarh(aes(xmin = Q5.5, xmax = Q94.5, height = 0), size = 0.7) +  # 89% Confidence intervals)
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5, height = 0), size = 0.3) +  # 95% Confidence intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    # Add a dashed line at x = 0
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Coefficients")

Gram2_fixed <- Gram2_fixed+
  annotate("text", x = Inf, y = Inf, label = "(a)", vjust = 1, hjust = 1, size = 6)


### Step 7: Random effects ####

# 1. Plot estimates of the standard deviation and CI of random effects to see which one explains the most in the unexplained variation 
summary(mGramGLM) #pull out the estimate and CI's from the model summary and create a new DF
Gram_ran_DF <- data.frame(
  variable = c("Site", "Year"),
  estimate = c(0.55, 2.28),
  lci = c(0.36, 1.04),
  uci = c(0.89, 4.86)
)

Gram_rand_all <- ggplot(Gram_ran_DF, aes(x = estimate, y = variable)) +
  geom_point(shape = 16, size = 2.5) +        
  geom_errorbarh(aes(xmin = lci, xmax = uci, height = 0), size = 0.3) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Estimated standard deviation")

Gram_rand_all <- Gram_rand_all +
  annotate("text", x = Inf, y = Inf, label = "(b)", vjust = 1, hjust = 1, size = 6)

ggsave("Output/Graminoid/Graminoid random effects plot.png", 
       plot = Gram_rand_all, width = 7, height = 3.5) 

#### Random effects: site and year (appendix)
#SITE
Gram_rand_site <- as.data.frame(ranef(mGramGLM, groups = "Site"))
Gram_rand_site <- rownames_to_column(Gram_rand_site, var = "Variable")
Gram_rand_site <- Gram_rand_site[order(Gram_rand_site$Site.Estimate.Intercept), ]

Gram_rand_site <- ggplot(Gram_rand_site, aes(x = Site.Estimate.Intercept, y = reorder(Variable, Site.Estimate.Intercept))) +
  geom_point(shape = 16, size = 2.5) +        
  geom_errorbarh(aes(xmin = Site.Q2.5.Intercept, xmax = Site.Q97.5.Intercept, height = 0), size = 0.3) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Estimated standard deviation")


#YEAR
Gram_rand2 <- as.data.frame(ranef(mGramGLM, groups = "Year.F"))
Gram_rand2 <- rownames_to_column(Gram_rand2, var = "Variable")
Gram_rand2 <- Gram_rand2[order(Gram_rand2$Year.F.Estimate.Intercept), ]

Gram_rand_year <- ggplot(Gram_rand2, aes(x = Year.F.Estimate.Intercept, y = reorder(Variable, Year.F.Estimate.Intercept))) +
  geom_point(shape = 16, size = 2.5) +        
  geom_errorbarh(aes(xmin = Year.F.Q2.5.Intercept, xmax = Year.F.Q97.5.Intercept, height = 0, size = 0.3)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +    
  scale_y_discrete(labels = custom_labels) +
  scale_x_continuous() +
  theme_half_open() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 12)) +
  xlab("Estimated standard deviation")


#Save both plots
plot <- grid.arrange(Gram_rand_site, Gram_rand_year,
                     ncol = 2)

ggsave("Output/Graminoid/Graminoid model output (random effects).png", 
       plot = plot, width = 10, height = 3.5) 


### Step 8: Plot marginal predictions ####

#### Step 8.1: Time since fire X CGD ####

mean(Graminoid2$Time_since_burnt) #28.29064
sd(Graminoid2$Time_since_burnt) #
#Fire values - high and low
##LOW fire frequency (at 83 years unburnt - transform (83 -mean)/SD = -1.932964)
(83 - 28.42308)/28.20378 
#HIGH fire frequency (at 4 years unburnt - transform (4 -mean)/SD = -0.863121)
(4 -28.42308)/28.20378 

#CGD values
mean(Graminoid2$cum.gdd) #46967.4
sd(Graminoid2$cum.gdd) #19446.54
range(Graminoid2$cum.gdd) #13384.1 77193.0

### LOW FIRE PREDICTIONS = 83 years##
#Create new dataframe
newdat_gram_F_CGD_Low <- data.frame(Site = "average site",
                                     Year.F = "average year",
                                     cum.gdd = seq(13300, 77200, by = 100), 
                                     TSF.std = 1.935092, #Predicting at low fire freq
                                     ele.std = 0,
                                     total = 1) %>% 
  dplyr::mutate(gdd.std = (cum.gdd - 46967.4)/19446.54)
#Predictions
predictions<- posterior_epred(mGramGLM, newdat= newdat_gram_F_CGD_Low, allow_new_levels = TRUE)
#Mean
mean <- apply(predictions, 2, mean)
#Confidence limits from predictions (the 95th credible intervals)
lci <- apply(predictions, 2, quantile, 0.025)
uci <- apply(predictions, 2, quantile, 0.975)
#Combine 
newdat_gram_F_CGD_Low$mean <- mean
newdat_gram_F_CGD_Low$lci <- lci
newdat_gram_F_CGD_Low$uci <- uci


#### HIGH FIRE FREG PREDICTIONS = 4 years ##
newdat_gram_F_CGD_High <- data.frame(Site = "average site",
                                      Year.F = "average year",
                                      cum.gdd = seq(13300, 77200, by = 100), 
                                      TSF.std = -0.8659506, #Predicting at high fire freq
                                      ele.std = 0,
                                      total = 1) %>% 
  dplyr::mutate(gdd.std = (cum.gdd - 46967.4)/19446.54)
#Predictions
predictions<- posterior_epred(mGramGLM, newdat= newdat_gram_F_CGD_High, allow_new_levels = TRUE)
#Mean
mean <- apply(predictions, 2, mean)
#Confidence limits from predictions (the 95th credible intervals)
lci <- apply(predictions, 2, quantile, 0.025)
uci <- apply(predictions, 2, quantile, 0.975)
#Combine 
newdat_gram_F_CGD_High$mean <- mean
newdat_gram_F_CGD_High$lci <- lci
newdat_gram_F_CGD_High$uci <- uci


##Plot predictions
Gram_plot_Fire_CGD <- ggplot(newdat_gram_F_CGD_High, aes(x = cum.gdd, y = mean)) + 
  geom_path(color = "red") +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2, fill = "red") +
  geom_path(data = newdat_gram_F_CGD_Low, aes(x = cum.gdd, y = mean), color = "blue") +
  geom_ribbon(data = newdat_gram_F_CGD_Low, aes(ymin = lci, ymax = uci), alpha = 0.2, fill = "blue") +
  #Put data points 
  #geom_point(data = Shrubs2, aes(x = cum.gdd, y = Cover), 
  #shape = 21, fill = "lightgrey", alpha = 0.2, colour = "black")+
  theme_cowplot()+
  ylim(0,1) +
  ylab(expression("Graminoid proportional cover"))+
  xlab(expression("Cumulative summer growing degrees (°C)")) +
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12)) 

Gram_plot_Fire_CGD <- Gram_plot_Fire_CGD+
  annotate("text", x = Inf, y = Inf, label = "(c)", vjust = 1, hjust = 1, size = 6)
  


#### Step 8.2: Elevation ####

mean(Graminoid2$ele) #1750.966
sd(Graminoid2$ele) #96.05066
range(Graminoid2$ele) #1574.000 1982.326
#Create new dataframe
newdat_Gram4 <- data.frame(Site = "average site",
                           Year.F = "average year",
                           ele = seq(1560, 2000, by = 10), 
                           TSF.std = 0, 
                           gdd.std = 0,
                           total = 1) %>% 
  dplyr::mutate(ele.std = (ele - 1750.966)/96.05066)
#Predictions
predictions<- posterior_epred(mGramGLM, newdat= newdat_Gram4, allow_new_levels = TRUE)
#Mean
mean <- apply(predictions, 2, mean)
#Confidence limits from predictions (the 95th credible intervals)
lci <- apply(predictions, 2, quantile, 0.025)
uci <- apply(predictions, 2, quantile, 0.975)
#Combine 
newdat_Gram4$mean <- mean
newdat_Gram4$lci <- lci
newdat_Gram4$uci <- uci


##Plot predictions
Gram_plot_ele <- ggplot(newdat_Gram4, aes(x = ele, y = mean)) + 
  geom_path() +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2) +
  #geom_point(data = Graminoid2, aes(x = ele, y = Cover), 
             #shape = 21, fill = "lightgrey", alpha = 0.2, colour = "black")+
  #geom_line(size = 0.75) +
  theme_cowplot()+
  ylim(0,1) +
  ylab(expression(" "))+
  xlab(expression("Elevation (m)")) +
  theme(axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12)) 

Gram_plot_ele <- Gram_plot_ele+
  annotate("text", x = Inf, y = Inf, label = "(d)", vjust = 1, hjust = 1, size = 6)


#### Step 8.3: PLot all together #####

plot_Gram <- grid.arrange(Gram2_fixed, Gram_rand_all,
                          Gram_plot_Fire_CGD,
                          Gram_plot_ele,
                            ncol = 2)
#Save
ggsave("Output/Graminoid/Graminoid GLM.png", 
       plot = plot_Gram, width = 11, height = 9)




###############################
# APPENDICES ######

### Forb model predictions #####
pdf("output/Appendix/Forb model predictions.pdf", width = 5, height = 3.5)
preds <- as.data.frame(fitted(mForbGLM))
plot(preds$Estimate ~ mForbGLM$data$count,  
     xlab = "Forb observations",
     ylab = "Model predictions")
abline(0, 1, col= 'red')
dev.off() 

### Graminoid model predictions #####
pdf("output/Appendix/Graminoid model predictions.pdf", width = 5, height = 3.5)
preds <- as.data.frame(fitted(mGramGLM))
plot(preds$Estimate ~ mGramGLM$data$count,  
     xlab = "Graminoid observations",
     ylab = "Model predictions")
abline(0, 1, col= 'red')
dev.off() 

### Shrub model predictions #####
pdf("output/Appendix/Shrub model predictions.pdf", width = 5, height = 3.5)
preds <- as.data.frame(fitted(mShrubGLM))
plot(preds$Estimate ~ mShrubGLM$data$count,  
     xlab = "Shrub observations",
     ylab = "Model predictions")
abline(0, 1, col= 'red')
dev.off() 


# END #############################################################################################################

####END