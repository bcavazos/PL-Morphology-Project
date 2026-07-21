# ----------------------------------
# Brittany Cavazos & Jules Vinke
# Data exploration & analysis script for ESA Poster Presentation
# ----------------------------------

# Housekeeping
rm(list = ls()); gc()

# Libraries
librarian::shelf(tidyverse, lme4, bbmle, lmerTest, emmeans)

# Read in tidy data
traits_v01 <- read.csv("data/PL_traits-tidy.csv") 

# remove rows where none of the 3 traits are measured (likely seedlings or broken link)
traits_v02 <- traits_v01 %>%
  dplyr:: filter(!(is.na(leaf_area_cm2) & is.na(infl_length_cm) & is.na(int_length_cm))) %>%
  dplyr::mutate(julianday = yday(date)) %>%
  filter(julianday > 100 & julianday < 300) 

glimpse(traits_v02)


# create some data subsets to play around with later -- 

# just samples in the US (to look closer at change over time or if longitude (moving westward) makes a difference)
PLUS <- traits_v02 %>%
  filter(range == "invasive")

post_introduction <- traits_v02 %>%
  filter(year > 1900)

# Date Exploration ----#
summary(as.factor(traits_v02$country))

summary(as.factor(traits_v02$range))

hist(traits_v02$latitude)

# is latitude correlated with invasive status?

ggplot(traits_v02, aes(x = latitude, fill = range)) +
  geom_histogram(position = "identity", alpha =0.8) +
  theme_classic()

# pretty much entirely...

hist(traits_v02$year)

# does year correlate with invasive status?
ggplot(traits_v02, aes(x = year, fill = range)) +
  geom_histogram(position = "identity", alpha =0.8) +
  theme_classic()

# mostly but not entirely

# representation across month?
hist(traits_v02$month)

ggplot(traits_v02, aes(x = month, fill = range)) +
  geom_histogram(position = "identity", alpha =0.8) +
  theme_classic()

# are our traits of interest normally distributed?
hist(traits_v02$leaf_area_cm2) # ish
hist(traits_v02$int_length_cm) # ish
hist(traits_v02$infl_length_cm) #ish
# Gaussian distribution for analysis will be okay

# Analysis

### Leaf Area ###
  # since range and lat are correlated so tightly, won't put them in the same model
  # include Julian Day 

LA_model1 <- lm(leaf_area_cm2 ~ range * year * julianday, data = traits_v02) 
LA_model2 <- lm(leaf_area_cm2 ~ range * year + julianday, data = traits_v02) 
LA_model3 <- lm(leaf_area_cm2 ~ range + year * julianday, data = traits_v02)
  # interactions are NS

LA_model4 <- lm(leaf_area_cm2 ~ range + year + julianday, data = traits_v02) 
LA_model5 <- lm(leaf_area_cm2 ~ range + year, data = traits_v02) 
# year is NS

LA_model6 <- lm(leaf_area_cm2 ~ range + julianday, data = traits_v02) #*
LA_model7 <- lm(leaf_area_cm2 ~ julianday + year, data = traits_v02) #*


AICctab(LA_model4, LA_model5, LA_model6, LA_model7, weights = TRUE)

summary(LA_model6) # use this for LA results

summary(LA_model7)
stats::anova(LA_model6)
stats::anova(LA_model8)
stats::anova(LA_model9)

# check model fit***

# Internode second


INT_model1  <- lm(int_length_cm ~ range * year * julianday, data = traits_v02) 
INT_model2 <- lm(int_length_cm ~ range * year + julianday, data = traits_v02) 
INT_model3 <- lm(int_length_cm ~ range + year * julianday, data = traits_v02)
# interactions are NS

INT_model4 <- lm(int_length_cm ~ range + year + julianday, data = traits_v02) 
INT_model5 <- lm(int_length_cm ~ range + year, data = traits_v02) 
INT_model6 <- lm(int_length_cm ~ range + julianday, data = traits_v02) #*
INT_model7 <- lm(int_length_cm ~ julianday + year, data = traits_v02) #*

# stats::anova(INT_model1)
# stats::anova(INT_model2)
# stats::anova(INT_model3) 
stats::anova(INT_model4) #** best model
stats::anova(INT_model5)
stats::anova(INT_model6) # second best
stats::anova(INT_model7)

# year has strongest affect
AICctab(INT_model4, INT_model5, INT_model6, INT_model7, weights = TRUE)

summary(INT_model4)
# model 4 is best fit - the additive range and year
# internode is longer in native range and smaller over time latitudes and LA has gets shorter over time


## check model fit ****

# Infl third

INF_model1  <- lm(infl_length_cm ~ range * year * julianday, data = traits_v02) 
INF_model2 <- lm(infl_length_cm ~ range * year + julianday, data = traits_v02) 
INF_model3 <- lm(infl_length_cm ~ range + year * julianday, data = traits_v02)
# interactions are NS
INF_model3.5 <- lm(infl_length_cm ~ range * year, data = traits_v02)
INF_model4 <- lm(infl_length_cm ~ range + year + julianday, data = traits_v02) 
INF_model5 <- lm(infl_length_cm ~ range + year, data = traits_v02) 
INF_model6 <- lm(infl_length_cm ~ range + julianday, data = traits_v02) 
INF_model7 <- lm(infl_length_cm ~ julianday + year, data = traits_v02) 
INF_model8 <- lm(infl_length_cm ~ range, data = traits_v02)

# stats::anova(INF_model1)
# stats::anova(INF_model2)
# stats::anova(INF_model3)
# stats::anova(INF_model4)
stats::anova(INF_model5)
# stats::anova(INF_model6)
stats::anova(INF_model7)

summary(INF_model2)


AICctab(INF_model2, INF_model4, INF_model5, INF_model6, INF_model7, INF_model8, weights = TRUE)

# models 5, 6, 2

# range matters the most but there is a marginally significant interaction between range and year

# check model fit ***


### visualization


ggplot(traits_v02, aes(x = year, y = infl_length_cm, color = range, fill = range)) + geom_point() + geom_smooth(method = "lm") + theme_classic()


# Leaf area - just range ####

# raw data
ggplot(traits_v02, aes(x = range, y = leaf_area_cm2, fill = range)) +
  geom_boxplot(show.legend = FALSE) +
  geom_violin(alpha = 0.25, show.legend = FALSE) +
  labs(x = "", y = expression("Leaf Area (" * cm^2 * ")")) + 
  scale_fill_manual(values = c("mediumpurple2", "gray")) + # change to the colors you want
  scale_x_discrete(labels = c(
    "invasive" = "Nonnative Range",
    "native" = "Native Range"))+
  theme_classic(base_size = 20)

# model results w/ 95% CIs
LAmodel <- lm(leaf_area_cm2 ~ range + julianday, data = traits_v02)
range_results <- as.data.frame(emmeans(LAmodel, ~ range))

ggplot(range_results, aes(x = range, y = emmean, fill = range)) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2
  ) +
  labs(
    x = "",
    y = expression("Leaf Area (" * cm^2 * ")")
  ) +
  scale_fill_manual(values = c("purple", "black")) +
  scale_x_discrete(labels = c(
    "invasive" = "Nonnative Range",
    "native" = "Native Range"
  )) +
  theme_classic(base_size = 20)

# Internode - range + year ####

model <- lm(int_length_cm ~ year + range, data = traits_v02)

newdat <- expand.grid(
  year = seq(min(traits_v02$year), max(traits_v02$year), length.out = 100),
  range = levels(as.factor(traits_v02$range))
)

newdat$pred <- predict(model, newdata = newdat)

ggplot(traits_v02, aes(x = year, y = int_length_cm, color = range)) +
  geom_point() +
  labs(x = "Year", y = "Internode Length (cm)") + 
  scale_color_manual(name = "Range", labels = c("Nonnative", "Native"), values = c("purple", "black")) +
  geom_line(data = newdat, aes(y = pred)) + 
  theme_classic(base_size = 20)

# infl - year by range interaction ####

# raw data

ggplot(traits_v02, aes(x = year, y = infl_length_cm, color = range)) +
  geom_point()+
  labs(x = "Year", y = "Inflorescence Length (cm)") + 
  scale_color_manual(name = "Range", labels = c("Nonnative", "Native"), values = c("purple", "black")) +
  geom_smooth(method = "lm") +
  theme_classic(base_size = 20)

# model resutls w/ 95% CIs
model.inf <- lm(infl_length_cm ~ year * range, data = traits_v02)

newdat <- expand.grid(
  year = seq(min(traits_v02$year), max(traits_v02$year), length.out = 100),
  range = levels(as.factor(traits_v02$range))
)

pred <- predict(model.inf, newdata = newdat, interval = "confidence")

newdat <- cbind(newdat, pred)

newdat$pred <- predict(model.inf, newdata = newdat)

ggplot(newdat, aes(x = year, y = fit, color = range, fill = range)) +
  geom_ribbon( aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Inflorescence Length (cm)", color = "Range", fill = "Range") +
  scale_color_manual(labels = c("invasive" = "Nonnative", "native" = "Native"), values = c("purple", "black")) +
  scale_fill_manual(labels = c("invasive" = "Nonnative","native" = "Native"),
    values = c("purple", "black")) +
  theme_classic(base_size = 20)
