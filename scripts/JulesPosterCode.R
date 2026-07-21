# Analysis from Poster Graphs #
# Code copied from analysis-ESA-PLtraits.R #

# Housekeeping
rm(list = ls()); gc()

# Libraries
librarian::shelf(tidyverse, lme4, bbmle)

# Read in tidy data
traits_v01 <- read.csv("data/PL_traits-tidy.csv") 

# remove rows where none of the 3 traits are measured (likely seedlings or broken link)
traits_v02 <- traits_v01 %>%
  dplyr:: filter(!(is.na(leaf_area_cm2) & is.na(infl_length_cm) & is.na(int_length_cm))) 

glimpse(traits_v02)

# Jules note - curious to see how data spread over month #
table(traits_v02$month)
# relatively makes sense #
# checked specimens - 1/1 was given to plants with only year data #

# nope -- primarily during summer months, which makes sense.. PL in January is suspect (would have to look at where it's located)

ggplot(traits_v02, aes(x = month, fill = range)) +
  geom_histogram(position = "identity", alpha =0.8) +
  theme_classic()

# are our traits of interest normally distributed?
hist(traits_v02$leaf_area_cm2) # ish
hist(traits_v02$int_length_cm) # ish
hist(traits_v02$infl_length_cm) #ish
# Gaussian distribution for analysis will be okay

# Analysis ----

# three traits - internode, inflorescence, and leaf area

# leaf area first 
# since range and lat are correlated so tightly, won't put them in the same model
# LA_model1 <- lm(leaf_area_cm2 ~ range * year, data = traits_v02) # NS
# LA_model2 <- lm(leaf_area_cm2 ~ latitude * year, data = traits_v02) # NS
LA_model3 <- lm(leaf_area_cm2 ~ range + year, data = traits_v02) # both sig
LA_model4 <- lm(leaf_area_cm2 ~ latitude + year, data = traits_v02) # both sig
LA_model5 <- lm(leaf_area_cm2 ~ range, data = traits_v02) # range
LA_model6 <- lm(leaf_area_cm2 ~ year, data = traits_v02) # year
LA_model7 <- lm(leaf_area_cm2 ~ latitude, data = traits_v02) # latitude

#stats::anova(LA_model1)
#stats::anova(LA_model2)
stats::anova(LA_model3)
stats::anova(LA_model4) # ***
stats::anova(LA_model5)
stats::anova(LA_model6)
stats::anova(LA_model7)

# year has strongest affect
AICctab(LA_model3, LA_model4, weights = TRUE)

summary(LA_model4)
# models are technically tied but model 4 fits the best - the additive latitude and year
# LA gets marginally larger with increasing latitudes and LA has gets smaller over time


# check model fit***

# Internode second
# leaf area first 
# since range and lat are correlated so tightly, won't put them in the same model
# INT_model1 <- lm(int_length_cm ~ range * year, data = traits_v02) # NS
# INT_model2 <- lm(int_length_cm ~ latitude * year, data = traits_v02) # NS
INT_model3 <- lm(int_length_cm ~ range + year, data = traits_v02) # both sig
INT_model4 <- lm(int_length_cm ~ latitude + year, data = traits_v02) # both sig
# INT_model5 <- lm(int_length_cm ~ range, data = traits_v02) # range
# INT_model6 <- lm(int_length_cm ~ year, data = traits_v02) # year
# INT_model7 <- lm(int_length_cm ~ latitude, data = traits_v02) # latitude

# stats::anova(INT_model1)
# stats::anova(INT_model2)
stats::anova(INT_model3) # best model
stats::anova(INT_model4)
# stats::anova(INT_model5)
# stats::anova(INT_model6)
# stats::anova(INT_model7)

# year has strongest affect
AICctab(INT_model3, INT_model4, weights = TRUE)

summary(INT_model3)
# model 3 is best fit - the additive range and year
# internode is longer in native range and smaller over time latitudes and LA has gets shorter over time


## check model fit ****

# Infl third

#INF_model1 <- lm(infl_length_cm ~ range * year, data = traits_v02) # NS
#INF_model2 <- lm(infl_length_cm ~ latitude * year, data = traits_v02) # NS
#INF_model3 <- lm(infl_length_cm ~ range + year, data = traits_v02) # range only
#INF_model4 <- lm(infl_length_cm ~ latitude + year, data = traits_v02) # lat only
INF_model5 <- lm(infl_length_cm ~ range, data = traits_v02) # range
#INF_model6 <- lm(infl_length_cm ~ year, data = traits_v02) # NS
INF_model7 <- lm(infl_length_cm ~ latitude, data = traits_v02) # latitude

# stats::anova(INF_model1)
# stats::anova(INF_model2)
# stats::anova(INF_model3)
# stats::anova(INF_model4)
stats::anova(INF_model5)
# stats::anova(INF_model6)
stats::anova(INF_model7)

# year has strongest affect
AICctab(INF_model5, INF_model7, weights = TRUE)

summary(INF_model7)
# model 7 is best fit - infl is best explained by latitude, longer inf at lower latitudes

# check model fit ***

### visualization

# leaf area - lat and year
ggplot(traits_v02, aes(x = year, y = leaf_area_cm2, color = latitude)) +
  geom_point() +
  geom_smooth() +
  theme_classic()


ggplot(traits_v02, aes(x = latitude, y = leaf_area_cm2, color = year )) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  scale_fill_distiller(palette = "Purples")

ggplot(traits_v02, aes(x = range, y = leaf_area_cm2))+
  geom_boxplot() +
  geom_violin(alpha = 0.5, size = 2)
              +
  theme_classic() +
  labs( x = "Range" , y = "Leaf Area (cm2)")
#Jules note - having trouble formatting this graph


ggplot(traits_v02, aes(x = year, y = leaf_area_cm2, color = range)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() 

# internode - range + year
ggplot(traits_v02, aes(x = year, y = int_length_cm, color = range)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_classic() +
  labs( x = "Year", y = "Internode Length (cm)") +
  scale_color_manual(values = c("native" = "purple", "invasive" = "black"))

# inflorescence - range + year
ggplot(traits_v02, aes(x = year, y = infl_length_cm, color = range)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_classic() +
  labs( x = "Year", y = "Inflorescence Length (cm)") +
  scale_color_manual(values = c("native" = "purple", "invasive" = "black"))

