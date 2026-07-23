# ----------------------------------
# Brittany Cavazos & Jules Vinke
# Data exploration & analysis script for ESA Poster Presentation
# ----------------------------------

# Housekeeping
rm(list = ls()); gc()

# Libraries
librarian::shelf(tidyverse, lme4, bbmle, lmerTest, emmeans, ggeffects)

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

summary(LA_model7) # use this for LA results

summary(LA_model4)
stats::anova(LA_model6)




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

traits_v02 %>%
  group_by(range) %>%
  summarise(mean = mean(int_length_cm, na.rm = T), 
            sd = sd(int_length_cm, na.rm = T))


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

summary(INF_model8)


AICctab(INF_model2, INF_model4, INF_model5, INF_model6, INF_model7, INF_model8, INF_model3.5, weights = TRUE)

summary(INF_model3.5)
# models 5, 6, 2

# range matters the most but there is a marginally significant interaction between range and year

# check model fit ***


### visualization

# Leaf area - just range ####
colors <- c("#762a83", "#7fbf7b")

# raw data
ggplot(traits_v02, aes(x = range, y = leaf_area_cm2, fill = range)) +
  geom_boxplot(show.legend = FALSE) +
  geom_violin(alpha = 0.25, show.legend = FALSE) +
  labs(x = "", y = expression("Leaf Area (" * cm^2 * ")")) + 
  scale_fill_manual(values = colors) + 
  scale_x_discrete(labels = c(
    "invasive" = "Non-native Range",
    "native" = "Native Range"))+
  theme_classic(base_size = 20)

# model results w/ 95% CIs
traits_v02$range <- as.factor(traits_v02$range)
LAmodel <- lm(leaf_area_cm2 ~ range + julianday, data = traits_v02)
range_results <- as.data.frame(emmeans(LAmodel, ~ range))

leafarea <- ggplot(range_results, aes(x = range, y = emmean, fill = range, color = range)) +
  geom_point(show.legend = FALSE, size = 5, aes(shape = range, fill = range)) +
  geom_errorbar(show.legend = FALSE, aes(ymin = lower.CL, ymax = upper.CL,
    width = 0.2), lwd = 2) +
  labs(x = "", y = expression("Leaf Area (" * cm^2 * ")")) +
  scale_color_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_fill_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) +
  scale_x_discrete(labels = c("invasive" = "Non-native Range", "native" = "Native Range")) +
  theme_classic(base_size = 20) 


ggsave(
  "leafarea_modelresults.png",
  plot = leafarea,
  width = 7.5,
  height = 5,
  units = "in",
  dpi = 300
)

# another way to visualize it 

pred <- ggeffects::ggpredict(LAmodel, terms = c("julianday", "range"))
pred <- as.data.frame(pred)
pred$julianday <- pred$x
pred$range <- pred$group

ggplot() +
  geom_point(data = traits_v02,
             aes(x = julianday, y = leaf_area_cm2, colour = range),
             alpha = 0.4,
             size = 2) +
  geom_line(data = pred,
            aes(x = x, y = predicted, colour = group),
            linewidth = 1.2) +
  geom_ribbon(data = pred,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = group),
              alpha = 0.2,
              colour = NA) +
  
  labs(x = "Day",
       y = "Area",
       colour = "range",
       fill = "range") +
  theme_classic()


leafarea_modelresults2 <- ggplot(data = traits_v02, aes(x = julianday, y = leaf_area_cm2, color = range)) +
  geom_point(aes(fill = range, shape = range), alpha = 0.45) +
  labs(x = "Month", y = expression("Leaf Area (" * cm^2 * ")")) + 
  scale_color_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_fill_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) +
  geom_line(data = pred, aes(y = predicted), lwd = 2) + 
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244, 274, 305),
    labels = c("May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov"
  )) +
  theme_classic(base_size = 20) +
  theme(legend.justification = "top") 

ggsave( "leafarea_modelresults2.png",
        plot = leafarea_modelresults2,
        width = 12,
        height = 6,
        units = "in",
        dpi = 300
)

# Internode - range + julian day + year ####

model.int <- lm(int_length_cm ~ range + year + julianday, data = traits_v02)

pred <- ggeffects::ggpredict(model.int, terms = c("julianday", "range"))
pred <- as.data.frame(pred)
pred$range <- pred$group
pred$julianday <- pred$x



internode2 <- ggplot(traits_v02, aes(x = julianday, y = int_length_cm, color = range)) +
  geom_point(aes(fill = range, shape = range), alpha = 0.45) +
  labs(x = "Month", y = "Internode Length (cm)") + 
  scale_color_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_fill_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) +
  geom_line(data = pred, aes(y = predicted), lwd = 2) + 
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244, 274, 305),
    labels = c("May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov"
    )) +
  theme_classic(base_size = 20) +
  theme(legend.justification = "top") 

ggsave(
  "internode_modelresults2.png",
  plot = internode2,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

# infl - year by range interaction ####
# actually just range

# raw data
traits_v02 %>%
  dplyr::filter(!is.na(infl_length_cm)) %>%
  ggplot(data = ., aes(x = year, y = infl_length_cm)) +
  geom_point(aes(fill = range, shape = range), alpha = 0.45)+
  geom_smooth(aes(color = range), formula = "y ~ x",
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Inflorescence Length (cm)") + 
  scale_color_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_fill_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) +
  theme_classic(base_size = 20) +
  theme(legend.justification = "top") 

# model resutls w/ 95% CIs
model.inf <- lm(infl_length_cm ~ year * range, data = traits_v02)

newdat.inf <- expand.grid(
  year = seq(min(traits_v02$year), max(traits_v02$year), length.out = 100),
  range = levels(as.factor(traits_v02$range))
)

pred.inf <- predict(model.inf, newdata = newdat.inf, interval = "confidence")

newdat.inf <- cbind(newdat.inf, pred.inf)

newdat.inf$pred.inf <- predict(model.inf, newdata = newdat.inf)


infl_modelresults <- ggplot(traits_v02, aes(x = year, y = infl_length_cm, color = range)) +
  geom_point(aes(fill = range, shape = range), alpha = 0.45) +
  labs(x = "Year", y = "Inflorescence Length (cm)") + 
  scale_color_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_fill_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) +
  geom_line(data = newdat.inf, aes(y = pred.inf), lwd = 2) + 
  theme_classic(base_size = 20) +
  theme(legend.justification = "top") 

ggsave(
  "infl_modelresults.png",
  plot = infl_modelresults,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)


# just range

model.if.range <- lm(infl_length_cm ~ range, data = traits_v02)
pred.if.range <- ggpredict(model.if.range, terms = "range")

ggplot(traits_v02, aes(x = range, y = infl_length_cm, color = range)) +
  # Raw data
  geom_boxplot(alpha = 0.5, size = 2) +
  
  # Predicted means
  geom_point(data = pred.if.range,
             aes(x = x, y = predicted, colour = group),
             size = 4,
             inherit.aes = FALSE) +
  
  # 95% confidence intervals
  geom_errorbar(data = pred.if.range,
                aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = group),
                width = 0.15,
                linewidth = 0.8,
                inherit.aes = FALSE) +
  
  labs(x = "Range",
       y = "Inflorescence length cm") +
  theme_classic()

infl2<- ggplot(traits_v02, aes(x = range, y = infl_length_cm, fill = range)) +
  # geom_boxplot(show.legend = FALSE) +
  geom_violin(draw_quantiles = 0.5, alpha = 0.25, show.legend = FALSE, aes(color = range)) +
  geom_jitter(alpha = 0.75, aes(color = range), width = 0.08, show.legend = FALSE)+
  labs(x = "", y = "Inflorescence Length (cm)") + 
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
  scale_x_discrete(labels = c(
    "invasive" = "Non-native Range",
    "native" = "Native Range"))+
  theme_classic(base_size = 20)


ggsave(
  "infl_modelresults2.png",
  plot = infl2,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

traits_v02 %>%
  group_by(range) %>%
  summarise(mean = mean(infl_length_cm, na.rm = T), 
            sd = sd(infl_length_cm, na.rm = T))
