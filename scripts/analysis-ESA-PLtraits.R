# ----------------------------------
# Brittany Cavazos & Jules Vinke
# Data exploration & analysis script for ESA Poster Presentation
# ----------------------------------

# Housekeeping
rm(list = ls()); gc()

# Libraries
librarian::shelf(tidyverse, lme4, bbmle, lmerTest, emmeans, ggeffects, car)

# color theme: non-native, native
colors <- c("#762a83", "#7fbf7b")

# Read in tidy data
traits_v01 <- read.csv("data/PL_traits-tidy.csv") 

# remove rows where none of the 3 traits are measured (likely seedlings or broken link)
traits_v02 <- traits_v01 %>%
  dplyr:: filter(!(is.na(leaf_area_cm2) & is.na(infl_length_cm) & is.na(int_length_cm))) %>%
  dplyr::mutate(julianday = yday(date)) %>%
  filter(julianday > 100 & julianday < 300) 

glimpse(traits_v02)

# Date Exploration ----#
summary(as.factor(traits_v02$country))
summary(as.factor(traits_v02$range))
hist(traits_v02$latitude)

# is latitude correlated with invasive status?
ggplot(traits_v02, aes(x = latitude, fill = range)) +
  geom_histogram(position = "identity", alpha =0.8) +
  theme_classic()

hist(traits_v02$year)

# does year correlate with invasive status?
ggplot(traits_v02, aes(x = year, fill = range)) +
  geom_histogram(position = "identity", alpha =0.8) +
  theme_classic()

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

### Leaf Area Analysis ####
  # since range and lat are correlated so tightly, won't put them in the same model
  # include Julian Day 

# LA_model1 <- lm(leaf_area_cm2 ~ year * julianday * range, data = traits_v02) 
# LA_model2 <- lm(leaf_area_cm2 ~ julianday + range * year, data = traits_v02) 
# LA_model3 <- lm(leaf_area_cm2 ~ year * julianday + range, data = traits_v02)

# LA_model4 <- lm(leaf_area_cm2 ~ year * julianday, data = traits_v02) 
# LA_model5 <- lm(leaf_area_cm2 ~ julianday * range, data = traits_v02) 
# LA_model6 <- lm(leaf_area_cm2 ~ year * range, data = traits_v02) 

LA_model7 <- lm(leaf_area_cm2 ~ year + julianday + range, data = traits_v02) 
# LA_model8 <- lm(leaf_area_cm2 ~ year + range, data = traits_v02) 

LA_model9 <- lm(leaf_area_cm2 ~ julianday + range, data = traits_v02) 
LA_model10 <- lm(leaf_area_cm2 ~ julianday + year, data = traits_v02)

AICctab(LA_model7, 
        LA_model9, 
        LA_model10, weights = TRUE)

# use this for poster -- range is marginally significant and julian day is significant
car::Anova(LA_model9, type = 2)
summary(LA_model9) 

median_iqr <- function(x) {
  data.frame(
    y = median(x),
    ymin = quantile(x, 0.25),
    ymax = quantile(x, 0.75)
  )
}

# Leaf Area between ranges
LA_range <- ggplot(traits_v02, aes(x = range, y = leaf_area_cm2, fill = range)) +
  geom_violin(aes(fill = range), 
              alpha = 0.25, 
              show.legend = FALSE) +
  geom_jitter(aes(fill = range, shape = range, color = range), 
              alpha = 0.45, 
              width = 0.1, 
              show.legend = FALSE) +
  labs(x = "", y = expression("Leaf Area (" * cm^2 * ")")) + 
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) + 
  scale_x_discrete(labels = c("invasive" = "Non-native Range", 
                              "native" = "Native Range"))+
  stat_summary(fun.data = median_iqr, geom = "linerange", color = "black", show.legend = FALSE) + 
  stat_summary(fun = median, geom = "point", color = "black", show.legend = FALSE) + 
  theme_classic(base_size = 30)

ggsave(
  "leafarea_ranges.png",
  plot = LA_range,
  width = 9,
  height = 7,
  units = "in",
  dpi = 300
)

# Leaf Area change over time
LA_season <- ggplot(traits_v02, aes(x = julianday, y = leaf_area_cm2)) +
  geom_point(aes(fill = range, color = range, shape = range), alpha = 0.45) +
  labs(x = "Month", y = expression("Leaf Area (" * cm^2 * ")")) + 
  scale_color_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_fill_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244, 274, 305),
    labels = c("May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov"
    )) +
  theme_classic(base_size = 30) +
  theme(legend.position = "inside", legend.position.inside = c(0.2,0.9)) 
  

ggsave(
  "leafarea_season.png",
  plot = LA_season,
  width = 9,
  height = 7,
  units = "in",
  dpi = 300
)

# Internode Analysis ####

# INT_model1  <- lm(int_length_cm ~ range * year * julianday, data = traits_v02) 
INT_model2 <- lm(int_length_cm ~ julianday + range * year, data = traits_v02) # NS
INT_model3 <- lm(int_length_cm ~ year * julianday + range, data = traits_v02) # NS

# INT_model4  <- lm(int_length_cm ~ range * year, data = traits_v02) 
# INT_model5  <- lm(int_length_cm ~ year * julianday, data = traits_v02) 
# INT_model6  <- lm(int_length_cm ~ range * julianday, data = traits_v02) 

INT_model7 <- lm(int_length_cm ~ year + julianday + range, data = traits_v02) 
# INT_model8 <- lm(int_length_cm ~ year + range, data = traits_v02) 
INT_model9 <- lm(int_length_cm ~ julianday + range, data = traits_v02) 
# INT_model10 <- lm(int_length_cm ~ julianday + year, data = traits_v02) 

AICctab(INT_model2, 
        INT_model3, 
        INT_model7, 
        INT_model9, 
        weights = TRUE)

car::Anova(INT_model7, type = 2)
# model 7 is best fit - the additive range and year and julian day

traits_v02 %>%
  group_by(range) %>%
  summarise(mean = mean(int_length_cm, na.rm = T), 
            sd = sd(int_length_cm, na.rm = T))

# internode range
INT_range <- ggplot(traits_v02, aes(x = range, y = int_length_cm, fill = range)) +
  geom_violin(aes(fill = range), 
              alpha = 0.25, 
              show.legend = FALSE) +
  geom_jitter(aes(fill = range, shape = range, color = range), 
              alpha = 0.45, 
              width = 0.1, 
              show.legend = FALSE) +
  labs(x = "", y = "Internode Length (cm)") + 
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) + 
  scale_x_discrete(labels = c("invasive" = "Non-native Range", 
                              "native" = "Native Range"))+
  stat_summary(fun.data = median_iqr, geom = "linerange", color = "black", show.legend = FALSE) + 
  stat_summary(fun = median, geom = "point", color = "black", show.legend = FALSE) + 
  theme_classic(base_size = 30)

ggsave(
  "INT_range.png",
  plot = INT_range,
  width = 9,
  height = 7,
  units = "in",
  dpi = 300
)

# Internode change over time
INT_season <- ggplot(traits_v02, aes(x = julianday, y = int_length_cm)) +
  geom_point(aes(fill = range, color = range, shape = range), alpha = 0.45) +
  labs(x = "Month", y = "Internode Length (cm)") + 
  scale_color_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_fill_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  scale_x_continuous(
    breaks = c(121, 152, 182, 213, 244, 274, 305),
    labels = c("May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov"
    )) +
  theme_classic(base_size = 30) +
  theme(legend.position = "inside", legend.position.inside = c(0.18,0.88)) 

ggsave(
  "INT_season.png",
  plot = INT_season,
  width = 9,
  height = 7,
  units = "in",
  dpi = 300
)




# Infl Analysis ####

#INF_model1  <- lm(infl_length_cm ~ range * year * julianday, data = traits_v02) 
#INF_model2 <- lm(infl_length_cm ~ julianday + range * year, data = traits_v02) 
#INF_model3 <- lm(infl_length_cm ~ year * julianday + range, data = traits_v02)

INF_model4 <- lm(infl_length_cm ~ range * year, data = traits_v02)
#INF_model5 <- lm(infl_length_cm ~ year * julianday, data = traits_v02)
#INF_model6 <- lm(infl_length_cm ~ range * julianday, data = traits_v02)

#INF_model7 <- lm(infl_length_cm ~ julianday + year + range, data = traits_v02) 
INF_model8 <- lm(infl_length_cm ~  year + range,  data = traits_v02) 
INF_model9 <- lm(infl_length_cm ~ julianday + range, data = traits_v02) 
#INF_model10 <- lm(infl_length_cm ~ julianday + year, data = traits_v02) 
INF_model11 <- lm(infl_length_cm ~ range, data = traits_v02)



AICctab(INF_model4, 
        INF_model8, 
        INF_model9, 
        INF_model11,
        weights = TRUE)

# use model 4 bc marginally sig interaction term
car::Anova(INF_model4, type = 2)

# range matters the most but there is a marginally significant interaction between range and year 

traits_v02 %>%
  group_by(range) %>%
  summarise(mean = mean(infl_length_cm, na.rm = T), 
            sd = sd(infl_length_cm, na.rm = T))

inf_intx <- traits_v02 %>%
  dplyr::filter(!is.na(infl_length_cm)) %>%
  ggplot(data = ., aes(x = year, y = infl_length_cm)) +
  geom_point(aes(fill = range, shape = range, color = range), alpha = 0.45)+
  geom_smooth(aes(color = range), formula = "y ~ x",
              method = "lm", se = FALSE) +
  labs(x = "Year", y = "Inflorescence Length (cm)") + 
  scale_color_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_fill_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) +
  theme_classic(base_size = 30) +
  theme(legend.justification = "top") 

ggsave(
  "inf_intx.png",
  plot = inf_intx,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

### visualization extras ####

# Leaf area - ###

# model results w/ 95% CIs
traits_v02$range <- as.factor(traits_v02$range)
LAmodel <- lm(leaf_area_cm2 ~ range + julianday, data = traits_v02)
range_results <- as.data.frame(emmeans(LAmodel, ~ range))

ggplot(range_results, aes(x = range, y = emmean, fill = range, color = range)) +
  geom_point(show.legend = FALSE, size = 5, aes(shape = range, fill = range)) +
  geom_errorbar(show.legend = FALSE, aes(ymin = lower.CL, ymax = upper.CL,
    width = 0.2), lwd = 2) +
  labs(x = "", y = expression("Leaf Area (" * cm^2 * ")")) +
  scale_color_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_fill_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) +
  scale_x_discrete(labels = c("invasive" = "Non-native Range", "native" = "Native Range")) +
  theme_classic(base_size = 20) 

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

ggplot(data = traits_v02, aes(x = julianday, y = leaf_area_cm2, color = range)) +
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


# Internode - range + julian day + year ####

model.int <- lm(int_length_cm ~ range + year + julianday, data = traits_v02)

pred <- ggeffects::ggpredict(model.int, terms = c("julianday", "range"))
pred <- as.data.frame(pred)
pred$range <- pred$group
pred$julianday <- pred$x


ggplot(traits_v02, aes(x = julianday, y = int_length_cm, color = range)) +
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



# infl


# model resutls w/ 95% CIs
model.inf <- lm(infl_length_cm ~ year * range, data = traits_v02)

newdat.inf <- expand.grid(
  year = seq(min(traits_v02$year), max(traits_v02$year), length.out = 100),
  range = levels(as.factor(traits_v02$range))
)

pred.inf <- predict(model.inf, newdata = newdat.inf, interval = "confidence")
newdat.inf <- cbind(newdat.inf, pred.inf)
newdat.inf$pred.inf <- predict(model.inf, newdata = newdat.inf)


ggplot(traits_v02, aes(x = year, y = infl_length_cm, color = range)) +
  geom_point(aes(fill = range, shape = range), alpha = 0.45) +
  labs(x = "Year", y = "Inflorescence Length (cm)") + 
  scale_color_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_fill_manual(name = "Range", labels = c("Non-native", "Native"), values = colors) +
  scale_shape_manual(name = "Range", labels = c("Non-native", "Native"), values = c("native" = 21, "invasive" = 23)) +
  geom_line(data = newdat.inf, aes(y = pred.inf), lwd = 2) + 
  theme_classic(base_size = 20) +
  theme(legend.justification = "top") 




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

ggplot(traits_v02, aes(x = range, y = infl_length_cm, fill = range)) +
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
