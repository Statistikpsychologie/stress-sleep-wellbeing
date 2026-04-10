# ---- 1. load data and Packages ----

data <- read.csv("data/data.csv")

#Packages
library(ggplot2)
library(performance)

# ---- 2. clean data ----

colSums(is.na(data))

# ---- 3. Data Exploration ----

## General overview
str(data)
summary(data)

## Outlier Analysis

# Stress
boxplot(data$stress)
stressstats <- boxplot.stats(data$stress)
stressstats$out

# Sleep Hours
boxplot(data$sleep_hours)
sleepstats <- boxplot.stats(data$sleep_hours)
sleepstats$out

# Sleep Quality
boxplot(data$sleep_quality)
sleepqstats <- boxplot.stats(data$sleep_quality)
sleepqstats$out

# Wellbeing
boxplot(data$wellbeing)
wellbeingstats <- boxplot.stats(data$wellbeing)
wellbeingstats$out

# ---- 4. Correlations ----
cor(data[, c("stress", "sleep_hours", "sleep_quality", "wellbeing")])

# ---- 5. Visualization ----
# Stress & Wellbeing
ggplot(data, aes(x = stress, y = wellbeing)) +
  geom_point() +
  geom_smooth(method = "lm")

ggsave("stress_wellbeing.png")

# Sleep Quality & Wellbeing
ggplot(data, aes(x = sleep_quality, y = wellbeing)) +
  geom_point() +
  geom_smooth(method = "lm")

ggsave("sleep_quality_wellbeing.png")

# ---- 6. Regression Models ----
# Model without interaction
model <- lm(wellbeing ~ stress + sleep_hours + sleep_quality, data = data)
summary(model)

# Model with gender as moderator
data$gender <- as.factor(data$gender)
model_mod_gender <- lm(wellbeing ~ stress * gender + sleep_hours + sleep_quality, data = data)
summary(model_mod_gender)

# Model with age as moderator
model_mod_age <- lm(wellbeing ~ stress * age + sleep_hours + sleep_quality, data = data)
summary(model_mod_age)




# ---- 7. Model Diagnostics ----
par(mfrow = c(2,2))
plot(model)

check_model(model)

# 7.1 Visualization
png("diagnostic_plots.png", width = 800, height = 800)
par(mfrow = c(2,2))
plot(model)
dev.off()

# ---- 8. Standardization ----
# Standardization for better comparability of effects
data$stress_z        <- scale(data$stress)
data$sleep_hours_z   <- scale(data$sleep_hours)
data$sleep_quality_z <- scale(data$sleep_quality)
data$wellbeing_z     <- scale(data$wellbeing)

model_z <- lm(wellbeing_z ~ stress_z + sleep_hours_z + sleep_quality_z, data = data)
summary(model_z)
