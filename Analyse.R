# ---- 1. Daten laden ----

data <- read.csv("data.csv")

#Packages
library(ggplot2)
library(performance)

# ---- 2. Daten bereinigen ----

# Fehlende Werte prüfen
colSums(is.na(data))

# ---- 3. Exploration ----

## Allgemeine Übersicht
str(data)
summary(data)

## Ausreißer-Analyse

# Stress
boxplot(data$stress)
stressstats <- boxplot.stats(data$stress)
stressstats$out

# Schlafstunden
boxplot(data$sleep_hours)
sleepstats <- boxplot.stats(data$sleep_hours)
sleepstats$out

# Schlafqualität
boxplot(data$sleep_quality)
sleepqstats <- boxplot.stats(data$sleep_quality)
sleepqstats$out

# Wohlbefinden
boxplot(data$wellbeing)
wellbeingstats <- boxplot.stats(data$wellbeing)
wellbeingstats$out

# ---- 4. Korrelationen ----
cor(data[, c("stress", "sleep_hours", "sleep_quality", "wellbeing")])

# ---- 5. Visualisierung ----
# Stress & Wohlbefinden
ggplot(data, aes(x = stress, y = wellbeing)) +
  geom_point() +
  geom_smooth(method = "lm")

ggsave("stress_wellbeing.png")

# Schlafqualität & Wohlbefinden
ggplot(data, aes(x = sleep_quality, y = wellbeing)) +
  geom_point() +
  geom_smooth(method = "lm")

ggsave("sleep_quality_wellbeing.png")

# ---- 6. Modelle ----
# Modell ohne Moderation
model <- lm(wellbeing ~ stress + sleep_hours + sleep_quality, data = data)
summary(model)

# Modell mit Gender und Stress als Moderation
data$gender <- as.factor(data$gender)
model_mod_gender <- lm(wellbeing ~ stress * gender + sleep_hours + sleep_quality, data = data)
summary(model_mod_gender)

# Modell mit Stress und Alter als Moderation
model_mod_age <- lm(wellbeing ~ stress * age + sleep_hours + sleep_quality, data = data)
summary(model_mod_age)




# ---- 7. Diagnostik ----
par(mfrow = c(2,2))
plot(model)

check_model(model)

# 7.1 Visualisierung
png("diagnostic_plots.png", width = 800, height = 800)
par(mfrow = c(2,2))
plot(model)
dev.off()

# ---- 8. Standardisierung ----
# Standardisierung zur besseren Vergleichbarkeit der Effekte
data$stress_z        <- scale(data$stress)
data$sleep_hours_z   <- scale(data$sleep_hours)
data$sleep_quality_z <- scale(data$sleep_quality)
data$wellbeing_z     <- scale(data$wellbeing)

model_z <- lm(wellbeing_z ~ stress_z + sleep_hours_z + sleep_quality_z, data = data)
summary(model_z)
