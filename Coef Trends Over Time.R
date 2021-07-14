
# Load and prepare data ---------------------------------------------------------------

library(tidyverse)
library(knitr)

shots <- read_csv("data/shots_2007-2019.csv")

dataex <- shots %>%
  select(season, goal, shotID, homeTeamCode, awayTeamCode, arenaAdjustedShotDistance,
         shotAngleAdjusted, shooterName, goalieNameForShot)
kable(dataex[1:5,])

  
nhl_shots <- shots %>%
  select(season, goal, arenaAdjustedShotDistance, shotAngleAdjusted, 
         goalieIdForShot, shooterPlayerId, homeSkatersOnIce, awaySkatersOnIce)

nhl_shots_2010 <- filter(nhl_shots, season == 2010)
nhl_shots_2011 <- filter(nhl_shots, season == 2011)
nhl_shots_2012 <- filter(nhl_shots, season == 2012)
nhl_shots_2013 <- filter(nhl_shots, season == 2013)
nhl_shots_2014 <- filter(nhl_shots, season == 2014)
nhl_shots_2015 <- filter(nhl_shots, season == 2015)
nhl_shots_2016 <- filter(nhl_shots, season == 2016)
nhl_shots_2017 <- filter(nhl_shots, season == 2017)
nhl_shots_2018 <- filter(nhl_shots, season == 2018)
nhl_shots_2019 <- filter(nhl_shots, season == 2019)

nhl_shots_2020 <- read_csv("data/shots_2020.csv")
nhl_shots_2020 <- nhl_shots_2020 %>%
  select(season, goal, arenaAdjustedShotDistance, shotAngleAdjusted, 
         goalieIdForShot, shooterPlayerId, homeSkatersOnIce, awaySkatersOnIce)

shots_2007_2020 <- rbind(nhl_shots, nhl_shots_2020)

# 2020 season -------------------------------------------------------------

xg_logit_2020 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = nhl_shots_2020,
                     family = "binomial")

summary(xg_logit_2020)

std_error_2020 <- coef(summary(xg_logit_2020))[, "Std. Error"]

shots_2020_sum <- nhl_shots_2020 %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2020$coefficients[3],
         distance_e = std_error_2020[3],
         angle_c =  xg_logit_2020$coefficients[2],
         angle_e =  std_error_2020[2])
shots_2020_sum <- head(shots_2020_sum, 1)


# 2019 season -------------------------------------------------------------

xg_logit_2019 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = nhl_shots_2019,
                     family = "binomial")
summary(xg_logit_2019)
std_error_2019 <- coef(summary(xg_logit_2019))[, "Std. Error"]

shots_2019_sum <- nhl_shots_2019 %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2019$coefficients[3],
         distance_e = std_error_2019[3],
         angle_c = xg_logit_2019$coefficients[2],
         angle_e = std_error_2019[2])
shots_2019_sum <- head(shots_2019_sum, 1)

# 2018 season -------------------------------------------------------------

xg_logit_2018 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = nhl_shots_2018,
                     family = "binomial")
summary(xg_logit_2018)

std_error_2018 <- coef(summary(xg_logit_2018))[, "Std. Error"]

shots_2018_sum <- nhl_shots_2018 %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2018$coefficients[3],
         distance_e = std_error_2018[3],
         angle_c = xg_logit_2018$coefficients[2],
         angle_e = std_error_2018[2])
shots_2018_sum <- head(shots_2018_sum, 1)

# 2017 season -------------------------------------------------------------

xg_logit_2017 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = nhl_shots_2017,
                     family = "binomial")
summary(xg_logit_2017)

std_error_2017 <- coef(summary(xg_logit_2017))[, "Std. Error"]

shots_2017_sum <- nhl_shots_2017 %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2017$coefficients[3],
         distance_e = std_error_2017[3],
         angle_c = xg_logit_2017$coefficients[2],
         angle_e = std_error_2017[2])
shots_2017_sum <- head(shots_2017_sum, 1)

# 2016 season -------------------------------------------------------------

xg_logit_2016 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = nhl_shots_2016,
                     family = "binomial")
summary(xg_logit_2016)

std_error_2016 <- coef(summary(xg_logit_2016))[, "Std. Error"]

shots_2016_sum <- nhl_shots_2016 %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2016$coefficients[3],
         distance_e = std_error_2016[3],
         angle_c = xg_logit_2016$coefficients[2],
         angle_e = std_error_2016[2])
shots_2016_sum <- head(shots_2016_sum, 1)

# 2015 season -------------------------------------------------------------

xg_logit_2015 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = nhl_shots_2015,
                     family = "binomial")
summary(xg_logit_2015)

std_error_2015 <- coef(summary(xg_logit_2015))[, "Std. Error"]

shots_2015_sum <- nhl_shots_2015 %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2015$coefficients[3],
         distance_e = std_error_2015[3],
         angle_c = xg_logit_2015$coefficients[2],
         angle_e = std_error_2015[2])
shots_2015_sum <- head(shots_2015_sum, 1)

# 2014 season -------------------------------------------------------------

xg_logit_2014 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = nhl_shots_2014,
                     family = "binomial")
summary(xg_logit_2014)

std_error_2014 <- coef(summary(xg_logit_2014))[, "Std. Error"]

shots_2014_sum <- nhl_shots_2014 %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2014$coefficients[3],
         distance_e = std_error_2014[3],
         angle_c = xg_logit_2014$coefficients[2],
         angle_e = std_error_2014[2])
shots_2014_sum <- head(shots_2014_sum, 1)

# 2013 season -------------------------------------------------------------

xg_logit_2013 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = nhl_shots_2013,
                     family = "binomial")
summary(xg_logit_2013)

std_error_2013 <- coef(summary(xg_logit_2013))[, "Std. Error"]

shots_2013_sum <- nhl_shots_2013 %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2013$coefficients[3],
         distance_e = std_error_2013[3],
         angle_c = xg_logit_2013$coefficients[2],
         angle_e = std_error_2013[2])
shots_2013_sum <- head(shots_2013_sum, 1)

# 2012 season -------------------------------------------------------------

xg_logit_2012 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = nhl_shots_2012,
                     family = "binomial")
summary(xg_logit_2012)

std_error_2012 <- coef(summary(xg_logit_2012))[, "Std. Error"]

shots_2012_sum <- nhl_shots_2012 %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2012$coefficients[3],
         distance_e = std_error_2012[3],
         angle_c = xg_logit_2012$coefficients[2],
         angle_e = std_error_2012[2])
shots_2012_sum <- head(shots_2012_sum, 1)

# 2011 season -------------------------------------------------------------

xg_logit_2011 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = nhl_shots_2011,
                     family = "binomial")
summary(xg_logit_2011)

std_error_2011 <- coef(summary(xg_logit_2011))[, "Std. Error"]

shots_2011_sum <- nhl_shots_2011 %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2011$coefficients[3],
         distance_e = std_error_2011[3],
         angle_c = xg_logit_2011$coefficients[2],
         angle_e = std_error_2011[2])
shots_2011_sum <- head(shots_2011_sum, 1)

# 2010 season -------------------------------------------------------------

xg_logit_2010 <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = nhl_shots_2010,
                     family = "binomial")
summary(xg_logit_2010)

std_error_2010 <- coef(summary(xg_logit_2010))[, "Std. Error"]

shots_2010_sum <- nhl_shots_2010 %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2010$coefficients[3],
         distance_e = std_error_2010[3],
         angle_c = xg_logit_2010$coefficients[2],
         angle_e = std_error_2010[2])
shots_2010_sum <- head(shots_2010_sum, 1)


# Bind datasets together --------------------------------------------------

trends <- rbind(shots_2010_sum, shots_2011_sum, shots_2012_sum, shots_2013_sum,
                shots_2014_sum, shots_2015_sum, shots_2015_sum, shots_2016_sum,
                shots_2017_sum, shots_2018_sum, shots_2019_sum, shots_2020_sum)

head(trends)

# Saving modified dataset

write.csv(trends, file = "data/trends.csv", row.names = FALSE)

# Trends over time plot ---------------------------------------------------

#Distance Coef Over Time

distance_coef <- trends %>%
  ggplot(aes(x = season, y = distance_c)) +
  geom_point() +
  scale_x_continuous(breaks = 0:2100)+
  geom_line(aes(group = 1)) +
  geom_errorbar(aes(ymin = distance_c - 2 * distance_e,
                    ymax = distance_c + 2 * distance_e),
                color = "red",
                width = .5) +
  labs(title = "xG Coefficient Trends Over Time: Distance",
       x = "Season",
       y = "Shot Distance Coefficient") +
  theme_bw()+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        plot.title = element_text(size = 20,face = "bold"))
plot(distance_coef)

# Angle Coef Over Time

trends %>%
  ggplot(aes(x = season, y = angle_c)) +
  geom_point() +
  scale_x_continuous(breaks = 0:2100)+
  geom_line(aes(group = 1)) +
  geom_errorbar(aes(ymin = angle_c - 2 * angle_e,
                    ymax = angle_c + 2 * angle_e),
                color = "red",
                width = .5) +
  labs(title = "xG Coefficient Trends Over Time: Angle",
       x = "Season",
       y = "Shot Angle Coefficient") +
  theme_bw()+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        plot.title = element_text(size = 20,face = "bold"))



shots_2020_sum %>%
  ggplot(aes(x = season, y = distance_c))+
  geom_point()+
  geom_errorbar(aes(ymin = distance_c - 2 * distance_e,
                    ymax = distance_c + 2 * distance_e),
                color = "red")+
  theme_bw()



# 5-on-5 regression -------------------------------------------------------

nhl_shots_even <- nhl_shots %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)

shots_2010_even <- filter(nhl_shots_even, season == 2010)
shots_2011_even <- filter(nhl_shots_even, season == 2011)
shots_2012_even <- filter(nhl_shots_even, season == 2012)
shots_2013_even <- filter(nhl_shots_even, season == 2013)
shots_2014_even <- filter(nhl_shots_even, season == 2014)
shots_2015_even <- filter(nhl_shots_even, season == 2015)
shots_2016_even <- filter(nhl_shots_even, season == 2016)
shots_2017_even <- filter(nhl_shots_even, season == 2017)
shots_2018_even <- filter(nhl_shots_even, season == 2018)
shots_2019_even <- filter(nhl_shots_even, season == 2019)

shots_2020_even <- nhl_shots_2020 %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)

# 2020 season Even Strength --------------------------------------------------

xg_logit_2020_even <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                     data = shots_2020_even,
                     family = "binomial")

summary(xg_logit_2020_even)

std_error_2020_even <- coef(summary(xg_logit_2020_even))[, "Std. Error"]

shots_2020_sum_even <- shots_2020_even %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2020_even$coefficients[3],
         distance_e = std_error_2020_even[3],
         angle_c =  xg_logit_2020_even$coefficients[2],
         angle_e =  std_error_2020_even[2])
shots_2020_sum_even <- head(shots_2020_sum_even, 1)

# 2019 season Even Strength -------------------------------------------------

xg_logit_2019_even <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                          data = shots_2019_even,
                          family = "binomial")

summary(xg_logit_2019_even)

std_error_2019_even <- coef(summary(xg_logit_2019_even))[, "Std. Error"]

shots_2019_sum_even <- shots_2019_even %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2019_even$coefficients[3],
         distance_e = std_error_2019_even[3],
         angle_c =  xg_logit_2019_even$coefficients[2],
         angle_e =  std_error_2019_even[2])
shots_2019_sum_even <- head(shots_2019_sum_even, 1)

# 2018 season Even Strength -------------------------------------------------

xg_logit_2018_even <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                          data = shots_2018_even,
                          family = "binomial")

summary(xg_logit_2018_even)

std_error_2018_even <- coef(summary(xg_logit_2018_even))[, "Std. Error"]

shots_2018_sum_even <- shots_2018_even %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2018_even$coefficients[3],
         distance_e = std_error_2018_even[3],
         angle_c =  xg_logit_2018_even$coefficients[2],
         angle_e =  std_error_2018_even[2])
shots_2018_sum_even <- head(shots_2018_sum_even, 1)

# 2017 season Even Strength -------------------------------------------------

xg_logit_2017_even <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                          data = shots_2017_even,
                          family = "binomial")

summary(xg_logit_2017_even)

std_error_2017_even <- coef(summary(xg_logit_2017_even))[, "Std. Error"]

shots_2017_sum_even <- shots_2017_even %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2017_even$coefficients[3],
         distance_e = std_error_2017_even[3],
         angle_c =  xg_logit_2017_even$coefficients[2],
         angle_e =  std_error_2017_even[2])
shots_2017_sum_even <- head(shots_2017_sum_even, 1)

# 2016 season Even Strength -------------------------------------------------

xg_logit_2016_even <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                          data = shots_2016_even,
                          family = "binomial")

summary(xg_logit_2016_even)

std_error_2016_even <- coef(summary(xg_logit_2016_even))[, "Std. Error"]

shots_2016_sum_even <- shots_2016_even %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2016_even$coefficients[3],
         distance_e = std_error_2016_even[3],
         angle_c =  xg_logit_2016_even$coefficients[2],
         angle_e =  std_error_2016_even[2])
shots_2016_sum_even <- head(shots_2016_sum_even, 1)

# 2015 season Even Strength -------------------------------------------------

xg_logit_2015_even <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                          data = shots_2015_even,
                          family = "binomial")

summary(xg_logit_2015_even)

std_error_2015_even <- coef(summary(xg_logit_2015_even))[, "Std. Error"]

shots_2015_sum_even <- shots_2015_even %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2015_even$coefficients[3],
         distance_e = std_error_2015_even[3],
         angle_c =  xg_logit_2015_even$coefficients[2],
         angle_e =  std_error_2015_even[2])
shots_2015_sum_even <- head(shots_2015_sum_even, 1)

# 2014 season Even Strength -------------------------------------------------

xg_logit_2014_even <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                          data = shots_2014_even,
                          family = "binomial")

summary(xg_logit_2014_even)

std_error_2014_even <- coef(summary(xg_logit_2014_even))[, "Std. Error"]

shots_2014_sum_even <- shots_2014_even %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2014_even$coefficients[3],
         distance_e = std_error_2014_even[3],
         angle_c =  xg_logit_2014_even$coefficients[2],
         angle_e =  std_error_2014_even[2])
shots_2014_sum_even <- head(shots_2014_sum_even, 1)

# 2013 season Even Strength -------------------------------------------------

xg_logit_2013_even <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                          data = shots_2013_even,
                          family = "binomial")

summary(xg_logit_2013_even)

std_error_2013_even <- coef(summary(xg_logit_2019_even))[, "Std. Error"]

shots_2013_sum_even <- shots_2013_even %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2013_even$coefficients[3],
         distance_e = std_error_2013_even[3],
         angle_c =  xg_logit_2013_even$coefficients[2],
         angle_e =  std_error_2013_even[2])
shots_2013_sum_even <- head(shots_2013_sum_even, 1)

# 2012 season Even Strength -------------------------------------------------

xg_logit_2012_even <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                          data = shots_2012_even,
                          family = "binomial")

summary(xg_logit_2012_even)

std_error_2012_even <- coef(summary(xg_logit_2012_even))[, "Std. Error"]

shots_2012_sum_even <- shots_2012_even %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2012_even$coefficients[3],
         distance_e = std_error_2012_even[3],
         angle_c =  xg_logit_2012_even$coefficients[2],
         angle_e =  std_error_2012_even[2])
shots_2012_sum_even <- head(shots_2012_sum_even, 1)

# 2011 season Even Strength -------------------------------------------------

xg_logit_2011_even <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                          data = shots_2011_even,
                          family = "binomial")

summary(xg_logit_2011_even)

std_error_2011_even <- coef(summary(xg_logit_2011_even))[, "Std. Error"]

shots_2011_sum_even <- shots_2011_even %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2011_even$coefficients[3],
         distance_e = std_error_2011_even[3],
         angle_c =  xg_logit_2011_even$coefficients[2],
         angle_e =  std_error_2011_even[2])
shots_2011_sum_even <- head(shots_2011_sum_even, 1)

# 2010 season Even Strength -------------------------------------------------

xg_logit_2010_even <- glm(goal ~ shotAngleAdjusted + arenaAdjustedShotDistance,
                          data = shots_2010_even,
                          family = "binomial")

summary(xg_logit_2010_even)

std_error_2010_even <- coef(summary(xg_logit_2010_even))[, "Std. Error"]

shots_2010_sum_even <- shots_2010_even %>%
  select(season) %>%
  mutate(distance_c = xg_logit_2010_even$coefficients[3],
         distance_e = std_error_2010_even[3],
         angle_c =  xg_logit_2010_even$coefficients[2],
         angle_e =  std_error_2010_even[2])
shots_2010_sum_even <- head(shots_2010_sum_even, 1)

# Bind datasets together Even Strength --------------------------------------

trends_even <- rbind(shots_2010_sum_even, shots_2011_sum_even, shots_2012_sum_even, 
                     shots_2013_sum_even, shots_2014_sum_even, shots_2015_sum_even,
                     shots_2015_sum_even, shots_2016_sum_even, shots_2017_sum_even, 
                     shots_2018_sum_even, shots_2019_sum_even, shots_2020_sum_even)

head(trends_even)

# Saving modified dataset

write.csv(trends_even, file = "data/trends_even.csv", row.names = FALSE)

# Trends over time plot Even Strength------------------------------------------

#Distance Coef Over Time Even Strength

trends_even %>%
  ggplot(aes(x = season, y = distance_c)) +
  geom_point() +
  scale_x_continuous(breaks = 0:2100)+
  geom_line(aes(group = 1)) +
  geom_errorbar(aes(ymin = distance_c - 2 * distance_e,
                    ymax = distance_c + 2 * distance_e),
                color = "red",
                width = .5) +
  labs(title = "xG Coefficient Trends Over Time: Distance, 5-on-5",
       x = "Season",
       y = "Shot Distance Coefficient") +
  theme_bw()+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        plot.title = element_text(size = 20,face = "bold"))

# Angle Coef Over Time Even Strength

trends_even %>%
  ggplot(aes(x = season, y = angle_c)) +
  geom_point() +
  scale_x_continuous(breaks = 0:2100)+
  geom_line(aes(group = 1)) +
  geom_errorbar(aes(ymin = angle_c - 2 * angle_e,
                    ymax = angle_c + 2 * angle_e),
                color = "red",
                width = .5) +
  labs(title = "xG Coefficient Trends Over Time: Angle, 5-on-5",
       x = "Season",
       y = "Shot Angle Coefficient") +
  theme_bw()+
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        plot.title = element_text(size = 20,face = "bold"))

