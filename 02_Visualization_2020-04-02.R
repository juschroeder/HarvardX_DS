library(tidyverse)



# Section 5: Data visualization principles --------------------------------

options(digits = 3)
install.packages("titanic")
library(titanic)

# Q1
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
?titanic_train

# Q2
titanic %>%
  ggplot(aes(x=Age, y=..count.., fill=Sex)) +
  geom_density(alpha=.2)

titanic_f <- titanic %>% 
  filter(Sex == "female" & !is.na(Age))
titanic_m <- titanic %>% 
  filter(Sex == "male" & !is.na(Age)) 

# proportion of males age 18-35 higher that that of females
pnorm(35, mean(titanic_f$Age), sd(titanic_f$Age)) - pnorm(18, mean(titanic_f$Age), sd(titanic_f$Age)) # 0.451
pnorm(35, mean(titanic_m$Age), sd(titanic_m$Age)) - pnorm(18, mean(titanic_m$Age), sd(titanic_m$Age)) # 0.422
# -> false --> actually correct, but data not completely normal

# proportion of females under 17 higher than that of males
pnorm(17, mean(titanic_f$Age), sd(titanic_f$Age)) # 0.22
pnorm(17, mean(titanic_m$Age), sd(tigettanic_m$Age)) # 0.175

titanic[which.max(titanic$Age),]


# Q3
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(sample=Age)) +
  geom_qq(dparams=params) +
  geom_abline()


# Q4
titanic %>% 
  ggplot(aes(x=Sex, fill=Survived)) +
  geom_bar(position=position_dodge())
titanic %>% count(Survived)

# Q5
titanic %>% 
  ggplot(aes(x=Age, fill=Survived)) +
  geom_density(alpha=0.2)


# Q6
titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(x=Survived, y=Fare)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.2) +
  scale_y_continuous(trans="log2")

# Q7
titanic %>%
  ggplot(aes(x=Pclass, fill=Survived)) +
  geom_bar()

titanic %>%
  ggplot(aes(x=Pclass, fill=Survived)) +
  geom_bar(position = position_fill())

titanic %>%
  ggplot(aes(x=Survived, fill= Pclass)) +
  geom_bar(position = position_fill())


# Q8
titanic %>%
  ggplot(aes(x=Age, y=..count.., fill=Survived)) +
  geom_density(alpha=.2) +
  facet_grid(Sex~Pclass)



# Assessment --------------------------------------------------------------

# Part 1: Properties of stars
library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3) 

# Q1:
mean(stars$magnitude)
sd(stars$magnitude)

# Q2: density plot
ggplot(stars, aes(x=magnitude)) +
  geom_density(alpha=0.2)

# Q3: distribution of temp
ggplot(stars, aes(x=temp)) +
  geom_density(alpha=0.2)

# Q4: scatter plot
ggplot(stars, aes(x=temp, y=magnitude)) +
  geom_point()

# Q5:
stars %>% #filter(star == "Sun") %>%
ggplot(aes(x=(temp), y=magnitude, label=star)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_reverse() +
  geom_text(nudge_y = 0.5)

# Q9:
stars %>% filter(type == "G") %>%
  ggplot(aes(x=(temp), y=magnitude, color=type, label=type)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_reverse() +
  geom_text(nudge_y = 0.5)


  
# Part2: Climate Change 
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
  
# Q1:
head(temp_carbon)
temp_carbon %>% .$year %>% max()  
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

# Q2:
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  min()
temp_carbon[which.min(temp_carbon$year),]
temp_carbon[temp_carbon$year == 2014,"carbon_emissions"]/temp_carbon[temp_carbon$year == 1751,"carbon_emissions"]

# Q3:
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  .$year %>%
  min()
temp_carbon[temp_carbon$year == 1880,]
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  .$year %>%
  max()
temp_carbon[temp_carbon$year == 2018,]
temp_carbon[temp_carbon$year >= 2010,]
temp_carbon[which.max(temp_carbon$temp_anomaly),]
temp_carbon[which.min(temp_carbon$temp_anomaly),]
temp_carbon[temp_carbon$year == 2018,"temp_anomaly"]-temp_carbon[temp_carbon$year == 1880,"temp_anomaly"]

# Q4:
mean(temp_carbon$temp_anomaly,na.rm=TRUE)
p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(x=year, y=temp_anomaly)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), col = "blue")
p

# Q5:
p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

# Q/:
p + geom_line(aes(x=year, y=ocean_anomaly), col="blue") +
  geom_line(aes(x=year, y=land_anomaly), col="green") 

# Q8:
head(greenhouse_gases)
greenhouse_gases %>%
  ggplot(aes(x=year, y=concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

# Q10:
head(temp_carbon)
ggplot(temp_carbon, aes(x=year, y=carbon_emissions)) +
  geom_line()
temp_carbon[temp_carbon$year == 1960,]
temp_carbon[temp_carbon$year == 2014,]

# Q11:
head(historic_co2)
co_time <- ggplot(historic_co2, aes(x=year/1e6, y=co2, col=source)) +
  geom_line()
co_time

# Q12:
co_time + xlim(c(-0.8,-0.775))
co_time + xlim(c(-0.375,-0.33))
co_time + xlim(c(-0.14,-0.12))
co_time + xlim(c(-0.003,0.002018))



