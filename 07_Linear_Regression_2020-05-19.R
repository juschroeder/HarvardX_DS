library(tidyverse)
library(dslabs)
library(data.table)
#install.packages("Lahman")
library(Lahman)



# Section 1: Introduction to regression -----------------------------------

# Q6
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(R_per_game, AB_per_game)) + 
  geom_point(alpha = 0.5)

# Q7
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  ggplot(aes(E_per_game, W_per_game)) + 
  geom_point(alpha = 0.5)

# Q8
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) + 
  geom_point(alpha = 0.5)


# 1.2 Correlation
# Q7
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  summarize(r = cor(R/G, AB/G))

# Q8
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  summarize(r = cor(W/G, E/G))
  
# Q9
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  summarize(r = cor(X3B/G, X2B/G))


# 1.3 Stratification and variance explained
set.seed(1989) #if you are using R 3.5 or earlier
#install.packages("HistData")
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
head(female_heights)

# Q1
m_mean <- mean(female_heights$mother)
m_sd <- sd(female_heights$mother)  
d_mean <- mean(female_heights$daughter)
d_sd <- sd(female_heights$daughter)

cor(female_heights$mother, female_heights$daughter)
cor <- mean(scale(female_heights$mother)*scale(female_heights$daughter))

# Q2
slope <- cor*(d_sd/m_sd)
intercept <- d_mean - slope*m_mean

# Q3
cor^2

# Q4
d_mean + cor*d_sd*(60-m_mean)/m_sd



# Section 2: Linear models ------------------------------------------------
# 2.2 Least Squares Estimates
# Q1
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)


# Q3
data <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_game = R/G, BB_game = BB/G, HR_game = HR/G)
head(data)

fit <- lm(R_game ~ BB_game + HR_game, data = data)
summary(fit)

# Q4
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# Q6
#1. 
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")
#2.
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

# Q7
set.seed(1989) #if you are using R 3.5 or earlier
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

model_fem <- lm(mother ~ daughter, data = female_heights)
model_fem$coefficients

# Q8
predicted_fem <- predict(model_fem, se.fit = TRUE)
predicted_fem$fit[1]
female_heights$mother[1]

female_heights %>% ggplot(aes(daughter, mother)) +
  geom_point() +
  geom_smooth(method = "lm")


# Q9
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99_01 <- Batting %>% filter(yearID >= 1999 & yearID <= 2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, yearID, singles, bb) %>%
  group_by(playerID) %>% 
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) 

bat_99_01 %>% filter(mean_singles > 0.2) %>% nrow
bat_99_01 %>% filter(mean_bb > 0.2) %>% nrow

# Q10
bat_99_02 <- inner_join(bat_99_01, bat_02)
cor(bat_99_02$singles, bat_99_02$mean_singles)
cor(bat_99_02$bb, bat_99_02$mean_bb)

# Q11
ggplot(bat_99_02,aes(mean_singles, singles)) +
  geom_point(alpha = 0.5)
ggplot(bat_99_02,aes(mean_bb, bb)) +
  geom_point(alpha = 0.5)

# Q12
model_singles <- lm(singles ~ mean_singles, data=bat_99_02)
model_singles$coefficients
model_bb <- lm(bb ~ mean_bb, data=bat_99_02)
model_bb$coefficients


# 2.3 Tibbles, do and broom
set.seed(1) # if you are using R 3.5 or earlier
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

# Q8
galton %>% group_by(pair) %>% filter(pair == "father_daughter") %>% nrow
galton %>% group_by(pair) %>% filter(pair == "mother_son") %>% nrow

# Q9+10
models <- galton %>% 
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int= TRUE)) %>%
  filter(term != "(Intercept)")
models

galton %>% 
  group_by(pair) %>%
  ggplot(aes(parentHeight, childHeight, col=pair)) +
  geom_smooth(method="lm")

models %>% 
  ggplot(aes(y=estimate, x=pair)) +
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, col=pair))

# 2.4: Regression & Baseball
# Q3
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
tidy(fit)
new_teams <- tibble(term = c("BB","singles","doubles","triples","HR"), teamA = c(2,4,1,0,1), teamB= c(1,6,2,1,0))
perform_teams <- new_teams %>%
  left_join(tidy(fit), by = "term") %>%
  mutate(predict_A = teamA*estimate, predict_B = teamB*estimate) %>%
  summarize(runsA = sum(predict_A), runsB = sum(predict_B))
  #mutate(predict_A = predict(fit, newdata = teamA), predict_B = predict(fit, newdata = teamB))
perform_teams

# Q9
Teams %>%
  filter(yearID == 1971) %>%
  do(tidy(lm(R ~ BB + HR, data = .)))

# Q10
fit_R_BB_HR <- Teams %>%
  filter(yearID >= 1961 & yearID <= 2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") 
fit_R_BB_HR %>%
  ggplot(aes(yearID, estimate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

# Q11
fit_R_BB_HR %>%
  ungroup() %>%
  do(tidy(lm(estimate ~ yearID, data = .), conf.int = TRUE))


# Assessment linear models
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

# Q1
#a
Teams_small %>%
  mutate(R_pg = R/G) %>%
  do(tidy(lm(avg_attendance ~ R_pg, data = .)))
Teams_small %>%
  mutate(HR_pg = HR/G) %>%
  do(tidy(lm(avg_attendance ~ HR_pg, data = .)))

#b
Teams_small %>%
  do(tidy(lm(avg_attendance ~ W, data = .)))

#c
#b
Teams_small %>%
  do(tidy(lm(avg_attendance ~ yearID, data = .)))

# Q2
Teams_small %>%
  mutate(R_pg = R/G) %>%
  summarize(cor = cor(W, R_pg))
Teams_small %>%
  mutate(HR_pg = HR/G) %>%
  summarize(cor = cor(W, HR_pg))

# Q3
Teams_small_str <- Teams_small %>%
  mutate(W_strata = round(W/10,0)) %>%
  filter(W_strata %in% 5:10)
#a
Teams_small_str %>%
  filter(W_strata == 8) %>%
  nrow
#b
Teams_small_str %>%
  mutate(R_pg = R/G) %>%
  group_by(W_strata) %>%
  do(tidy(lm(avg_attendance ~ R_pg, data = .))) %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(estimate))
Teams_small_str %>%
  mutate(HR_pg = HR/G) %>%
  group_by(W_strata) %>%
  do(tidy(lm(avg_attendance ~ HR_pg, data = .))) %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(estimate))

# Q4
fit_q4 <- Teams_small %>%
  mutate(R_pg = R/G, HR_pg = HR/G) %>%
  do(lm(avg_attendance ~ R_pg + HR_pg + W + yearID, data = .))
fit_q4

Teams_small_mod <- Teams_small %>%
  mutate(R_pg = R/G, HR_pg = HR/G)
fit_Q4 <- lm(avg_attendance ~ R_pg + HR_pg + W + yearID, data = Teams_small_mod)
tidy(fit_Q4)

# Q5
pred_2002 <- data.frame(R_pg = 5, HR_pg = 1.2, W = 80, yearID = 2002)
pred_1960 <- data.frame(R_pg = 5, HR_pg = 1.2, W = 80, yearID = 1960)

predict(fit_Q4, pred_2002)
predict(fit_Q4, pred_1960)

# Q6
Teams_mod <- Teams %>% 
  filter(yearID == 2002) %>%
  mutate(R_pg = R/G, HR_pg = HR/G, avg_attendance = attendance/G) %>%
  select(teamID, yearID, R_pg, HR_pg, W, avg_attendance) %>%
  mutate(att_hat = predict(fit_Q4, newdata = .))
cor(Teams_mod$avg_attendance, Teams_mod$att_hat)



# Section 3: Confounding --------------------------------------------------

data("research_funding_rates")
research_funding_rates

# Q1
a <- sum(research_funding_rates$awards_men)
b <- sum(research_funding_rates$awards_women)
c <- sum(research_funding_rates$applications_men) - sum(research_funding_rates$awards_men)
d <- sum(research_funding_rates$applications_women) - sum(research_funding_rates$awards_women)
tab <- tibble(men = c(a,c), women = c(b,d))
tab

# Q2
a / (a+c)
b / (b+d)

# Q3
tidy(chisq.test(tab))

# Q4
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")

dat %>%
  ggplot(aes(x = discipline, y = success, color = gender, size = applications)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

dat %>% filter(gender == "women") %>% arrange(desc(applications))

research_funding_rates %>% arrange(success_rates_total) %>% select(discipline, success_rates_total)
head(research_funding_rates)
