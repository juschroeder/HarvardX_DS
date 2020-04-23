library(tidyverse)
#install.packages("gtools")
library(gtools)
library(dslabs)


# Section 1: Discrete probability -----------------------------------------
# Q1: Olympic running
# a: How many different ways can the 3 medals be distributed across 8 runners?
runners <- c(letters[seq( from = 1, to = 8)])
medals <- c(1,2,3)
combi <- permutations(8,3,runners, set=FALSE)
nrow(combi)

# b: among 3 runners from Jamaica?
nrow(permutations(3,3,runners, set=FALSE))

# c: probability that all medals are won by Jamaica?
countries <- c(rep(1,3),rep(0,5))
combi_countries <- permutations(8,3,countries,set=FALSE)
results <- rowSums(combi_countries)
mean(results == 3)

# d: Monte Carlo simulation
set.seed(1, sample.kind="Rounding")
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
sim <- replicate(10000,{
  res <- sample(runners,3)
  identical(res,c(rep("Jamaica",3)))
})
mean(sim)


# Q2: 

# a: number of possible combinations 
combi_sides <- combinations(6,2,sides,repeats.allowed = FALSE)
nrow(combi_sides)*6*2
# 180

# b: number of possible combinations with 3 drink options
nrow(combi_sides)*6*3
# 270

# c: number of possible combinations with selection of 3 sides
combi_sides <- combinations(6,3)
nrow(combi_sides)*6*3

# d: min number of entree options for 365 combinations
n_entree <- seq(6,12)

meals <- function(n){
  x <- n*nrow(combinations(6,2))*3
  print(x)
}

opts <- sapply(n_entree, meals)
df <- data.frame(n_entree, opts)
df

# e: min number of sides for 365 combinations
n_sides <- seq(6,12)

meals <- function(n){
  6*nrow(combinations(n,2))*3
}

opts <- sapply(n_sides, meals)
df <- data.frame(n_sides, opts)
df


# Q3: esophageal cancer + alcohol/tobacco use
head(esoph)

# a: n of groups in study
nrow(esoph)

# b: n of cases
all_cases <- sum(esoph$ncases)

# c: n of controls
all_controls <- sum(esoph$ncontrols)

# Q4
# a: probability of subject in highes alcohol consumption group is case
class(esoph$alcgp)
max(esoph$alcgp)
max_alc <- esoph[esoph$alcgp == "120+",]
max_alc_cases <- sum(max_alc$ncases)
max_alc_cons <- sum(max_alc$ncontrols)
max_alc_cases/(max_alc_cases+max_alc_cons)

# b: same for lowest alcohol consumption group
min(esoph$alcgp)
min_alc <- esoph[esoph$alcgp == "0-39g/day",]
min_alc_cases <- sum(min_alc$ncases)
min_alc_cons <- sum(min_alc$ncontrols)
min_alc_cases/(min_alc_cases+min_alc_cons)

# c: for cases, probability to smoke ≥10g/day
levels(esoph$tobgp)
cases_smoker <- esoph %>% filter(tobgp >= "10-19") %>% summarize(sum(ncases)) %>% .$`sum(ncases)`
cases_smoker/all_cases

# d: same for controls
cons_smoker <- esoph %>% filter(tobgp >= "10-19") %>% summarize(sum(ncontrols)) %>% .$`sum(ncontrols)`
cons_smoker/all_controls


# Q5:
# a: probability of case being in highest alcohol group
cases_alc <- esoph %>% filter(alcgp == "120+") %>% summarize(sum(ncases)) %>% .$`sum(ncases)`
p_cases_alc <- cases_alc/all_cases

# b: in highest tobacco group
cases_max_smoker <- esoph %>% filter(tobgp == "30+") %>% summarize(sum(ncases)) %>% .$`sum(ncases)`
p_cases_max_smoker <- cases_max_smoker/all_cases

# c: in both
cases_max_both <- esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>% summarize(sum(ncases)) %>% .$`sum(ncases)`
cases_max_both/all_cases

# d: one or the other
cases_max_one <- esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(sum(ncases)) %>% .$`sum(ncases)`
p_cases_max_one <- cases_max_one/all_cases


# Q6
# a: probability for control being in highest alcohol group
controls_alc <- esoph %>% filter(alcgp == "120+") %>% summarize(sum(ncontrols)) %>% .$`sum(ncontrols)`
p_controls_alc <- controls_alc/all_controls

# b: how many times more likely are cases that controls to be in highest alcohol group?
p_cases_alc/p_controls_alc

# c: controls highest tobacco group
controls_max_smoker <- esoph %>% filter(tobgp == "30+") %>% summarize(sum(ncontrols)) %>% .$`sum(ncontrols)`
p_controls_max_smoker <- controls_max_smoker/all_controls

# d: in both
controls_max_both <- esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>% summarize(sum(ncontrols)) %>% .$`sum(ncontrols)`
controls_max_both/all_controls

# e: one or the other
controls_max_one <- esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(sum(ncontrols)) %>% .$`sum(ncontrols)`
p_controls_max_one <- controls_max_one/all_controls

# f: how many times more likely
p_cases_max_one/p_controls_max_one




# Section 2: Continuous probabilities -------------------------------------

# Q1: ACT scores
set.seed(16)
act_scores <- rnorm(10000, 20.9, 5.7)

# a: mean
mean <- mean(act_scores)

# b: SD
sd <- sd(act_scores)

# c: how many perfect scores ≥36
sum(act_scores >= 36)

# d: probability of > 30
1- pnorm(30,mean,sd)

# e: probability of ≤ 10
pnorm(10,mean,sd)

# Q2:
x <- seq(1,36)
f_x <- dnorm(x, 20.9, 5.7)
plot(x,f_x)

# Q3: z-score
x_z <- (x-mean(x))/sd(x)
# a: probability of z ≥ 2
1-pnorm(2)

# b: which ACT score corresponds to Z=2
mean(act_scores)+2*sd(act_scores)

# c: 97.5th percnetile of act_scores
qnorm(0.975, mean(act_scores), sd(act_scores))


# Q4: write CDF and apply to range of 1-36
x <- seq(1,36)
cdf_act <- function(a) mean(act_scores <= a)
p_act <- sapply(x,cdf_act)
act <- data.frame(x,p_act)
head(act)
sum(p_act)

# a: min integer score so that probability of score or lower is at least 0.95
# You should use the variable act_scores you defined previously. 
# You need to find a score x from 1- 36 so that the probability of act_scores <= x is 0.95.
which(p_act <= 0.95)
# correct answer: 31 -> 30 only has p=0.9473

# b: 95th percentile of scores
qnorm(0.95, 20.9, 5.7)

# c: percentile of score 26
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
sample_quantiles[which(sample_quantiles >= 25.5 & sample_quantiles <= 26.5)]
# 82nd percentile

# d: theoretical quantile
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
plot(theoretical_quantiles, sample_quantiles)
abline(0,1)




# Section 3: Random variables, sampling models and the CLT -------------

# Q1: SAT testing
# a: probability for guessing correctly on one question
1/5

# b: expected value for guessing 1 question
e1 <- 1*0.2 + -0.25*0.8
e1

# c: expected value for guessing 44 questions
e_all <- 44*e1
e_all

# d: standard error of guessing on all 44 questions
sd_all <- sqrt(44)*abs(-0.25-1)*sqrt(0.2*0.8)
sd_all

# e: use CLT to guess probability of ≥8
1 - pnorm(8,e_all,sd_all)

# f: use Monte Carlo simulation
set.seed(21)
B <- 10000
S <- replicate(B,{
  X <- sample(c(1,-0.25),44,replace=TRUE,prob=c(0.2,0.8))
  sum(X) >= 8
})
mean(S)


# Q2
# a: expected value on 44 questions
exp <- 44*(1*0.25+0*0.8)
exp

# b: lowest p so that probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)
se <- sqrt(44)*abs(0-1)*sqrt(0.25*0.75)

p <- 0.95
sat <- function(p){
  exp <- 44*(1*p + 0*(1-p))
  se <- sqrt(44)*abs(0-1)*sqrt(p*(1-p))
  1-pnorm(35,exp,se)
}
sat(p)

p_sat <- sapply(p,sat)
df <- data.frame(p,p_sat)
which(p_sat > 0.8)
df[13,]


# Q3: roulette
# a: expected value for 1 bet
win <- 5/38
lose <- 1-win
exp <- 6*win+(-1)*lose
exp

# b: SE for 1 bet
se <- abs(-1-6)*sqrt(win*lose)
se

# c: expected value of average payout over 500 bets
# expected value of average of draws = original expected value
exp

# d: SE of average of 500 bets
se/sqrt(500)

# e: expected value of sum of 500 bets
exp_500 <- exp*500
exp_500

# f: SE of sum of 500 bets
se_500 <- sqrt(500)*se
se_500

# g: probability of losing money over 500 bets
pnorm(0,exp_500, se_500)



# Section 4: The Big Short ------------------------------------------------
data(death_prob)
head(death_prob)
loss <- -150000
gain <- 1150
n <- 1000

# Q1: insurance rates
# a: death probability of 50yo female
p <- death_prob %>% filter(sex == "Female" & age == 50) %>% .$prob

# b: expected value for 1 female
(loss*p + gain*(1 - p))

# c: SE for 1 female
abs(gain-loss)*sqrt(p*(1-p))

# d: expected value 1000 females
mu <- (loss*p + gain*(1 - p))*n

# e: SE for 1000 females
sigma <- abs(gain-loss)*sqrt(p*(1-p))*sqrt(n)

# f: use CLT for probability that insurance company loses money
pnorm(0,mu,sigma)


# Q2: 50yo male
# a: probability of death of 50yo male
p <- death_prob %>% filter(sex == "Male" & age == 50) %>% .$prob

# b: premium
a <- -150000
premium <- ((700000-n*a*p)/(1-p))/1000

# c: SE of sum of 1000 premiums
sigma <- sqrt(1000)*abs(a-premium)*sqrt(p*(1-p))

# d: probability to lose monea on 1000 50yo males
mu <- n*(a*p + premium*(1-p))
pnorm(0,mu,sigma)

# Q3
n <- 1000
a <- -150000
b <- 1150
p <- 0.015

# a: expected value over 1000 policies
mu <- n*(a*p+b*(1-p))

# b: SE over 1000 policies
sigma <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))

# c: probability of losing money
pnorm(0,mu,sigma)

# d: probability of losing more than 1 million
pnorm(-1e6,mu,sigma)

# e: lowest death probability for chance of losing money > 90%
p <- seq(0.01,0.03,0.001)
a <- -150000
b <- 1150
n <- 1000

prob_lose <- function(p){
  mu <- n*(a*p+b*(1-p))
  sigma <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))
  pnorm(0,mu,sigma)
}
prob <- sapply(p,prob_lose)

df <- data.frame(p=p,prob=prob)
df %>% filter(prob > 0.9)

# f: same for losing 1mio, new death probs
p <- seq(0.01,0.03,0.0025)
prob_lose <- function(p){
  mu <- n*(a*p+b*(1-p))
  sigma <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))
  pnorm(-1e6,mu,sigma)
}
prob <- sapply(p,prob_lose)
df <- data.frame(p=p,prob=prob)
df %>% filter(prob > 0.9)

# Q4:
# a: reported profit in millions
p_loss <- 0.015
a <- -150000
b <- 1150
set.seed(25)
n <- 1000

default <- sample(c(-150000,1150),n,replace=TRUE,prob=c(p_loss,(1-p_loss)))
sum(default)/1e6

# b: probabilty of losing 1mio or more
set.seed(27)
B <- 10000
default <- replicate(B,{
  sample <- sample(c(-150000,1150),n,replace=TRUE,prob=c(p_loss,(1-p_loss)))
  sum(sample) <= -1e6
})
mean(default)

# Q5:
# a: premium
p <- 0.015
n <- 1000
l <- -150000
z <- qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

# b: expected profit per policy
l*p + x*(1-p)

# c: expeted profit over 1000 policies
n*(l*p + x*(1-p))

# d: MCsim 
B <- 10000
set.seed(28)
defaults <- replicate(B,{
  sample <- sample(c(l,x), n, replace=TRUE, prob = c(p,(1-p)))
  sum(sample) < 0
})
mean(defaults)

# Q6: MCsim
set.seed(29)
defaults <- replicate(B,{
  p <- 0.015 + sample(seq(-0.01,0.01,length=100),1)
  sample <- sample(c(l,x), n, replace=TRUE, prob = c(p,(1-p)))
  sum(sample)
})
# a: expected value
mean(defaults)

# b: probability of losing money
mean(defaults < 0)

# b: probability of losing more than 1mio
mean(defaults < -1e6)

