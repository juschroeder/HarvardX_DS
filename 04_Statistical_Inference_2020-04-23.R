library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

# Q1: expected value and SE of a poll
N <- 1500

# expected value of total number of voters choosing remain
N*p 
# SE
sqrt(N*p*(1-p)) 

# expected value of X_hat (proportion of remain voters)
p
# SE
sqrt(p*(1-p)/N)

# expected value of spread d
p -(1-p)
# SE
2*sqrt(p*(1-p)/N)


# Q2: actual Brexti poll estimates
head(brexit_polls)

brexit_polls <- brexit_polls %>% 
  mutate(x_hat = (spread +1)/2)
mean(brexit_polls$spread)
sd(brexit_polls$spread)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

# Q3: CI of a Brexit poll
brexit_polls[1,]
brexit_polls[1,"x_hat"]-qnorm(0.975)*sqrt(brexit_polls[1,"x_hat"]*(1-brexit_polls[1,"x_hat"])/brexit_polls[1,"samplesize"])
brexit_polls[1,"x_hat"]+qnorm(0.975)*sqrt(brexit_polls[1,"x_hat"]*(1-brexit_polls[1,"x_hat"])/brexit_polls[1,"samplesize"])

# Q4: CI for polls in June
june_polls <- brexit_polls %>% 
  filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize), 
         se_spread = 2*se_x_hat, 
         lower = (spread - qnorm(0.975)*se_spread),
         upper = (spread + qnorm(0.975)*se_spread), 
         hit = ifelse(-0.038 >= lower& -0.038 <= upper,TRUE,FALSE),
         cover0 = ifelse(lower <= 0 & upper >= 0,TRUE,FALSE),
         over0 = ifelse(lower >= 0 & upper >= 0,TRUE,FALSE)) 

nrow(june_polls)
mean(june_polls$cover0)
mean(june_polls$over0)
mean(june_polls$hit)

# Q5: hit rate by pollster
hits_pollster <- june_polls %>% 
  group_by(pollster) %>% 
  summarize(hit_rate  = mean(hit), n=n()) %>%
  arrange(desc(hit_rate))

hits_pollster %>% select(pollster,hit_rate,n)

# Q6: Boxplot by poll type
ggplot(june_polls, aes(x=poll_type,y=spread))+
  geom_boxplot() +
  geom_point()

# Q7: combined spread across poll type
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

combined_by_type <- combined_by_type %>%
  mutate(se_spread = 2*sqrt(p_hat*(1-p_hat)/N),
         lower = spread - qnorm(0.975)*se_spread,
         upper = spread + qnorm(0.975)*se_spread,
         length = upper - lower)
combined_by_type

# Q9: chi-squared p-value
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
brexit_hit %>% count(poll_type,hit)
a <- brexit_hit %>% filter(poll_type == "Online" & hit == TRUE) %>% nrow()
c <- brexit_hit %>% filter(poll_type == "Online" & hit == FALSE) %>% nrow()
b <- brexit_hit %>% filter(poll_type == "Telephone" & hit == TRUE) %>% nrow()
d <- brexit_hit %>% filter(poll_type == "Telephone" & hit == FALSE) %>% nrow()

df <- data.frame("Online"=c(a,c),"Telephone"=c(b,d))
rownames(df) <- c("true","false")
df
chisq.test(df)

# Q10: OR of hit rates
odds_online <- (a/(a+c))/(c/(a+c))
odds_online
odds_phone <- (b/(b+d))/(d/(b+d))
odds_phone

or <- (a*d)/(b*c)
or

# Q11: plotting spread over time
ggplot(brexit_polls, aes(enddate, spread, color=poll_type)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.4) +
  geom_hline(yintercept = -0.038)

# Q12: plotting raw percentages over time
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

ggplot(brexit_long, aes(enddate, proportion, color=vote))+
  geom_point() +
  geom_smooth(method = "loess", span = 0.3)



