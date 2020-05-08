library(dslabs)
library(ggplot2)
library(dplyr)
library(gtools)
library(tidyverse)
options(digits = 3)
data("brexit_polls")
p <- 0.481 #Actual Remain Result
d <- 2*p - 1 #Official Spread
N <- 1500
E <- p * N #Expected Number of Remain Voters
SE <- N * sqrt(p*(1-p))
SE
E_hat <- p #Expected value of X bar
SE_hat <- sqrt((p*(1-p))/N)
SE_hat
E_spread <- 2*E_hat - 1
E_spread
SE_spread <- 2 * SE_hat
SE_spread
brexit_polls <-  brexit_polls %>% mutate(x_hat = (spread + 1)/2)
spread_avg <- mean(brexit_polls$spread)
spread_avg
spread_sd <- sd(brexit_polls$spread)
spread_sd
avg_xhat <- (sum(brexit_polls$x_hat)/length(brexit_polls$x_hat))
avg_xhat
sd_xhat <- sd(brexit_polls$x_hat)
sd_xhat
lower <- (brexit_polls[1,]$x_hat - qnorm(0.975)*((sqrt(brexit_polls[1,]$x_hat*(1 - brexit_polls[1,]$x_hat)/brexit_polls[1,]$samplesize))))
lower
upper <- (brexit_polls[1,]$x_hat + qnorm(0.975)*((sqrt(brexit_polls[1,]$x_hat*(1 - brexit_polls[1,]$x_hat)/brexit_polls[1,]$samplesize))))
upper
d_true <- -0.038
june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = (sqrt((x_hat*(1-x_hat))/samplesize))) %>%
  mutate(se_spread = 2 * se_x_hat) %>%
  mutate(lower = spread - qnorm(0.975)*se_spread, upper = spread + qnorm(0.975)*se_spread) %>% mutate(hit = lower <= d_true & upper >= d_true)
nrow(june_polls) # Number of Polls in June
sum(june_polls$lower<=0 & june_polls$upper>=0) / nrow(june_polls) #Proportion of polls having confidence interval that covers the value 0
sum(june_polls$lower<=d_true & june_polls$upper>=d_true) / nrow(june_polls) #Proportion of polls having confidence interval covering the true value of d
sum(june_polls$lower>=0 & june_polls$upper>=0) / nrow(june_polls) #Proportion of polls with confidence interval covering the Remain value
june_grouped <- june_polls %>% group_by(pollster) %>% summarise(phits = mean(hit), n = n()) %>% arrange(desc(phits))
june_grouped
june_polls %>% ggplot(aes(poll_type, spread)) + geom_boxplot() + geom_point() #Boxplot of spread by poll type
combined_by_type <- june_polls %>% group_by(poll_type) %>%
  summarise(N = sum(samplesize), spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type
cis <- combined_by_type %>% mutate(li = spread - qnorm(0.975)*2*(sqrt(p_hat*(1- p_hat)/N)), ui = spread + qnorm(0.975)*2*(sqrt(p_hat*(1- p_hat)/N))) #confidence interval for spread
cis
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(0.975)*se_spread,
         spread_upper = spread + qnorm(0.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
two_table <- brexit_hit %>% group_by(poll_type, hit) %>% summarize(hit_status = n()) %>%
  spread(poll_type, hit_status)
two_table
chi_test <- two_table %>% select(-hit) %>% chisq.test()
chi_test$p.value
odds_online <- with(two_table, (Online[2]/sum(Online)) / (Online[1]/sum(Online)))
odds_online
odds_telephone <- with(two_table, (Telephone[2]/sum(Telephone)) / (Telephone[1]/sum(Telephone)))
odds_telephone
odds_online / odds_telephone
brexit_polls %>% ggplot(aes(enddate, spread, col = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) + geom_point() +
  geom_hline(aes(yintercept = -0.038), col = "black")
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long %>% ggplot(aes(enddate,proportion, col = vote)) +
  geom_smooth(method = "loess", span = 0.3) + geom_point()
