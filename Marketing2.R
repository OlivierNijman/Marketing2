setwd("~/Marketing")
rm(list=ls())
library(mlogit) 
library(gmnl)

robots <- read.csv("Robots_CBC_2.csv")	
n <- 112
c <- 10
m <- 4
# convert data to work with the mlogit format. makes dummy TRUE or FALSE
robots <- mlogit.data(robots, choice="Selection_Dummy", shape="long", alt.var ="Alternative_id")

# calculate models


# start with a partworth model
ml1<- mlogit(Selection_Dummy ~ Design.1.Machine + Design.2.Facial.expressions+ Social.Interaction.1.Passive + 
               Level.of.Autonomy.1.Passive + Level.of.Autonomy.2.Assertive + Price.199 + Price.249 + Price.249.Discount + 
               Price.299 +  
               User.Rating.3.8.stars + User.Rating.4.1.stars + User.Rating.4.5.stars + 
               None_option | 0, robots) # note: the "| 0" part means that no alternative-specific constants should be considered
summary(ml1)

#log-likelihood NULL model, chisq test
ll_0 <- n*c*log(1/m)
ll_beta <- as.numeric(ml1$logLik)
df <- 13

lltest <- -2*(LL_0 -ml1$logLik)

p_value <- 1-pchisq(lltest, df = 13)

# adjusted R squared, normal_r is considered acceptable (0.219) < 0.4
normal_r <- 1- ll_beta/ll_0
adjusted_r <- (ll_beta - df)/ll_0

