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

lltest <- -2*(ll_0 -ml1$logLik)

p_value <- 1-pchisq(lltest, df = 13)

# adjusted R squared, normal_r is considered acceptable (0.219) < 0.4
normal_r <- 1- ll_beta/ll_0
adjusted_r <- (ll_beta - df)/ll_0

#find reference levels
estimates <-  ml1$coefficients

Design.3.Facial.and.Body <- -sum(estimates[1:2])
Social.2.Acive <- -sum(estimates[3])
level.3.autonomous <- -sum(estimates[4:5])
price.349 <- -sum(estimates[6:9])
user.rating.4.8.stars <- -sum(estimates[10:12])

#make table
est <- data.frame(c(Design.3.Facial.and.Body, Social.2.Acive, level.3.autonomous, price.349, user.rating.4.8.stars))
colnames(est) <- "estimates"
rownames(est) <- c("Design.3.Facial.and.Body", "Social.2.Acive", "level.3.autonomous", "price.349", "user.rating.4.8.stars")
all_estimates <- rbind(data.frame(estimates), est)

#interaction effect
ml2<- mlogit(Selection_Dummy ~ Design.1.Machine + Design.2.Facial.expressions+ Social.Interaction.1.Passive + 
               Level.of.Autonomy.1.Passive + Level.of.Autonomy.2.Assertive + Price.199 + Price.249 + Price.249.Discount + 
               Price.299 +  
               User.Rating.3.8.stars + User.Rating.4.1.stars + User.Rating.4.5.stars + I(None_option*Household.size) +
               None_option | 0, robots) # note: the "| 0" part means that no alternative-specific constants should be considered
summary(ml2)

#price linear
ml3<- mlogit(Selection_Dummy ~ Design.1.Machine + Design.2.Facial.expressions+ Social.Interaction.1.Passive + 
               Level.of.Autonomy.1.Passive + Level.of.Autonomy.2.Assertive + Price.val + Price.discount+ 
               User.Rating.3.8.stars + User.Rating.4.1.stars + User.Rating.4.5.stars + 
               None_option | 0, robots) # note: the "| 0" part means that no alternative-specific constants should be considered
summary(ml3)


lrtest2 <- -2*(ml3$logLik - ml1$logLik)
1-pchisq(lrtest2, df = 11)

#price linear and userrating linear
ml4<- mlogit(Selection_Dummy ~ Design.1.Machine + Design.2.Facial.expressions+ Social.Interaction.1.Passive + 
               Level.of.Autonomy.1.Passive + Level.of.Autonomy.2.Assertive + Price.val + Price.discount+ 
               User.Rating.val + 
               None_option | 0, robots) # note: the "| 0" part means that no alternative-specific constants should be considered
summary(ml4)


lrtest3 <- -2*(ml4$logLik - ml3$logLik)
1-pchisq(lrtest3, df = 9)
