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

# Calculate incremental WTP
c(coef(ml3)[8:10],User.rating.4.8.stars= -sum(coef(ml3)[8:10]))/abs(coef(ml3)["Price.val"])


#lc model 2 segments
lc <- gmnl(Selection_Dummy ~ Design.1.Machine + Design.2.Facial.expressions+ Social.Interaction.1.Passive + 
             Level.of.Autonomy.1.Passive + Level.of.Autonomy.2.Assertive + Price.199 + Price.249 + Price.249.Discount + 
             Price.299 +  
             User.Rating.3.8.stars + User.Rating.4.1.stars + User.Rating.4.5.stars + 
             None_option | 0 | 0 | 0| 1, 
           data = robots,
           model = "lc",
           Q=2,
           method = "NR")

summary(lc)



#error 
memb_prob <- lc$Qir

mean(1 - apply(memb_prob,1,max))

#probability belonging to class 2
prob2 <- exp(lc$coefficients["(class)2"])/(1+exp(lc$coefficients["(class)2"]))

112 * prob2

#make estimation dataa frame
lc_est <- data.frame(lc$coefficients[1:12])
class.1.Design.3.Facial.and.Body <- -sum(lc_est[1:2,])
Class.1.Social.2.Acive <- -sum(lc_est[3,])
Class.1.level.3.autonomous <- -sum(lc_est[4:5,])
Class.1.price.349 <- -sum(lc_est[6:9,])
Class.1.user.rating.4.8.stars <- -sum(lc_est[10:12,])

lc_est <- rbind(lc_est, class.1.Design.3.Facial.and.Body, Class.1.Social.2.Acive, Class.1.level.3.autonomous, Class.1.price.349, Class.1.user.rating.4.8.stars)
rownames(lc_est)[13:17] <- c("design3facialandbody", "social2active", "autonomy3autonomous", "price349", "user4.8")
colnames(lc_est) <- "estimate"

#utilities
Uprice199<-sum(lc_est[1,], lc_est[3,],lc_est[4,],lc_est[6,],lc_est[17,])
Uprice249 <- sum(lc_est[1,], lc_est[3,],lc_est[4,],lc_est[7,],lc_est[17,])
Uprice249D <- sum(lc_est[1,], lc_est[3,],lc_est[4,],lc_est[8,],lc_est[17,])
Uprice299<-sum(lc_est[1,], lc_est[3,],lc_est[4,],lc_est[9,],lc_est[17,])
Uprice349<-sum(lc_est[1,], lc_est[3,],lc_est[4,],lc_est[16,],lc_est[17,])

#probabilities
P199 <- exp(Uprice199)/(exp(Uprice199)+exp(Uprice249)+exp(Uprice249D)+exp(Uprice299)+exp(Uprice349))
P249 <- exp(Uprice249)/(exp(Uprice199)+exp(Uprice249)+exp(Uprice249D)+exp(Uprice299)+exp(Uprice349))
P249D <- exp(Uprice249D)/(exp(Uprice199)+exp(Uprice249)+exp(Uprice249D)+exp(Uprice299)+exp(Uprice349))
P299 <- exp(Uprice299)/(exp(Uprice199)+exp(Uprice249)+exp(Uprice249D)+exp(Uprice299)+exp(Uprice349))
P349 <- exp(Uprice349)/(exp(Uprice199)+exp(Uprice249)+exp(Uprice249D)+exp(Uprice299)+exp(Uprice349))

#revenue = prob*price
rev199 <- P199*199
rev249 <- P249*249
rev249D <- P249D*249
rev299 <- P299*299
rev349<- P349*349

#Utility competition
Ucomp <- sum(lc_est[13,], lc_est[14,], lc_est[15,],lc_est[8,],lc_est[11,])

#Probabilities with competition
CP199 <- exp(Uprice199)/(exp(Uprice199)+exp(Uprice249)+exp(Uprice249D)+exp(Uprice299)+exp(Uprice349)+exp(Ucomp))
CP249 <- exp(Uprice249)/(exp(Uprice199)+exp(Uprice249)+exp(Uprice249D)+exp(Uprice299)+exp(Uprice349)+exp(Ucomp))
CP249D <- exp(Uprice249D)/(exp(Uprice199)+exp(Uprice249)+exp(Uprice249D)+exp(Uprice299)+exp(Uprice349)+exp(Ucomp))
CP299 <- exp(Uprice299)/(exp(Uprice199)+exp(Uprice249)+exp(Uprice249D)+exp(Uprice299)+exp(Uprice349)+exp(Ucomp))
CP349 <- exp(Uprice349)/(exp(Uprice199)+exp(Uprice249)+exp(Uprice249D)+exp(Uprice299)+exp(Uprice349)+exp(Ucomp))
#revenue = prob*price with competition
crev199 <- CP199*199
crev249 <- CP249*249
crev249D <-CP249D*249
crev299 <- CP299*299
crev349<- CP349*349