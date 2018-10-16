#setwd("~/Marketing")
rm(list=ls())
library(mlogit) 
robots <- read.csv("Robots_CBC_2.csv")	
# convert data to work with the mlogit format. makes dummy TRUE or FALSE
robots <- mlogit.data(robots, choice="Selection_Dummy", shape="long", alt.var ="Alternative_id")

# calculate models

# start with a partworth model
ml1<- mlogit(Selection_Dummy ~ Design.1.Machine + Design.2.Facial.expressions+ Social.Interaction.1.Passive + 
               Level.of.Autonomy.1.Passive + Level.of.Autonomy.2.Assertive + Price.199 + Price.249 + Price.249.Discount + 
               Price.299 + Price.val + 
               Price.discount + User.Rating.3.8.stars + User.Rating.4.1.stars + User.Rating.4.5.stars +User.Rating.val+ 
               None_option | 0, robots) # note: the "| 0" part means that no alternative-specific constants should be considered
summary(ml1)
