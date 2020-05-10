#This is the starting file for replication described in the README document.


rm(list = ls())
library(foreign)
library(readstata13)

data <- read.dta13("Ideology_Trump.dta")

####################################
#Table 1 - Regression Results
####################################

#interacting knowledge - with controls
summary(model1 <- lm(Support ~ + libtrump*knowledge + contrump*knowledge + trump_approve 
                     + ideo5b + republican + party_strength + race_white, 
                     data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
length(model1$resid)

#interacting party strength - with controls
summary(model2 <- lm(Support ~ libtrump*party_strength + contrump*party_strength 
                     + knowledge + ideo5b + trump_approve + republican + race_white, 
                     data = data[data$pid7 %in% c(4, 5, 6, 7) 
                                 & (data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
length(model2$resid)

#interacting Trump approval - with controls
summary(model3 <- lm(Support ~ libtrump*trump_approve + contrump*trump_approve 
                     + knowledge + ideo5b + republican + party_strength + race_white, 
                     data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
length(model3$resid)


#interacting self-placed ideology - with controls
summary(model4 <- lm(Support ~ libtrump*ideo5b + contrump*ideo5b + knowledge 
                     + trump_approve + republican + party_strength + race_white, 
                     data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
length(model4$resid)


###########################################################
#Figure 1: average treatment effect by party affiliation
###########################################################

#regression for average treatment effect among Republicans
#conservative treatment
summary(model1 <- lm(Support ~ race_white + contrump, data = data[data$republican == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment
summary(model2 <- lm(Support ~ race_white + libtrump, data = data[data$republican == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment
summary(model3 <- lm(support_gop ~ race_white + gopleader, data = data[data$republican == 1 & (data$gopleader == 1 | data$self == 1),]))


#regression for average treatment effect among Democrats
#conservative treatment
summary(model1dem <- lm(Support ~ race_white + contrump, data = data[data$democrat == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment
summary(model2dem <- lm(Support ~ race_white + libtrump, data = data[data$democrat == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment
summary(model3dem <- lm(support_gop ~ race_white + gopleader, data = data[data$democrat == 1 & (data$gopleader == 1 | data$self == 1),]))

#regression for average treatment effect among Independents
#conservative treatment
summary(model1ind <- lm(Support ~ race_white + contrump, data = data[data$republican == 0 & data$democrat == 0 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment
summary(model2ind <- lm(Support ~ race_white + libtrump, data = data[data$republican == 0 & data$democrat == 0 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment
summary(model3ind <- lm(support_gop ~ race_white + gopleader, data = data[data$republican == 0 & data$democrat == 0 & (data$gopleader == 1 | data$self == 1),]))


conservative.treat <- c(model1$coef[3], model1dem$coef[3], model1ind$coef[3])
conservative.se <- c(coef(summary(model1))[, "Std. Error"][3], coef(summary(model1dem))[, "Std. Error"][3], coef(summary(model1ind))[, "Std. Error"][3])
lower.cons <- conservative.treat - 1.96*conservative.se
upper.cons <- conservative.treat + 1.96*conservative.se


liberal.treat <- c(model2$coef[3], model2dem$coef[3], model2ind$coef[3])
liberal.se <- c(coef(summary(model2))[, "Std. Error"][3], coef(summary(model2dem))[, "Std. Error"][3], coef(summary(model2ind))[, "Std. Error"][3])
lower.liberal <- liberal.treat - 1.96*liberal.se
upper.liberal <- liberal.treat + 1.96*liberal.se


gop.treat <- c(model3$coef[3], model3dem$coef[3], model3ind$coef[3])
gop.se <- c(coef(summary(model3))[, "Std. Error"][3], coef(summary(model3dem))[, "Std. Error"][3], coef(summary(model3ind))[, "Std. Error"][3])
lower.gop <- gop.treat - 1.96*gop.se
upper.gop <- gop.treat + 1.96*gop.se


a <- c(.8, 1, 1.2)
dev.off()

plot(a, liberal.treat, pch = c(15, 16, 17), axes = F, xlab = "Treatment Condition", ylab = "Increased Probability of Voting for Liberal Policy", ylim = c(-.21, .21), col  = c("dark red", "dark blue", "dark green"), xlim = c(.7, 3.3), cex = 1.5, main = "Average Treatment Effect of Policy Cues")
segments(x0 = a, y0 = lower.liberal, x1 = a, y1 = upper.liberal)
points(a+1, conservative.treat, pch = c(15, 16, 17), col  = c("dark red", "dark blue", "dark green"), cex = 1.5)
segments(x0 = a+1, y0 = lower.cons, x1 = a+1, y1 = upper.cons)
points(a+2, gop.treat, pch = c(15, 16, 17), col  = c("dark red", "dark blue", "dark green"), cex = 1.5)
segments(x0 = a+2, y0 = lower.gop, x1 = a+2, y1 = upper.gop)
axis(1, at = c(1,2,3), labels = c("Liberal Trump", "Conservative Trump", "Republicans\n in Congress"), cex.axis = .8)
axis(2, at = seq(-.2, .2, .05), las = 2, cex.axis = .8)
abline(h = 0, lty = 2)
abline(v = seq(1.5, 3.5, 1), lty = 2, col = "grey")
text(2.8,.28, "comparison group is no cue")
text(1, .16, "Republicans", cex = .7)
text(.82, -.01, "Democrats", cex = .7)
text(1.4, .04, "Independents", cex = .7)
box()


################################################
#Figure 2: average treatment effect by knowledge
################################################

summary(model1 <- lm(Support ~ race_white + libtrump*knowledge + contrump*knowledge, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
#conrol
newdata = as.data.frame(cbind(rep(1, 9), rep(0, 9), rep(0, 9), seq(0, 8, 1)))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "knowledge")

#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 9), rep(1, 9), rep(0,9), seq(0, 8, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "knowledge")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#dev.new(width = 4.5, height = 7)
plot(seq(0, 8, 1), diff.cons, ylim = c(-.25, .15), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Political Knowledge", axes = F, main = "Conservative Trump Treatment")
segments(x0 = c(seq(0, 8, 1), x1 = seq(0, 8, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(0, 8, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(0, 8, 1))
axis(2, at = seq(-.25, .15, .05), las = 2)
box()

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 9), rep(0, 9), rep(1,9), seq(0, 8, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "knowledge")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(0, 8, 1), diff.lib, ylim = c(-.25, .15), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Political Knowledge", axes = F, main = "Liberal Trump Treatment")
segments(x0 = c(seq(0, 8, 1), x1 = seq(0, 8, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(0, 8, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(0, 8, 1))
axis(2, at = seq(-.25, .15, .05), las = 2)
box()


###########################################################
#Figure 3: average treatment effect by party strength
###########################################################
summary(model1 <- lm(Support ~ race_white + libtrump*pid7 + contrump*pid7, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$pid7 %in% c(4, 5, 6, 7),]))
#control
newdata = as.data.frame(cbind(rep(1, 4), rep(0, 4), rep(0, 4), seq(4, 7, 1)))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "pid7")

#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 4), rep(1, 4), rep(0, 4), seq(4, 7, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "pid7")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#dev.new(width = 4.5, height = 7)
plot(seq(4, 7, 1), diff.cons, ylim = c(-.25, .25), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Partisan Affilitation", axes = F, main = "Conservative Trump Treatment")
segments(x0 = c(seq(4, 7, 1), x1 = seq(4, 7, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(4, 7, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(4, 7, 1), labels = c("Independent", "Lean GOP", "Weak GOP", "Strong GOP"), cex.axis = .8)
axis(2, at = seq(-.45, .25, .05), las = 2)
box()

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 4), rep(1, 4), rep(0, 4), rep(0, 4), rep(1, 4), seq(4, 7, 1)))
colnames(newdata1) <- c("race_white", "republican", "democrat", "contrump", "libtrump", "pid7")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(4, 7, 1), diff.lib, ylim = c(-.25, .25), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Levels of Partisan Affiliation", axes = F, main = "Liberal Trump Treatment")
segments(x0 = c(seq(4, 7, 1), x1 = seq(4, 7, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(4, 7, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(4, 7, 1), labels = c("Independent", "Lean GOP", "Weak GOP", "Strong GOP"), cex.axis = .8)
axis(2, at = seq(-.25, .45, .05), las = 2)
box()


###########################################################
#Figure 4: average treatment effect by Trump approval
###########################################################

summary(model1 <- lm(Support ~ race_white + libtrump*trump_approve + contrump*trump_approve, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
#control
newdata = as.data.frame(cbind(rep(1, 5), rep(0, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "trump_approve")

#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(1, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "trump_approve")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.cons, ylim = c(-.15, .22), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Trump Approval", axes = F, main = "Conservative Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(1, 5, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strongly\nDisapprove", "", "Neutral", "", "Strongly\nApprove"), cex.axis = .8)
axis(2, at = seq(-.25, .25, .05), las = 2)
box()

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(1, 5), rep(0, 5), rep(0, 5), rep(1, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "republican", "democrat", "contrump", "libtrump", "trump_approve")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.lib, ylim = c(-.15, .22), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Trump Approval", axes = F, main = "Liberal Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(1, 5, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strongly\nDisapprove", "", "Neutral", "", "Strongly\nApprove"), cex.axis = .8)
axis(2, at = seq(-.25, .25, .05), las = 2)
box()


###########################################################
#Figure 5: average treatment effect by self-placed ideology
###########################################################

summary(model1 <- lm(Support ~ race_white + libtrump*ideo5b + contrump*ideo5b, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
#control
newdata = as.data.frame(cbind(rep(1, 5), rep(0, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "ideo5b")

#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(1, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "ideo5b")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.cons, ylim = c(-.16, .16), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Self-Placed Ideology", axes = F, main = "Conservative Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(1, 5, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strong\nLiberal", "", "Moderate", "", "Strong\nConservative"), cex.axis = .8)
axis(2, at = seq(-.25, .25, .05), las = 2)
box()

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(0, 5), rep(1, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "ideo5b")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.lib, ylim = c(-.16, .16), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Self-Placed Ideology", axes = F, main = "Liberal Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(1, 5, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strong\nLiberal", "", "Moderate", "", "Strong\nConservative"), cex.axis = .8)
axis(2, at = seq(-.25, .25, .05), las = 2)
box()

###########################################################
#Figure 5: average treatment effect by self-placed ideology
###########################################################

mturk <- read.csv("Trump_Obama.csv") #Data collection for this is described in the README file.

mturk$republican <- ifelse(mturk$Q13 == 2 | mturk$Q16 == 2, 1, 0)
mturk$democrat <- ifelse(mturk$Q13 == 1 | mturk$Q16 == 1, 1, 0)

mturk$pid7 <- NA
mturk$pid7[mturk$Q13 == 1 & mturk$Q14 == 1] <- 1
mturk$pid7[mturk$Q13 == 1 & mturk$Q14 == 2] <- 2
mturk$pid7[mturk$Q13 == 3 & mturk$Q16 == 1] <- 3
mturk$pid7[mturk$Q13 == 3 & mturk$Q16 == 3] <- 4
mturk$pid7[mturk$Q13 == 3 & mturk$Q16 == 2] <- 5
mturk$pid7[mturk$Q13 == 2 & mturk$Q15 == 2] <- 6
mturk$pid7[mturk$Q13 == 2 & mturk$Q15 == 1] <- 7

mturk$support <- abs(mturk$Q1 - 6)

mturk$treat_trump <- ifelse(mturk$policy == ". This is President Donald Trumps current policy.", 1, 0)
mturk$treat_obama <- ifelse(mturk$policy == ". This was President Barack Obamas policy during most of his presidency.", 1, 0)
mturk$control <- ifelse(mturk$policy == ".", 1, 0)

summary(model3 <- lm(support ~ treat_trump, data = mturk[mturk$republican == 1 & mturk$control == 0,]))
summary(model4 <- lm(support ~ treat_trump, data = mturk[mturk$democrat == 1 & mturk$control == 0,]))

democrat.effect <- model4$coefficients[2]
democrat.se <- sqrt(vcov(model4)[2,2])

republican.effect <- model3$coefficients[2]
republican.se <- sqrt(vcov(model3)[2,2])

plot(1, democrat.effect, pch = 16, cex = 2, col = "dark blue", main = "Immigration Asylum Policy", 
     ylab = "Trump Treatment - Obama Treatment", xlab = "", xlim = c(.75, 2.25), 
     ylim = c(-1.4, 1.4), axes = F)
axis(1, at = c(1, 2), labels = c("Democrats", "Republicans"))
axis(2, at = seq(-1.4, 1.4, .4), labels = round(seq(-1.4, 1.4, .4), 2), las = 2)
abline(h = 0, lty = 2)
segments(x0 = 1, x1 = 1, y0 = democrat.effect - 1.96*democrat.se,  y1 = democrat.effect + 1.96*democrat.se)
segments(x0 = 2, x1 = 2, y0 = republican.effect - 1.96*republican.se, y1 = republican.effect + 1.96*republican.se)
segments(x0 = 1, x1 = 1, y0 = democrat.effect - 1.64*democrat.se,  y1 = democrat.effect + 1.64*democrat.se, lwd = 3)
segments(x0 = 2, x1 = 2, y0 = republican.effect - 1.64*republican.se, y1 = republican.effect + 1.64*republican.se, lwd = 3)
points(2, republican.effect, pch = 15, cex = 2, col = "dark red")
points(1, democrat.effect, pch = 16, cex = 2, col = "dark blue")
box()


###########################################
#Figure 7 - ideal point distributions
###########################################
#See Figure_7.R in replication folder


############################################################
############################################################
############################################################
#Supplemental Materials - Figures and Tables
############################################################
############################################################
############################################################

#######################################
#Table A1 - #balance across conditions
#######################################
#condition: 
#  1 = liberal trump treatment 
#  2 = conservative trump treatment
#  3 = gop leaders treatment
#  5 = control

#one observation per person
data_balance <- data[data$Question == "Minimum_Wage",]
table(data_balance$condition)

by(data_balance$age, data_balance$condition, mean)
summary(model1 <- lm(age ~ libtrump + contrump + gopleader, data = data_balance))

by(data_balance$race_white, data_balance$condition, mean)
summary(model1 <- lm(race_white ~  libtrump + contrump + gopleader, data = data_balance))

by(data_balance$male, data_balance$condition, mean)
summary(model1 <- lm(male ~  libtrump + contrump + gopleader, data = data_balance))

by(data_balance$knowledge, data_balance$condition, mean)
summary(model1 <- lm(knowledge ~  libtrump + contrump + gopleader, data = data_balance))

by(data_balance$trump_approve, data_balance$condition, mean, na.rm = T)
summary(model1 <- lm(trump_approve ~ libtrump + contrump + gopleader, data = data_balance))

by(data_balance$ideo5b, data_balance$condition, function(x) mean(x, na.rm = T))
summary(model1 <- lm(ideo5b ~  libtrump + contrump + gopleader, data = data_balance))

by(data_balance$pid7b, data_balance$condition, function(x) mean(x, na.rm = T))
summary(model1 <- lm(pid7b ~  libtrump + contrump + gopleader, data = data_balance))

by(data_balance$income, data_balance$condition, function(x) mean(x, na.rm = T))
summary(model1 <- lm(income ~  libtrump + contrump + gopleader, data = data_balance))

by(data_balance$educ, data_balance$condition, function(x) mean(x, na.rm = T))
summary(model1 <- lm(educ ~  libtrump + contrump + gopleader, data = data_balance))

by(data_balance$interest, data_balance$condition, function(x) mean(x, na.rm = T))
summary(model1 <- lm(interest ~  libtrump + contrump + gopleader, data = data_balance))

by(data_balance$republican, data_balance$condition, function(x) mean(x, na.rm = T))
summary(model1 <- lm(republican ~  libtrump + contrump + gopleader, data = data_balance))

by(data_balance$democrat, data_balance$condition, function(x) mean(x, na.rm = T))
summary(model1 <- lm(democrat ~  libtrump + contrump + gopleader, data = data_balance))

by(data_balance$independent, data_balance$condition, function(x) mean(x, na.rm = T))
summary(model1 <- lm(independent ~ libtrump + contrump + gopleader, data = data_balance))


###########################################
#Table A2 - additional regression models
###########################################
#See Table_A2.do in replication folder


###########################################
#Table A3 - additional regression models
###########################################
#interacting knowledge - without controls
summary(model1 <- lm(Support ~ + libtrump*knowledge + contrump*knowledge + race_white, 
                     data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
length(model1$resid)

#interacting party strength - without controls
summary(model2a <- lm(Support ~ libtrump*party_strength + contrump*party_strength + race_white, 
                      data = data[data$pid7 %in% c(4, 5, 6, 7) 
                                  & (data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
length(model2a$resid)

#interacting Trump approval - with controls
summary(model3a <- lm(Support ~ libtrump*trump_approve + contrump*trump_approve + race_white, 
                      data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
length(model3a$resid)

#interacting self-placed ideology - without controls
summary(model4a <- lm(Support ~ libtrump*ideo5b + contrump*ideo5b + race_white, 
                      data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
length(model4a$resid)

#Conservative Treatment - all interactions together
summary(model5a <- lm(Support ~ contrump*knowledge + contrump*party_strength 
                      + contrump*trump_approve + contrump*ideo5b + race_white, 
                      data = data[(data$contrump == 1 | data$self == 1),]))
length(model5a$resid)

#Liberal Treatment - all interactions together
summary(model6a <- lm(Support ~ libtrump*knowledge + libtrump*party_strength
                      + libtrump*trump_approve +  libtrump*ideo5b + race_white, 
                      data = data[(data$libtrump == 1 | data$self == 1),]))
length(model6a$resid)

#Both Treatments, Compared to Each Other
summary(model7a <- lm(Support ~ libtrump*knowledge + libtrump*party_strength 
                      + libtrump*trump_approve +  libtrump*ideo5b + race_white, 
                      data = data[(data$libtrump == 1 | data$contrump == 1),]))
length(model7a$resid)


######################################################
#Table A4 - Correlations between interaction variables
######################################################

cor_data <- subset(data, select = c("ideo5b", "trump_approve", "party_strength", "knowledge"))
round(cor(cor_data, use = "complete.obs"), 3)



#############################################################
#Figure A1 - correlation between party and ideology over time
#############################################################

anes <- read.dta("party_ideology_correlation.dta")
anes <- anes[order(anes$var1),]

plot(anes$var1, anes$var2, xlab = "Year", ylab = "Correlation Coefficient", main = "Correlation Between Partisan Identity and Ideology \n ANES Time Series", pch = 16, ylim = c(.2, .8), axes = F)
lines(anes$var1, anes$var2)
axis(1, at = seq(1972, 2016, 4))
axis(2, at = seq(0, .8, .1), las = 2)
box()


#############################################################
#Figure A2 - correlation between party and ideology over time
#############################################################

trade <- read.csv("pew_free_trade.csv")

trade$dt <- as.Date(as.character(trade$date), "%m/%d/%Y")
trade <- trade[!is.na(trade$dt),]

#dev.new(width = 5, height = 7)
plot(trade$dt, trade$republican, type = "l", lwd = 3, col = "dark red", ylim = c(9, 70), ylab = "Percent Support", xlab = "", main = "Positive View of Free Trade Agreements")
lines(trade$dt, trade$democrat, lwd = 3, col = "dark blue")
text(as.Date("2016-03-01"), 23, "Republicans")
text(as.Date("2016-03-01"), 70, "Democrats")

putin <- read.csv("yougov_putin.csv")
putin$dt <- as.Date(as.character(putin$Date), "%m/%d/%Y")
putin <- putin[!is.na(putin$dt),]

#dev.new(width = 5, height = 7)
plot(putin$dt, putin$Republican_approve, type = "l", lwd = 3, col = "dark red", ylim = c(9, 70), ylab = "Percent Support", xlab = "", main = "Favorable View of Vladimir Putin")
lines(putin$dt, putin$Democrat_approve, lwd = 3, col = "dark blue")
text(as.Date("2016-03-01"), 23, "Republicans")
text(as.Date("2016-07-01"), 10, "Democrats")


############################################################
#Figure A3: baseline level of support among control group
#############################################################
question.support <- tapply(data$Support[data$self == 1], data$Question[data$self == 1], function(x) mean(x, na.rm = T))

label <- names(question.support)
label <- gsub("_", "\n", label)

plot(sort(question.support), xlab = "Question", main = "Proportion in Favor of Liberal Policy in Control Group", ylim = c(0, 1), cex = 0, axes = F, ylab = "")
segments(x0 = seq(1, 10, 1), x1 = seq(1, 10, 1), y0 = 0, y1 = sort(question.support), lwd = 10, lend = 2)
axis(1, at = seq(1, 10, 1), labels = label[order(question.support)], cex.axis = .8)
axis(2, at = seq(0, 1, .1), las = 2)
box()

m <- match(data$Question, names(question.support))

data$base.support <- NA
data$base.support <- question.support[m]


#############################################
#Figure A4: Average Treatment Effect
##############################################

#conservative treatment
summary(model1 <- lm(Support ~ race_white + contrump, data = data[(data$contrump == 1 | data$self == 1),]))
#liberal treatment
summary(model2 <- lm(Support ~ race_white + libtrump, data = data[(data$libtrump == 1 | data$self == 1),]))
#GOP treatment
summary(model3 <- lm(support_gop ~ race_white + gopleader, data = data[(data$gopleader == 1 | data$self == 1),]))

conservative.treat <- c(model1$coef[3])
conservative.se <- c(coef(summary(model1))[, "Std. Error"][3])
lower.cons <- conservative.treat - 1.96*conservative.se
upper.cons <- conservative.treat + 1.96*conservative.se

liberal.treat <- c(model2$coef[3])
liberal.se <- c(coef(summary(model2))[, "Std. Error"][3])
lower.liberal <- liberal.treat - 1.96*liberal.se
upper.liberal <- liberal.treat + 1.96*liberal.se

gop.treat <- c(model3$coef[3])
gop.se <- c(coef(summary(model3))[, "Std. Error"][3])
lower.gop <- gop.treat - 1.96*gop.se
upper.gop <- gop.treat + 1.96*gop.se

points <- c(liberal.treat, conservative.treat, gop.treat)

a <- c(1, 2, 3)

plot(a, points, pch = c(15, 16, 17), axes = F, xlab = "Treatment Condition", ylab = "Increased Probability of Voting for Liberal Policy", ylim = c(-.10, .10), col  = c("dark red", "dark blue", "dark green"), xlim = c(.7, 3.3), cex = 1.5, main = "Average Treatment Effect of Policy Cues")
segments(x0 = 1, y0 = lower.liberal, x1 = 1, y1 = upper.liberal)
segments(x0 = 2, y0 = lower.cons, x1 = 2, y1 = upper.cons)
segments(x0 = 3, y0 = lower.gop, x1 = 3, y1 = upper.gop)

axis(1, at = c(1,2,3), labels = c("Liberal Trump", "Conservative Trump", "Republicans\n in Congress"), cex.axis = .8)
axis(2, at = seq(-.2, .2, .05), las = 2, cex.axis = .8)
abline(h = 0, lty = 2)
abline(v = seq(1.5, 3.5, 1), lty = 2, col = "grey")

text(2.8,.09, "comparison group is no cue")

box()


####################################################################
#Figure A5: Treatment effect for each question - Among Republicans
####################################################################

#ABORTION
#conservative treatment, 1 = oppose penalties for abortions
summary(model1a <- lm(Support ~ race_white + contrump, data = data[data$Question == "Abortion" & data$republican == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = oppose penalties for abortions
summary(model2a <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Abortion" & data$republican == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = oppose penalties for abortions
summary(model3a <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Abortion" & data$republican == 1 & (data$gopleader == 1 | data$self == 1),]))

#Climate Change
#conservative treatment, 1 = acknowledge humans are largest factor in climate change
summary(model1b <- lm(Support ~ race_white + contrump, data = data[data$Question == "Climate_Change" & data$republican == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = acknowledge humans are largest factor in climate change
summary(model2b <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Climate_Change" & data$republican == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = acknowledge humans are largest factor in climate change
summary(model3b <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Climate_Change" & data$republican == 1 & (data$gopleader == 1 | data$self == 1),]))

#Background Checks on Guns
#conservative treatment, 1 = background checks on all weapons purchases
summary(model1c <- lm(Support ~ race_white + contrump, data = data[data$Question == "Guns_Background" & data$republican == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = background checks on all weapons purchases
summary(model2c <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Guns_Background" & data$republican == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = background checks on all weapons purchases
summary(model3c <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Guns_Background" & data$republican == 1 & (data$gopleader == 1 | data$self == 1),]))

#Universal Government-Run Health Care
#conservative treatment, 1 = support government health plan
summary(model1d <- lm(Support ~ race_white + contrump, data = data[data$Question == "Health_Care" & data$republican == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = support government health plan
summary(model2d <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Health_Care" & data$republican == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = support government health plan
summary(model3d <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Health_Care" & data$republican == 1 & (data$gopleader == 1 | data$self == 1),]))

#Allow Illegal Immigrants to become Legal
# conservative treatment, 1 = support legalization
# Can't do this one because survey was coded wrong
# summary(model1e <- lm(Support ~ race_white + contrump, data = data[data$Question == "Immigration" & data$republican == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = support legalization
summary(model2e <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Immigration" & data$republican == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = support legalization
summary(model3e <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Immigration" & data$republican == 1 & (data$gopleader == 1 | data$self == 1),]))

#Maintain The Iran Agreement
#conservative treatment, 1 = maintain the agreement
summary(model1f <- lm(Support ~ race_white + contrump, data = data[data$Question == "Iran_Agreement" & data$republican == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = maintain the agreement
summary(model2f <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Iran_Agreement" & data$republican == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = maintain the agreement
summary(model3f <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Iran_Agreement" & data$republican == 1 & (data$gopleader == 1 | data$self == 1),]))

#Minimum Wage of $10/hour
#conservative treatment, 1 = support increasing min wage
summary(model1g <- lm(Support ~ race_white + contrump, data = data[data$Question == "Minimum_Wage" & data$republican == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = support increasing min wage
summary(model2g <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Minimum_Wage" & data$republican == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = support increasing min wage
summary(model3g <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Minimum_Wage" & data$republican == 1 & (data$gopleader == 1 | data$self == 1),]))

#Support Fundning for Planed Parenthood
#conservative treatment, 1 = support funding for planned parenthood
summary(model1h <- lm(Support ~ race_white + contrump, data = data[data$Question == "Planned_Parenthood" & data$republican == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = support funding for planned parenthood
summary(model2h <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Planned_Parenthood" & data$republican == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = support funding for planned parenthood
summary(model3h <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Planned_Parenthood" & data$republican == 1 & (data$gopleader == 1 | data$self == 1),]))

#Allow Teachers to Carry Guns in School
#conservative treatment, 1 = opposing this policy
summary(model1i <- lm(Support ~ race_white + contrump, data = data[data$Question == "School_Guns" & data$republican == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = opposing this policy
summary(model2i <- lm(Support ~ race_white + libtrump, data = data[data$Question == "School_Guns" & data$republican == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = opposing this policy
summary(model3i <- lm(Support ~ race_white + gopleader, data = data[data$Question == "School_Guns" & data$republican == 1 & (data$gopleader == 1 | data$self == 1),]))

#Increase Taxes on the Wealthy
#conservative treatment, 1 = increase taxes on wealthy
summary(model1j <- lm(Support ~ race_white + contrump, data = data[data$Question == "Tax_Increase" & data$republican == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = increase taxes on wealthy
summary(model2j <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Tax_Increase" & data$republican == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = increase taxes on wealthy
summary(model3j <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Tax_Increase" & data$republican == 1 & (data$gopleader == 1 | data$self == 1),]))

liberal.treat <- c(model2a$coef[3], model2b$coef[3], model2c$coef[3], model2d$coef[3], model2e$coef[3], model2f$coef[3], model2g$coef[3], model2h$coef[3], model2i$coef[3], model2j$coef[3])
liberal.se <- c(coef(summary(model2a))[, "Std. Error"][3], coef(summary(model2b))[, "Std. Error"][3], coef(summary(model2c))[, "Std. Error"][3], coef(summary(model2d))[, "Std. Error"][3], coef(summary(model2e))[, "Std. Error"][3], coef(summary(model2f))[, "Std. Error"][3], coef(summary(model2g))[, "Std. Error"][3], coef(summary(model2h))[, "Std. Error"][3], coef(summary(model2i))[, "Std. Error"][3], coef(summary(model2j))[, "Std. Error"][3])

lower.liberal <- liberal.treat - 1.96*liberal.se
upper.liberal <- liberal.treat + 1.96*liberal.se

conservative.treat <- c(model1a$coef[3], model1b$coef[3], model1c$coef[3], model1d$coef[3], 99, model1f$coef[3], model1g$coef[3], model1h$coef[3], model1i$coef[3], model1j$coef[3])
conservative.se <- c(coef(summary(model1a))[, "Std. Error"][3], coef(summary(model1b))[, "Std. Error"][3], coef(summary(model1c))[, "Std. Error"][3], coef(summary(model2d))[, "Std. Error"][3], 0, coef(summary(model1f))[, "Std. Error"][3], coef(summary(model1g))[, "Std. Error"][3], coef(summary(model1h))[, "Std. Error"][3], coef(summary(model1i))[, "Std. Error"][3], coef(summary(model1j))[, "Std. Error"][3])

lower.conservative <- conservative.treat - 1.96*conservative.se
upper.conservative <- conservative.treat + 1.96*conservative.se

gop.treat <- c(model3a$coef[3], model3b$coef[3], model3c$coef[3], model3d$coef[3], model3e$coef[3], model3f$coef[3], model3g$coef[3], model3h$coef[3], model3i$coef[3], model3j$coef[3])
gop.se <- c(coef(summary(model3a))[, "Std. Error"][3], coef(summary(model3b))[, "Std. Error"][3], coef(summary(model3c))[, "Std. Error"][3], coef(summary(model3d))[, "Std. Error"][3], coef(summary(model3e))[, "Std. Error"][3], coef(summary(model3f))[, "Std. Error"][3], coef(summary(model3g))[, "Std. Error"][3], coef(summary(model3h))[, "Std. Error"][3], coef(summary(model3i))[, "Std. Error"][3], coef(summary(model3j))[, "Std. Error"][3])

lower.gop <- gop.treat - 1.96* gop.se
upper.gop <- gop.treat + 1.96* gop.se

a <- rank(liberal.treat)
label <- sort(unique(data$Question))
label <- gsub("_", "\n", label)

#Three different panels
#liberal trump treatment - Republicans
#dev.new(width = 9, height = 4)
plot(a, liberal.treat, pch = 16, axes = F, xlab = "", ylim = c(-.1, .4), col  = "dark blue", xlim = c(.8, 10.2), cex = 1.5, main = "Probability of Supporting Liberal Policy - Liberal Trump Treatment", ylab = "Estimated Treatment Effect")
segments(x0 = a, y0 = lower.liberal, x1 = a, y1 = upper.liberal)
axis(1, at = a, labels = label, cex.axis = .8)
axis(2, at = seq(-1, 1, .1), las = 2)
abline(h = 0, lty = 2)
abline(v = seq(1.5, 9.5, 1), lty = 2, col = "grey")
box()

#conservative trump treatment - Republicans
#dev.new(width = 9, height = 4)
b <- rank(conservative.treat)
plot(b, conservative.treat, pch = 17, col = "dark green", axes = F, xlab = "", ylim = c(-.5, .15), xlim = c(.8, 10.2), cex = 1.5, main = "Probability of Supporting Liberal Policy - Conservative Trump Treatment", ylab = "Estimated Treatment Effect")
segments(x0 = b, y0 = lower.conservative, x1 = b, y1 = upper.conservative)
axis(1, at = b, labels = label, cex.axis = .8)
axis(2, at = seq(-1, 1, .1), las = 2)
abline(h = 0, lty = 2)
abline(v = seq(1.5, 9.5, 1), lty = 2, col = "grey")
box()

#Congressional Republicans treatment - Republicans
#dev.new(width = 9, height = 4)
c <- rank(gop.treat)
plot(c, gop.treat, pch = 17, col = "dark red", axes = F, xlab = "", ylim = c(-.25, .33), xlim = c(.8, 10.2), cex = 1.5, main = "Probability of Supporting Liberal Policy - Congressional GOP Treatment", ylab = "Estimated Treatment Effect")
segments(x0 = c, y0 = lower.gop, x1 = c, y1 = upper.gop)
axis(1, at = c, labels = label, cex.axis = .8)
axis(2, at = seq(-1, 1, .1), las = 2)
abline(h = 0, lty = 2)
abline(v = seq(1.5, 9.5, 1), lty = 2, col = "grey")
box()


#################################################################
#Figure A6: Treatment effect for each question - Among Democrats
#################################################################

#ABORTION
#conservative treatment, 1 = oppose penalties for abortions
summary(model1a <- lm(Support ~ race_white + contrump, data = data[data$Question == "Abortion" & data$democrat == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = oppose penalties for abortions
summary(model2a <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Abortion" & data$democrat == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = oppose penalties for abortions
summary(model3a <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Abortion" & data$democrat == 1 & (data$gopleader == 1 | data$self == 1),]))

#Climate Change
#conservative treatment, 1 = acknowledge humans are largest factor in climate change
summary(model1b <- lm(Support ~ race_white + contrump, data = data[data$Question == "Climate_Change" & data$democrat == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = acknowledge humans are largest factor in climate change
summary(model2b <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Climate_Change" & data$democrat == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = acknowledge humans are largest factor in climate change
summary(model3b <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Climate_Change" & data$democrat == 1 & (data$gopleader == 1 | data$self == 1),]))

#Background Checks on Guns
#conservative treatment, 1 = background checks on all weapons purchases
summary(model1c <- lm(Support ~ race_white + contrump, data = data[data$Question == "Guns_Background" & data$democrat == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = background checks on all weapons purchases
summary(model2c <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Guns_Background" & data$democrat == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = background checks on all weapons purchases
summary(model3c <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Guns_Background" & data$democrat == 1 & (data$gopleader == 1 | data$self == 1),]))

#Universal Government-Run Health Care
#conservative treatment, 1 = support government health plan
summary(model1d <- lm(Support ~ race_white + contrump, data = data[data$Question == "Health_Care" & data$democrat == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = support government health plan
summary(model2d <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Health_Care" & data$democrat == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = support government health plan
summary(model3d <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Health_Care" & data$democrat == 1 & (data$gopleader == 1 | data$self == 1),]))

#Allow Illegal Immigrants to become Legal
# conservative treatment, 1 = support legalization
# Can't do this one because survey was coded wrong
# summary(model1e <- lm(Support ~ race_white + contrump, data = data[data$Question == "Immigration" & data$democrat == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = support legalization
summary(model2e <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Immigration" & data$democrat == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = support legalization
summary(model3e <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Immigration" & data$democrat == 1 & (data$gopleader == 1 | data$self == 1),]))

#Maintain The Iran Agreement
#conservative treatment, 1 = maintain the agreement
summary(model1f <- lm(Support ~ race_white + contrump, data = data[data$Question == "Iran_Agreement" & data$democrat == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = maintain the agreement
summary(model2f <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Iran_Agreement" & data$democrat == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = maintain the agreement
summary(model3f <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Iran_Agreement" & data$democrat == 1 & (data$gopleader == 1 | data$self == 1),]))

#Minimum Wage of $10/hour
#conservative treatment, 1 = support increasing min wage
summary(model1g <- lm(Support ~ race_white + contrump, data = data[data$Question == "Minimum_Wage" & data$democrat == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = support increasing min wage
summary(model2g <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Minimum_Wage" & data$democrat == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = support increasing min wage
summary(model3g <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Minimum_Wage" & data$democrat == 1 & (data$gopleader == 1 | data$self == 1),]))

#Support Fundning for Planed Parenthood
#conservative treatment, 1 = support funding for planned parenthood
summary(model1h <- lm(Support ~ race_white + contrump, data = data[data$Question == "Planned_Parenthood" & data$democrat == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = support funding for planned parenthood
summary(model2h <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Planned_Parenthood" & data$democrat == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = support funding for planned parenthood
summary(model3h <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Planned_Parenthood" & data$democrat == 1 & (data$gopleader == 1 | data$self == 1),]))

#Allow Teachers to Carry Guns in School
#conservative treatment, 1 = opposing this policy
summary(model1i <- lm(Support ~ race_white + contrump, data = data[data$Question == "School_Guns" & data$democrat == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = opposing this policy
summary(model2i <- lm(Support ~ race_white + libtrump, data = data[data$Question == "School_Guns" & data$democrat == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = opposing this policy
summary(model3i <- lm(Support ~ race_white + gopleader, data = data[data$Question == "School_Guns" & data$democrat == 1 & (data$gopleader == 1 | data$self == 1),]))

#Increase Taxes on the Wealthy
#conservative treatment, 1 = increase taxes on wealthy
summary(model1j <- lm(Support ~ race_white + contrump, data = data[data$Question == "Tax_Increase" & data$democrat == 1 & (data$contrump == 1 | data$self == 1),]))
#liberal treatment, 1 = increase taxes on wealthy
summary(model2j <- lm(Support ~ race_white + libtrump, data = data[data$Question == "Tax_Increase" & data$democrat == 1 & (data$libtrump == 1 | data$self == 1),]))
#GOP treatment, 1 = increase taxes on wealthy
summary(model3j <- lm(Support ~ race_white + gopleader, data = data[data$Question == "Tax_Increase" & data$democrat == 1 & (data$gopleader == 1 | data$self == 1),]))

liberal.treat <- c(model2a$coef[3], model2b$coef[3], model2c$coef[3], model2d$coef[3], model2e$coef[3], model2f$coef[3], model2g$coef[3], model2h$coef[3], model2i$coef[3], model2j$coef[3])
liberal.se <- c(coef(summary(model2a))[, "Std. Error"][3], coef(summary(model2b))[, "Std. Error"][3], coef(summary(model2c))[, "Std. Error"][3], coef(summary(model2d))[, "Std. Error"][3], coef(summary(model2e))[, "Std. Error"][3], coef(summary(model2f))[, "Std. Error"][3], coef(summary(model2g))[, "Std. Error"][3], coef(summary(model2h))[, "Std. Error"][3], coef(summary(model2i))[, "Std. Error"][3], coef(summary(model2j))[, "Std. Error"][3])

lower.liberal <- liberal.treat - 1.96*liberal.se
upper.liberal <- liberal.treat + 1.96*liberal.se

conservative.treat <- c(model1a$coef[3], model1b$coef[3], model1c$coef[3], model1d$coef[3], 99, model1f$coef[3], model1g$coef[3], model1h$coef[3], model1i$coef[3], model1j$coef[3])
conservative.se <- c(coef(summary(model1a))[, "Std. Error"][3], coef(summary(model1b))[, "Std. Error"][3], coef(summary(model1c))[, "Std. Error"][3], coef(summary(model2d))[, "Std. Error"][3], 0, coef(summary(model1f))[, "Std. Error"][3], coef(summary(model1g))[, "Std. Error"][3], coef(summary(model1h))[, "Std. Error"][3], coef(summary(model1i))[, "Std. Error"][3], coef(summary(model1j))[, "Std. Error"][3])

lower.conservative <- conservative.treat - 1.96*conservative.se
upper.conservative <- conservative.treat + 1.96*conservative.se

gop.treat <- c(model3a$coef[3], model3b$coef[3], model3c$coef[3], model3d$coef[3], model3e$coef[3], model3f$coef[3], model3g$coef[3], model3h$coef[3], model3i$coef[3], model3j$coef[3])
gop.se <- c(coef(summary(model3a))[, "Std. Error"][3], coef(summary(model3b))[, "Std. Error"][3], coef(summary(model3c))[, "Std. Error"][3], coef(summary(model3d))[, "Std. Error"][3], coef(summary(model3e))[, "Std. Error"][3], coef(summary(model3f))[, "Std. Error"][3], coef(summary(model3g))[, "Std. Error"][3], coef(summary(model3h))[, "Std. Error"][3], coef(summary(model3i))[, "Std. Error"][3], coef(summary(model3j))[, "Std. Error"][3])

lower.gop <- gop.treat - 1.96* gop.se
upper.gop <- gop.treat + 1.96* gop.se

a <- rank(liberal.treat)
label <- sort(unique(data$Question))
label <- gsub("_", "\n", label)

#Three different panels
#liberal trump treatment - Democrats
#dev.new(width = 9, height = 4)
plot(a, liberal.treat, pch = 16, axes = F, xlab = "", ylim = c(-.2, .2), col  = "dark blue", xlim = c(.8, 10.2), cex = 1.5, main = "Probability of Supporting Liberal Policy - Liberal Trump Treatment", ylab = "Estimated Treatment Effect")
segments(x0 = a, y0 = lower.liberal, x1 = a, y1 = upper.liberal)
axis(1, at = a, labels = label, cex.axis = .8)
axis(2, at = seq(-1, 1, .1), las = 2)
abline(h = 0, lty = 2)
abline(v = seq(1.5, 9.5, 1), lty = 2, col = "grey")
box()

#conservative trump treatment - Democrats
#dev.new(width = 9, height = 4)
b <- rank(conservative.treat)
plot(b, conservative.treat, pch = 17, col = "dark green", axes = F, xlab = "", ylim = c(-.2, .2), xlim = c(.8, 10.2), cex = 1.5, main = "Probability of Supporting Liberal Policy - Conservative Trump Treatment", ylab = "Estimated Treatment Effect")
segments(x0 = b, y0 = lower.conservative, x1 = b, y1 = upper.conservative)
axis(1, at = b, labels = label, cex.axis = .8)
axis(2, at = seq(-1, 1, .1), las = 2)
abline(h = 0, lty = 2)
abline(v = seq(1.5, 9.5, 1), lty = 2, col = "grey")
box()

#congressional republicans treatment - Democrats
#dev.new(width = 9, height = 4)
c <- rank(gop.treat)
plot(c, gop.treat, pch = 17, col = "dark red", axes = F, xlab = "", ylim = c(-.2, .2), xlim = c(.8, 10.2), cex = 1.5, main = "Probability of Supporting Liberal Policy - Congressional GOP Treatment", ylab = "Estimated Treatment Effect")
segments(x0 = c, y0 = lower.gop, x1 = c, y1 = upper.gop)
axis(1, at = c, labels = label, cex.axis = .8)
axis(2, at = seq(-1, 1, .1), las = 2)
abline(h = 0, lty = 2)
abline(v = seq(1.5, 9.5, 1), lty = 2, col = "grey")
box()


############################################################################
#Figure A7: Heterogeneity of Treatment Effects by Baseline Question Support
############################################################################

summary(model1 <- lm(Support ~ race_white + libtrump*base.support + contrump*base.support, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
#control
newdata = as.data.frame(cbind(rep(1, 41), rep(0, 41), rep(0, 41), seq(.5, .9, .01)))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "base.support")
#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 41), rep(1, 41), rep(0, 41), seq(.5, .9, .01)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "base.support")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#dev.new(width = 4.5, height = 7)
plot(seq(.5, .9, .01), diff.cons, ylim = c(-.26, .10), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Support for Liberal Policy in Control Group", axes = F, main = "Conservative Trump Treatment")
segments(x0 = c(seq(.5, .9, .01), x1 = seq(.5, .9, .01)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(.5, .9, .01), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(.5, .9, .1))
axis(2, at = seq(-.25, .25, .05), las = 2)
box()

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 41), rep(0, 41), rep(1, 41), seq(.5, .9, .01)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "base.support")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(.5, .9, .01), diff.lib, ylim = c(-.10, .25), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Overall Support for Liberal Policy in Control Group", axes = F, main = "Liberal Trump Treatment")
segments(x0 = c(seq(.5, .9, .01), x1 = seq(.5, .9, .01)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(.5, .9, .01), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(.5, .9, .1))
axis(2, at = seq(-.25, .25, .05), las = 2)
box()


###########################################################
#Figure A8: distribution of political knowledge
###########################################################

plot(0:8, table(data$knowledge), main = "Distribution of Political Knowledge", cex = 0, xlab = "Number of Questions Answered Correctly", ylab = "Proportion", ylim = c(0, 2500), axes = F)
segments(x0 = 0:8, x1 = 0:8, y0 = 0, y1 = table(data$knowledge), lwd = 20, lend = 1)
axis(1, at = seq(0, 8, 1))
axis(2, at = seq(0, .16, .02)*15000, labels = seq(0, .16, .02), las = 2)
box()

###########################################################
#Figure A9: distribution of Trump approval
###########################################################

plot(1:5, table(data$trump_approve), main = "Distribution of Trump Approval", cex = 0, xlab = "Level of Approval", ylab = "Proportion", ylim = c(0, 6500), axes = F)
segments(x0 = 1:5, x1 = 1:5, y0 = 0, y1 = table(data$trump_approve), lwd = 20, lend = 1)
axis(1, at = seq(1, 5, 1), labels = c("Srongly\nDisapprove", "Somewhat\nDisapprove", "Neither Approve\nNor Disapprove", "Somewhat\nDisapprove", "Stongly\nApprove"), cex.axis = .8)
axis(2, at = seq(0, .45, .05)*15000, labels = seq(0, .45, .05), las = 2)
box()


###########################################################
#Figure A10: distribution of party identification
###########################################################

plot(1:7, table(data$pid7[data$pid7 %in% c(1:7)]), main = "Distribution of Party Identification", cex = 0, xlab = "Partisanship", ylab = "Proportion", ylim = c(0, 4500), axes = F)
segments(x0 = 1:7, x1 = 1:7, y0 = 0, y1 = table(data$pid7[data$pid7 %in% c(1:7)]), lwd = 20, lend = 1)
axis(1, at = seq(1, 7, 1), labels = c("Strong\nDemocrat", "Weak\nDemocrat", "Lean\nDemocrat", "Independent", "Lean\nRepublican", "Weak\nRepublican", "Strong\nRepublican"), cex.axis = .8)
axis(2, at = seq(0, .45, .05)*15000, labels = seq(0, .45, .05), las = 2)
box()


###########################################################
#Figure A11: distribution of self-placed ideology
###########################################################

plot(1:5, table(data$ideo5b), main = "Distribution of Self-Placed Ideology", cex = 0, xlab = "Level of Approval", ylab = "Proportion", ylim = c(0, 6500), axes = F)
segments(x0 = 1:5, x1 = 1:5, y0 = 0, y1 = table(data$ideo5b), lwd = 20, lend = 1)
axis(1, at = seq(1, 5, 1), labels = c("Very\nLiberal", "Liberal", "Moderate", "Conservative", "Very\nConservative"), cex.axis = .8)
axis(2, at = seq(0, .45, .05)*15000, labels = seq(0, .45, .05), las = 2)
box()


###########################################################
#Figure A12: average treatment effect by knowledge - Among Republicans
###########################################################

#As average treatment effects rather than as predicted values
summary(model1 <- lm(Support ~ race_white + libtrump*knowledge + contrump*knowledge, data = data[(data$republican == 1 & (data$contrump == 1 | data$self == 1 | data$libtrump == 1)),]))
#conrol
newdata = as.data.frame(cbind(rep(1, 9), rep(0, 9), rep(0, 9), seq(0, 8, 1)))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "knowledge")

#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 9), rep(1, 9), rep(0,9), seq(0, 8, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "knowledge")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#dev.new(width = 4.5, height = 7)
plot(seq(0, 8, 1), diff.cons, ylim = c(-.4, .05), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Political Knowledge", axes = F, main = "Conservative Trump Treatment\nAmong Republicans")
segments(x0 = c(seq(0, 8, 1), x1 = seq(0, 8, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(0, 8, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(0, 8, 1))
axis(2, at = seq(-.4, .05, .05), las = 2)
box()

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 9), rep(0, 9), rep(1,9), seq(0, 8, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "knowledge")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(0, 8, 1), diff.lib, ylim = c(-.05, .4), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Political Knowledge", axes = F, main = "Liberal Trump Treatment\nAmong Republicans")
segments(x0 = c(seq(0, 8, 1), x1 = seq(0, 8, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(0, 8, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(0, 8, 1))
axis(2, at = seq(-.05, .4, .05), las = 2)
box()


######################################################################
#Figure A13: average treatment effect by knowledgee - Among Democrats
######################################################################

#As average treatment effects rather than as predicted values
summary(model1 <- lm(Support ~ race_white + libtrump*knowledge + contrump*knowledge, data = data[(data$democrat == 1 & (data$contrump == 1 | data$self == 1 | data$libtrump == 1)),]))

#conrol
newdata = as.data.frame(cbind(rep(1, 9), rep(0, 9), rep(0, 9), seq(0, 8, 1)))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "knowledge")

#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 9), rep(1, 9), rep(0,9), seq(0, 8, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "knowledge")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#dev.new(width = 4.5, height = 7)
plot(seq(0, 8, 1), diff.cons, ylim = c(-.2, .10), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Political Knowledge", axes = F, main = "Conservative Trump Treatment\nAmong Democrats")
segments(x0 = c(seq(0, 8, 1), x1 = seq(0, 8, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(0, 8, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(0, 8, 1))
axis(2, at = seq(-.4, .10, .05), las = 2)
box()

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 9), rep(0, 9), rep(1,9), seq(0, 8, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "knowledge")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(0, 8, 1), diff.lib, ylim = c(-.10, .2), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Political Knowledge", axes = F, main = "Liberal Trump Treatment\nAmong Democrats")
segments(x0 = c(seq(0, 8, 1), x1 = seq(0, 8, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(0, 8, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(0, 8, 1))
axis(2, at = seq(-.10, .4, .05), las = 2)
box()


###############################################################
#Figure A14: average treatment effect by knowledge - non-linear
###############################################################

data$know2 <- data$knowledge^2
data$know3 <- data$knowledge^3
data$know4 <- data$knowledge^4

summary(model1 <- lm(Support ~ race_white + libtrump*knowledge + libtrump*know2 + libtrump*know3 + libtrump*know4 + contrump*knowledge + contrump*know2 + contrump*know3 + contrump*know4, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
#conrol
newdata = as.data.frame(cbind(rep(1, 9), rep(0, 9), rep(0, 9), seq(0, 8, 1), seq(0, 8, 1)^2, seq(0, 8, 1)^3, seq(0, 8, 1)^4))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "knowledge", "know2", "know3", "know4")
#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 9), rep(1, 9), rep(0,9), seq(0, 8, 1), seq(0, 8, 1)^2, seq(0, 8, 1)^3, seq(0, 8, 1)^4))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "knowledge", "know2", "know3", "know4")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 9), rep(0, 9), rep(1,9), seq(0, 8, 1), seq(0, 8, 1)^2, seq(0, 8, 1)^3, seq(0, 8, 1)^4))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "knowledge", "know2", "know3", "know4")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(0, 8, 1), diff.cons, ylim = c(-.25, .35), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Political Knowledge", axes = F, main = "Conservative Trump Treatment")
segments(x0 = c(seq(0, 8, 1), x1 = seq(0, 8, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(0, 8, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(0, 8, 1))
axis(2, at = seq(-.25, .35, .05), las = 2)
box()

#dev.new(width = 4.5, height = 7)
plot(seq(0, 8, 1), diff.lib, ylim = c(-.25, .35), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Political Knowledge", axes = F, main = "Liberal Trump Treatment")
segments(x0 = c(seq(0, 8, 1), x1 = seq(0, 8, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(0, 8, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(0, 8, 1))
axis(2, at = seq(-.25, .35, .05), las = 2)
box()


#############################################################################
#Figure A15: average treatment effect by Trump approval  - Among Republicans
#############################################################################

summary(model1 <- lm(Support ~ race_white + libtrump*trump_approve + contrump*trump_approve, data = data[data$republican == 1 & (data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
#control
newdata = as.data.frame(cbind(rep(1, 5), rep(0, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "trump_approve")

#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(1, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "trump_approve")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.cons, ylim = c(-.2, .35), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Trump Approval", axes = F, main = "Conservative Trump Treatment\nAmong Republicans")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(1, 5, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strongly\nDisapprove", "", "Neutral", "", "Strongly\nApprove"), cex.axis = .8)
axis(2, at = seq(-.2, .35, .05), las = 2)
box()

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(0, 5), rep(1, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "trump_approve")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.lib, ylim = c(-.2, .35), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Trump Approval", axes = F, main = "Liberal Trump Treatment\nAmong Republicans")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(1, 5, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strongly\nDisapprove", "", "Neutral", "", "Strongly\nApprove"), cex.axis = .8)
axis(2, at = seq(-.25, .35, .05), las = 2)
box()


#########################################################################
#Figure A16: average treatment effect by Trump approval  - Among Democrats
#########################################################################

summary(model1 <- lm(Support ~ race_white + libtrump*trump_approve + contrump*trump_approve, data = data[data$democrat == 1 & (data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
#control
newdata = as.data.frame(cbind(rep(1, 5), rep(0, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "trump_approve")

#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(1, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "trump_approve")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.cons, ylim = c(-.3, .2), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Trump Approval", axes = F, main = "Conservative Trump Treatment\nAmong Democrats")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(1, 5, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strongly\nDisapprove", "", "Neutral", "", "Strongly\nApprove"), cex.axis = .8)
axis(2, at = seq(-.30, .31, .05), labels = round(seq(-.30, .31, .05), 2), las = 2)
box()

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(0, 5), rep(1, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "trump_approve")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.lib, ylim = c(-.3, .2), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Trump Approval", axes = F, main = "Liberal Trump Treatment\nAmong Democrats")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(1, 5, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strongly\nDisapprove", "", "Neutral", "", "Strongly\nApprove"), cex.axis = .8)
axis(2, at = seq(-.3, .3, .05), labels = round(seq(-.30, .31, .05), 2), las = 2)
box()


####################################################################
#Figure A17: average treatment effect by Trump approval - non-linear
####################################################################

summary(model1 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$trump_approve == 1,]))
summary(model2 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$trump_approve == 2,]))
summary(model3 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$trump_approve == 3,]))
summary(model4 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$trump_approve == 4,]))
summary(model5 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$trump_approve == 5,]))

#liberal treatment
lib.st.dis <- model1$coef[3]
lib.wk.dis <- model2$coef[3]
lib.indiff <- model3$coef[3]
lib.wk.app <- model4$coef[3]
lib.st.app <- model5$coef[3]

lib.approve <- c(lib.st.dis, lib.wk.dis, lib.indiff, lib.wk.app, lib.st.app)

se.st.diss <- sqrt(vcov(model1)[3,3])
se.wk.diss <- sqrt(vcov(model2)[3,3])
se.indiff <- sqrt(vcov(model3)[3,3])
se.wk.app <- sqrt(vcov(model4)[3,3])
se.st.app <- sqrt(vcov(model5)[3,3])

se.lib.approve <- c(se.st.diss, se.wk.diss, se.indiff, se.wk.app, se.st.app)

upper.liberal <- lib.approve + 1.96*se.lib.approve
lower.liberal <- lib.approve - 1.96*se.lib.approve

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), lib.approve, ylim = c(-.25, .35), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Self-Placed Ideology", axes = F, main = "Liberal Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.liberal, y1 = upper.liberal, col = "#4286f480", lwd = 2)
points(seq(1, 5, 1), lib.approve, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strongly\nDisapprove", "", "Neutral", "", "Strongly\nApprove"), cex.axis = .8)
axis(2, at = seq(-.25, .45, .05), las = 2)
box()

#conservative treatment
cons.st.diss <- model1$coef[2]
cons.wk.diss <- model2$coef[2]
cons.indiff <- model3$coef[2]
cons.wk.app <- model4$coef[2]
cons.st.app <- model5$coef[2]

cons.approve <- c(cons.st.diss, cons.wk.diss, cons.indiff, cons.wk.app, cons.st.app)

se.st.diss <- sqrt(vcov(model1)[2,2])
se.wk.diss <- sqrt(vcov(model2)[2,2])
se.indiff <- sqrt(vcov(model3)[2,2])
se.wk.app <- sqrt(vcov(model4)[2,2])
se.st.app <- sqrt(vcov(model5)[2,2])

se.cons.approve <- c(se.st.diss, se.wk.diss, se.indiff, se.wk.app, se.st.app)

upper.cons <- cons.approve + 1.96*se.cons.approve
lower.cons <- cons.approve - 1.96*se.cons.approve

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), cons.approve, ylim = c(-.25, .35), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Self-Placed Ideology", axes = F, main = "Conservative Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(1, 5, 1), cons.approve, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strongly\nDisapprove", "", "Neutral", "", "Strongly\nApprove"), cex.axis = .8)
axis(2, at = seq(-.25, .45, .05), las = 2)
box()


###################################################################################
#Figure A18: average treatment effect by self-placed ideology  - Among Republicans
###################################################################################

summary(model1 <- lm(Support ~ race_white + libtrump*ideo5b + contrump*ideo5b, data = data[data$republican == 1 & (data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
#control
newdata = as.data.frame(cbind(rep(1, 5), rep(0, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "ideo5b")

#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(1, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "ideo5b")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.cons, ylim = c(-.4, .4), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Self-Placed Ideology", axes = F, main = "Conservative Trump Treatment\nAmong Republicans")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(1, 5, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strong\nLiberal", "", "Moderate", "", "Strong\nConservative"), cex.axis = .8)
axis(2, at = seq(-.4, .4, .1), las = 2)
box()

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(0, 5), rep(1, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "ideo5b")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.lib, ylim = c(-.4, .4), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Self-Placed Ideology", axes = F, main = "Liberal Trump Treatment\nAmong Republicans")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(1, 5, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strong\nLiberal", "", "Moderate", "", "Strong\nConservative"), cex.axis = .8)
axis(2, at = seq(-.4, .4, .1), las = 2)
box()


################################################################################
#Figure A19: average treatment effect by self-placed ideology  - Among Democrats
################################################################################

summary(model1 <- lm(Support ~ race_white + libtrump*ideo5b + contrump*ideo5b, data = data[data$democrat == 1 & (data$contrump == 1 | data$self == 1 | data$libtrump == 1),]))
#control
newdata = as.data.frame(cbind(rep(1, 5), rep(0, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata) <- c("race_white", "contrump", "libtrump", "ideo5b")

#conservative treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(1, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "ideo5b")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.cons, ylim = c(-.26, .10), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Self-Placed Ideology", axes = F, main = "Conservative Trump Treatment\nAmong Democrats")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(1, 5, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strong\nLiberal", "", "Moderate", "", "Strong\nConservative"), cex.axis = .8)
axis(2, at = seq(-.25, .25, .05), las = 2)
box()

#liberal treatment
newdata1 = as.data.frame(cbind(rep(1, 5), rep(0, 5), rep(1, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("race_white", "contrump", "libtrump", "ideo5b")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.lib, ylim = c(-.10, .25), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Self-Placed Ideology", axes = F, main = "Liberal Trump Treatment\nAmong Democrats")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(1, 5, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strong\nLiberal", "", "Moderate", "", "Strong\nConservative"), cex.axis = .8)
axis(2, at = seq(-.25, .25, .05), las = 2)
box()


##########################################################################
#Figure A20: average treatment effect by self-placed ideology - non-linear
##########################################################################

summary(model1 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$ideo5b == 1,]))
summary(model2 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$ideo5b == 2,]))
summary(model3 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$ideo5b == 3,]))
summary(model4 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$ideo5b == 4,]))
summary(model5 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$ideo5b == 5,]))

#liberal treatment
lib.st.lib <- model1$coef[3]
lib.wk.lib <- model2$coef[3]
lib.moderate <- model3$coef[3]
lib.wk.cons <- model4$coef[3]
lib.st.cons <- model5$coef[3]

lib.ideol <- c(lib.st.lib, lib.wk.lib, lib.moderate, lib.wk.cons, lib.st.cons)

se.st.lib <- sqrt(vcov(model1)[3,3])
se.wk.lib <- sqrt(vcov(model2)[3,3])
se.moderate <- sqrt(vcov(model3)[3,3])
se.wk.cons <- sqrt(vcov(model4)[3,3])
se.st.cons <- sqrt(vcov(model5)[3,3])

se.lib.ideol <- c(se.st.lib, se.wk.lib, se.moderate, se.wk.cons, se.st.cons)

upper.liberal <- lib.ideol + 1.96*se.lib.ideol
lower.liberal <- lib.ideol - 1.96*se.lib.ideol

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), lib.ideol, ylim = c(-.25, .35), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Self-Placed Ideology", axes = F, main = "Liberal Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.liberal, y1 = upper.liberal, col = "#4286f480", lwd = 2)
points(seq(1, 5, 1), lib.ideol, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strong\nLiberal", "", "Moderate", "", "Strong\nConservative"), cex.axis = .8)
axis(2, at = seq(-.25, .45, .05), las = 2)
box()

#conservative treatment
cons.st.lib <- model1$coef[2]
cons.wk.lib <- model2$coef[2]
cons.moderate <- model3$coef[2]
cons.wk.cons <- model4$coef[2]
cons.st.cons <- model5$coef[2]

cons.ideol <- c(cons.st.lib, cons.wk.lib, cons.moderate, cons.wk.cons, cons.st.cons)

se.st.lib <- sqrt(vcov(model1)[2,2])
se.wk.lib <- sqrt(vcov(model2)[2,2])
se.moderate <- sqrt(vcov(model3)[2,2])
se.wk.cons <- sqrt(vcov(model4)[2,2])
se.st.cons <- sqrt(vcov(model5)[2,2])

se.cons.ideol <- c(se.st.lib, se.wk.lib, se.moderate, se.wk.cons, se.st.cons)

upper.cons <- cons.ideol + 1.96*se.cons.ideol
lower.cons <- cons.ideol - 1.96*se.cons.ideol

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), cons.ideol, ylim = c(-.25, .35), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Self-Placed Ideology", axes = F, main = "Conservative Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(1, 5, 1), cons.ideol, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Strong\nLiberal", "", "Moderate", "", "Strong\nConservative"), cex.axis = .8)
axis(2, at = seq(-.25, .45, .05), las = 2)
box()


#######################################################################
#Figure A21: average treatment effect by partisan strength - non-linear
#######################################################################

summary(model1 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$pid7 == 4,]))
summary(model2 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$pid7 == 5,]))
summary(model3 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$pid7 == 6,]))
summary(model4 <- lm(Support ~ contrump + libtrump, data = data[(data$contrump == 1 | data$self == 1 | data$libtrump == 1) & data$pid7 == 7,]))

#liberal treatment
lib.ind <- model1$coef[3]
lib.lean <- model2$coef[3]
lib.weak <- model3$coef[3]
lib.strong <- model4$coef[3]

lib.pid <- c(lib.ind, lib.lean, lib.weak, lib.strong)

se.lib.ind <- sqrt(vcov(model1)[3,3])
se.lib.lean <- sqrt(vcov(model2)[3,3])
se.lib.weak <- sqrt(vcov(model3)[3,3])
se.lib.strong <- sqrt(vcov(model4)[3,3])

se.lib.pid <- c(se.lib.ind, se.lib.lean, se.lib.weak, se.lib.strong)

upper.liberal <- lib.pid + 1.96*se.lib.pid
lower.liberal <- lib.pid - 1.96*se.lib.pid

#dev.new(width = 4.5, height = 7)
plot(seq(4, 7, 1), lib.pid, ylim = c(-.25, .30), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Levels of Partisan Affiliation", axes = F, main = "Liberal Trump Treatment")
segments(x0 = c(seq(4, 7, 1), x1 = seq(4, 7, 1)), y0 = lower.liberal, y1 = upper.liberal, col = "#4286f480", lwd = 2)
points(seq(4, 7, 1), lib.pid, pch = 16)
abline(h = 0)
axis(1, at = seq(4, 7, 1), labels = c("Independent", "Lean GOP", "Weak GOP", "Strong GOP"), cex.axis = .8)
axis(2, at = seq(-.25, .45, .05), las = 2)
box()

#conservative treatment
cons.ind <- model1$coef[2]
cons.lean <- model2$coef[2]
cons.weak <- model3$coef[2]
cons.strong <- model4$coef[2]

cons.pid <- c(cons.ind, cons.lean, cons.weak, cons.strong)

se.cons.ind <- sqrt(vcov(model1)[2,2])
se.cons.lean <- sqrt(vcov(model2)[2,2])
se.cons.weak <- sqrt(vcov(model3)[2,2])
se.cons.strong <- sqrt(vcov(model4)[2,2])

se.cons.pid <- c(se.cons.ind, se.cons.lean, se.cons.weak, se.cons.strong)

upper.cons <- cons.pid + 1.96*se.cons.pid
lower.cons <- cons.pid - 1.96*se.cons.pid

#dev.new(width = 4.5, height = 7)
plot(seq(4, 7, 1), cons.pid, ylim = c(-.25, .30), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Levels of Partisan Affiliation", axes = F, main = "Conservative Trump Treatment")
segments(x0 = c(seq(4, 7, 1), x1 = seq(4, 7, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(4, 7, 1), cons.pid, pch = 16)
abline(h = 0)
axis(1, at = seq(4, 7, 1), labels = c("Independent", "Lean GOP", "Weak GOP", "Strong GOP"), cex.axis = .8)
axis(2, at = seq(-.25, .45, .05), las = 2)
box()




