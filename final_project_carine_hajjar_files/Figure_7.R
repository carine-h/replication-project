rm(list = ls())
library(readstata13)
library(foreign)

data <- read.dta13("Ideology_Trump.dta")

################################
###IDEAL POINT MODEL
################################



library(wnominate)
library(pscl)

votes <- c("minwage", "taxincrease", "abortionpenalties", "immigration", "gunsteacher", "irandeal", "healthcare", "gunsbackground", "climatechange", "plannedparent")

starts <- rep(0, nrow(rollcall))

mat <- rollcall(rollcall, yea = 1, nay = 0, notInLegis=NA, missing=9, legis.names = rownames(rollcall))

idealpoints <- ideal(mat, maxiter = 10000, burnin = 5000, verbose = T, startvals = list(x =starts))

d1 <- as.data.frame(apply(idealpoints$x, 2, function(x) quantile(x, .5, na.rm =T)))

d1 <- as.data.frame(d1)
names(d1) <- "median"

m <- match(rownames(d1), data$caseid)
table(is.na(m))

d1$pid7 <- data$pid7[m]
d1$condition <- data$condition[m]

m <- match(rownames(d1[is.na(d1$pid7),]), data2$caseid)
table(is.na(m))

d1$pid7[is.na(d1$pid7)] <- as.numeric(data2$pid7[m])
d1$condition[is.na(d1$condition)] <- 5

d1$republican <- ifelse(d1$pid7 %in% c(5, 6, 7), 1, 0)
d1$democrat <- ifelse(d1$pid7 %in% c(1, 2, 3), 1, 0)

starts <- ifelse(d1$republican == 1, 1, ifelse(d1$democrat == 1, -1, 0))


plot(density(d1$median[d1$republican == 1 & d1$condition == 1]), main = "", xlab = "", lty = 2, ylim = c(0, 1.3))
lines(density(d1$median[d1$republican == 1 & d1$condition == 2]), lty = 3, col = "dark red")
lines(density(d1$median[d1$republican == 1 & d1$condition == 5]), col = "dark red")
lines(density(d1$median[d1$democrat == 1 & d1$condition == 5]), col = "dark blue")
lines(density(d1$median[d1$democrat == 0 & d1$republican == 0 & d1$condition == 5]), col = "dark green")


par(mfrow = c(3, 1))
republican.libtrump <- density(d1$median[d1$republican == 1 & d1$condition == 1])
republican.contrump <- density(d1$median[d1$republican == 1 & d1$condition == 2])
republican.self <- density(d1$median[d1$republican == 1 & d1$condition == 5])
democrat.self <- density(d1$median[d1$democrat == 1 & d1$condition == 5])
independent.self <- density(d1$median[d1$democrat == 0 & d1$republican == 0 & d1$condition == 5])

plot(republican.self, xlab = "", lwd = 0, main = "Republicans - All Conditions", xlim = c(-2, 2.2), axes = F, ylim = c(0, .7))
polygon(republican.self, col = "#ff000050", border = "black")
abline(v = median(d1$median[d1$republican == 1 & d1$condition == 5]))
axis(1, at = seq(-2, 2, 1))
axis(2, at = seq(0, .6, .2), las = 2)

#plot(republican.contrump, xlab = "", lwd = 0, main = "Republicans - Conservative Trump Condition", xlim = c(-2, 2.2))
polygon(republican.contrump, col = "#80003350", border = "black", lty = 2)
abline(v = median(d1$median[d1$republican == 1 & d1$condition == 2]))

#plot(republican.libtrump, xlab = "", lwd = 0, main = "Republicans - Liberal Trump Condition", xlim = c(-2, 2.2))
polygon(republican.libtrump, col = "#ffccff80", border = "black", lty = 3)
abline(v = median(d1$median[d1$republican == 1 & d1$condition == 1]))

text(.85, .67, "Conservative Condition", pos = 4)
text(.4, .67, "Liberal Condition", pos = 2)
mtext("Control", side = 3, line = 0, at = .65, cex = .7)

plot(independent.self, xlab = "", lwd = 0, main = "Independents - Control Condition", xlim = c(-2, 2.2), ylim = c(0, .6), axes = F)
polygon(independent.self, col = "#97c6a670", border = "white")
abline(v = median(d1$median[d1$democrat == 0 & d1$republican == 0 & d1$condition == 5]))
axis(1, at = seq(-2, 2, 1))
axis(2, at = seq(0, .6, .2), las = 2)

plot(democrat.self, xlab = "", lwd = 0, main = "Democrats - Control Condition", xlim = c(-2, 2.2), ylim = c(0, 1.2), axes = F)
polygon(democrat.self, col = "#0015ff70", border = "white")
abline(v = median(d1$median[d1$democrat == 1 & d1$condition == 5]))
axis(1, at = seq(-2, 2, 1))
axis(2, at = seq(0, 1.2, .4), las = 2)



 



##############################

#run separately by treatment group and examine discrimination parameters

condition <- c(data$condition, rep(5, nrow(data2)))
library(pscl)
library(wnominate)

rollcall.self <- rollcall[condition == 5,]
self.start <- as.numeric(data2$pid7)
self.start <- ifelse(self.start %in% c(1,2,3), -1, ifelse(self.start %in% c(5, 6,7), 1, 0))

mat.self <- rollcall(rollcall.self, yea = 1, nay = 0, notInLegis = NA, missing = 9, legis.names = rownames(rollcall.self))

starts <- rep(0, nrow(rollcall.self))

idealpoints.self <- ideal(mat.self, maxiter = 10000, burnin = 5000, verbose = T, startvals = list(x = self.start), store.item = TRUE)

item.self <- as.data.frame(idealpoints.self$betabar)
item.self$vote <- colnames(rollcall)





rollcall.liberal <- rollcall[condition == 1,]
liberal.start <- as.numeric(data$pid7[data$condition == 1])
liberal.start <- ifelse(liberal.start %in% c(1,2,3), -1, ifelse(liberal.start %in% c(5, 6,7), 1, 0))

mat.liberal <- rollcall(rollcall.liberal, yea = 1, nay = 0, notInLegis = NA, missing = 9, legis.names = rownames(rollcall.liberal))

idealpoints.liberal <- ideal(mat.liberal, maxiter = 10000, burnin = 5000, verbose = T, startvals = list(x = liberal.start), store.item = TRUE)

item.liberal <- as.data.frame(idealpoints.liberal$betabar)
item.liberal$vote <- colnames(rollcall)





rollcall.conservative <- rollcall[condition == 2,]
conservative.start <- as.numeric(data$pid7[data$condition == 2])
conservative.start <- ifelse(conservative.start %in% c(1,2,3), -1, ifelse(conservative.start %in% c(5, 6,7), 1, 0))

mat.conservative <- rollcall(rollcall.conservative, yea = 1, nay = 0, notInLegis = NA, missing = 9, legis.names = rownames(rollcall.conservative))

idealpoints.conservative <- ideal(mat.conservative, maxiter = 10000, burnin = 5000, verbose = T, startvals = list(x = conservative.start), store.item = TRUE)

item.conservative <- as.data.frame(idealpoints.conservative$betabar)
item.conservative$vote <- colnames(rollcall)

dev.new()
plot(item.self[,1], item.liberal[,1], pch = 16, cex = 1.5, xlim = c(-6, 0), ylim = c(-6, 0), xlab = "Control Condition", ylab = "Liberal Trump Condition")
text(item.self[,1], item.liberal[,1] - .2, item.self$vote, cex =.6)
abline(0,1)

plot(item.self[,1], item.conservative[,1], pch = 16, cex = 1.5, xlim = c(-6, 0), ylim = c(-6, 0), xlab = "Control Condition", ylab = "Conservative Trump Condition")
text(item.self[,1], item.conservative[,1] - .2, item.conservative$vote, cex =.6)
abline(0,1)

plot(item.liberal[,1], item.conservative[,1], pch = 16, cex = 1.5, xlim = c(-6, 0), ylim = c(-6, 0), xlab = "Liberal Trump Condition", ylab = "Conservative Trump Condition")
text(item.liberal[,1], item.conservative[,1] - .2, item.conservative$vote, cex =.6)
abline(0,1)





plot(item.self[,2], item.liberal[,2], pch = 16, cex = 1.5, xlim = c(-3, 0), ylim = c(-3, 0), xlab = "Control Condition", ylab = "Liberal Trump Condition")
text(item.self[,2], item.liberal[,2] - .2, item.self$vote, cex =.6)
abline(0,1)

plot(item.self[,2], item.conservative[,2], pch = 16, cex = 1.5, xlim = c(-3, 0), ylim = c(-3, 0), xlab = "Control Condition", ylab = "Conservative Trump Condition")
text(item.self[,2], item.conservative[,2] - .2, item.conservative$vote, cex =.6)
abline(0,1)

plot(item.liberal[,1], item.conservative[,1], pch = 16, cex = 1.5, xlim = c(-6, 0), ylim = c(-6, 0), xlab = "Liberal Trump Condition", ylab = "Conservative Trump Condition")
text(item.liberal[,1], item.conservative[,1] - .2, item.conservative$vote, cex =.6)
abline(0,1)


