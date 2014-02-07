rm(list=ls(all=TRUE))
require(xtable)
da <- read.csv("./Data/bac.csv", header = TRUE)
da[,1] <- as.Date(da[,1], format = "%d/%m/%Y")
str(da)
head(da)
tail(da)
BAC.R <- diff(da$BAC)/da$BAC[-length(da$BAC)]
SPY.R <- diff(da$SPY)/da$SPY[-length(da$SPY)]
da <- cbind(da[-length(da$BAC),], BAC.R, SPY.R)

pdf("./Figures/BACR.pdf", paper = "a4r", width = 9, title = "BAC.R")
plot(da$BAC.R ~ da$Date, type = 'l', main = "BAC returns", 
     xlab = "Date", ylab = "BAC.R")
dev.off()
pdf("./Figures/SPYR.pdf", paper = "a4r", width = 9, title = "SPY.R")
plot(da$SPY.R ~ da$Date, type = 'l', main = "SPY returns", 
     xlab = "Date", ylab = "SPY.R")
dev.off()
pdf("./Figures/BACeq.pdf", paper = "a4r", width = 9, 
    title = "BAC and Market Returns")
plot(da$BAC.R ~ da$SPY.R, main = "Scatter plot of BAC and Market Returns", 
     pch = 4, xlab = "Market Return", ylab = "BAC Return")
BAC.eq <- lm(da$BAC.R ~ da$SPY.R)
abline(BAC.eq, col = 'red')
dev.off()
summary(BAC.eq)
names(BAC.eq)
xtable(BAC.eq)
confint(BAC.eq)
#-----------------------------------
pu <- read.csv("./Data/GDPemp.csv", header = TRUE)
head(pu)
empeq <- lm(employment ~ GDP, data = pu)
summary(empeq)
pdf("./Figures/empeq.pdf", paper = "a4r", width = 9, 
    title = "GDP and Employment")
plot(employment ~ GDP, data = pu, pch = 4, main = "Scatter plot of 
     GDP and Employment")
abline(empeq, col = 'red')
dev.off()
# T-stats and dist
# xpd allows text outside plotting area
x <- rnorm(mean = 0, sd = 0.2240, n = 10000)
pdf(file = "./Knitr/Eviews/Figures/Coef.pdf", paper = "a4r",  
    title = "Variance of beta estimate", width = 11)
par(xpd = FALSE)
hist(x, prob = TRUE, main = "Estimates of beta", xlab = "beta")
lines(density(x), col = 'red')
abline(v = 0.22, col = 'blue')
abline(v = 0.44, col = 'green')
abline(v = 0.66, col = 'yellow')
par(xpd = NA)
text(x = 0.22, y = 1.9, "1 SD")
text(x = 0.44, y = 1.9, "2 SD")
text(x = 0.66, y = 1.9, "3 SD")
dev.off()
