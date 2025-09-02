# import Starcraft 2 dataset
starcraft <- read.csv("C:/data/starcraft.csv")

# Finding Model 1
# Response: APM, Possible predictors: SelectByHotkeys, ActionLatency, Age, Number of PACS (Perception Acton Cycles)
# Models and summaries
m1.sbh <- lm(APM ~ SelectByHotkeys, data=starcraft)
summary(m1.sbh)

m1.actlatency <- lm(APM ~ ActionLatency, data=starcraft)
summary(m1.actlatency)

m1.age <- lm(APM ~ Age, data=starcraft)
summary(m1.age)

m1.numpacs <- lm(APM ~ NumberOfPACs, data=starcraft)
summary(m1.numpacs)

# Plot graphs with line of best fit
par(mfrow=c(2,2)) 
plot(starcraft$SelectByHotkeys, starcraft$APM, xlab="Select By Hotkeys", ylab="APM")
abline(m1.sbh, col="red", lwd = 2)

plot(starcraft$ActionLatency, starcraft$APM, xlab="Action Latency", ylab="APM")
abline(m1.actlatency, col="blue", lwd = 2)

plot(starcraft$Age, starcraft$APM, xlab="Age", ylab="APM")
abline(m1.age, col="green", lwd = 2)

plot(starcraft$NumberOfPACs, starcraft$APM, xlab="Number of PACs", ylab="APM")
abline(m1.numpacs, col="hotpink", lwd = 2)

# Finding AIC
AIC(m1.sbh, m1.actlatency, m1.age, m1.numpacs)

# m1.sbh has lowest AIC and highest R^2: the best one predictor model

# plot m1.sbh
plot(m1.sbh)

par(mfrow=c(1,1)) 
# This model is APM = beta_O + beta_1*SelectByHotkeys + beta_2*sqrt(SelectByHotkeys)
m1.sbh.sqrt <- lm(APM ~ SelectByHotkeys + sqrt(SelectByHotkeys), data=starcraft)
summary(m1.sbh.sqrt)
AIC(m1.sbh, m1.actlatency, m1.age, m1.numpacs, m1.sbh.sqrt)
plot(m1.sbh.sqrt)

# Plotting linear function and square root
x <- starcraft$SelectByHotkeys
xmesh <- seq(min(x),max(x), length.out=100)
yhat <- predict(m1.sbh.sqrt, newdata=data.frame(SelectByHotkeys=xmesh))

plot(starcraft$SelectByHotkeys, starcraft$APM, xlab="Select By Hotkeys", ylab="APM", main="APM vs. Select By Hotkeys")
abline(m1.sbh, col="red", lwd=2)
lines(xmesh, yhat, col="blue", lwd=2)

# Legend for plot
legend("bottomright",
       c("Linear Model", "Square Root Model"),
       lty=c(1,1), lwd=c(1,1),
       col=c("red", "blue")
)

# Square Root Model yields the lowest AIC and best R^2 value and will be our M1.
m1 <- m1.sbh.sqrt
summary(m1)

# Finding Model 2
# Multiple Linear Regression - Select By Hotkeys and Action Latency
m2.sbh.actlatency <- lm(APM ~ SelectByHotkeys + ActionLatency, data=starcraft)
summary(m2.sbh.actlatency)

m2.sbh.actlatency.sqrt <- lm(APM ~ SelectByHotkeys + sqrt(SelectByHotkeys) + ActionLatency + sqrt(ActionLatency), data=starcraft)

AIC(m1.sbh, m1.sbh.sqrt, m2.sbh.actlatency, m2.sbh.actlatency.sqrt)

# m2.sbh.actlatency.sqrt has the lowest AIC and highest R^2 value and will be our M2.
m2 <- m2.sbh.actlatency.sqrt
summary(m2)


