q4 = read.csv("challenger.csv")

## Ordinary bootstrap
# Bootstrap procedure
set.seed(777)
pre = c()
for (i in 1:2000){
  ind = sample(nrow(q4), replace = TRUE)
  q4_boot = q4[ind,]
  fit = glm(failure ~ temperature,family = binomial(link=logit),data=q4_boot)
  pre = cbind(pre,predict(fit, data.frame(temperature = q4$temperature), type = "response"))
}


l_per = apply(pre, 1, FUN = quantile, probs = 0.025)
u_per = apply(pre, 1, FUN = quantile, probs = 0.975)

plot(q4$temperature, q4$failure, pch = 16, xlab = "Temperature", ylab = "Failure", xlim = range(q4$temperature), ylim = c(0, 1),main = "Ordinary bootstrap")
lines(q4new$temperature, q4new$predicted, lty = 1 , col = "blue", lwd = 2)
for (i in 1:nrow(q4)) {
  lines(c(q4$temperature[i], q4$temperature[i]), c(l_per[i], u_per[i]), col = "red")
}
legend("topright",lty=c(1,1), legend = c("Estimated probability","95% CI"), col = c("blue", "red"),bg="transparent")


## Parametric Bootstrap
# Bootstrap procedure
pre = c()
beta_hat = coef(model)
X = matrix(c(rep(1,nrow(q4)),q4$temperature),nrow(q4),2)
set.seed(777)
for (i in 1:2000){
  q4$y_bootP = as.numeric(rbinom(nrow(q4),1, expit(X%*%beta_hat)))
  model = glm(y_bootP ~ temperature,family = binomial(link=logit),data=q4)
  pre = cbind(pre, predict(model, data.frame(temperature = q4$temperature), type = "response"))
}

l_per = apply(pre, 1, FUN = quantile, probs = 0.025)
u_per = apply(pre, 1, FUN = quantile, probs = 0.975)

plot(q4$temperature, q4$failure, pch = 16, xlab = "Temperature", ylab = "Failure", xlim = range(q4$temperature), ylim = c(0, 1),main = "Parametric bootstrap")
lines(q4new$temperature, q4new$predicted, lty = 1 , col = "blue", lwd = 2)
for (i in 1:nrow(q4)) {
  lines(c(q4$temperature[i], q4$temperature[i]), c(l_per[i], u_per[i]), col = "red")
}
legend("topright",lty=c(1,1), legend = c("Estimated probability","95% CI"), col = c("blue", "red"),bg="transparent")
