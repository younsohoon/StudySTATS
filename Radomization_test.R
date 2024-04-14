## randomazation test

rand_test <- function(x, y, N, alternative, moi, ...) { 
  n1 <- length(x)
  n2 <- length(y)
  t <- moi(x, ...) - moi(y, ...) 
  comb <- c(x, y)
  t_star <- rep(0, N)
  for (i in 1:N) {
    s <- sample(x = comb, size = length(comb), replace = FALSE)
    x_star <- s[1:n1]
    y_star <- s[(n1 + 1):length(comb)]
    t_star[i] <- moi(x_star, ...) - moi(y_star, ...)
  }
  par(mfrow = c(1, 1))
  if (alternative == "two.sided") {
    p_value = sum(t_star >= abs(t))/N + sum(t_star <= -abs(t))/N
  } else if (alternative == "less") {
    p_value = sum(t_star <= t)/N
  } else if (alternative == "greater") {
    p_value = sum(t_star >= t)/N
  }
  p_value
}



# Normal distributions
set.seed(777)

delta = seq(0, 15, length.out = 10)
num_simulations = 500
n = 100

results1 = matrix(0, nrow = length(delta), ncol = 5)
results1[,1] = c(1:10)

for (i in 1:length(delta)) {
  for (j in 1:num_simulations) {
    ref_dist = rnorm(n, mean = 100, sd = sqrt(225))
    comp_dist = rnorm(n, mean = 100 + delta[i], sd = sqrt(225))
    
    t_result = t.test(ref_dist, comp_dist, alternative = "two.sided", var.equal = TRUE)
    wil_result = wilcox.test(ref_dist, comp_dist, alternative = "two.sided")
    mean_result = rand_test(ref_dist, comp_dist, 100, alternative = "two.sided", mean)
    med_result = rand_test(ref_dist, comp_dist, 100, alternative = "two.sided", median)
    
    results1[i, 2] = results1[i, 2] + (t_result$p.value < 0.05)
    results1[i, 3] = results1[i, 3] + (wil_result$p.value < 0.05)
    results1[i, 4] = results1[i, 4] + (mean_result < 0.05)
    results1[i, 5] = results1[i, 5] + (med_result < 0.05)
  }
  
  results1[i, 2] = results1[i, 2] / num_simulations
  results1[i, 3] = results1[i, 3] / num_simulations
  results1[i, 4] = results1[i, 4] / num_simulations
  results1[i, 5] = results1[i, 5] / num_simulations
}

colnames(results1) = c("i","ttest","Wilcoxon", "Rand_means", "Rand_med")
results1 = data.frame(results1)
results1


gfg_plot1 = ggplot(results1, aes(i)) +
  geom_point(aes(y = ttest, color = "T-Test"), size = 1) +
  geom_point(aes(y = Wilcoxon, color = "Wilcoxon"), size = 1) +
  geom_point(aes(y = Rand_means, color = "Random Means"), size = 1) +
  geom_point(aes(y = Rand_med, color = "Random Median"), size = 1) +
  geom_line(aes(y = ttest, color = "T-Test"), size = 0.5) +
  geom_line(aes(y = Wilcoxon, color = "Wilcoxon"), size = 0.5) +
  geom_line(aes(y = Rand_means, color = "Random Means"), size = 0.5) +
  geom_line(aes(y = Rand_med, color = "Random Median"), size = 0.5) +
  ylab("proportions") +
  theme_bw()+
  scale_x_continuous(name = "i", breaks = c(1:10)) +
  scale_color_manual(name = "Tests",
                     values = c("T-Test" = "black", "Wilcoxon" = "red", "Random Means" = "green", "Random Median" = "blue"),
                     labels = c("T-Test", "Wilcoxon", "Random Means", "Random Median"))+
  theme(legend.position = c(0.8, 0.2))
gfg_plot1



# Gamma distributions
delta = seq(1, 2, length.out = 10)
results2 = matrix(0, nrow = length(delta), ncol = 5)
results2[,1] = c(1:10)

for (i in 1:length(delta)) {
  for (j in 1:num_simulations) {
    ref_dist = rgamma(n, shape = 2, scale = 1)
    comp_dist = rgamma(n, shape = 2, scale = delta[i])
    
    t_result = t.test(ref_dist, comp_dist, alternative = "two.sided", var.equal = FALSE)
    wil_result = wilcox.test(ref_dist, comp_dist, alternative = "two.sided")
    mean_result = rand_test(ref_dist, comp_dist, 100, alternative = "two.sided", mean)
    med_result = rand_test(ref_dist, comp_dist, 100, alternative = "two.sided", median)
    
    results2[i, 2] = results2[i, 2] + (t_result$p.value < 0.05)
    results2[i, 3] = results2[i, 3] + (wil_result$p.value < 0.05)
    results2[i, 4] = results2[i, 4] + (mean_result < 0.05)
    results2[i, 5] = results2[i, 5] + (med_result < 0.05)
  }
  
  results2[i, 2] = results2[i, 2] / num_simulations
  results2[i, 3] = results2[i, 3] / num_simulations
  results2[i, 4] = results2[i, 4] / num_simulations
  results2[i, 5] = results2[i, 5] / num_simulations
}

colnames(results2) = c("i","ttest","Wilcoxon", "Rand_means", "Rand_med")
results2 = data.frame(results2)
results2


gfg_plot2 = ggplot(results2, aes(i)) +
  geom_point(aes(y = ttest, color = "T-Test"), size = 1) +
  geom_point(aes(y = Wilcoxon, color = "Wilcoxon"), size = 1) +
  geom_point(aes(y = Rand_means, color = "Random Means"), size = 1) +
  geom_point(aes(y = Rand_med, color = "Random Median"), size = 1) +
  geom_line(aes(y = ttest, color = "T-Test"), size = 0.5) +
  geom_line(aes(y = Wilcoxon, color = "Wilcoxon"), size = 0.5) +
  geom_line(aes(y = Rand_means, color = "Random Means"), size = 0.5) +
  geom_line(aes(y = Rand_med, color = "Random Median"), size = 0.5) +
  theme_bw()+
  ylab("proportions") +
  scale_x_continuous(name = "i", breaks = c(1:10)) +
  scale_color_manual(name = "Tests",
                     values = c("T-Test" = "black", "Wilcoxon" = "red", "Random Means" = "green", "Random Median" = "blue"),
                     labels = c("T-Test", "Wilcoxon", "Random Means", "Random Median"))+
  theme(legend.position = c(0.8, 0.2))


gfg_plot2


# Exponential Distributions
results3 = matrix(0, nrow = length(delta), ncol = 5)
results3[,1] = c(1:10)

delta = seq(0, 2, length.out = 10)

for (i in 1:length(delta)) {
  for (j in 1:num_simulations) {
    ref_dist = rexp(n, 1)
    comp_dist = rexp(n, 1/(1+delta[i]))
    
    t_result = t.test(ref_dist, comp_dist, alternative = "two.sided", var.equal = FALSE)
    wil_result = wilcox.test(ref_dist, comp_dist, alternative = "two.sided")
    mean_result = rand_test(ref_dist, comp_dist, 100, alternative = "two.sided", mean)
    med_result = rand_test(ref_dist, comp_dist, 100, alternative = "two.sided", median)
    
    results3[i, 2] = results3[i, 2] + (t_result$p.value < 0.05)
    results3[i, 3] = results3[i, 3] + (wil_result$p.value < 0.05)
    results3[i, 4] = results3[i, 4] + (mean_result < 0.05)
    results3[i, 5] = results3[i, 5] + (med_result < 0.05)
  }
  
  results3[i, 2] = results3[i, 2] / num_simulations
  results3[i, 3] = results3[i, 3] / num_simulations
  results3[i, 4] = results3[i, 4] / num_simulations
  results3[i, 5] = results3[i, 5] / num_simulations
}
colnames(results3) = c("i","ttest","Wilcoxon", "Rand_means", "Rand_med")
results3 = data.frame(results3)
results3

gfg_plot3 <- ggplot(results3, aes(i)) +
  geom_point(aes(y = ttest, color = "T-Test"), size = 1) +
  geom_point(aes(y = Wilcoxon, color = "Wilcoxon"), size = 1) +
  geom_point(aes(y = Rand_means, color = "Random Means"), size = 1) +
  geom_point(aes(y = Rand_med, color = "Random Median"), size = 1) +
  geom_line(aes(y = ttest, color = "T-Test"), size = 0.5) +
  geom_line(aes(y = Wilcoxon, color = "Wilcoxon"), size = 0.5) +
  geom_line(aes(y = Rand_means, color = "Random Means"), size = 0.5) +
  geom_line(aes(y = Rand_med, color = "Random Median"), size = 0.5) +
  theme_bw()+
  ylab("proportions") +
  scale_x_continuous(name = "i", breaks = c(1:10)) +
  scale_color_manual(name = "Tests",
                     values = c("T-Test" = "black", "Wilcoxon" = "red", "Random Means" = "green", "Random Median" = "blue"),
                     labels = c("T-Test", "Wilcoxon", "Random Means", "Random Median"))+
  theme(legend.position = c(0.8, 0.2))

gfg_plot3