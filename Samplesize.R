# Calculating Non-Standard Sample Size 

## sample size calculation
find.n <- function(m, n, alpha, beta, delta=3){
  
  power <- function(alpha, beta, delta, N=1000){
    tem = rep(0,N)
    pvalue = for(i in 1:N){
      group1 = rexp(m,log(2)/log(2)/18)
      group2 = rexp(n,log(2)/log(2)/(18+delta))
      test = wilcox.test(group1, group2, alternative = 'less')
      tem[i] = test$p.value
    }
    power = sum(tem<alpha)/1000 
    return(power)
  }
  
  
  while(power(alpha, beta, delta, N=1000) < (1-beta)){
    n = n + 14
    m = m + 15
  }
  return (list(n,m))
}









## sample size -- in the context of Mood's median test
find.n2 <- function(m,n,alpha, beta, delta=3){
  
  power <- function(alpha, beta, delta, N=1000){
    tem = rep(0,N)
    pvalue = for(i in 1:N){
      group1 = rexp(m,log(2)/log(2)/18)
      group2 = rexp(n,log(2)/log(2)/(18+delta))
      df = data.frame(x=c(rep(1,n),rep(2,m)),y=c(group1,group2))
      test = median_test(y~factor(x),df)
      tem[i] = pvalue(test)
    }
    power = sum(tem<alpha)/1000 
    return(power)
  }
  
  while(power(alpha, beta, delta, N=1000) < (1-beta)){
    n = n + 14
    m = m + 15
  }
  return (list(n,m))
}

f = find.n2(m=2000,n=2000,0.01,0.01,3)
