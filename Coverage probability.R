res <- function(n, p) {
  x = rbinom(n,1,p)
  mean = mean(x)
  L = mean - 1.96 * sqrt(mean*(1-mean)/n)
  U = mean + 1.96 * sqrt(mean*(1-mean)/n)
  if(p >= L && p <= U){
    value = 1
  }else{
    value = 0
  }
  return(value)
}
#Following runs are where n = 5 with varying values of p.
n = 5
p = 0.05
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 5
p = 0.1
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 5
p = 0.25
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 5
p = 0.5
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 5
p = 0.9
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 5
p = 0.95
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
#Following runs are where n = 10 with varying values of p.
n = 10
p = 0.05
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 10
p = 0.1
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 10
p = 0.25
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 10
p = 0.5
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 10
p = 0.9
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 10
p = 0.95
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
#Following runs are where n = 30 with varying values of p.
n = 30
p = 0.05
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 30
p = 0.1
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 30
p = 0.25
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 30
p = 0.5
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 30
p = 0.9
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 30
p = 0.95
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
#Following runs are where n = 50 with varying values of p.
n = 50
p = 0.05
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 50
p = 0.1
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 50
p = 0.25
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 50
p = 0.5
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 50
p = 0.9
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 50
p = 0.95
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
#Following runs are where n = 100 with varying values of p.
n = 100
p = 0.05
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 100
p = 0.1
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 100
p = 0.25
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 100
p = 0.5
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 100
p = 0.9
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
n = 100
p = 0.95
ans = do.call(rbind, replicate(1000, res(n, p), simplify=FALSE))
coverage = mean(ans)
#The above data was collected and stored in a separate CSV file called coverage.csv
#Reading the CSV file.
data = read.csv("coverage.csv")
#Plotting the graph - Coverage probability for different values of n and p
plot(data[[4]],data[[5]],xlab = "n", ylab = "Coverage probability", main = "Coverage probability for different values of n and p", type = 'o', col = "blue", xaxt = 'n', las = 2, pch = 21, ylim = range(0,1))
lines(data[[6]],data[[7]],xlab = "n", type = 'o', col = "red", xaxt = 'n', las = 2, pch = 21)
lines(data[[8]],data[[9]],xlab = "n", type = 'o', col = "green", xaxt = 'n', las = 2, pch = 21)
lines(data[[10]],data[[11]],xlab = "n", type = 'o', col = "orange", xaxt = 'n', las = 2, pch = 21)
lines(data[[12]],data[[13]],xlab = "n", type = 'o', col = "purple", xaxt = 'n', las = 2, pch = 21)
lines(data[[14]],data[[15]],xlab = "n", type = 'o', col = "brown", xaxt = 'n', las = 2, pch = 21)
axis(1, at = c(5, 10, 30, 50, 100), lab = c(5, 10, 30, 50, 100), las = 1)
legend(85, 0.50, title = "Legend", c("p = 0.05","p = 0.1","p = 0.25","p = 0.5","p = 0.9","p = 0.95"), cex=1, col=c("blue","red","green","orange","purple","brown"), pch=21:21:21:21:21:21, lty= 1)