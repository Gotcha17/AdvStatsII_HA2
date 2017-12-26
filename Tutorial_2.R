# Problem 1 ---------------------------------------------------------------
# tells R to construct 
# random numbers from inserted starting point
set.seed(666) 
#define parameters
MC = c(100,500,1000)
n = c(10,100,1000)

# create pdf
path = file.path(getwd(), paste("Problem_1", "pdf", sep = "."))
pdf(file=path)
par(mfrow = c(3,3))

for (i in n){
  for (j in MC){
    #faster than using one more for-loop
    X_min = replicate(j, min(runif(i)))
    X_max = replicate(j, max(runif(i)))
    hist(X_min, breaks = "Scott", probability = T, 
         xlim = c(0,1), ylim = NULL, xlab = "x", 
         main = paste("n = ", i, "MC = ", j), col = "darkred")
    hist(X_max, breaks = "Scott", probability = T, 
         xlim = c(0,1), ylim = NULL, xlab = "x", 
         main = paste("n = ", i, "MC = ", j),
         col = "darkgreen", add = T) #add to current plot
    x1 = seq(from = 0, to = 1, length.out = 1000)
    y_min = i*(1-x1)^(i-1)
    y_max = i*x1^(i-1)
    lines(x1, y_min, col = "red")
    lines(x1, y_max, col = "green")
    legend("top", legend = c("min", "max"),
           fill = c("darkred", "darkgreen"))
    box()
  }
}
dev.off()
par(mfrow = c(1,1))

# Problem 2 ---------------------------------------------------------------
#defining global variables
MC = 100
m = c(0, 1, 5)
sig =c(1, 2, 5)
n = seq(from = 3, to = 100, by = 1)

mu1 = function(x){
  m = sum(x)/(length(x)-1)
  return(m) #here m refers to local variable
}

mu2 = function(x){
  m = sum(x[1:3])/3
  return(m)
}

mu3 = function(x){
  m = mean(x)
  return(m)
}

MSE = function(x, m){ #here x refers to the sequence of estimator values
  MSE = (mean(x)-m)^2+var(x) #m is global--> the mean
  return(MSE)
}

# create pdf
path = file.path(getwd(), paste("Problem_2", "pdf", sep = "."))
pdf(file=path)
par(mfrow = c(3,3))

for(i in m){
  for(j in sig){
    #y's are vectors, so each values for n are saved y's
    y1 = c()
    y2 = c()
    y3 = c()
    for (k in n){
      #100 times compute mu1 for n numbers of N(m,sig)
      x1 = replicate(MC, mu1(x = rnorm(k, mean = i, sd = j)))
      x2 = replicate(MC, mu2(x = rnorm(k, mean = i, sd = j)))
      x3 = replicate(MC, mu3(x = rnorm(k, mean = i, sd = j)))
      #calculating MSE for x1,x2,x3 and m=the true mean
      y1[k-2] = MSE(x = x1, m = i) #since k (n) starts at 3
      y2[k-2] = MSE(x = x2, m = i)
      y3[k-2] = MSE(x = x3, m = i)
    }
    plot(n, y1, type = "l", col = "blue", xlab = "n",
         ylab = "MSE", main = paste("mean = ", i,
                                    ", sd = ", j), lwd = 2)
    lines(n, y2, col = "green", lwd = 2) #adds to current plot
    lines(n, y3, col = "red", lwd = 2)
    legend("topright", legend = c("mu1", "mu2", "mu3"),
           fill = c("blue", "green", "red"))
    box()
  }
}
dev.off()
par(mfrow = c(1,1))
#for higher variance, variance of MSE gets larger
#for higher mean, the quadratic bias of MSE gets larger