##### a) ##################################################################################################

# Define Variables / Parameters
MC = 100
n = seq(from = 1, to = 100, by = 1)
theta = c(0.1, 1 ,5 , 25)

# Define T_n
T_n = function(x){
  m = mean(x)
  return(m)
}

# Define T'_n
T_dash_n = function(x){
  m = ((1/(2*length(x))) * sum(x^2))^(1/2)
  return(m)
}

##### b) ##################################################################################################

# Define Loss Function
Loss = function(x, m){
  Bias = (mean(x)-m)
  Var = var(x)
  MSE = Bias^2 + Var
  return(c(Bias,Var,MSE))
}

##### c) ##################################################################################################

# Create PDF
path = file.path(getwd(), paste("HA2_c_stu127762_stu107307_stu107135_stu128161", "pdf", sep = "."))
pdf(file=path)
par(mfrow = c(2,2))

# Plotting
for (i in theta){
    Y1 = c()
    Y2 = c()
    for (k in n){
      X1 = replicate(MC, T_n(x = rexp(k, rate = 1/i)))
      X2 = replicate(MC, T_dash_n(x = rexp(k, rate = 1/i)))
      Y1[k] = Loss(x = X1, m = i)[3]
      Y2[k] = Loss(x = X2, m = i)[3]
    }
    plot(n, Y1, type = "l", col = "blue", xlab = "n", ylab = "MSE", main = bquote(theta * " = " * .(i)),  lwd = 2)
    lines(n, Y2, col = "green", lwd = 2)
    legend("topright", legend = c("T_n", "T'_n"), fill = c("blue", "green"))
    box()
}
dev.off()

##### Comments ############################################################################################

# As visible in the Plots / derived in the Pen&Paper-Tutorial, both Estimators are consistent, whereas
# T_n is asymptotically more efficient 
# As n increases the MSE of both Estimators tends to zero for all Thetas.
# As Theta increases, the Y axis scale is much larger (0.000 - 0.008 vs 0 - 500) (since the MSE increases) 
# This means, that we have a slower convergence of the MSE to zero as opposed to smaller values of Theta

##### d) ##################################################################################################

# Create PDF
path = file.path(getwd(), paste("HA2_d_stu127762_stu107307_stu107135_stu128161", "pdf", sep = "."))
pdf(file=path)
par(mfrow = c(2,2))

# Plotting
for (i in theta){
  Y1 = c()
  Y2 = c()
  for (k in n){
    X1 = replicate(MC, T_n(x = rexp(k, rate = 1/i)))
    X2 = replicate(MC, T_dash_n(x = rexp(k, rate = 1/i)))
    Y1[k] = Loss(x = X1, m = i)[2]
    Y2[k] = Loss(x = X2, m = i)[2]
  }
  plot(n, Y1, type = "l", col = "blue", xlab = "n", ylab = "Var", main = bquote(theta * " = " * .(i)),  lwd = 2)
  lines(n, Y2, col = "green", lwd = 2)
  legend("topright", legend = c("T_n", "T'_n"), fill = c("blue", "green"))
  box()
  
  
# Checking which Estimator has lower Mean Variance for small and large n
dmean_low = mean(Y1[1:9]-Y2[1:9])
dmean_high = mean(Y1[76:100]-Y2[76:100])

if (dmean_low>0){
    print(paste("Mean of var of T_n is higher than var of T'_n for n<10 and theta = ", i, " by ", dmean_low))
  } else {
    if (dmean_low<0){
      print(paste("Mean of var of T'_n is higher than var of T_n for n<10 and theta = ", i, " by ", -dmean_low))
    } else {
      print(paste("Mean of var of T'_n and T_n for n<10 and theta = ", i, " are equal"))
    }
  }
if (dmean_high>0){
    print(paste("Mean of var of T_n is higher than var of T'_n for n>75 and theta = ", i, " by ", dmean_high))
  } else {
    if (dmean_high<0){
      print(paste("Mean of var of T'_n is higher than var of T_n for n>75 and theta = ", i, " by ", -dmean_high))
    } else {
      print(paste("Mean of var of T'_n and T_n for n>75 and theta = ", i, " are equal"))
    }
  }

}
dev.off()

##### Comments ############################################################################################

# The Results indicate that the Estimator T'_n has lower variance than T_n for small n (n<10)
# and T_n has lower variance than T'_n for large n (n>75)
# This is also visible in the Plots and already mentioned above

###########################################################################################################

# Create PDF
path = file.path(getwd(), paste("HA2_e", "pdf", sep = "."))
pdf(file=path)
par(mfrow = c(2,2))

# Plotting
for (i in theta){
  Y1 = c()
  Y2 = c()
  for (k in n){
    X1 = replicate(MC, T_n(x = rexp(k, rate = 1/i)))
    X2 = replicate(MC, T_dash_n(x = rexp(k, rate = 1/i)))
    Y1[k] = Loss(x = X1, m = i)[1]^2
    Y2[k] = Loss(x = X2, m = i)[1]^2
  }
  plot(n, Y1, type = "l", col = "blue", xlab = "n", ylab = "Bias^2", main = bquote(theta * " = " * .(i)),  lwd = 2)
  lines(n, Y2, col = "green", lwd = 2)
  legend("topright", legend = c("T_n", "T'_n"), fill = c("blue", "green"))
  box()
}
dev.off()  
  