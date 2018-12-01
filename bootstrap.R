#######################################bootstrap coverage (mean) #######################


n <- 200 #no of sample
r <- 500 #no of replication
B <- 300 #bootstrap replcation
true.mean <- 3 #assuming

s <- numeric()
b.ci <- matrix(NA,nrow=r,ncol=2) #confidence interval


set.seed(100)
for (i in 1:r) {
  x <- rnorm(n,true.mean)        ####generating random numbers
  
  for (j in 1:B) {                              #######another loop starts to do the boostrap,
    
    boot <- sample(n,replace=TRUE)              #sampling from the data I generated
    s[j] <- mean(x[boot])                       #taking mean
  }
  
  
  b.ci[i,] <- quantile(s,c(.025,.975))         #####confdence interval of bootstrapped means
}

b.contains <- (true.mean > b.ci[,1] & true.mean < b.ci[,2])     ##checking whether the truemean contains within the interval or not
b.ci
b.contains
s
boot
sum(b.contains)/r                ####percentage of time it is in the interval


##############Bootstrap coverage (regression coefficient beta) #############################


n = 50      #########number of sample
B = 30      ###number of bootstap resample
R = 20      ###number of replications
beta = 2

U=replicate(n, rnorm(1,1,100))   #############generating U, error term
X=replicate(n, runif(1,0,100))   #############Generating X
Y= 2*X+U                         #############DGP
reg1 <- lm(Y ~ X)                #############regression


                        
coefmat <- matrix(NA, B, 2)             ###definig the coefficient matrix
Z = numeric()                           #####defining Z (matrix of the betas)
z.ci <- matrix(NA,nrow=R,ncol=2)        #####define matrix of confidence interval


set.seed(1234)

for (i in 1:R) {
  
  U=replicate(n, rnorm(1,1,100))
  Y= beta*X+U 
  reg1 <- lm(Y ~ X)
  resids <- residuals(reg1)     ####getting the residuals
  preds <- fitted(reg1)         ###predicted values beta*X
  
  for (j in 1:B) {
    
    boot.y <- preds + sample(resids, replace = T)   #####getting new Y's using resampled residuals and predicted values
    newmodel <- update(reg1, boot.y ~ X)            ###########refit model
    
    coefmat[j,] = coef(newmodel)                    ########putting the coefficients in matrix coefmat
  }
  
  coefmat
  
  Z <- coefmat[,-1]                     #######saving the coefficient x only
  
  z.ci[i,] <- quantile(Z,c(.025,.975))  ###sorting and finding the cofidence interval
  
}

z.ci
z.contains <- (beta > z.ci[,1] & beta < z.ci[,2])   ##checking whether the true beta contains within the interval or not
z.contains
sum(z.contains)/R            ####percentage of time it is in the interval
