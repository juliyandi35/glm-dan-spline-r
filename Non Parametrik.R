library(readxl)
data <- read_excel('MArket Discipline Data untuk Regresi.xlsx')
X <- data$PCR
Y <- data$PG
plot(X,Y,pch=20)

#perbandingan bandwidth
Kreg1 = ksmooth(x=X,y=Y,kernel = "normal",bandwidth = 0.01)
Kreg2 = ksmooth(x=X,y=Y,kernel = "normal",bandwidth = 0.03)
Kreg3 = ksmooth(x=X,y=Y,kernel = "normal",bandwidth = 0.07)
plot(X,Y,pch=20,main="Nadaraya-Watson Estimator Perbandingan Bandwidth")
lines(Kreg1, lwd=3, col="orange")
lines(Kreg2, lwd=3, col="purple")
lines(Kreg3, lwd=3, col="limegreen")
legend("bottomleft", c("h=0.01","h=0.03","h=0.07"), lwd=3, col=c("orange","purple","limegreen"),cex=0.8)

#CV nadaraya-watson
n = length(X)
# n: sample size
h_seq = seq(1.062898, 1.628043, l = 700)
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i]
    Y_tr = Y[-i]
    # training set
    Y_val_predict = ksmooth(x=X_tr,y=Y_tr,kernel = "normal",bandwidth=h_using, x.points = X_val)
    CV_err[i] = (Y_val - Y_val_predict$y)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err)
}
CV_err_h

plot(x=h_seq, y=CV_err_h, type="b", lwd=3, col="blue",
     xlab="Smoothing bandwidth", ylab="LOOCV prediction error")

h_opt=h_seq[which(CV_err_h == min(CV_err_h))]
h_opt

mNW <- function(x, X, Y, h, K = dnorm) {
  # x: evaluation points
  # X: vector (size n) with the predictors
  # Y: vector (size n) with the response variable
  # h: bandwidth
  # K: kernel

  # Matrix of size n x length(x)
  Kx <- sapply(X, function(Xi) K((x - Xi) / h) / n*h)
  # Weights
  W <- Kx / rowSums(Kx) # Column recycling!
  # Means at x ("drop" to drop the matrix attributes)
  drop(W %*% Y)
}
m <- function(x) (sin(2*pi*x^3))^3
xGrid <- seq(1.062898, 1.628043, l = 700)
h2 <- h_opt
# Plot data
plot(X, Y,pch=20,col="#42b883",ylab="Y,m,mh",main="Plot Nadaraya Watson dan Fit Regression")
lines(xGrid, m(xGrid), col = 1,lwd=3)
lines(xGrid, mNW(x = xGrid, X = X, Y = Y, h = h2), col = 2,lwd=3)
legend("bottom", legend = c("Fit regression", "Nadaraya-Watson Estimator regression h=1.628043"),
       lwd = 3, col = 1:2,cex=0.6)

library(KernSmooth)

#localpolynomial regression
plot(X, Y,pch=20,main="Plot Local Polynomial Regression")
fit <- locpoly(X, Y, bandwidth = 0.02)
lines(fit,col="green",lwd=3)

# Spline
SS1 = smooth.spline(x=X,y=Y,spar=0.2)
SS2 = smooth.spline(x=X,y=Y,spar=0.7)
SS3 = smooth.spline(x=X,y=Y,spar=1.2)
plot(X,Y,pch=20)
lines(SS1, lwd=3, col="orange")
lines(SS2, lwd=3, col="purple")
lines(SS3, lwd=3, col="limegreen")
legend("bottomleft", c("spar=0.2","spar=0.7","spar=1.2"), lwd=6,
       col=c("orange","purple","limegreen"),cex=0.8)

n = length(X)
# n: sample size
sp_seq = seq(1.062898, 1.628043, l = 700)
# values of spar we are exploring
CV_err_sp = rep(NA,length(sp_seq))
for(j in 1:length(sp_seq)){
  spar_using = sp_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i]
    Y_tr = Y[-i]
    # training set
    SS_fit = smooth.spline(x=X_tr,y=Y_tr,spar=spar_using)
    Y_val_predict = predict(SS_fit,x=X_val)
    # we use the 'predict()' function to predict a new value
    CV_err[i] = (Y_val - Y_val_predict$y)^2
  }
  CV_err_sp[j] = mean(CV_err)
}
CV_err_sp

plot(x=sp_seq, y=CV_err_sp, type="b", lwd=3, col="blue",
     xlab="Value of 'spar'", ylab="LOOCV prediction error")

spar<-sp_seq[which(CV_err_sp == min(CV_err_sp))]
spar

#spline regression
plot(X,Y,pch=20)
xm <- data.frame(cbind(X,Y))
xm <- xm[order(X),]
mh <- smooth.spline(xm$X, xm$Y, spar=spar)
lines(xm$X,fitted(mh),col="red",lwd=3)

plot(X,Y,pch=20)
lines(xGrid, mNW(x = xGrid, X = X, Y = Y, h = h2), col = "purple",lwd=3) #nadaraya
lines(xm$X,fitted(mh),col="red",lwd=3) #spline
lines(fit,col="blue",lwd=3) #localpoli
legend("bottom", legend = c("Nadaraya-Watson","Spline","Locpoly"),col=c("purple","red","blue"),
       lwd = 2,cex=0.8)

# Menghitung akurasi tiap-tiap model
# MAPE
MAPE_NW <- mean(abs((Y - mNW(x = xGrid, X = X, Y = Y, h = h2)) / Y)) * 100
MAPE_NW
MAPE_Spline <- mean(abs((Y - fitted(mh)) / Y)) * 100
MAPE_Spline
MAPE_LocPoly <- mean(abs((Y - fit$y) / Y)) * 100
MAPE_LocPoly

# MSE
MSE_NW <- mean((Y - mNW(x = xGrid, X = X, Y = Y, h = h2))^2)
MSE_NW
MSE_Spline <- mean((Y - fitted(mh))^2)
MSE_Spline
MSE_LocPoly <- mean((Y - fit$y)^2)
MSE_LocPoly
