library(readxl)
data <- read_excel('MArket Discipline Data untuk Regresi.xlsx')
str(data)

library(ggplot2)
ggplot(data,aes(x=1:length(PG),y=PG))+geom_line()
ggplot(data,aes(x=1:length(TG),y=TG))+geom_line()
ggplot(data,aes(x=1:length(PCR),y=PCR))+geom_line()

m1 <- lm(PG~PCR+TG,data = data)
summary(m1)

plot(PG~PCR,data)
lines(data$PCR,predict(m1),col="blue")
plot(PG~TG,data)
lines(data$TG,predict(m1),col="blue")

m2 <- lm(PG ~ poly(PCR,2)+PCR,data=data)
summary(m2)

library(splines)
library(Ecdat)

model <- lm(PG~bs(PCR,knots = c(1.1,1.23,1.3,1.44)),data = data)
summary(model)

PCRlims <- range(data$PCR)
PCRlength <- length(data$PCR)
PCR.grid <- seq(from=PCRlims[1],to=PCRlims[2],length.out=PCRlength)
pred <- predict(model,newdata = list(PCR=PCR.grid),se=T)

plot(data$PCR,data$PG,main="Regression Spline Plot")
lines(PCR.grid,pred$fit,col="red",lwd=3)
lines(PCR.grid,pred$fit+2*pred$se.fit,lty="dashed",lwd=2,col="green")
lines(PCR.grid,pred$fit-2*pred$se.fit,lty="dashed",lwd=2,col="green")
segments(1.1,0,x1=1.1,y1=0.7,col="blue")
segments(1.23,0,x1=1.23,y1=0.7,col="blue")
segments(1.3,0,x1=1.3,y1=0.7,col="blue")
segments(1.44,0,x1=1.44,y1=0.7,col="blue")

#------------------------------------------------------------#
# Log(data)
data$PCR<- log(data$PCR)
plot(1:length(data$PCR),data$PCR)
plot(PG~PCR,data)
lines(data$PCR,predict(m1),col="blue")
model <- lm(PG~bs(PCR,knots = c(0.1,0.2,0.3,0.46)),data = data)
summary(model)

PCRlims <- range(data$PCR)
PCRlength <- length(data$PCR)
PCR.grid <- seq(from=PCRlims[1],to=PCRlims[2],length.out=PCRlength)
pred <- predict(model,newdata = list(PCR=PCR.grid),se=T)

plot(data$PCR,data$PG,main="Regression Spline Plot")
lines(PCR.grid,pred$fit,col="red",lwd=3)
lines(PCR.grid,pred$fit+2*pred$se.fit,lty="dashed",lwd=2,col="green")
lines(PCR.grid,pred$fit-2*pred$se.fit,lty="dashed",lwd=2,col="green")
segments(0.1,0,x1=0.1,y1=0.7,col="blue")
segments(0.2,0,x1=0.2,y1=0.7,col="blue")
segments(0.3,0,x1=0.3,y1=0.7,col="blue")
segments(0.46,0,x1=0.46,y1=0.7,col="blue")
