data1 <- read.csv("C:/Users/User/Downloads/Dataset1.csv")
data1=data.frame(data1)
mod1=lm(y~x1+x2, data=data1)
summary(mod1)
#QQplot
qqnorm(mod1$residuals,main="qqplot_res")
qqline(mod1$residual, col = "steelblue", lwd = 2)
#constant variance
scatterplot(mod1$residuals~mod1$fitted.values , data=data1,
            xlab="yhat", ylab="residual",
            main=" Scatter Plot")

attach(data1)
plot(mod1$fitted.values , mod1$residuals, main="scat",
     xlab="yhat ", ylab="residual ", pch=19)
mean(mod1$residuals)

#collinearity

attach(data1)
plot(x1 , x2, main="scat",
     xlab="x1 ", ylab="x2 ", pch=19)

#linearity btwn y and regressors
attach(data1)
plot(x1 , y, main="scat",
     xlab="x1 ", ylab="y ", pch=19)
attach(data1)
plot(x2 , y, main="scat",
     xlab="x2 ", ylab="y ", pch=19)


#################################################


data2 <- read.csv("C:/Users/User/Downloads/Dataset2.csv")
data2=data.frame(data2)
mod2=lm(y~x1+x2, data=data2)
summary(mod2)
#QQplot

qqnorm(mod2$residuals,main="qqplot_res")
qqline(mod2$residual, col = "steelblue", lwd = 2)
#constant variance
scatterplot(mod2$residuals~mod2$fitted.values , data=data2,
            xlab="yhat", ylab="residual",
            main=" constant variance")

attach(data2)
plot(mod2$fitted.values , mod2$residuals, main="scat",
     xlab="yhat ", ylab="residual ", pch=19)

#collinearity

attach(data2)
plot(x1 , x2, main="covariance of x1 and x2",
     xlab="x1 ", ylab="x2 ", pch=19)

#linearity btwn y and regressors
attach(data2)
plot(x1 , y, main="linearity for x1",
     xlab="x1 ", ylab="y ", pch=19)
attach(data2)
plot(x2 , y, main="linearity for x2",
     xlab="x2 ", ylab="y ", pch=19)


########################################


data3 <- read.csv("C:/Users/User/Downloads/Dataset3.csv")
data3=data.frame(data3)
mod3=lm(y~x1+x2, data=data3)
summary(mod3)
#QQplot

qqnorm(mod3$residuals,main="qqplot_res")
qqline(mod3$residual, col = "steelblue", lwd = 2)
#constant variance
scatterplot(mod3$residuals~mod3$fitted.values , data=data3,
            xlab="yhat", ylab="residual",
            main=" constant variance")

attach(data3)
plot(mod3$fitted.values , mod3$residuals, main="scat",
     xlab="yhat ", ylab="residual ", pch=19)

#collinearity

attach(data3)
plot(x1 , x2, main="covariance of x1 and x2",
     xlab="x1 ", ylab="x2 ", pch=19)

#linearity btwn y and regressors
attach(data3)
plot(x1 , y, main="linearity for x1",
     xlab="x1 ", ylab="y ", pch=19)
attach(data3)
plot(x2 , y, main="linearity for x2",
     xlab="x2 ", ylab="y ", pch=19)


#################################################

data4 <- read.csv("C:/Users/User/Downloads/Dataset4.csv")
data4=data.frame(data4)
for (i in c(1:101)){
  if(data4$x1[i]>20 | data4$x2[i]>20){
    data4=c(NA,NA,NA)
    #data4$x2
  }
}
mod4=lm(y~x1+x2, data=data4)
summary(mod4)

#QQplot

qqnorm(mod4$residuals,main="qqplot_res")
qqline(mod4$residual, col = "steelblue", lwd = 2)
#constant variance
scatterplot(mod4$residuals~mod4$fitted.values , data=data4,
            xlab="yhat", ylab="residual",
            main=" constant variance")

attach(data4)
plot(mod4$fitted.values , mod4$residuals, main="scat",
     xlab="yhat ", ylab="residual ", pch=19)

#collinearity

attach(data4)
plot(x1 , x2, main="covariance of x1 and x2",
     xlab="x1 ", ylab="x2 ", pch=19)

#linearity btwn y and regressors
attach(data4)
plot(x1 , y, main="linearity for x1",
     xlab="x1 ", ylab="y ", pch=19)
attach(data4)
plot(x2 , y, main="linearity for x2",
     xlab="x2 ", ylab="y ", pch=19)



