#hw3-1
u<-rnorm(100, mean=0, sd=1)
x<-runif(100,-10,10)
y=c()
for (i in c(1:100)) {
y[i]=0.1+0.5*x[i]+u[i]
}
dat=cbind(y,x)
dat


#3-2
datf=data.frame(dat)

mod=lm((y-0.1)~x, data= datf)
summary(mod)


#3-3
i=0
j=0
coef=matrix(nrow=10000 , ncol = 2)
rsq=c()
for (j in c(1:10000)) {
 

u<-rnorm(100, mean=0, sd=1)
x<-runif(100,-10,10)
y=c()
for (i in c(1:100)) {
  y[i]=0.1+0.5*x[i]+u[i]
}
dat=cbind(y,x)


datf=data.frame(dat)

mod=lm((y-0.1)~x, data= datf)

coef[j,1]=mod$coefficients[1]
coef[j,2]=mod$coefficients[2]
rsq[j]=summary(mod)$r.squared
}
n=0
densityPlot(coef[,1],xlab='intercept')
densityPlot(coef[,2],xlab='x')
densityPlot(rsq)
 for (a in c(1:10000)){
  if (coef[a,1]>(-0.1717)&coef[a,1]<0.2336){
    n=n+1
  }
}
for (a in c(1:10000)){
  if (coef[a,2]>(.4662)&coef[a,2]<.5361){
    n=n+1
  }
}

#3-4
densityPlot(rsq[1:10000]/(1-rsq[1:10000])*98/1)

