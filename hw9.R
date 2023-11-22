fallr=read.table("C:/Users/User/Downloads/fallr.dat", header = T)
v=1:2185



for (i in c(1:2185)){
    if(is.na(fallr$NFALL[i])|fallr$NFALL[i] != 0){
      v[i]=1
    }
  else {
    v[i]=0
  }
  fallr$dfall[i]=v[i]
}
a=0
for (i in c(1:2185)){
    if(fallr$dfall[i]==1){
      a=a+1
    }
  if(is.na(fallr$SEX[i])){
      fallr$SEX[i]='M'
  }
}
96/2185
MODEL1 <- glm(dfall ~ SEX, family = "binomial" ,data= fallr)
summary(MODEL1)

f=0
m=0
fe=0
ma=0
for (i in c(1:2185)){
  if(fallr$SEX[i]=='F'&fallr$dfall[i]==1){
    f=f+1
  }
  if(fallr$SEX[i]=='F'){
    fe=fe+1
  }
  if(fallr$SEX[i]=='M'&fallr$dfall[i]==1){
    m=m+1
  }
  if(fallr$SEX[i]=='M'){
    ma=ma+1
  }
}
m/(ma-m)
f/(fe-f)
exp(-3.3119+0.5131)
exp(-3.3119)
-]