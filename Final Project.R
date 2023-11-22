#1
seg=read.csv('C:/Users/User/Downloads/seg-large.csv')
seg$utterance= as.factor(seg$utterance)
#2
or=read.csv("C:/Users/User/Downloads/past-tense-or.csv")
or$correct=(or$n.past-or$n.or)/or$n.past
#3
modor=lm(correct~age, data=or)
par(mfrow=c(2,2))
plot(modor)
summary(modor)

for (i in c(1:92)){
  if(or$correct[i]<0.383){
    newor <- or[-i,] 
  }
}
plot(newor$age,newor$correct)
or.ols=lm(correct~age, data=newor)
plot(or.ols)
summary(or.ols)
#4
or.ols$coefficients[1]+1.5*or.ols$coefficients[2]
or.ols$coefficients[1]+8*or.ols$coefficients[2]
#5
par(mfrow=c(2,2))
curve(log(x/(1-x)),0,1)
#6
or$log.odds=log(or$correct/(1-or$correct))
or.logit=lm(log.odds~age,data=or)
plot(or.logit)
summary(or.logit)
#7
expoyhat=exp(or.logit$coefficients[1]+c(12*(1:10))*or.logit$coefficients[2])
expoyhat/(1+expoyhat)

#8
or.glm.quasi=glm(cbind(n.past-n.or, n.or)~age,data=or, family='quasibinomial' )
summary(or.glm.quasi)
#9
or.glm=glm(cbind(n.past-n.or,n.or)~age, data=or, family='binomial')

expoyhat=exp(or.glm$coefficients[1]+c(12*c(1:10))*or.glm$coefficients[2])
expoyhat/(1+expoyhat)
#10
plot(or$age,or$correct,main=' ', xlab=' ', ylab=' ')
par(new = T)
curve((exp(0.98+x*0.02388))/(1+exp(0.98+x*0.02388)), 
      xlim=(range(or$age)), ylim=range(or$correct),main='predicted curve & obs. data', xlab='age', ylab='rate of correct')
par(mfrow=c(1,1))

#11
seg.pmi=glm(boundary~pmi , data=seg, family='binomial')
summary(seg.pmi)#no overdispersion
plot(seg$pmi, seg$boundary,main=' ', xlab=' ', ylab=' ')
par(new = T)
curve(exp(2.90593-0.97588*x)/(1+exp(2.90593-0.97588*x)),
    xlim=(range(seg$pmi)), ylim=range(seg$boundary),main='predicted curve & obs. data', xlab='pmi', ylab='probability')


#12
crct=c()
segcol=c()
segcol= predict(seg.pmi, type='response')>0.5  
a=0
for (i in c(1:821)){
  if(segcol[i]==seg$boundary[i]){
   a=a+1
  }
}
a/821#0.772229

#13
  #plot(seg$pmi, seg$boundary,main=' ', xlab=' ', ylab=' ')
  #par(new = T)
  #curve(exp(2.90593-0.97588*x)/(1+exp(2.90593-0.97588*x)),
  #     xlim=(range(seg$pmi)), ylim=range(seg$boundary),main='predicted curve & obs. data', xlab='pmi', ylab='prob')
b=0
for (i in c(1:821)){
  if(seg$boundary[i]==FALSE){
    b=b+1
  }
}    
b/821#0.6394641

#14
seg.full=glm(boundary~pmi+utterance+phoneme, data=seg, family='binomial')
summary(seg.full)

#15
anova(seg.pmi,seg.full, test= "Chisq")

#16

seg.model=step(model.full)
summary(seg.model)

#17
segcol= predict(seg.model, type='response')>0.5  
a=0
for (i in c(1:821)){
  if(segcol[i]==seg$boundary[i]){
    a=a+1
  }
}
a/821#0.8794153
segpmi=c()
segmodel=c()
segpmi= predict(seg.pmi, type='response')>0.5 
segmodel= predict(seg.model, type='response')>0.5  
rightpmi=0
rightmodel=0
for (i in c(1:821)){
  if(segpmi[i]==seg$boundary[i]){
    rightpmi=rightpmi+1
  }
  if(segmodel[i]==seg$boundary[i]){
  rightmodel=rightmodel+1
  }
}

mat=matrix(c(rightpmi,(821-rightpmi),rightmodel,(821-rightmodel)),2,2)
mat
fisher.test(mat)#reject H0, the 2 model are diffrent

#18
seg.pmihrh=glm(seg$boundary[1:405]~seg$pmi[1:405]+seg$h[1:405]+seg$rh[1:405], data=seg, family='binomial')
segcol= predict(seg.pmihrh, type='response')>0.5  
a=0
for (i in c(1:821)){
  if(segcol[i]==seg$boundary[i]){
    a=a+1
  }
}
a/821

#19
seg.pmi405=glm(seg$boundary[1:405]~seg$pmi[1:405], data=seg, family='binomial')
segcol= predict(seg.pmi405, type='response')>0.5  
a=0
for (i in c(1:821)){
  if(segcol[i]==seg$boundary[i]){
    a=a+1
  }
}
a/821
