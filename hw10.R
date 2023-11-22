library(PoEdata)
data(mexican)
library(dplyr)
library(tidyverse)
library(magrittr)
library(plm)

fe1<-plm(lnprice~regular+rich+alcohol+nocondom+bar+street, 
         data=mexican, model='within', effect='individual')
summary(fe1)
re1<-plm(lnprice~regular+rich+alcohol+nocondom+bar+street+age+attractive+school, 
         data=mexican, model='random',effect= 'individual')
summary(re1)
15.6c


fenzi=fe1$coefficients[1:6]-re1$coefficients[2:7]
fenmu=sqrt((summary(fe1)$coefficients[1:6,2])^2- 
  (summary(re1)$coefficients[2:7,2])^2) #varfe-varre
tvalue=fenzi/fenmu



ht <- pht(lnprice~regular+rich+alcohol+nocondom+bar+street+age+attractive+school
          |regular+rich+alcohol+bar+street+age+attractive+school, 
          data=mexican, model = "ht")
summary(ht)
15.12b
data("nls_panel")
poo=plm(lwage~educ+exper+I(exper^2)+hours+black,data=nls_panel
        ,model='pooling',effect='individual')
summary(poo)
15.12c
library(lmtest)
library(multiwayvcov)
coeftest(poo, vcov=vcovHC(poo, type="sss", cluster="group"))

15.12d
re12d<-plm(lwage~educ+exper+I(exper^2)+hours+black,
           data=nls_panel, model='random',effect= 'individual')
summary(re12d)
15.12e

fe12e<-plm(lwage~educ+exper+I(exper^2)+hours+black, 
           data=nls_panel, model='within',effect= 'individual')
fenzi=fe12e$coefficients[1:3]-re12d$coefficients[3:5]
fenzi
fenmu=sqrt((summary(fe12e)$coefficients[1:3,2])^2- 
             (summary(re12d)$coefficients[3:5,2])^2) #varfe-varre
tvalue=fenzi/fenmu
tvalue
phtest(fe12e,re12d)

15.12f
ht <- pht(lwage~educ+exper+I(exper^2)+hours+black
          |exper+I(exper^2)+black, 
          data=nls_panel, model = "ht")
summary(ht)

15.13a
data('grunfeld11')
firm=grunfeld11$firm 
firm.f = factor(firm)
 dummies = model.matrix(~firm.f)
asd= data.frame(dummies)
lm(grunfeld11$k~firm.f2+firm.f3
   +firm.f4+firm.f5+firm.f6+firm.f7+firm.f8+firm.f9+firm.f10+firm.f11,data=asd)

lm(inv~v+k,data=grunfeld11)


poo=plm(inv~v+k,data=grunfeld11
        ,model='pooling',effect='individual')
summary(poo)
