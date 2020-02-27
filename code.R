# 그래프 및 설명은 pdf 파일 참조

### chapter6

library(dplyr)
library(ggplot2)
library(car)

## 6.9번
# a)
data6_9<-read.table("C:/Users/KimMinyoung/Documents/CH06PR09.txt")
names(data6_9)<-c("Y","X1","X2","X3")
stem(data6_9$X1)
stem(data6_9$X2)

# b)time plot

i<-1:52
ggplot(data6_9,aes(i,X1))+geom_point()+geom_line()
ggplot(data6_9,aes(i,X2))+geom_point()+geom_line()
ggplot(data6_9,aes(i,X3))+geom_point()+geom_line()


# c) scatter plot matrix
pairs(data6_9)


# correlation matrix
cor(data6_9)

## 6.10번
# a)
dx<-cbind(rep(1,52),data6_9$X1,data6_9$X2,data6_9$X3)
beta<-solve(t(dx)%*%dx)%*%t(dx)%*%data6_9$Y
round(beta,5)

# b)
hat.mat <- dx%*%solve(t(dx)%*%dx)%*%t(dx)
dim(hat.mat)
I.hat.mat<-diag(52)-hat.mat
fitted<-hat.mat%*%data6_9$Y
resid<-I.hat.mat%*%data6_9$Y
resid
boxplot(resid)

# c)
plot(fitted,resid)
plot(data6_9$X1,resid)
plot(data6_9$X2,resid)
plot(data6_9$X3,resid)
plot(data6_9$X1*data6_9$X2,resid)

qq.x<- qqnorm(resid)

ggplot(data6_9,aes(i,resid))+geom_point()+geom_line()

# e) brown-forsthe test

rank<-rank(fitted)
df<-data.frame(fitted,rank,resid)
d1<-filter(df,rank<=26)
abs_d1 <-abs(d1$resid-median(d1$resid))
d2<-filter(df,rank>26)
abs_d2<-abs(d2$resid-median(d2$resid))
t.test(abs_d1,abs_d2,var.equal = TRUE)


## 6.11번

# a)

Y<-data6_9$Y
SSR<-c(t(Y)%*%(hat.mat-(1/52)*rep(1,52)%*%t(rep(1,52)))%*%Y)
SSE<-c(t(Y)%*%I.hat.mat%*%Y)
SST<-c(t(Y-mean(Y))%*%(Y-mean(Y)))
MSR<-SSR/3
MSE<-SSE/(52-4)
F<-MSR/MSE
paste("F :",F,"MSE :",MSE,"MSR :",MSR, "P-Value :",1-pf(F,3,52-4))

# b)
data6_9.lm<-lm(Y~X1+X2+X3,data=data6_9)
data6_9.lm
summary(data6_9.lm)
anova(data6_9.lm)


## 6.12번

ggplot(data6_9,aes(X2,Y))+geom_point(aes(x=7.2,y=4370.016),size=4)+geom_smooth()

ggplot(data6_9,aes(X2,Y))+geom_point(aes(x=9.9,y=4334.457),size=4)+geom_smooth()


## 6.18번

# a)
data6_18 <-read.table("c:/Users/KimMinyoung/Documents/CH06PR18.txt")
data6_18
names(data6_18)<-c("Y","X1","X2","X3","X4")
stem(data6_18$X1)
stem(data6_18$X2)
stem(data6_18$X3)
stem(data6_18$X4)

# b) 
pairs(data6_18)

cor(data6_18)

# c) 
lm.data6_18<-lm(Y~X1+X2+X3+X4,data=data6_18)
lm.data6_18

# d)

Dx<-cbind(rep(1,81),data6_18$X1,data6_18$X2,data6_18$X3,data6_18$X4)
beta<-solve(t(Dx)%*%Dx)%*%t(Dx)%*%data6_18$Y
hat.mat <- Dx%*%solve(t(Dx)%*%Dx)%*%t(Dx)
dim(hat.mat)
I.hat.mat<-diag(81)-hat.mat
fitted<-hat.mat%*%data6_18$Y
resid<-I.hat.mat%*%data6_18$Y
resid
boxplot(resid)


# e)
plot(fitted,resid)
plot(data6_18$X1,resid)
plot(data6_18$X2,resid)
plot(data6_18$X3,resid)
plot(data6_18$X4,resid)

plot(data6_18$X1*data6_18$X2, resid)
plot(data6_18$X1*data6_18$X3, resid)
plot(data6_18$X1*data6_18$X4, resid)
plot(data6_18$X2*data6_18$X3, resid)
plot(data6_18$X2*data6_18$X4, resid)
plot(data6_18$X3*data6_18$X4, resid)

qq.x<-qqnorm(resid)

# g)
rank<-rank(fitted)
df<-data.frame(fitted,rank,resid)
d1<-filter(df,rank<=40)
abs_d1 <-abs(d1$resid-median(d1$resid))
d2<-filter(df,rank>40)
abs_d2<-abs(d2$resid-median(d2$resid))
t.test(abs_d1,abs_d2,var.equal = TRUE)

## 6.19)

# a)
Y<-data6_18$Y
SSR<-c(t(Y)%*%(hat.mat-(1/81)*rep(1,81)%*%t(rep(1,81)))%*%Y)
SSE<-c(t(Y)%*%I.hat.mat%*%Y)
SST<-c(t(Y-mean(Y))%*%(Y-mean(Y)))
MSR<-SSR/4
MSE<-SSE/(81-5)
F<-MSR/MSE
paste("F :",F,"MSE :",MSE,"MSR :",MSR, "P-Value :",1-pf(F,4,81-5))

# 결과창
data6_18.lm <- lm(Y~X1+X2+X3+X4, data=data6_18)
data6_18.lm
summary(data6_18.lm)
anova(data6_18.lm)

# chapter7

# 7.4번
pr04<-read.table("C:/Users/KimMinyoung/Documents/CH06PR09.txt")
names(pr04)<-c("Y","X1","X2","X3")
lm.pr04.132<-lm(Y~X1+X3+X2,data=pr04)
summary(lm.pr04.132)
anova(lm.pr04.132)
qf(0.95,1,48)

## 7.5번

pr05<-read.table("C:/Users/KimMinyoung/Documents/CH06PR15.txt")
names(pr05)<-c("Y","X1","X2","X3")
lm.pr05.213<-lm(Y~X2+X1+X3,data=pr05)
summary(lm.pr05.213)
anova(lm.pr05.213)

qf(0.975,1,42)



## 7.6번

lm.pr05.123<-lm(Y~X1+X2+X3,data=pr05)
summary(lm.pr05.123)
anova(lm.pr05.123)

SSR321<-(480.9+364.2)
F.stat <- SSR321/2/101.2
F.stat
pval=1-pf(F.stat,2,42)
pval




## 7.13번

pr13<-read.table("c:/Users/KimMinyoung/Documents/CH06PR09.txt")
names(pr13)<-c("Y","X1","X2","X3")
lm.pr13.1<-lm(Y~X1,data=pr13)
summary(lm.pr13.1)
anova(lm.pr13.1)

lm.pr13.2<-lm(Y~X2,data=pr13)
summary(lm.pr13.2)
anova(lm.pr13.2)

lm.pr13.x1.x2<-lm(X1~X2, data=pr13)
summary(lm.pr13.x1.x2)
anova(lm.pr13.x1.x2)

lm.pr13.21<-lm(Y~X2+X1,data=pr13)
anova(lm.pr13.21)
lm.pr13.12<-lm(Y~X1+X2,data=pr13)
anova(lm.pr13.12)

r.21<-130697/(130697+3020044)
r.12<-5726/(5726+3020044)
r.21
r.12

lm.pr13.132<-lm(Y~X1+X3+X2,data=pr13)
summary(lm.pr13.132)
anova(lm.pr13.132)

pr18<-read.table("C:/Users/KimMinyoung/Documents/CH06PR15.txt")
names(pr18)<-c("Y","X1","X2","X3")
pr18.s<-as.data.frame(scale(pr18))
head(pr18.s)
lm.pr18.s<-lm(Y~X1+X2+X3,data=pr18.s)
summary(lm.pr18.s)

# 7.18-b)
#X1,X2
X1X2<-lm(X1~X2,data=pr18)
summary(X1X2)

# X1,X3
X1X3<-lm(X1~X3,data=pr18)
summary(X1X3)

#X2,X3
X2X3<-lm(X2~X3,data=pr18)
summary(X2X3)

# 7.18-c) 
lm.pr18<-lm(Y~X1+X2+X3,data=pr18)
summary(lm.pr18)

sy<-17.2365
s1<-8.91809
s2<-4.31356
s3<-0.29934
lm.pr18.s$coefficients
beta.s1<- -5.906664e-01
beta.s2<- -1.106149e-01
beta.s3<- -2.339312e-01
beta.1<-sy/s1*beta.s1
beta.1
beta.2<-sy/s2*beta.s2
beta.2
beta.3<-sy/s3*beta.s3
beta.3

y.bar<-mean(pr18$Y)
X1.bar <- mean(pr18$X1)
X2.bar <- mean(pr18$X2)
X3.bar <- mean(pr18$X3)
beta.0<-y.bar-beta.1*X1.bar-beta.2*X2.bar-beta.3*X3.bar
beta.0

## 7.26번
# correlation matrix
cor(pr18)

pr26<-pr18
lm.pr26.1 <- lm(Y~X1+X2,data=pr26)
summary(lm.pr26.1)
anova(lm.pr26.1)

lm.pr26.31<-lm(Y~X3+X1,data=pr18)
anova(lm.pr26.31)

lm.pr26.2<-lm(Y~X2,data=pr18)
lm.pr26.32<-lm(Y~X3+X2,data=pr18)
anova(lm.pr26.2)
anova(lm.pr26.32)

# Chapter 6 6.15번
lm.pr18<-lm(Y~X1+X2+X3,data=pr18)
summary(lm.pr18)

# chapter8

## 8.4번
# a)
pr04<-read.table("C:/Users/KimMinyoung/Documents/CH01PR27.txt")
names(pr04)<-c("Y","X")
pr04
x<-pr04$X-mean(pr04$X)
lm.pr04.1<-lm(Y~x+I(x^2),data=pr04)
plot(pr04$X,pr04$Y)
lines(pr04$X,lm.pr04.1$fitted)

# b)
summary(lm.pr04.1)

# c)
X.h<-data.frame(X=48)
X.h
lm.pr04<-lm(Y~X+I(X^2),data=pr04)
estimate.pr04<-predict.lm(lm.pr04,newdata=X.h,se.fit=T,interval='confidence')
estimate.pr04

# d)
pred.pr04<-predict.lm(lm.pr04,newdata=X.h,se.fit=T,interval='prediction')
pred.pr04

# e)
lm.pr04.1<-lm(Y~x+I(x^2),data=pr04)
summary(lm.pr04.1)
anova(lm.pr04.1)

# f) 
lm.pr04<-lm(Y~X+I(X^2),data=pr04)
summary(lm.pr04)

# g) 
x.square<-x^2
cor(x,x.square)
X<-pr04$X
X.square<-X^2
cor(X,X.square)


## 8.5번
# a)
plot(lm.pr04$fitted,lm.pr04$resid)
plot(x,lm.pr04$resid)
qqnorm(lm.pr04$resid)

# b)
lm.pr04.lof<-lm(Y~factor(x)+factor(I(x^2)),data=pr04)
full<-lm.pr04.lof
smaller<-lm.pr04.1
anova(smaller,full)

# c)
lm.pr04.3<-lm(Y~x+I(x^2)+I(x^3),data=pr04)
summary(lm.pr04.3)

## 8.8번
# a)
pr08<-read.table("C:/Users/KimMinyoung/Documents/CH06PR18.txt")
names(pr08)<-c("Y","X1","X2","X3","X4")
x1<-pr08$X1-mean(pr08$X1)
lm.pr08<-lm(Y~x1+I(x1^2)+X2+X4,data=pr08)
par(mfrow=c(2,2))
plot(lm.pr08$fitted,pr08$Y)

# b)
summary(lm.pr08)

# d)
X1<-pr08$X1
cor(X1,X1^2)
X1_new<-8-mean(X1)
Xh<-data.frame(x1=X1_new,X2=16,X4=250000)
Xh
estimation<-predict.lm(lm.pr08,newdata=Xh,se.fit=T,interval="confidence")
estimation

# e)
lm.pr08.origin<-lm(Y~X1+I(X1^2)+X2+X4,data=pr08)
summary(lm.pr08.origin)

## 8.15번

# b)
pr15<-read.table("C:/Users/KimMinyoung/Documents/CH01PR20.txt")
names(pr15)<-c("Y","X1")
