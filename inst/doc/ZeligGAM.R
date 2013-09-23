### R code from vignette source 'ZeligGAM.Rnw'

###################################################
### code chunk number 1: ZeligGAM.Rnw:55-56
###################################################
library(ZeligGAM)


###################################################
### code chunk number 2: ZeligGAM.Rnw:57-74
###################################################
set.seed(0)
n<-400 
sig<-2
x0 <- runif(n, 0, 1)
x1 <- runif(n, 0, 1) 
x2 <- runif(n, 0, 1)
x3 <- runif(n, 0, 1)
f0 <- function(x) 2 * sin(pi * x) 
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
f3 <- function(x) 0*x
f <- f0(x0) + f1(x1) + f2(x2)
g <- (f-5)/3
g <- binomial()$linkinv(g)
y <- rbinom(g,1,g)
my.data <- as.data.frame(cbind(y, x0, x1, x2, x3))



###################################################
### code chunk number 3: ZeligGAM.Rnw:90-93
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3), model="logit.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1,residuals=TRUE)


###################################################
### code chunk number 4: ZeligGAM.Rnw:99-100
###################################################
plot(z.out$result,pages=1,residuals=TRUE)


###################################################
### code chunk number 5: ZeligGAM.Rnw:106-110
###################################################
 x.out <- setx(z.out)
 s.out <- sim(z.out, x = x.out) 
 summary(s.out) 
 plot(s.out) 


###################################################
### code chunk number 6: ZeligGAM.Rnw:115-116
###################################################
plot(s.out)


###################################################
### code chunk number 7: ZeligGAM.Rnw:125-132
###################################################
x.high <- setx(z.out,  x3= quantile(my.data$x3, 0.8))
x.low <- setx(z.out, x3 = quantile(my.data$x3, 0.2))

s.out <- sim(z.out, x=x.high, x1=x.low)
summary(s.out)
plot(s.out)



###################################################
### code chunk number 8: ZeligGAM.Rnw:137-138
###################################################
plot(s.out)


###################################################
### code chunk number 9: ZeligGAM.Rnw:147-152
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3), H=diag(0.5,37), 
   model="logit.gam", data=my.data) 
summary(z.out)
plot(z.out$result,pages=1,residuals=TRUE)



###################################################
### code chunk number 10: ZeligGAM.Rnw:155-160
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3),sp=c(0.01,-1,-1,-1), 
   model="logit.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1)



###################################################
### code chunk number 11: ZeligGAM.Rnw:163-168
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3),min.sp=c(0.001,0.01,0,10),  
    model="logit.gam", data=my.data) 
summary(z.out)
plot(z.out$result, pages=1)



###################################################
### code chunk number 12: ZeligGAM.Rnw:171-176
###################################################
z.out <-zelig(y~s(x0,k=4,fx=TRUE,bs="tp")+s(x1,k=12)+s(x2,k=15), 
    model="logit.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1)



###################################################
### code chunk number 13: ZeligGAM.Rnw:341-342
###################################################
library(ZeligGAM)


###################################################
### code chunk number 14: ZeligGAM.Rnw:343-355
###################################################
set.seed(0); n<-400; sig<-2
set.seed(0);  n <- 400; sig <- 2; 
x0 <- runif(n, 0, 1);  x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1);  x3 <- runif(n, 0, 1)
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * x)^3 * (1 - x)^10
f3 <- function(x) 0 * x
f <- f0(x0) + f1(x1) + f2(x2)
e <- rnorm(n, 0, sig); y <- f + e
my.data <- as.data.frame(cbind(y, x0, x1, x2, x3))



###################################################
### code chunk number 15: ZeligGAM.Rnw:359-362
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3), model="normal.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1,residuals=TRUE)


###################################################
### code chunk number 16: ZeligGAM.Rnw:368-369
###################################################
plot(z.out$result,pages=1,residuals=TRUE)


###################################################
### code chunk number 17: ZeligGAM.Rnw:375-379
###################################################
 x.out <- setx(z.out)
 s.out <- sim(z.out, x = x.out) 
 summary(s.out) 
 plot(s.out) 


###################################################
### code chunk number 18: ZeligGAM.Rnw:384-385
###################################################
plot(s.out)


###################################################
### code chunk number 19: ZeligGAM.Rnw:394-401
###################################################
x.high <- setx(z.out,  x3= quantile(my.data$x3, 0.8))
x.low <- setx(z.out, x3 = quantile(my.data$x3, 0.2))

s.out <- sim(z.out, x=x.high, x1=x.low)
summary(s.out)
plot(s.out)



###################################################
### code chunk number 20: ZeligGAM.Rnw:406-407
###################################################
plot(s.out)


###################################################
### code chunk number 21: ZeligGAM.Rnw:416-421
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3), H=diag(0.5,37), 
   model="normal.gam", data=my.data) 
summary(z.out)
plot(z.out$result,pages=1,residuals=TRUE)



###################################################
### code chunk number 22: ZeligGAM.Rnw:424-429
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3),sp=c(0.01,-1,-1,-1), 
   model="normal.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1)



###################################################
### code chunk number 23: ZeligGAM.Rnw:432-437
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3),min.sp=c(0.001,0.01,0,10),  
    model="normal.gam", data=my.data) 
summary(z.out)
plot(z.out$result, pages=1)



###################################################
### code chunk number 24: ZeligGAM.Rnw:440-445
###################################################
z.out <-zelig(y~s(x0,k=4,fx=TRUE,bs="tp")+s(x1,k=12)+s(x2,k=15), 
    model="normal.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1)



###################################################
### code chunk number 25: ZeligGAM.Rnw:591-592
###################################################
library(ZeligGAM)


###################################################
### code chunk number 26: ZeligGAM.Rnw:593-604
###################################################
set.seed(0); n<-400; sig<-2
x0 <- runif(n, 0, 1); x1 <- runif(n, 0, 1); x2 <- runif(n, 0, 1); x3 <- runif(n, 0, 1)
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
f3 <- function(x) 0*x
f <- f0(x0) + f1(x1) + f2(x2)
g<-exp(f/4)
y<-rpois(rep(1,n),g)
my.data <- as.data.frame(cbind(y, x0, x1, x2, x3))



###################################################
### code chunk number 27: ZeligGAM.Rnw:624-627
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3), model="poisson.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1,residuals=TRUE)


###################################################
### code chunk number 28: ZeligGAM.Rnw:633-634
###################################################
plot(z.out$result,pages=1,residuals=TRUE)


###################################################
### code chunk number 29: ZeligGAM.Rnw:640-644
###################################################
 x.out <- setx(z.out)
 s.out <- sim(z.out, x = x.out) 
 summary(s.out) 
 plot(s.out) 


###################################################
### code chunk number 30: ZeligGAM.Rnw:649-650
###################################################
plot(s.out)


###################################################
### code chunk number 31: ZeligGAM.Rnw:659-666
###################################################
x.high <- setx(z.out,  x3= quantile(my.data$x3, 0.8))
x.low <- setx(z.out, x3 = quantile(my.data$x3, 0.2))

s.out <- sim(z.out, x=x.high, x1=x.low)
summary(s.out)
plot(s.out)



###################################################
### code chunk number 32: ZeligGAM.Rnw:671-672
###################################################
plot(s.out)


###################################################
### code chunk number 33: ZeligGAM.Rnw:681-686
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3), H=diag(0.5,37), 
   model="poisson.gam", data=my.data) 
summary(z.out)
plot(z.out$result,pages=1,residuals=TRUE)



###################################################
### code chunk number 34: ZeligGAM.Rnw:689-694
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3),sp=c(0.01,-1,-1,-1), 
   model="poisson.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1)



###################################################
### code chunk number 35: ZeligGAM.Rnw:697-702
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3),min.sp=c(0.001,0.01,0,10),  
    model="poisson.gam", data=my.data) 
summary(z.out)
plot(z.out$result, pages=1)



###################################################
### code chunk number 36: ZeligGAM.Rnw:705-710
###################################################
z.out <-zelig(y~s(x0,k=4,fx=TRUE,bs="tp")+s(x1,k=12)+s(x2,k=15), 
    model="poisson.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1)



###################################################
### code chunk number 37: ZeligGAM.Rnw:858-859
###################################################
library(ZeligGAM)


###################################################
### code chunk number 38: ZeligGAM.Rnw:860-873
###################################################
set.seed(0); n<-400; sig<-2
x0 <- runif(n, 0, 1); x1 <- runif(n, 0, 1); x2 <- runif(n, 0, 1); x3 <- runif(n, 0, 1)
f0 <- function(x) 2 * sin(pi * x) 
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
f3 <- function(x) 0*x
f <- f0(x0) + f1(x1) + f2(x2)

g <- (f-5)/3
g <- binomial()$linkinv(g)
y <- rbinom(g,1,g)
my.data <- as.data.frame(cbind(y, x0, x1, x2, x3))



###################################################
### code chunk number 39: ZeligGAM.Rnw:889-892
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3), model="probit.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1,residuals=TRUE)


###################################################
### code chunk number 40: ZeligGAM.Rnw:898-899
###################################################
plot(z.out$result,pages=1,residuals=TRUE)


###################################################
### code chunk number 41: ZeligGAM.Rnw:905-909
###################################################
 x.out <- setx(z.out)
 s.out <- sim(z.out, x = x.out) 
 summary(s.out) 
 plot(s.out) 


###################################################
### code chunk number 42: ZeligGAM.Rnw:914-915
###################################################
plot(s.out)


###################################################
### code chunk number 43: ZeligGAM.Rnw:924-931
###################################################
x.high <- setx(z.out,  x3= quantile(my.data$x3, 0.8))
x.low <- setx(z.out, x3 = quantile(my.data$x3, 0.2))

s.out <- sim(z.out, x=x.high, x1=x.low)
summary(s.out)
plot(s.out)



###################################################
### code chunk number 44: ZeligGAM.Rnw:936-937
###################################################
plot(s.out)


###################################################
### code chunk number 45: ZeligGAM.Rnw:946-951
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3), H=diag(0.5,37), 
   model="probit.gam", data=my.data) 
summary(z.out)
plot(z.out$result,pages=1,residuals=TRUE)



###################################################
### code chunk number 46: ZeligGAM.Rnw:954-959
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3),sp=c(0.01,-1,-1,-1), 
   model="probit.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1)



###################################################
### code chunk number 47: ZeligGAM.Rnw:962-967
###################################################
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3),min.sp=c(0.001,0.01,0,10),  
    model="probit.gam", data=my.data) 
summary(z.out)
plot(z.out$result, pages=1)



###################################################
### code chunk number 48: ZeligGAM.Rnw:970-975
###################################################
z.out <-zelig(y~s(x0,k=4,fx=TRUE,bs="tp")+s(x1,k=12)+s(x2,k=15), 
    model="probit.gam", data=my.data)
summary(z.out)
plot(z.out$result,pages=1)



