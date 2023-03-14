# 3번 슬라이드
panel1 <- c(0,0);panel2 <-c(4,4)
plot(c(panel1[1],panel2[1]),c(panel1[2],panel2[2]),type='n',axes=F,xlab=expression(X),ylab=expression(Y))
# expression():그래프 제목에 함수를 넣을 수 있다.
arrows(panel1[1],0,panel2[1],0,length=.1);arrows(0,panel1[2],0,panel2[2],length = .1)
lines(c(0,4),c(2,2),lty=2);lines(c(2,2),c(0,4),lty=2)
x<-runif(30,0,4);e<-rnorm(30,0,.4) # runif, rnorm: 난수 생성
points(x, x+e,pch=19)
text(c(3.8,.2,.2,3.8), c(2.3, 2.3, 1.7, 1.7),
     c(expression((1)),expression((2)),expression((3)),expression((4)))
plot(c(panel1[1],panel2[1]),c(panel1[2],panel2[2]),type='n',axes=F,xlab=expression(X),ylab=expression(Y))
arrows(panel1[1],0,panel2[1],0,length=.1);arrows(0,panel1[2],0,panel2[2],length=.1)
lines(c(0,4),c(2,2),lty=2);lines(c(2,2),c(0,4),lty=2)
x<-runif(30,0,4);e<-rnorm(30,0,.4)
points(x,-x+4+e,pch=19)
text(c(3.8,.2,.2,3.8),c(2.3,2.3,1.7,1.7),
     c(expression((1)),expression((2)),expression((3)),expression((4))))
# 9번 슬라이드
par(mfrow=c(1,1))
df<-read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P029a.txt',
             sep='\t')
plot(df$X, df$Y, xlab = 'X', ylab = 'Y', pch = 19)
# 10번 슬라이드
par(mfrow=c(2,2))
df<-read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P029b.txt',
             sep='\t')
plot(df$X1,df$Y1,pch=19,xlab='X1',ylab='Y1')
abline(coef=coef(lm(Y1~X1,data=df)),col='red')
plot(df$X2,df$Y2,pch=19,xlab='X2',ylab='Y2')
abline(coef=coef(lm(Y2~X2, data=df)), col='red')
plot(df$X3, df$Y3, pch=19, xlab='X3', ylab='Y3')
abline(coef=coef(lm(Y3~X3, data=df)), col='red')
plot(df$X4, df$Y4, pch=19, xlab = 'X4', ylab='Y4')
abline(coef=coef(lm(Y4 ~ X4, data = df)), col='red')
#coef(회선의 계수)
# 22번 슬라이드
df<-read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P031.txt',
             sep='\t')
par(mfrow=c(1,1))
plot(df$Units,df$Minutes,pch=19,xlab='Units',ylab='Minutes')
abline(coef=coef(lm(Minutes ~ Units, data=df)))
# 31번 슬라이드
par(mfrow=c(1,1))
plot(function(x)dnorm(x),-3,3,ylab='density')
plot(function(x) dt(x, df = 20), -3, 3, col = 'red', add = T)
plot(function(x) dt(x, df = 3), -3, 3, col = 'blue', add = T)
# 32번 슬라이드
par(mfrow=c(1,2))
plot(c(-3,3), c(-.05,.5), xlab='',ylab='',axes=F,type='n')
plot(function(x)dt(x, 1), -3,3, add=T)
seq0<-seq(2.2, 2.0, length.out=100)
x <- c(seq0,rev(seq0))
y1 <- rep(0, 100)
y2 <- rev(dt(seq0,1))
polygon(x, c(y1,y2), col="gray")
arrows(-3, 0, 3, 0, length = .075);lines(c(0,0), c(0,dt(0,1)))
text(2.5, dt(2.5,1)+.03, expression(alpha));text(0,-0.25,0);text(2.2,-.025, expression(t[paste(n,'',alpha)]))
plot(c(-3,3),c(-.05,.5),xlab='',ylab='',axes=F,type='n')
plot(function(x)dt(x,1),03,3,add=T)
seq0<-seq(-2.9,-2.2,length.out=100)
x<-c(seq0,rev(seq0))
y1<-rep(0,100)
y2<-rev(dt(seq0,1))
polygon(x,c(y1,y2),col="gray")
arrows(-3,0,3,0,length=.075);lines(c(0,0), c(0,dt(0,1)))
text(-2.5, dt(-2.5,1) + .03, expression(pt(t)));text(0,-0.25,0); text(-2.2, -.025, 't')
# 컴퓨터 수리 데이터
rm(list=ls())
# 37번 슬라이드
x<-c(1,2,3,4,4,5,6,6,7,8,9,9,10,10)
y<-c(23,29,49,64,74,87,96,97,109,119,149,154,166)
p027<-data.frame(x = x, y = y)
plot(p027)
lm.fit<-lm(formula=y~x,data=p027)
predicted <- lm.fit$fitted.values
residual<-lm.fit$residuals
summary(lm.fit)
cbind(OBS=1:length(x), predicted, residual)
plot(p027)
abline(coef=lm.fit$coefficients, lty=2)
points(8, predicted[10],col=2,pch=19,cex=3)

