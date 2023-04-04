# 2.5
# (a)
x<-c(1,2,3,4,4,5,6,6,7,8,9,9,10,10)
y<-c(23,29,49,64,74,87,96,97,109,119,149,145,154,166)
yhat<-round(predict(lm(y~x)),4) # predict(): 예측값(추정량)을 표시하는 함수
yhat
round(sum(y-yhat), 4)
# Sum of the ordinary least squares residuals is zero
# (b)
# 2.26
summary(lm(y~x))
# from the output, beta 1 hat=15.509/s.e(beta 1 hat)=0.505
t11<-round(15.509/0.505,2)
t11
# 2.32
t12<-round(cor(y,x)*sqrt(14-2)/sqrt(1-(cor(y,x))^2),2)
t12
# (c)
plot(y,x)
plot(y,yhat)
# (d)
cor(y, yhat)
# 2.6
# (a)
round(cor(y,x),4)==round(cor(y,yhat),4)
# (b)
sst=round(sum((y-mean(y))^2),3)
sst
# (c)
sse=round(sum((y-yhat)^2),3)
sse
# 2.7
# (a)
x1<-c(10,8,13,9,11,14,6,4,12,7,5)
y1<-c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10,84,4.82,5.68)
b11<-sum((y1-mean(y1))*(x1-mean(x1)))/sum((x1-mean(x1))^2)
b11
b01<-mean(y1)-b11*mean(x1)
b01
x2<-c(10,8,13,9,11,14,6,4,12,7,5)
y2<-c(9.14,9.14,9.74,8.77,9.26,8.1,6.13,3.1,9.13,7.26,4.74)
b12<-sum((y2-mean(y2))*(x2-mean(x2)))/sum((x2-mean(x2))^2)
b12
b02<-mean(y2)-b12*mean(x2)
b02
# 2.10
H<-c(186,180,160,186,163,172,192,170,174,191,182,178,181,168,162,188,168,183,188,166,180,176,185,169,182,162,169,176,180,157,170,186,180,188,153,179,175,165,156,185,172,166,179,181,176,170,165,183,162,192,185,163,185,170,176,176,160,167,157,180,172,184,185,165,181,170,161,188,181,156,161,152,179,170,170,165,165,169,171,192,176,168,169,184,171,161,185,184,179,184,175,173,164,181,187,181)
W<-c(175,168,154,166,162,152,179,163,172,170,170,147,165,162,154,166,167,174,173,164,163,163,171,161,167,160,165,167,175,157,172,181,166,181,148,169,170,157,162,174,168,162,159,155,171,159,164,175,156,180,167,157,167,157,168,167,145,156,153,162,156,174,160,152,175,169,149,176,165,143,158,141,160,149,160,148,154,171,165,175,161,162,162,176,160,158,175,174,168,177,158,161,146,168,178,170)
# (a)
cov(H,W)
# (b)
# The covariance would be smaller if heights were measured in inches rather than in centimeters
Hinch<-H/2.54
Winch<-W/2.54
cov(Hinch,Winch)
# (c)
cor(H,W)
# (d)
# The correlation is same
cor(Hinch,Winch)
# (e)
# The correlation is 1
W2<-H-5
cor(H,W2)
# (f)
# I choose heights of husbands as the response variable because heights of hubands are higher than the wives'.
# (g)
summary(lm(H~W)) 
# From the result, the statistic slope is observed 11.458, and thus Pr(>|t|)=2e-16, null hypothesis rejected on alpha=0.05
# (h)
# From the result, the statistic intercept is observed 3.169, and thus Pr(>|t|)=0.00207, null hypothesis rejected on alpha=0.05