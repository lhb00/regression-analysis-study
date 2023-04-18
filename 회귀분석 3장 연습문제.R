# 3.1
data1<-read.csv('https://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P060.txt', header = T, sep = '\t')
lm(Y~X2, data=data1) # fitted equation is Yhat = 42.1087+0.4239X2
yhat<-predict(lm(Y~X2, data=data1))
eY.X2<-data1$Y-yhat
lm(X1~X2, data=data1) # fitted equation is Yhat = 34.3196+0.6075X1
x1hat<-predict(lm(X1~X2, data=data1))
eX1.X2<-data1$X1-x1hat
lm(eY.X2~eX1.X2) # eY,X2hat = 0.0000+0.7803ex1,x2
# 3.2
# 데이터 만들어낼 시간 없음 ㅅㄱ
# 3.3
data3<-read.csv('https://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P083.txt', header = T, sep = '\t')
# (a)
# model 1
M1<-lm(F~P1, data = data3) # Fhat = -22.342 + 1.261P1
# model 2
M2<-lm(F~P2, data = data3) # Fhat = -1.854 + 1.004P2
# model 3
M3<-lm(F~P1+P2, data = data3) # Fhat = -14.5005 + 0.4883P1 + 0.6720P2
# (b)
summary(M1) # intercept의 p-value가 0.0676으로 0.05보다 큰 것을 확인, 기각 불가능
summary(M2) # intercept의 p-value가 0.809로 0.05보다 큰 것을 확인, 기각 불가능
summary(M3) # intercept의 p-value가 0.13290으로 0.05보다 큰 것을 확인, 기각 불가능
# (c)
# model1의 R^2과 수정된 R^2이 각각 0.8023/0.7924
# model2의 R^2과 수정된 R^2이 각각 0.86/0.853
# P1보다 P2가 더 나은 예측변수라고 할 수 있다.
# (d)
# P1과 P2가 모두 들어있는 model이 model3이므로 model3를 사용할 것이다.(R^2/수정된 R^2도 가장 크다.)
# -14.5005 + 0.4883*78 + 0.6720*85 = 80.7069가 예측값
# 3.4 
# 데이터를 찾거나 만들라는데 그럴 시간 없음 ㅅㄱ
# 3.5
Y = data3$F
X1 = data3$P1
X2 = data3$P2
# (a)
lm(Y~X1) # beta1prime은 1.261이다.
lm(Y~X1+X2) # beta1은 0.4883이다. # beta2는 0.6720이다.
lm(X2~X1) # alpha1hat은 1.149이다.
# 0.4883 + 0.6720*1.149 = 1.261이므로 성립.
# (b)
lm(Y~X2) # beta2prime은 1.004이다.
lm(X1~X2) # alpha2hat은 0.6803이다.
# 0.6720 + 0.4883*0.6803 1.004이므로 성립
# 3.6 ~ 3,9는 손으로 풀었음.
# 3.10
# (a)
Y1<-data1$Y-data1$X3
V=data1$X1-data1$X3
summary(lm(Y1~V)) # R^2=0.5744
summary(lm(data1$Y~data1$X1+data1$X3)) # R^2=0.708
# F = 0.708-0.5744/2-1/(1-0.7080/30-2-1)=12.353
# F(1, 24, 0.05)=4.21이므로 귀무가설 기각
# (b)
summary(lm(data1$Y~data1$X1+data1$X2+data1$X3))
# F = (0.715-0.5744/3-1)/(1-0.715/30-3-1)=6.4133
# F(1,24, 0.05)=4.21이므로 귀무가설 기각