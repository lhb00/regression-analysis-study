rm(list=ls())
SP <- read.csv('http://www1.aucegypt.edu/faculty/hadi/RABE5/Data5/P060.txt', header=T, sep='\t')
# Getting the fitted table
model <- lm(Y~., data = SP)
summary(model)
# F-test
# significance of model
M0 <- lm(Y ~ 1, data = SP)
M1 <- lm(Y ~ ., data = SP)
anova(M0, M1) # anova(): aov()와 역할은 동일하다. 다만 aov(종속변수~그룹변수, data) 형태인 것과 다르게 anova()는 위처럼 lm() 이용 모형 선생성이 필수이다.
qf(.99, 6, 30 -6 -1) # F-value for alpha = 0.01
# comparision between models
M0 <- lm(Y ~ 1 + X1 + X3, data = SP)
M1 <- lm(Y ~ ., data = SP)
anova(M0, M1)
qf(.95, 4, 30 - 4 -1) # F-value for alpha = 0.05 # qf(확률, 표본 1의 자유도, 표본 2의 자유도, 비중심모수?)
# 비중심 분포(t, F, 카이제곱 분포) 평균이 0이 아닌 정규 랜덤 변수의 표본에서 파생 가능, 비중심 모수에 의해 중심 분포와 구별
# 비중심 모수: 비모수 평균이 귀무 가설이 참일 시 검정 통계량의 평균이 평균으로부터 떨어진 정토를 나타냄, 일반적으로 사용되는 검정 통계량 설명 시 유용
# Testing subset of regression coef . are zero
M0 <- lm(Y ~ 1 + I(X1 + X3), data = SP)
M1 <- lm(Y ~ 1 + X1 + X3, data = SP)
anova(M0, M1)
# Constrained regression :
M0 <- lm(Y - X3 ~ 1 + I(X1 - X3), data = SP)
M1 <- lm(Y ~ 1 + X1 + X3, data = SP)
Rq2 <- 1-sum((SP$Y-(M0$fit+SP$X3))^2)/sum((SP$Y-mean(SP$Y))^2)
Rp2 <- summary(M1)$r.squared
((Rp2-Rq2)/(2 - 1))/((1 - Rp2)/(30 - 2 - 1))
