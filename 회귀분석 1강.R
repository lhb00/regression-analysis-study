par(mfrow=c(1,2)) # par():그래프 모양을 다양하게 조절 가능한 그래픽 인수 설정/조회 함수
#mfrow:행을 우선적으로 지정하는 옵션, 즉 행이 1개, 열이 2개다.
panel1<-c(-1,-1);panel2<-c(3,3)
plot(c(panel1[1],panel2[1]),c(panel1[2],panel2[2]),type='n',axes=F,xlab="X=father's height", ylab="son's height")
# type='n'=>점을 찍지 않음
arrows(panel1[1], 0, panel2[1], 0, length=.1);arrows(0,panel1[2],0,panel1[2], 0, panel2[2], length=.1)
lines(c(0,2),c(1,1),lty=2);lines(c(2,2),c(0,1),lty=2) # lty:선의 스타일 결정
points(2,1,pch=19, cex=.5);text(2,-.25,'x');text(-.25,1,'y');text(2,1.25,'(x,y)')
# pch:점의 스타일 결정, cex:점의 크기 결정, 기본값 1
library(UsingR);data(father.son)
plot(father.son, xlab="Father's Height (Inches)", main="Galton's data", 
     ylab = "Son's Height (Inches)", cex=.25)

par(mfrow=c(1,1))
hist(father.son$sheight, main = "Histogram of Son's height", xlab = '')
axis(side=1, at=68, labels=68) # axis: 축을 그려주는 함수, side=1, at=68, labels=68로 68의 위치에 축 추가하고 68이라고 적어준다.

par(mfrow=c(1,2))
fs.withg <- father.son[order(father.son$fheight),]
fs.withg<-cbind(fs.withg, g=round(seq(1,13,length.out=nrow(fs.withg))))
plot(fs.withg[,-3],xlab="Father's Height (Inches)", ylab="Son's Height (Inches)",
     main = "Plot for subgroups", col=fs.withg$g, cex=.5)
hist(fs.withg[fs.withg$g==10,"sheight"],main="Histogram of Son's height with Father's height is about 72", xlab = '',
     breaks=7)
axis(side=1, at=71, labels=71)


par(mfrow=c(1,1))
plot(fs.withg[,-3], xlab="Father's Height (Inches)", ylab = "Son's Height (Inches)",
     main = "local means and regression line", col=fs.withg$g, cex=.5)
abline(lm(sheight~fheight, data=father.son))
for(i in 2:12){
  text(mean(fs.withg[fs.withg$g==i,"fheight"]),mean(fs.withg[fs.withg$g==i,"sheight"]),'m')
}