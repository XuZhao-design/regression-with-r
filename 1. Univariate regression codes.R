# title: "如何利用 R 软件进行回归分析1"
# author: "赵旭"

#1. 导入数据
#获取数据
data(birthwt, package="MASS")
head(birthwt)

#查看数据集的描述
help(birthwt)
attach(birthwt)

#2. 计算变量相关性
#自建绘制散点图和计算相关性的程序
myCor <- function(x,y){
  plot(x,y,pch=19, col="red")
  mtext(paste("Sample Correlation =", round(cor(x,y),3)), cex=1.2)
}

#查看数据中三个连续变量的相互关系
myCor(bwt, lwt)
myCor(bwt, age)
myCor(lwt, age)

library(corrplot)
birthwt2 <- na.omit(birthwt[,c("bwt","lwt","age")])

sigcorr <- cor.mtest(birthwt2,conf.level=0.95)
sigcorr
corrplot.mixed(cor(birthwt2),lower.col="black",upper = "ellipse",
               tl.col = "black",number.cex=0.7,tl.pos = "lt",tl.cex=0.7)

#3. 执行回归分析
#拟合模型
lm1 <- lm(bwt ~ lwt)

#模型生成的变量
names(lm1)
lm1$call
lm1$coefficients

#绘制回归曲线
plot(lwt,bwt,col = 'red', pch = 19)
abline(lm1, col = "blue", lwd = 3)
mtext(paste("bwt =",round(lm1$coef[1],3)," + ",round(lm1$coef[2],3),"*lwt"), line = 0)

#4. 识别显著性变量
#模型总结
names(summary(lm1))
summary(lm1)
confint(lm1,'lwt')

#5. 回归模型诊断
library(car)

#normal quantile plot of residuals
qqPlot(lm1$resid, pch = 19, col = 'blue')

#plot of fitted vs. residuals
plot(lm1, which = 1, pch = 19, col = 'blue', lwd = 2)

#计算Correlation与R2
#Correlation
cor(bwt,lwt)

#R^2
cor(bwt,lwt)^2

summary(lm1)
#fraction of variance explained
var(fitted(lm1)) / var(bwt) #  R-squared  - variance explained
var(resid(lm1)) / var(bwt)  #   1 - Rquared - variance unexplained

#寻找异常值
#创建数据表
data <- matrix(c(1,0,0,1,1,2,2,1,10,10),byrow=T,ncol=2)
plot(data,pch=19)

mod1 <- lm(data[,2]~data[,1])
#Cook‘s distance
plot(mod1,which=4)
abline(mod1$coef, col='red',lwd=2)

# A simulation to see how much the line changes without each point
for (i in 1:5){
  mod2 <- lm(data[-i,2]~data[-i,1])
  plot(data, pch=19, cex=1.5)
  points(data[i,1],data[i,2],pch=19,col='red',cex=4)
  abline(mod1$coef, col='blue', lwd=3)
  abline(mod2$coef, col='red', lwd=3, lty=2)
  legend("topleft",c("Original Regression","Without Red Point"), col=c("blue", "red"), lwd=4, lty = c(1,2))
  Sys.sleep(3)
}
summary(mod1)
summary(mod2)

library(olsrr)
ols_plot_cooksd_bar(mod1)
ols_plot_cooksd_bar(lm1)
