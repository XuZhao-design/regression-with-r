# title: "如何利用 R 软件进行回归分析2"
# author: "赵旭"

#1. 导入数据
#获取数据
data(birthwt, package="MASS")

#2. 计算变量相关性，去掉多余的变量，变量类型准备
birthwt <- birthwt[,!names(birthwt) %in% c("low")]
birthwt2 <- birthwt
cols_to_factor <- c("race","smoke","ptl","ht","ui","ftv")
birthwt[cols_to_factor] <- lapply(birthwt[cols_to_factor], as.factor)

summary(birthwt)

#3. 执行多元回归分析
##加入所有变量
# *因变量*
#  *bwt：新生儿的出生体重，以克为单位*
#  
#  *自变量*
#  *lwt：母亲末次月经时的体重，以磅为单位*
#  *age：母亲的年龄，以岁为单位*
#  *race：母亲的种族（1 = 白种人，2 = 黑种人，3 = 其他）*
#  *smoke：孕期吸烟状况*
#  *ptl：此前早产次数*
#  *ht：高血压病史*
#  *ui：子宫易激状态*
#  *ftv：孕早期看医生的次数*

lm1 <- lm(bwt ~ age+lwt+race+smoke+ptl+ht+ui+ftv, data=birthwt)

#4. 识别显著性变量
summary(lm1)
confint(lm1)

#5. 回归模型诊断
library(car)
#normal quantile plot of residuals
qqPlot(lm1$resid, pch = 19, col = 'blue')

#plot of fitted vs. residuals
plot(lm1, which = 1, pch = 19, col = 'blue', lwd = 2)

###敏感性分析
#使用`leaps`包筛选最佳子集
library(leaps)
mod1 <- regsubsets(bwt ~ ., data = birthwt2, nvmax = 10)
mod1sum <- summary(mod1)
names(mod1sum)

#notice that 'which' is actually a matrix with TRUE/FALSE information on inclusion of each possible predictor
mod1sum$which

#Best model according to R-squared (should be model with all predictors)
which.max(mod1sum$rsq)

#Which variables are in the model
names(birthwt2)[mod1sum$which[which.max(mod1sum$rsq),][-1]]

#Fit this model and show results
WBtemp <- birthwt2[,mod1sum$which[which.max(mod1sum$rsq),]]
cols_to_factor <- c("race","smoke","ptl","ht","ui","ftv")
WBtemp[cols_to_factor] <- lapply(WBtemp[cols_to_factor], as.factor)

lmod3 <- lm(bwt ~ .,data=WBtemp)
summary(lmod3)
#此法计算的最佳模型：纳入全部变量

#Best model according to Adjusted R-squared
which.max(mod1sum$adjr2)

#Which variables are in the model
names(birthwt2)[mod1sum$which[which.max(mod1sum$adjr2),][-1]]

#Fit this model and show results
WBtemp <- birthwt2[,mod1sum$which[which.max(mod1sum$adjr2),][-1]]
WBtemp <- cbind(WBtemp, bwt=birthwt2$bwt)
cols_to_factor <- c("race","smoke","ht","ui")
WBtemp[cols_to_factor] <- lapply(WBtemp[cols_to_factor], as.factor)

lmod3 <- lm(bwt ~ .,data=WBtemp)
summary(lmod3)

#5. 回归模型诊断
library(car)
#normal quantile plot of residuals
qqPlot(lmod3$resid, pch = 19, col = 'blue')

#Best model according to Bayesian Information Criteria (BIC)
which.min(mod1sum$bic)

#Which variables are in the model
names(birthwt2)[mod1sum$which[which.min(mod1sum$bic),][-1]]

#Fit this model and show results
WBtemp <- birthwt2[,mod1sum$which[which.min(mod1sum$bic),][-1]]
WBtemp <- cbind(WBtemp, bwt=birthwt2$bwt)
cols_to_factor <- c("race","smoke","ht","ui")
WBtemp[cols_to_factor] <- lapply(WBtemp[cols_to_factor], as.factor)

lmod3 <- lm(bwt ~ .,data=WBtemp)
summary(lmod3)

#5. 回归模型诊断
library(car)
#normal quantile plot of residuals
qqPlot(lmod3$resid, pch = 19, col = 'blue')

#plot of fitted vs. residuals
plot(lmod3, which = 1, pch = 19, col = 'blue', lwd = 2)

#Best model according to AIC
npred <- length(mod1sum$bic)
AICvec <- rep(NA, npred)
for (i in 1:npred){
  WBtemp <- birthwt2[,mod1sum$which[i,][-1]]
  WBtemp <- as.data.frame(cbind(WBtemp, bwt=birthwt2$bwt))
  AICvec[i] <- AIC(lm(bwt ~ .,data=WBtemp))
}

AICvec
#Fit best model and show results
names(birthwt2)[mod1sum$which[which.min(AICvec),][-1]]
WBtemp <- birthwt2[,mod1sum$which[which.min(AICvec),][-1]]
WBtemp <- cbind(WBtemp, bwt=birthwt2$bwt)
cols_to_factor <- c("race","smoke","ht","ui")
WBtemp[cols_to_factor] <- lapply(WBtemp[cols_to_factor], as.factor)

lmod3 <- lm(bwt ~ .,data=WBtemp)
summary(lmod3)
