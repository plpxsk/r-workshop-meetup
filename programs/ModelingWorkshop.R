## read in dataset
college <- read.csv("College.csv")
rownames(college) <- college$X
college$X <- NULL
college <- college[college$Grad.Rate < 100,]

## can we predict the graduation rate by student / faculty ratio?
plot(college$S.F.Ratio,college$Grad.Rate)
lm.fit <- lm(Grad.Rate ~ S.F.Ratio,data=college)
abline(lm.fit)
print(summary(lm.fit))

## what if we remove that outlier?
college.s <- college[college$S.F.Ratio < 30,]
plot(college.s$S.F.Ratio,college.s$Grad.Rate)
lm.fit.s <- lm(Grad.Rate ~ S.F.Ratio,data=college.s)
abline(lm.fit.s)
## in ggplot2:
## qplot(S.F.Ratio, Grad.Rate, data = college,
##       geom=c("point", "smooth"), method=lm)


print(summary(lm.fit.s))  ## basically the same

## how about number of top 10 percent hs students predicting graduation rate?
plot(college$Top10perc,college$Grad.Rate)
lm.fit <- lm(Grad.Rate ~ Top10perc,data=college)
abline(lm.fit,col='blue')
print(summary(lm.fit))

## what linear model have we learned?
coef(lm.fit)
confint(lm.fit)

## what if we had 4 all new colleges? Could we guess their graudation rate?
newcolleges <- data.frame(
  CollegeName=c("MattU","PavoTech","ApoorvaCollege","SheamusInstitute"),
  Top10perc=c(50,60,99,5)
)
rownames(newcolleges) <- newcolleges$CollegeName
predict(lm.fit,newdata=newcolleges)
predict(lm.fit,newdata=newcolleges,interval="prediction")

## lm diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)

## Multiple Regression (multiple independant variables)
lm.fit.mult <- lm(Grad.Rate ~ Top10perc + Private,data=college)
.coef(lm.fit.mult) ## some might argue this means being a private school adds 10% to the graduation rate
## only true if Top10perc and Private are totally independant, which is not true, but still a useful measure
par(mfrow=c(1,1))
plot(college$Top10perc,college$Grad.Rate,col=ifelse(college$Private=="Yes",'red','blue'))
abline(lm.fit.mult,col='blue')
abline(a=coef(lm.fit.mult)[1] + coef(lm.fit.mult)[3], b= coef(lm.fit.mult)[2],col='red')
legend('bottomright',legend=c("Private (1)","Not Private (0)"),fill=c("red","blue"))

## what about just using every variable?
lm.fit.all <- lm(Grad.Rate ~ ., data=college)
print(summary(lm.fit.all)) ## good fit, but probably overfit, and all variables are correlated

#################################################
## logistic regression
## guess if a college is private by its acceptance rate and size
college$AcceptanceRate <- college$Accept / college$Apps
glm.fit <- glm(Private ~ AcceptanceRate + F.Undergrad,data=college,family='binomial')
predict(glm.fit,type='response')
## plot the results
p <- predict(glm.fit,type='response')
plot(p[order(p)],col=ifelse(college$Private=="Yes",'red','blue')[order(p)],ylab="Probability of PrivateYes")
legend('bottomright',legend=c("PrivateYes","PrivateNo"),fill=c("red","blue"))
abline(h=0.5,lty=2)

## how often would we have been right?
table(PredictedPrivate=p > .5, isPrivate=college$Private)

## Cross validated model?
college$CV.index <- rep(1:10,length=nrow(college))
for(i in 1:10) {
  in.fold <- college$CV.index != i
  glm.fit.cv <- glm(Private ~ AcceptanceRate + F.Undergrad, data=college[in.fold,],family='binomial')
  p <- predict(glm.fit.cv,type='response',newdata=college[!in.fold,])
  college[!in.fold,'prediction'] <- p
}
table(PredictedPrivate=college$prediction > .5, isPrivate=college$Private)

install.packages("ROCR")
library("ROCR")
pred <- prediction(college$prediction,college$Private=="Yes")
perf <- performance(pred,measure='tpr',x.measure='fpr')
perf.auc <- performance(pred,measure='auc')
plot(perf)
abline(a=0,b=1,lty=3)
text(.5,.2,paste("AUC:",formatC(perf.auc@y.values[[1]])))
