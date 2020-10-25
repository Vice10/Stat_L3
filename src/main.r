library(datarium)
marketingLib <- datarium::marketing
marketing <- read.csv("C:\\Statistics_Labs\\L3_Regress\\data\\myMarketing.csv")

library(ggplot2)
ggplot(marketing,aes(youtube,sales))+geom_point()+geom_smooth()
ggplot(marketing,aes(facebook,sales))+geom_point()+geom_smooth()

library(car)
pairs(~sales+youtube+facebook+newspaper, data=marketing)

model<-lm(sales ~ youtube,data = marketing)
summary(model)
confint(model)

library(DMwR)
regr.eval(marketing$sales,model$fitted.values)

plot(model)

durbinWatsonTest(model)


boxplot(model$residuals)

install.packages("rgl")
library(rgl)
scatter3d(x=marketing$youtube, y=marketing$facebook, z=marketing$sales)
scatter3d(x=marketing$youtube, y=marketing$facebook, z=marketing$sales, fit="smooth")
scatter3d(x=marketing$youtube, y=marketing$facebook, z=marketing$sales, fit="smooth", surface=FALSE, ellipsoid=TRUE, grid=FALSE)

scatter3d(sales~facebook * youtube, data=marketingLib)

library(ggiraph)
library(ggiraphExtra)
library(plyr)
model2<-lm(sales ~ youtube,data = marketing)
ggPredict(model2,se=TRUE,interactive=TRUE)


plot(marketingLib$sales, model2$residuals, ylab = "Residuals", xlab = "Response", main = "Residuals vs Response")

