library(gvlma)
library(trafo)

summary(fit2)
gvlma(model2)


assumptions(model2, method = "ml", plotit = FALSE)

x<-logshiftopt(object = model2, method = "div.ks", plotit = TRUE)

x$lambdavector[x$measvector == x$measoptim]


linMod_trafo2 <- trafo_lm(object = fit1, trafo = "logshiftopt", method = "skew")

plot(linMod_trafo2)
plot(fit2)

fit1<-lm(log(sales+23) ~ youtube,data = marketing)
fit2<-lm(log(sales+23) ~ I(poly(youtube,6)),data = marketing)
anova(fit1 ,fit2)

plot(fit2)
