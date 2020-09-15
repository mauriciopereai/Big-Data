remove(list = ls())
#2.1.1
set.seed(10101)
house_prices_ps1 <- read_csv("house_prices_ps1.csv")
coc<-house_prices_ps1

#prcesar NA'S 40 MINUTOS!!!!!
library(mice)
impute<-mice(coc[,16:19],m=1)
data<-complete(impute,1)
## remplazar nas por datos estimados 
coc$bedrooms<-data$bedrooms
coc$bathrooms<-data$bathrooms
coc$surface_total<-data$surface_total
coc$surface_covered<-data$surface_covered
install.packages("xlsx")

#2.1.2
library(table1)
table1::table1(~bedrooms + bathrooms + lon + lat + surface_total + surface_covered+property_type, data = coc)
coc$property_type<-as.factor(coc$property_type)

#2.1.3.a
##partición train test
indic<-sample(1:nrow(coc),floor(.7*nrow(coc)))
train<-coc[indic,]
test<-coc[-indic,]


#2.1.3.b
reg2<-lm(price~ 1 ,data=train)
yhat<-predict(reg2,test)
test$yhat<-predict(reg2,test)


reg3<-lm(price~ bedrooms + bathrooms + lon + lat + surface_total + surface_covered  ,data=train)
test$yhat1<-predict(reg3,test)

reg4<-lm(price~ (bedrooms^2) + lon + lat +(bathrooms^2)+(surface_total^2)+(surface_covered^2),data=train)
test$yhat2<-predict(reg4,test)

reg5<-lm(price~ (bedrooms^2) + lon + lat +(bathrooms^2)+(surface_total^2),data=train)
test$yhat3<-predict(reg5,test)

reg6<-lm(price~ (bedrooms*bathrooms)+(surface_total)+(property_type),data=train)
test$yhat4<-predict(reg6,test)

test$reg2<-(test$price-test$yhat)^2
test$reg3<-(test$price-test$yhat1)^2
test$reg4<-(test$price-test$yhat2)^2
test$reg5<-(test$price-test$yhat3)^2
test$reg6<-(test$price-test$yhat4)^2

mean(test$reg2)
mean(test$reg3)
mean(test$reg4)
mean(test$reg5)
mean(test$reg6)

Require("stargazer")

 stargazer(reg6,type="text")


#2.5.1
#LOOCV
laa<-data.frame()

for (i  in (1:nrow(coc))) {
regre<-coc[-i,]
prueba<-coc[i,]
regt<-lm(price~(bedrooms*bathrooms)+(surface_total)+(property_type) ,data=regre)
prueba$yhat<-predict(regt,prueba)
laa[i,1]<-(prueba[,20]-prueba$yhat)^2
}
mean(laa$V1)
#2.5.2
x<- subset(coc, select = c(bedrooms, bathrooms, lon, lat, surface_total, surface_covered ) )
xx<- data.matrix(x) 
hi<-data.frame()
for (i  in (1:nrow(coc))) {
hi[i,1]<-t(xx[i,])%*%solve(t(xx)%*%xx)%*%xx[i,]
}
reg_loocv<-lm(price~ bedrooms + bathrooms + lon + lat + surface_total + surface_covered  ,data=coc)
hi$yhat<-predict(reg_loocv,coc)
hi$precio<-coc$price
hi$loocv<-((hi$precio-hi$yhat)/(1-hi$V1))^2
loocv<-mean(hi$loocv)

#2.5.3
x2<- subset(coc, select = c(bedrooms, bathrooms, lon, lat, surface_total, surface_covered, property_type ) )
x2$bedrooms2<-(x2$bedrooms)^2
x2$surface_total2<-(x2$surface_total)^2
x2$surface_covered2<-(x2$surface_covered)^2
x2$bathrooms2<-(x2$bathrooms)^2
x2$babe<-x2$bathrooms*x2$bedrooms
##modelo
x3<- subset(x2, select = c(bedrooms2, bathrooms2, lon, lat, surface_total2, surface_covered2 ) )
xm1<- data.matrix(x3) 
hm1<-data.frame()
da<-solve((t(xm1)%*%xm1),tol = 1e-60)
for (i  in (1:nrow(coc))) {
  hm1[i,1]<-t(xm1[i,])%*%(da)%*%xm1[i,]
}
reghm1<-lm(price~ (bedrooms^2) + lon + lat +(bathrooms^2)+(surface_total^2)+(surface_covered^2)  ,data=coc)
hm1$yhat<-predict(reghm1,coc)
hm1$precio<-coc$price
hm1$loocv<-((hm1$precio-hm1$yhat)/(1-hm1$V1))^2
loocv1<-mean(hm1$loocv)


##modelo
x4<-subset(x2, select = c(bedrooms2, bathrooms2, lon, lat, surface_total2 ) )
xm2<- data.matrix(x4) 
hm2<-data.frame()
da1<-solve((t(xm2)%*%xm2),tol = 1e-60)
for (i  in (1:nrow(coc))) {
  hm2[i,1]<-t(xm2[i,])%*%(da1)%*%xm2[i,]
}
reghm2<-lm(price~ (bedrooms^2) + lon + lat +(bathrooms^2)+(surface_total^2)  ,data=coc)
hm2$yhat<-predict(reghm2,coc)
hm2$precio<-coc$price
hm2$loocv<-((hm2$precio-hm2$yhat)/(1-hm2$V1))^2
loocv2<-mean(hm2$loocv)
##modelo
x5<-matrix(1, nrow =nrow(coc), ncol = 1)
xm3<- data.matrix(x5) 
hm3<-data.frame()
for (i  in (1:nrow(coc))) {
  hm3[i,1]<-t(xm3[i,])%*%solve(t(xm3)%*%xm3)%*%xm3[i,]
}
reghm3<-lm(price~ 1 ,data=coc)
hm3$yhat<-predict(reghm3,coc)
hm3$precio<-coc$price
hm3$loocv<-((hm3$precio-hm3$yhat)/(1-hm3$V1))^2
loocv3<-mean(hm3$loocv)

#2.6.1
library(tidyverse)

df <- coc[,c('bedrooms', 'bathrooms', 'lon', 'lat', 'surface_total', 'price')] %>%
      mutate(bedrooms2 = bedrooms^2, bathrooms2 = bathrooms^2, surface_total2 = surface_total^2) %>%
      select(-bedrooms, -bathrooms, -surface_total) %>%
      cbind(intercept = 1)

x <- df[, c('intercept', 'bedrooms2', 'bathrooms2', 'lon', 'lat', 'surface_total2')]
y<- data.matrix(df$price) 

qr.coef(qr(x), y)
a <- as.data.frame(qr.resid(qr(x), y)) %>%
      rename(residuales = V1)
a$residualescuad<-(a$price)^2
mean(a$residualescuad)

##comparar con eso
reg<-lm(price~ bedrooms2 + lon + lat + bathrooms2 + surface_total2, data=df)
summary(reg)
#2.6.2

Bogota<- df[ which(coc$l3 =='Bogotá D.C' ),]
Barranquilla<- df[ which(coc$l3 =='Barranquilla' ),]
Cali<- df[ which(coc$l3 =='Cali' ),]
Medellin<- df[ which(coc$l3 =='Medellín' ),]
#2.6.3

# Bogotá
      x_bogota <- Bogota[, c('intercept', 'bedrooms2', 'bathrooms2', 'lon', 'lat', 'surface_total2')]
      y_bogota <- data.matrix(Bogota$price)
      
      qr.coef(qr(x_bogota), y_bogota)
      a_bogota <- as.data.frame(qr.resid(qr(x_bogota), y_bogota)) %>%
            rename(residuales = V1)
      a_bogota$residualescuad <- (a_bogota$residuales)^2
      mean(a_bogota$residualescuad)
      
      ##comparar con eso
      reg_bogota<-lm(price~ bedrooms2 + lon + lat + bathrooms2 + surface_total2, data=Bogota)
      summary(reg_bogota)

# Barranquilla
      x_barranquilla <- Barranquilla[, c('intercept', 'bedrooms2', 'bathrooms2', 'lon', 'lat', 'surface_total2')]
      y_barranquilla <- data.matrix(Barranquilla$price)
      
      qr.coef(qr(x_barranquilla), y_barranquilla)
      a_barranquilla <- as.data.frame(qr.resid(qr(x_barranquilla), y_barranquilla)) %>%
            rename(residuales = V1)
      a_barranquilla$residualescuad <- (a_barranquilla$residuales)^2
      mean(a_barranquilla$residualescuad)
      
      ##comparar con eso
      reg_barranquilla<-lm(price~ bedrooms2 + lon + lat + bathrooms2 + surface_total2, data=Barranquilla)
      summary(reg_barranquilla)

# Cali
      x_cali <- Cali[, c('intercept', 'bedrooms2', 'bathrooms2', 'lon', 'lat', 'surface_total2')]
      y_cali <- data.matrix(Cali$price)
      
      qr.coef(qr(x_cali), y_cali)
      a_cali <- as.data.frame(qr.resid(qr(x_cali), y_cali)) %>%
            rename(residuales = V1)
      a_cali$residualescuad <- (a_cali$residuales)^2
      mean(a_cali$residualescuad)
      
      ##comparar con eso
      reg_cali<-lm(price~ bedrooms2 + lon + lat + bathrooms2 + surface_total2, data=Cali)
      summary(reg_cali)

# Medellín
      x_medellin <- Medellin[, c('intercept', 'bedrooms2', 'bathrooms2', 'lon', 'lat', 'surface_total2')]
      y_medellin <- data.matrix(Medellin$price)
      
      qr.coef(qr(x_medellin), y_medellin)
      a_medellin <- as.data.frame(qr.resid(qr(x_medellin), y_medellin)) %>%
            rename(residuales = V1)
      a_medellin$residualescuad <- (a_medellin$residuales)^2
      mean(a_medellin$residualescuad)
      
      ##comparar con eso
      reg_medellin<-lm(price~ bedrooms2 + lon + lat + bathrooms2 + surface_total2, data=Medellin)
      summary(reg_medellin)

# Los resultados obtenidos entre la regresión lineal lm y la descomposición QR son iguales. Esto es debido a que la estimación QR se logra a traves de una trasformación de matrices que no afecta los betas de las estimaciones.


#2.6.4

# Map
  xl <- list()
  xl[[1]] <- x_bogota
  xl[[2]] <- x_barranquilla
  xl[[3]] <- x_cali
  xl[[4]] <- x_medellin
  
  yl <- list()
  yl[[1]] <- y_bogota
  yl[[2]] <- y_barranquilla
  yl[[3]] <- y_cali
  yl[[4]] <- y_medellin
  
  QR = list()
  for(j in 1:4) QR[[j]] = list(Q=qr.Q(qr(as.matrix(xl[[j]]))), R=qr.R(qr(as.matrix(xl[[j]]))))
  
  R = QR[[1]]$R
  for(j in 2:4) R = rbind(R,QR[[j]]$R)
  Q_2 = qr.Q(qr(as.matrix(R)))
  R_2 = qr.R(qr(as.matrix(R)))
  
  Q_2list <- list()
  for(j in 1:4) Q_2list[[j]] = Q_2[(j-1)*6+1:6,]

# Reduce
  Q_3list = list()
  for(j in 1:4) Q_3list[[j]] = QR[[j]]$Q %*% Q_2list[[j]]
  
  Vl = list()
  for(j in 1:4) Vl[[j]] = t(Q_3list[[j]]) %*% yl[[j]] 
  
  sumV = Vl[[1]]
  for(j in 2:4) sumV = sumV+Vl[[j]]
  solve(R_2) %*% sumV