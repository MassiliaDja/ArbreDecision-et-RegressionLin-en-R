

V<- c(4,7,8,9,10,11,11,12,12,13,14,15,15,16,17,18,19,20,24,25)

D<- c(2,4,16,10,26,17,28,20,28,26,36,26,54,40,50,76,46,48,92,85)

#1) Moyennes
mean(V)
mean(D)

#2) Variances
var(V)
var(D)


# Visualisation de données
plot(D,V,main="Distance en fonction de la Vitesse", col="black",cex=1, pch=16)
plot(V,D,main="Vitesse en fonction de la Distance ", col="black",cex=1, pch=16)


## Droite regression D (D/V)  
## intercept represente le b de y= ax+b
lm(V ~ D)

#L'équation de notre droite est donc y = 0,1991x + 6,7317
plot(D, V)
abline(6.7317, 0.1991, col = 'red')



## Droite regression D (V/D)  
lm(D ~ V)
 
#L'équation, de notre droite est donc y = 4,297 x - 23,655
plot(V, D)
abline(-23.655, 4.297, col = 'red')

#representation des deux droites dans un seul graphe
plot(V,D,main="Distance en fonction de la Vitesse et Regresion Lineaire", col="black",cex=1, pch=16)
abline(-23.655, 4.297, col = 'red')
abline(6.7317, 0.1991, col = 'Blue')

#coef de corrélation (constat: forte corrélation)
cor(V,D)