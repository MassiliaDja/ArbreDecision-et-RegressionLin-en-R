
library(rpart)
library(rpart.plot) 
library("readxl")
library(caret) 


md <- read_excel("TreeSet1.xlsx")
show(md)

# Factorisation des variables
md <- md[,c("DF","BH","BT","GP","DEC")]
md$DEC <- as.factor(md$DEC)
md$DF <- as.factor(md$DF)
md$BH <- as.factor(md$BH)
md$BT <- as.factor(md$BT)
md$GP <- as.factor(md$GP)

str(md)
show(md)

# Train et test
train <- createDataPartition(md$DEC,p=0.7,list=FALSE)
mdtrn <- md[train,]
mdtst <- md[-train,]


# modéle sur les données train
mdtree <- rpart(DEC ~., data =mdtrn,method="class" ,control=rpart.control(minsplit=0.01, mindepth = 4,cp= 0))

#Graphe de l'arbre
rpart.plot(mdtree, extra = 10)

#Affichage du résultat des donnees coisees
printcp(mdtree)

#Elagage de cp 
ffo <- prune(mdtree, cp=mdtree$cptable[which.min(mdtree$cptable[,5]),5])

#Affichage cp optimal, plus cp est petit, plus l'arbre peut être grand (beaucoup de noeuds)
print(mdtree$cptable[which.min(mdtree$cptable[,5]),5])

#Arbre aprés elagage
rpart.plot(ffo, extra = 10)

#Affichage du résultat de donnees coisees
printcp(ffo)


###### PREDICTIONS

#Predict
pred<-  predict(ffo, mdtst, type = "class")

#matrice de confusion
confusionMatrix(table(mdtst$DEC,pred))

#Arbre final
rpart.plot(ffo)

