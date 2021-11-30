
library(rpart)
library(rpart.plot)
library("readxl")
library(caret) 

md <- read_excel("TreeSet2.xlsx")
show(md)

# Factorisation
md <- md[,c("meteo","amis","vent","jour","DEC")]
md$DEC <- as.factor(md$DEC)
md$meteo <- as.factor(md$meteo)
md$amis <- as.factor(md$amis)
md$vent <- as.factor(md$vent)
md$jour <- as.factor(md$jour)

str(md)
summary(md)
show (md)


# Train et test
train <- createDataPartition(md$DEC,p=0.75,list=FALSE)
mdtr <- md[train,]
mdts <- md[-train,]


# Modéle sur les données train
ftree <- rpart(DEC ~., data =mdtr,method="class", control=rpart.control(minsplit=0.0001, mindepth = 4,cp= -2))

#Arbre
rpart.plot(ftree, extra = 10)

#Affichage des règles de construction
print(ftree)

#Affichage du résultat de donnees coisees
printcp(ftree)

cpp <- ftree$cptable[which.min(ftree$cptable[,4]),1]

#Elagage de cp 
mtt <- prune(ftree, cp=cpp)

#Affichage cp optimal 
print(cpp)

#Arbre
rpart.plot(mtt, extra = 10)


#Affichage du résultat de donnees coisees
printcp(mtt)


############# PREVISIONS

#Prédiction
predi<-  predict(mtt, mdts, type = "class")

#Matrice de confusion
confusionMatrix(table(mdts$DEC,predi))

#Arbre final
rpart.plot(mtt)

