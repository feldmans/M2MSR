#install.packages("rmarkdown")
#install.packages("corrplot")
#install.packages("psy")
library(corrplot)
library(psy)


rt <- read.csv2("data/presentationTPretinol.csv")

# Q1/Dans un premier temps décrivez vos variables. 
dim(rt)
summary(rt)
#J'ai 14 variables.
#Les variables sont reconnues comme quantitatives 
#mais en regardant plus attentivement les min et max on peut voir que sexe a un minimum de 1 et un maximum de 2 c'est donc une variables qualitative binaire.
#Les autres variables sont quantitatives:
#soit continues : age, bmi, calories, graisses, fibres, alcool, cholestérol, betadiet,retdiet,betaplasma,retplasma
#soit discètes : tabac, vitamine

#Observer ses variables sert également à dépister les "bizarreries".
#Par exemple, j'ai un doute concernant le max d'alcool, je vais donc regarder plus attentivement la variable, par exemple avec une table
table(rt$alcool) #en effet, je passe de 35 à 203 verres...203/7=29 verres par jour, il y a probablement un erreur de codage

#summary ne montre pas de données manquantes mais je préfère m'en assurer: 
for (i in names(rt)){
  res <-  c(i,sum(is.na(rt[ , i])))
  print(res)
}

sapply(names(rt),function(x) print(c(x,sum(is.na(rt[,x]))))) #comment enlever le tableau à la fin?


#Il est important d'avoir une idée de la distribution de ses variables, pour ce faire, je peux tracer un histogramme pour chaque variable quantitative:
# win.graph() : ne marche pas : ouvre une fenêtre et les graphiques 5à 9 écrasent les 1 à 4
# windows() :idem
pdf("mygraph.pdf")
par(mfrow=c(2,2))
non_cont<- c("sexe","tabac","vitamine")
names_rt<-names(rt)[! names(rt) %in% non_cont]
for (i in names_rt){
hist(rt[,i],breaks = 20, main=i) #je rajoute des breaks pour avoir un histogramme plus précis
} 
dev.off()

prop.table(table(rt$alcool)) #la plus petite classe d'alcool comprend 35% des sujets :distribution non normale

#bmi, graisses, cholestérol, betadiet, retdiet et betaplasme on des distribution à peu près normale mais asymétriques.
#calories, fibres, ret plasma ont de distributions d'allure normales
#age a une distribution irrégulière avec 2 cloches à 40 et 75 ans.
#alcool a une distribution très asymétrique



# Q2/Puis, étudiez les relations existant entre toutes les paires possibles de variables. 
#NB : ce ne snot que les 9 varianles qui sont concernées
var <- c("retplasma","age","sexe","bmi","tabac","vitamine","cholesterol","alcool","retdiet") 
var %in% names(rt)#pour voir quelle variable j'ai mal recopié
round(cor(rt[,var]),3)

corrplot(cor(rt[,var]),method="circle")
mdspca(rt[,var]) 

# Q3/Effectuez ensuite une régression linéaire où la variable à expliquer sera la concentration en rétinol plasmatique, 
# les autres variables étant explicatives. 
# Recherchez des interactions entre les variables explicatives. 
# Q4/Transformez la variable "rétinol plasmatique" en une variable binaire (en la coupant en deux au niveau de la médiane). 
# Refaites les calculs précédents en ayant recours cette fois à une régression logistique.