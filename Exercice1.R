#' ---
#'  " Mini-projet statistiques" 
#' Ahlem JOUIDI
#' ---

# set.seed()

set.seed(1,kind="Marsaglia-Multicarry")

############ Question 1 ##############
dataBase = read.csv("NuclearPowerAccidents2016.csv",header=TRUE)


dataBase$Date<-as.Date(dataBase$Date,format="%m/%d/%Y")
cout=subset(dataBase, Date<"1979-03-28")  ## Cout est le vecteur des coût des accidents avant l’accident de Three Mile Island
cout=cout[,c("Cost..millions.2013US..")]  ## Ici on va extraire les couts qui sont situés sous la colonne nommée "Cost..millions.2013US..'

#cout<-cout[cout != ""]  ##Elimination des elements manquants dans le vecteurs des couts
cout <- cout[!is.na(cout)]
n=length(cout)   ##Nombre des observations x1, ..., xn

########## Question 2 ###############
#### b)

ordres = (1/(2*n)) * seq(from = 1, to = 2*n-1, by = 2)  ## Droite Niveau
cout_croi=sort(cout, decreasing = FALSE)  ##Vecteur des obsérvations classées par ordre décroissant
quant_normal = qnorm(ordres,mean=0,sd=1); ## Quantile d'une normale N(0,1) 
plot(quant_normal,cout_croi,main='QQ-plot normal',xlab="Les quantiles théoriques",ylab="Les quantiles des observations",
     col.main="red",col.lab="green")
qqline(cout_croi, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
#### c)
qqnorm(cout_croi, plot.it = TRUE, datax = FALSE,col.main="red",col.lab="green")  ## Produire un normal QQ plot
qqline(cout_croi, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7) ##qqline ajoute une ligne à un tracé «théorique», 
##par défaut normal, normal, qui parcourt les quantiles probs, 
##par défaut les premier et troisième quartiles.

########## Question 3 ###############
#### b)
quant_exp = qexp(ordres,rate=1) ## La fonction quantile d'une loi exponentielle de parametre lampda=1
plot(quant_exp,cout_croi,main='QQ-plot exp',xlab="Les quantiles théoriques",ylab="Les quantiles des observations",
     col.main="red",col.lab="green")
qqline(cout_croi, datax = FALSE, distribution = qexp, probs = c(0.25, 0.75), qtype = 7)

#### c)

cout_decr=sort(cout, decreasing = TRUE) ##Vecteur des obsérvations classées par ordre croissant
abs=matrix(0, 1, n)
plot(cout_decr,abs,main='Echantillons rangées par ordre décroissant',
     col.main="red") ## Representation des valeurs de l’échantillon rangées par ordre décroissant
##pour une valeur 0 en ordonnée.

#### d)
density1=dexp(cout,rate=n/sum(cout))
hist(cout, freq =F)  ##L'histogramme des données observées
par(new = TRUE)
plot(density)

