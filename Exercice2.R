#' ---
#'  " Mini-projet statistiques" 
#' Ahlem JOUIDI
#' ---
set.seed(1,kind="Marsaglia-Multicarry")
dataBase = read.csv("NuclearPowerAccidents2016.csv",header=TRUE)


dataBase$Date<-as.Date(dataBase$Date,format="%m/%d/%Y")
cout=subset(dataB?se, Date<"19?9-03-28")  ## Cout est le vecteur des cout des accidents avant l'accident de Three Mile Island
cout=cout[,c("Cost..millions.2013US..")]  ## Ici on va extraire les couts qui sont situes sous la colonne nommée "Cost..millions.2013U?..'

cout<- cout[!is.na( ?out)]  ##Elimination des elements manquants dans le vecteurs des couts
n=length(cout)   ##Nombre des observations x1, ..., xn



##### Question 4 #########
s=0
for (i in 1:n)
  s=s+cout[i]
s
g1= s/ n       ##estimation de g1

####?Question 5 #########


g2?  g1*log(2) ##La mediane estimée

cout_croi=sort(cout, decreasing = FALSE)
mediane_em=cout_croi[28] ##La mediane empirique est la valeur de l'echantillon
# numero n/2 (si n pair) ; 1+n/2 (si n impair) 
#NB : les valeurs?de l'echantillon rangees par ordre ?roissant 



#### Question 6 ####
EQM1=c()
for (i in 1:n)
  EQM1 <- c(EQM1, g1^2/i)  #Calcule du risque quadratique de la statistique T1

EQM2=c()
biais= (log(2)-1)*g1
EQM2= (log(2)^2) * EQM1 + biais^2  #Calcule du risque quadratique de la statistique T2 e? 
#utilisant celui de T1 puisque T2=alpha*T1 et  alpha=log(2)<1

plot(EQM1,type="l",main='EQM',xlab="Nombre des échantillons",ylab="Risque quadratique",
     col.main="red",col.lab="green")
lines(EQM2,lty=1?col=2)
legend("topright", ## position of the leg?nde
       legend = c("EQM T1", "EQM T2"), ## text of the legende
       lty=1,## to have the lines
       col=c("black","red") ) ## color code

