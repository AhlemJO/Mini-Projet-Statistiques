#' ---
#'  " Mini-projet statistiques" 
#' Ahlem JOUIDI
#' ---

set.seed(1,kind="Marsaglia-Multicarry")
dataBase = read.csv("NuclearPowerAccidents2016.csv",header=TRUE)


dataBase$Date<-as.Date(dataBase$Date,format="%m/%d/%Y")
cout=subset(dat?Base, Date<"1?79-03-28")  ## Cout est le vecteur des cout des accidents avant l'accident de Three Mile Island
cout=cout[,c("Cost..millions.2013US..")]  ## Ici on va extraire les couts qui sont situÃ©s sous la colonne nommee "Cost..millions.201?US..'

cout<- cout[!is.na(?cout)]  ##Elimination des elements manquants dans le vecteurs des couts
n=length(cout)   ##Nombre des observations x1, ..., xn

#### Question 2 ####
lampda0=0.001
t=qgamma(0.05, n, rate = lampda0, lower.tail = TRUE,  ### La valeur su seuil
         log. = ?ALSE)
T=0
T=sum(cout)  ###La statistique T(X)

###On voit que T(X)<t donc on peut rejeter H0 pour lampda0=0.01

### pvalue = inf {alpha in [0,1] tel que T(x)> qGamme (alpha)} => pvalue = 1 - F(T(x))
### avec qGamme est le quantile de la loi gamma(n,lampda0?
### F est la fonction de répartition de la loi gamma(n,lampda0)
pvalue = 1 - pga?ma(sum(cost),n,lampda0)

#### Question 3 ####


rga= rgamma(n, shape=n, lampda0);
den= dgamma(rga, n, lampda0)
plot( rga, den,col=ifelse(rga > t, 'green', 'red'),main='Densit?? de la statistique') 
##La region bleu correspond a la region ou on accepte?l'hypothese H0
##La region rouge correspond a la region ou on accepte l'hypothese H1
abline(v=t, col='red')
                                                  
 
# Question 4
alpha=?.05
normal=rnorm(n, n/lampda0,sqrt(n)/lampda0) ##La densite de la loi?normale
plot(normal,dnorm(normal,n/lampda0,sqrt(n)/lampda0),xlab='x',ylab='densite',main='La densite de la loi normale')
abline(v=qnorm(alpha,n/lampda0,sqrt(n)/lampda0),col='red') ## On ?race la droite de separation
                                  ?                       ##entre les deux regions d'acceptation

##La region gauche correspond a la region ou on accepte l'hypothese H0
##La resion droite correspond a la region ou on accepte l'?ypothese H1

# Question 5
n_ve?tor = c(10,50,100,500,100000)
puissance = c()
for (i in n_vector)
  puissance = c(puissance, 1-pgamma(qgamma(alpha, shape = i ,lampda0, lower.tail=TRUE, log.p = FALSE),shape=i,lampda0))
plot (n_vector, puissance, type='l')

