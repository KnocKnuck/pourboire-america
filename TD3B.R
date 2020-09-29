setwd("D:/One Drive/OneDrive/Univ Joseph/Universit� Lille 3 - MIASHS/Master 1 SID/Semestre 2/Visualisation de l'information - FRAISSE/LAVIGNE/TD 3-20171012")
table=read.table('Tips.csv', sep=';',dec='.', header = TRUE)

head(table)
tail(table)
str(table)


table$SEX = as.factor(table$SEX)
table$SMOKER = as.factor(table$SMOKER)
table$DAY = as.factor(table$DAY)
table$TIME = as.factor(table$TIME)

summary(table$TOTBILL)
summary(table$SEX)

"Partie 2" 

x = table$TOTBILL
y = table$TIP
plot(x,y)

cor(table$TOTBILL, table$TIP)
"Quand le prix augmente le pourboir augmente, plus c'est proche de 1 plus c'est correlé" 
cor.test(table$TOTBILL, table$TIP)
"Il y a une relation de correlation croissante entre le pourboir et le prix"

table$PTIP = (table$TIP)/(table$TOTBILL)
plot(table$TOTBILL, table$PTIP)

"PARTIE 3"

"Y1,...Yny : MOntant en % des pourboires des clients en journée
X1,...,Xny : MOntant en % des pourvoires des clients en journée
2 échantillons indépendants càd non appariés
Xi indépendant E(Xi) = mux  
Yi indépendant E(Yi = muy 
On teste H0 = {mux = muy} et H1 {mux \dif mux}
On va utiliser le test de Student de comparaison de moyenne
Soit Yi et Xi ~N(......)
Soit nx et ny > 30 ~ TCL "

jour = table$PTIP[table$TIME==0]
which(table$TIME==0)
soir = table$PTIP[which(table$TIME==1)]

hist(jour)
hist(soir)

which(table$PTIP > 0.6)

var.test(jour,soir)
"Demander pour l'explication de df, denom df et F et comment l'interpreté
Probabilité critique pc = 0.098, si c'était pc < alpha = 0.05
Donc on rejette H0, les 2 variances ne sont pas égales"

t.test(jour,soir, alternative = "two.sided", paired = FALSE, var.equal = FALSE)
?t.test

" pc = 0.22 > 0.05/alpha on conserve donc H0"

H = table$PTIP[which(table$SEX==0)]
Fem = table$PTIP[which(table$SEX==1)]
hist(H)
hist(Fem)
var.test(H,Fem)

Smoke = table$PTIP[which(table$SMOKER==1)]
NoSmoke = table$PTIP[which(table$SMOKER==0)]
hist(Smoke)
hist(NoSmoke)
var.test(Smoke,NoSmoke)

aov(TIP~DAY, data=table) 
" a revoir "
?aov

summary(table$DAY)

str(table)
class(table$DAY)

aovDAY = aov(PTIP~DAY, data = table)
summary(aovDAY)

#Df Sum Sq  Mean Sq F value Pr(>F)
#DAY           3 0.0095 0.003169   0.848  0.469
#Residuals   240 0.8968 0.003737 

plot(aovDAY)

