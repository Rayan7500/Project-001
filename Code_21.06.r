#Jour 1
library(readxl)
base <- read_excel("base.xlsx")
View(base)
base <- na.omit(base)
summary(base)

#Calcul d'agr?gats par groupe (permet de filtrer les donn?es) pour ici analyser les donn?es d'emissions selon leur Type poste.

classe_co2 <- aggregate(base$CO2f, by = list(typ2 = base$`Type poste`), sum)
classe_ch4f <- aggregate(base$CH4f, by = list(typ2 = base$`Type poste`), sum)
classe_ch4b <- aggregate(base$CH4b, by = list(typ2 = base$`Type poste`), sum)
classe_n2o <- aggregate(base$N2O, by = list(typ2 = base$`Type poste`), sum)

#Boite ? moustache

Moustache_co2 <- boxplot(base$CO2f)
Moustache_CH4f <- boxplot(base$CH4f)
Moustache_CH4b <- boxplot(base$CH4b)
Moustache_n2o <- boxplot(base$N2O)

library(vioplot)

#Permet de visualiser les diff?rents FE entre eux

vioplot(co2, CH4f, CH4b, N2O, names=c("co2", "ch4f", "ch4b", "n2o"),
        col="gold")
title("Facteurs d'emissions")

library(doBy)

classe_co2 <- classe_co2[-which(classe_co2$x>=0),] #Impossible de faire une pie ici car il y avait des valeurs n?gatives, on enl?ve donc les valeurs inf?rieurs ? 0.
classe_transport <- classe_co2_categorie[which(classe_co2_categorie$typ2=="Transport"),]
orderBy(~ x, classe_co2 ) #retrie le tableau

#PIE

slices <- classe_co2_categorie$x #Pour chaque classe
lbls <- classe_co2_categorie$typ2
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Facteur d'emissions par categorie")

#Jour 2, aggr?gation sur les categories

classe_co2_categorie <- aggregate(base$CO2f, by = list(typ2 = base$`Code de la categorie`), sum)	
classe_co2_categorie<-na.omit(classe_co2_categorie)

orderBy(~ x, classe_co2_categorie)
orderBy(~typ2, classe_co2_categorie)

classe_co2_transport <- classe_co2_categorie[,1]
classe_co2_transport <- classe_co2_categorie[43:63,] #Trie par orthographe pour ensuite selectionner tout les "Transport > ...", 43:63 repr?sente les n lignes qui contenant "Transport"

sumCO2 <- sum(classe_co2_categorie$x)	
sumCO2_transport <- sum(classe_co2_transport$x)




#test pour filtrer les lignes contenant le mot "Transport" sans succ?s, j'ai donc du trier par orthographe le tableau et selectionner les n valeur corespondant le mot "Transport", "Combustibles",... Pour ensuite sommer tout ces elements.	
classe_transport <- classe_co2_categorie[which(classe_co2_categorie$typ2=="Transport"),]
classe_transport <- aggregate(classe_categorie$x, by list(typ2 = classe_categorie$typ2[which classe_co2_categorie$typ2=="Transport"]),sum)
classe_co2_transport <- aggregate(base$CO2f, by = list(typ2 = base$`Code de la categorie`), sum)[which(str_detect(base$`Code de la categorie`, "Transport")]

#PLOT 1

x11()

categorie=c("UTCF","Combustibles","Achats","Transports","Electricit?")
total=c(sumCO2_UTCF, sumCO2_combustibles, sumCO2_Achats, sumCO2_transport, sumCO2_Electricite)
barplot(total,horiz=T,names=categorie,las=1,col="#2D69B3")
barplot(total,horiz=T,names=categorie,las=1,col="#2D69B3")
mtext(side=1, "Quantit? d'emissions de CO2e",line=2.5)
somme = sum(total)
total_s = c(sumCO2_UTCF, sumCO2_combustibles, sumCO2_Achats, sumCO2_transport, sumCO2_Electricite)/somme*100 ; total_s
par(new=TRUE)
barplot(total_s,axes=F,horiz=T,plot=T,col="#FE642E")
axis(3,pretty(total_s,10),col="#F5821F")
mtext(side=3, "%",line=2.5)


#PLOT 2	

barre1 = c(0,0,0,0,0,0,0,1350000,770000) # UTCF, On a mis 0 pour diff?rencier les categories
barre2 = c(0,0,0,220000,164000,174000,520000,0,0) # Combustibles
barre3 = c(0,0,31000,0,0,0,0,0,0) # Achats
barre4 = c(0,256,0,0,0,0,0,0,0) # Transport
barre5 = c(15,0,0,0,0,0,0,0,0) # Elecrticite


couleurs_1 = c("#B45F04","#E0CDA9")
couleurs_2 = c("#2EFE64","#BEF781","#088A29","#ADFF2F")
couleurs_3 = c("#08a995")
couleurs_4 = c("#0d00bc") 
couleurs_5 = c("#FFFF00") 

data = cbind(barre5, barre4, barre3, barre2, barre1)
couleurs_compil = c(couleurs_5,couleurs_4,couleurs_3,couleurs_2,couleurs_1)

x11()

par(mar = c(10,10,10,10)) # change les marges pour permettre l'affichages des titres des barres

barplot(height = data,
        space=2,horiz=T,col=couleurs_compil,xlim=c(0,2000000),ylim=c(0,12),
        names.arg = c("Electricit?","Transports","Achats","Combustibles","UTCF"),
        legend.text = c("Electricit?","Transports","Achats","Charbon","Usage Sp?ciaux","Avions","Autres","Perturbation des sols","D?forestation"), #Ici, ce sont les sous-categories
        args.legend = list(x = "bottomright"),las=1)
mtext(side=1, "Quantit? d'emissions de CO2e en KgCO2e",line=2.5)


#Jour 3, FEM

library(readxl)
FEM <- read_excel("FEM.xlsx")
View(FEM)
FEM<-na.omit(FEM)
FEM <- FEM[order(FEM$Valeur),]


x11()	
par(las=2) # make label text perpendicular to axis
par(mar=c(22,5,2,5)) # increase y-axis
barplot(FEM$Valeur, main="Repr?sentation des FEM",
        names.arg=FEM$`categorie d'achat`, col=c("blue","red"), ylab="Valeur en kgCO2e/K??? HT")	


x11()	
par(las=2) # make label text perpendicular to axis
par(mar=c(22,5,2,5)) # increase y-axis margin.abline(h=mean(x))
barplot(FEM$X__1, main="Graphique des d?penses des m?nages",
        names.arg=FEM$`categorie d'achat`, col=c("yellow","darkgreen"), ylab="En milliard ???")	

#Grouped Bar Plot
x11()
par(las=2)
par(mar=c(12,5,2,5)) # 
a<-FEM$Valeur# en kgCO2e/keuros
b<-FEM$X__1 #en milliard
c<-(FEM$X__1*FEM$Valeur)*1000 #pour afficher en T
barplot_profilH<-barplot(a,ylim=c(0,3000), col="orange",names.arg=FEM$`categorie d'achat`)

lines(barplot_profilH,b,col="green",type="o",lwd=2)
legend("topleft", legend=c("Qt? totale de CO2 ?mis par les m?nages en 2017 (TCO2e)", "Courbe de l'allure des d?penses des m?nages selon l'activit?","FEM"),
       col=c("red", "green","orange"), lty=1:2, cex=0.8, bg="#CCFFCC")
box()

xlim1 <- par()$usr[1:2]
par(new=TRUE,xaxs="i")
par(mar=c(18,5,5,5))

plot(barplot_profilH,c,col="red",type="o",lwd=2,axes=FALSE,xlab="",ylab="",xlim=xlim1)
axis(4,col="red")

#Gigatone
x11()
par(las=2)
par(mar=c(12,5,2,5)) # 
a<-FEM$Valeur# en kgCO2e/keuros
b<-FEM$X__1 #en milliard
c<-(FEM$X__1*FEM$Valeur)*10^(-6) #pour afficher en GT
barplot_profilH<-barplot(a,ylim=c(0,3000), col="orange",names.arg=FEM$`categorie d'achat`)

lines(barplot_profilH,b,col="green",type="o",lwd=2)
legend("topleft", legend=c("Qt? totale de CO2 ?mis par les m?nages en 2017 (GTCO2e)", "Courbe de l'allure des d?penses des m?nages selon l'activit?","FEM"),
       col=c("red", "green","orange"), lty=1:2, cex=0.8, bg="#CCFFCC")
box()

xlim1 <- par()$usr[1:2]
par(new=TRUE,xaxs="i")
par(mar=c(18,5,5,5))

plot(barplot_profilH,c,col="red",type="o",lwd=2,axes=FALSE,xlab="",ylab="",xlim=xlim1)
axis(4,col="red")



#Jour 3

############### BOITE A MOUSTACHES A POSITIONS VARIABLES
x11()
par(las=2)
par(mar=c(12,5,2,5)) #
g = boxplot(Carbone$`Transfor-mation ?nergie`,Carbone$`Industrie manufac-turi?re`,Carbone$`R?si-dentiel / tertiaire`,
            Carbone$`Agricul-ture/syl-viculture hors UTCATF (**)`,
            Carbone$`Transport routier`,Carbone$`Autres trans-ports (*)`, col=topo.colors(6),
            main="Comparaison des emissions de CO2 des secteurs entre 1990-2016",horizontal=F,ylab="En MtCO2 (TgCO2)", names = c("Transformation ?nergie","Industrie manufacturi?re","R?sidentiel/Tertaire","Agriculture","Transport routier","Autres transports"))

#legend("bottom", c("Transformation ?nergie","Industrie manufacturi?re","R?sidentiel/Tertaire","Agriculture","Transport routier","Autres transports"),
#       col=topo.colors(6),lty=1, cex=0.8, bg="#CCFFCC")

legend("bottom", title="Secteurs",
       c("Transformation ?nergie","Industrie manufacturi?re","R?sidentiel/Tertaire","Agriculture","Transport routier","Autres transports"),
       fill=topo.colors(6), cex=0.8, bg="#CCFFCC")




points(1,76.4,col=4,pch=13)
points(2,163,col=4,pch=13)
points(3,88.9,col=4,pch=13)
points(4,92.7,col=4,pch=13)
points(5,112,col=4,pch=13)
points(6,6.96,col=4,pch=13)
points(4,170,col=4,pch=13)
text(4.1,170,"Repr?sente l'annee 1990",pos = 4)

points(1,45.5,col="red",pch=13)
points(2,96,col="red",pch=13)
points(3,87.8,col="red",pch=13)
points(4,88.8,col="red",pch=13)
points(5,127,col="red",pch=13)
points(6,6.30,col="red",pch=13)
points(4,160,col="red",pch=13)
text(4.1,160,"Repr?sente l'annee 2016",pos = 4)


library(tidyr)
library(dplyr)
df <- va_secteurs %>%
  select(date, agri, industrie) %>%
  gather(key = "variable", value = "value", -date)
head(df, 3)

ggplot(df, aes(x = date, y = value, group = 2)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

#Valeur ajoutee
ggplot(va_secteurs, aes(date, agri, group = 1)) +
  +     geom_point() +
  +     geom_line() +
  +     labs(x = "Year", y = "Valeur ajoutee (milliard)", 
             +          title = "Valeur ajoutee du secteur de l'agriculture dans le temps")

df <- melt(va_secteurs[, c("date", "agri", "industrie","energie","construction","tertiaire_m","transports","tertaire")], id="date")
df$value <- as.numeric(df$value)

x11()
qplot(date, value, data = df) +
  facet_wrap(~ variable)

x11()
x11()
ggplot(df, aes(x = date, y = value)) + 
  geom_area(aes(color = variable, fill = variable), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c(topo.colors(7))) +
  scale_fill_manual(values = c(topo.colors(7)))


#DF = as.matrix(as.data.frame(lapply(va_secteurs, as.numeric)))

#Import carbone.xlsx et va_secteurs

Carbone <- read_excel("D:/DATA/r.martin/Desktop/Stage_Elioth/data/Carbone.xlsx")
va_secteurs <- read_excel("D:/DATA/r.martin/Desktop/Stage_Elioth/data/va_secteurs.xlsx")

Carbone <- Carbone[10:26,]
va_secteurs <- va_secteurs[1:17,]

#va_secteurs[,2:9] <- as.numeric(unlist(va_secteurs[,2:9]))
va_secteurs[,2] <- as.numeric(unlist(va_secteurs[,2]))
va_secteurs[,3] <- as.numeric(unlist(va_secteurs[,3]))
va_secteurs[,4] <- as.numeric(unlist(va_secteurs[,4]))
va_secteurs[,5] <- as.numeric(unlist(va_secteurs[,5]))
va_secteurs[,6] <- as.numeric(unlist(va_secteurs[,6]))
va_secteurs[,7] <- as.numeric(unlist(va_secteurs[,7]))
va_secteurs[,8] <- as.numeric(unlist(va_secteurs[,8]))
va_secteurs[,9] <- as.numeric(unlist(va_secteurs[,9]))

industrie_construction <- (va_secteurs$industrie)+(va_secteurs$construction)
tertiaire <- (va_secteurs$tertaire)+(va_secteurs$tertiaire_m)

va_secteurs <- cbind(va_secteurs[,1:2],industrie_construction,va_secteurs[,3:9])
va_secteurs <- cbind(va_secteurs[,1:2],tertiaire,va_secteurs[,3:9])


va_secteurs<-va_secteurs[,c(1,2,4,6,9,3)]


#Calcul d'indicateur
idcc <- c(((Carbone[,2]/va_secteurs[,2])),(Carbone[,3]/va_secteurs[,3]),(Carbone[,4]/va_secteurs[,4]),(Carbone[,5]/va_secteurs[,5]),(Carbone[,6]/va_secteurs[,6]))
idcc<-data.frame(idcc)
idcc <- cbind(idcc[,1:1],Carbone$annee,idcc[,1:5])
#idcc <- idcc[,c(3,1,2,4,5,6)]
idcc <- idcc[,c(2,3,4,5,6,7)]
names(idcc) <- c("Intensit? carbone","Agriculture-Sylviculture (hors UTCF)","Industrie manufacturi?re","Industrie de l'?nergie","Transport routier","R?sidentiel-Tertiaire")


#Plot
dff <- melt(idcc[, c(1,2,3,4,5,6)], id=c(1))
dff <- na.omit(dff)

x11()
qplot(dff$`Intensit? carbone`, value, data = dff, colour = variable) + theme(axis.text.x=element_text(angle=60, hjust=1))

x11()
qplot(dff$`Intensit? carbone`, value, data = dff) + facet_wrap(~ variable) + theme(axis.text.x=element_text(angle=60, hjust=1))


x11()
qplot(dff$`Intensit? carbone`, value, data = dff, colour = variable)


#Namea, calcul C02e
library(stringr)
library(ggplot2)
library(xlsx)
library(here)
library(data.table)
library(readxl)

nameach4 <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/CITEPA/Namea_carbone.xls"), range = "C13:K84", sheet = "CH4"))
nameaco2 <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/CITEPA/Namea_carbone.xls"), range = "C13:K84", sheet = "CO2_hors_biomasse"))
namean2o <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/CITEPA/Namea_carbone.xls"), range = "C13:K84", sheet = "N2O"))


names(nameaco2) <- c("naf","sous_secteur","2008","2009","2010","2011","2012","2013","2014")
names(nameach4) <- c("naf","sous_secteur","2008","2009","2010","2011","2012","2013","2014")
names(namean2o) <- c("naf","sous_secteur","2008","2009","2010","2011","2012","2013","2014")

nameaco2e <- c(nameach4$secteur,nameach4$naf,nameach4$sous_secteur,(nameaco2$`2008`)+25*(nameach4$`2008`)+298*(namean2o$`2008`))

#PRG (pouvoir de r?chauffement global nous permet duniformiser les diff?rents GES : co2=1, ch4=25, n2o=298)
nameaco2e = data.table(c(nameach4$naf),c(nameach4$sous_secteur),c((nameaco2$`2008`)+25*(nameach4$`2008`)/1000+298*(namean2o$`2008`)/1000)
                       ,c((nameaco2$`2009`)+25*(nameach4$`2009`)/1000+298*(namean2o$`2009`)/1000),c((nameaco2$`2010`)+25*(nameach4$`2010`)/1000+298*(namean2o$`2010`)/1000),c((nameaco2$`2011`)+25*(nameach4$`2011`)/1000+298*(namean2o$`2011`)/1000),
                       c((nameaco2$`2012`)+25*(nameach4$`2012`)/1000+298*(namean2o$`2012`)/1000),c((nameaco2$`2013`)+25*(nameach4$`2013`)/1000+298*(namean2o$`2013`)/1000),
                       c((nameaco2$`2014`)+25*(nameach4$`2014`)/1000+298*(namean2o$`2014`)/1000))

nameaco2e=data.table(c(nameach4$naf),c(nameach4$sous_secteur),c((nameaco2$`2014`)+25*(nameach4$`2014`)/1000+298*(namean2o$`2014`)/1000))


names(nameaco2e) <- c("naf","sous_secteur","2008","2009","2010","2011","2012","2013","2014")

write.xlsx(nameaco2e, file="Resultat_nameaCO2e.xlsx", 
           sheetName="co2e")

#croiser les deux code naf
#ans <- CA_2014[CA_2014$`Tranche de taille d'effectif` == "Ensemble des tranches d'effectif",]
#ans$BRANCHE <- substring(ans$BRANCHE, 1,2) #code naf diff?rent, n?cessite de le rendre identique
#a80 <- ans[293:375,]
#tapply(ans$`Chiffre d'affaires de la branche`, ans$BRANCHE, sum)


#import a80.xlsx & co2.xlsx
library(xlsx)
library(data.table)
library(ggplot2)
library(ggpubr)
library(ggalt)
library(ggdendro)

a80 <- read_excel("D:/DATA/r.martin/Desktop/Stage_Elioth/data/a80.xlsx")
co2 <- read_excel("D:/DATA/r.martin/Desktop/Stage_Elioth/data/co2.xlsx")

names(co2) <- c("naf","co2e")
names(a80) <- c("naf","secteur","ca")
n <- merge(co2, a80, by = "naf")
n<-n[,c(1,3,2,4)]


fem <- data.table(c(n$naf),c(n$secteur),c(n$ca),c(n$co2e),c((n$co2e)/(n$ca)))

names(fem) <- c("NAF", "Secteur","CA","CO2e","FEM")
fem <- fem[order(-fem$FEM),]
Resultat_Fem<-fem
max <- fem[1:3,]
fem <- fem[4:52,]


# a<-data.frame("42","Construction",281.318,278.549,(278.549/281.318))
#names(a) <- c("NAF", "Secteur","CA","CO2e","FEM")
#new <- rbind(fem,a)
#head(new)

#Plot


p1 <- ggplot(Resultat_Fem) + aes(x = reorder(Secteur,-CA), y = CA) + geom_lollipop() + xlab("Secteurs ?conomiques") + ylab("Chiffres d'affaires (en milliard)") + coord_flip() + geom_point( size=1, color="green", fill=alpha("orange", 0.5), alpha=0.7, shape=21, stroke=2)


p2 <- ggplot(Resultat_Fem) + aes(x = reorder(Secteur,-CO2e), y = CO2e) + geom_lollipop() + xlab("Secteurs ?conomiques") + ylab("Emissions de CO2e (Gg)") + coord_flip() +  geom_point( size=1, color="blue", fill=alpha("orange", 0.5), alpha=0.7, shape=21, stroke=2)

p4 <- ggplot(max) + aes(reorder(Secteur,FEM),FEM) + geom_bar(stat = "identity", fill = "white", colour = "red") + xlab("Secteurs ?conomiques") + ylab("Facteurs d'emissions mon?taires (KgCO2e/K???)") + coord_flip() + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_minimal()

p3 <- ggplot(fem) + aes(reorder(Secteur,-FEM),FEM) + geom_bar(stat = "identity", fill = "white", colour = "red") + xlab("Secteurs ?conomiques") + ylab("Facteurs d'emissions mon?taires (KgCO2e/K???)") + coord_flip() + theme_minimal()
x11()
ggarrange(p1,p2,p3,p4,labels = c("A", "B", "C","D"))
ggarrange(p1,p2,p3,p4)

#x11()
#ggplot(Resultat_Fem) + aes(Secteur,FEM) + geom_bar(stat = "identity") + xlab("Secteurs ?conomiques") + ylab("Emissions de CO2e (Gg)") + coord_flip()



x11()
data("fem")  
femtest <- fem
femtest$Secteur <- rownames(femtest)  
femtest$FEM <- round((femtest$FEM - mean(femtest$FEM))/sd(femtest$FEM), 2)  #normalise
femtest$FEM_type <- ifelse(femtest$FEM < mean(femtest$FEM), "below", "above")  # above / below avg 
femtest <- femtest[order(femtest$FEM), ]  # sort
femtest$Secteur <- factor(femtest$Secteur, levels = femtest$Secteur)  

# Diverging Barcharts
ggplot(femtest, aes(reorder(fem$Secteur,-FEM),FEM)) + 
  geom_bar(stat='identity', aes(fill=FEM_type), width=.5)  +
  scale_fill_manual(name="Ratio mon?taires", #ne sont pas inclus : les 3 cas extremes
                    labels = c("En dessous de la moyenne(fem faible)","Au dessus de la moyenne(fem ?lev?)" ), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalisation des FEM", 
       title= "") + 
  coord_flip()


# plot dendo

x11()
theme_set(theme_bw())
hc <- hclust(dist(fem), "ave")  # hierarchical clustering
ggdendrogram(hc, rotate = TRUE, size = 2)


#import a88_ca_va_emplois.xlsx & co2.xlsx
library(data.table)
library(xlsx)
library(readxl)
library(ggplot2)
library(ggpubr)

a88 <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/INSEE/a88_ca_va_emplois.xlsx") ,col_types = c("text", "text","numeric", "numeric", "numeric","numeric")))
co2 <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/CITEPA/co2.xlsx"),col_types = c("text","numeric") ))


a88 <- a88_ca_va_emplois

names(co2) <- c("naf","co2e")
names(a88) <- c("naf","secteur","nb_entreprises","effectifs_salaries","ca","va")
n <- merge(co2, a88, by = "naf")
n<-n[,c(1,3,4,5,2,6,7)]

n$ca<-(n$ca)/1000
n$va<-(n$va)/1000


fem <- data.table(c(n$naf),c(n$secteur),c(n$ca),c(n$va),c(n$effectifs_salaries),c((n$ca)/(n$effectifs_salaries)),c((n$va)/(n$effectifs_salaries)),c(n$co2e),c((n$co2e)/(n$ca)),c((n$co2e)/(n$va)),c((n$co2e)/(n$ca)/(n$effectifs_salaries)),c((n$co2e)/(n$va)/(n$effectifs_salaries)))

#c((n$co2e)/(n$ca)*(n$ca)/(n$effectifs_salaries)*n$effectifs_salaries) KAYA

names(fem) <- c("NAF", "Secteur","CA","VA","Effectifs salaries","Productivite(CA)","Productivite(VA)","CO2e","FEM(CA)","FEM(VA)","FEM(productivite CA)","FEM(productivite VA)")



femvisu <- fem[,c(2,6,7,9,10,11,12)]

fem <- na.omit(fem)


#Plot Prod CA & VA
x11()
fem1 <- fem[order(-fem$`Productivite(CA)`),]
data <- data.frame(secteur = c(fem1$Secteur), Productivite_CA= fem1$`Productivite(CA)`, Productivite_VA= fem1$`Productivite(VA)`)

vals <- as.matrix(data[,2:3])
rownames(vals) <- data[,1]

par(mar=c(3, 19, 1, 1))
p1 <- barplot(t(vals),horiz=TRUE, cex.names=0.8,cex.main=0.8, cex.axis=0.8, beside = T, legend = T, las = 2, col=c("#FFFFAA","red"), main = "Comparaison de la productivite Chiffre d'affaires et Valeur ajoutee")

#Plot FEM CA & FEM VA
x11()
fem1 <- fem[order(-fem$`FEM(CA)`),]
data <- data.frame(secteur = c(fem1$Secteur), FEM_CA= fem1$`FEM(CA)`, FEM_VA= fem1$`FEM(VA)`)

vals1 <- as.matrix(data[,2:3])
rownames(vals1) <- data[,1]

par(mar=c(3, 19, 1, 1))
p2 <- barplot(t(vals1),horiz = TRUE, cex.names=0.8,cex.main=0.8, cex.axis=0.8, beside = T, legend = T, las = 2, col=c("#FFFFAA","blue"), main = "Comparaison du FEM Chiffre d'affaires et du FEM Valeur ajoutee")

#Plot FEM prod & FEM prod
x11()
fem1 <- fem[order(-fem$`FEM(productivite CA)`),]
data <- data.frame(secteur = c(fem1$Secteur), FEM_productivite_ca= fem1$`FEM(productivite CA)`, FEM_productivite_va= fem1$`FEM(productivite VA)`)

vals2 <- as.matrix(data[,2:3])
rownames(vals2) <- data[,1]

par(mar=c(3, 19, 1, 1))
p3 <- barplot(t(vals2),horiz=TRUE, cex.names=0.8,cex.main=0.8, cex.axis=0.8, beside = T, legend = T, las = 2, col=c("#FFFFAA","brown"), main = "Comparaison du ratio emissions de GES et productivite au seins des secteurs")


par(mfrow=c(1,2))
par(mar=c(3, 17, 1, 1))
x11()
barplot(t(vals2),horiz=TRUE, cex.names=0.8,cex.main=0.8, cex.axis=0.8, beside = T, legend = F, las = 2, col=c("#FFFFAA","brown"), main = "Comparaison du ratio emissions de GES et productivite au seins des secteurs")
barplot(t(vals1),horiz = TRUE, cex.names=0.8,cex.main=0.8, cex.axis=0.8, beside = T, legend = F, las = 2, col=c("#FFFFAA","blue"), main = "Comparaison du FEM Chiffre d'affaires et du FEM Valeur ajoutee")
#barplot(t(vals),horiz=TRUE, cex.names=0.8,cex.main=0.8, cex.axis=0.8, beside = T, legend = T, las = 2, col=c("#FFFFAA","red"), main = "Comparaison de la productivite Chiffre d'affaires et Valeur ajoutee")



#Resultat FEM-TES (suite de tes.R) [Productivite, FEM, Indicateur emissions/carbone]
library(stringr)
library(ggplot2)
library(xlsx)
library(here)
library(data.table)
library(readxl)


a88_ca_va_emplois <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/INSEE/a88_ca_va_emplois.xlsx")))
CI_naf <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/INSEE/va_emissions.xlsx")))


productivite <- merge(a88_ca_va_emplois,CI_naf, by="naf_88")
zz<-data.table(fem@x)
zz$V1 <- zz$V1*1000

str_to_lower(productivite$Secteur)
str_to_title(productivite$Secteur)

productivite <- productivite[,c(9,4,10,17,18)]

productivite$value_added <- productivite$value_added*1000 #VA en millions

productivite[,2] <- as.numeric(unlist((productivite[,2])))
productivite$prod <-(productivite$value_added)/(productivite[,2]) #Richese cr?e par salaries en millions


productivite$fem <- zz$V1
productivite$fem_prod <- (productivite$`2014`)/(productivite$prod)

x11()
p <- ggplot(productivite)
p <- p + geom_bar(aes(x = a38, y = prod, fill=a38), stat ="identity") + ggtitle("Productivite des secteurs economiques") + ylab("(Millions euros par salaries)") + xlab("Secteur (Naf : a38)")
p


x11()
p <- ggplot(productivite)
p <- p + geom_bar(aes(x = a38, y = productivite$fem, fill=a38), stat ="identity") + ggtitle("Facteurs d'emissions monetaires") + ylab("fem kgCO2e/k euros") + xlab("Secteur (Naf : a38)")
p

x11()
p <- ggplot(productivite)
p <- p + geom_bar(aes(x = a38, y = productivite$fem_prod, fill=a38), stat ="identity") + ggtitle("Indicateur Emissions / Productivite") + ylab("fem KgCO2e/euros/salaries") + xlab("Secteur (Naf : a38)")
p

title <- merge(a88_ca_va_emplois,CI_naf, by="naf_88")
title <- title[,c(8,7)]
str_to_lower(title$Secteur)
str_to_title(title$Secteur)

#Evolution FEM de 2008-2014

FEM <- function(){

fem <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/Resultat Evolution (emplois, tes)/evolution_FEM.xlsx")))
fem <- fem[,-("X__1")]
melted_fem <- melt(fem, id="secteur" )
names(melted_fem) <- c("secteur","annee","value")

x11()
qplot(secteur, value, data = melted_fem, colour = annee, group = annee) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution des FEM (tCO2e/K euros)") + geom_line()

x11()
qplot(annee, value, data = melted_fem, colour = secteur, group = secteur) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution des FEM (tCO2e/K euros)") + geom_line()

}

#Evolution Emplois

emplois <- function(){

library(ggplot2)
library(stringr)
library(ggplot2)
library(xlsx)
library(here)
library(data.table)
library(readxl)

emplois <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/INSEE/evolution_emplois.xls"), sheet="Feuil1"))
names(emplois[,1]) = "annee"

emplois$annee <- substring(emplois$annee, 5,6)

dff1 <- melt(emplois, id=c(1))
dff1 <- na.omit(dff1)



x11()
qplot(variable, value, data = dff1) + facet_wrap(~ annee) + theme(axis.text.x=element_text(angle=60, hjust=1))

x11()
qplot(variable, value, data = dff1, colour = annee, group = annee) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution des emplois nationaux (en milliers)") + geom_line()

}
#Evolution production

production <- function(){

setwd(here("..","data/Etude ratio monetaire/INSEE/TES"))
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.xls") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  ldf[[k]] <- read_excel(listcsv[k], sheet = "CPR_CEB", range = "C6:AL6",col_names = FALSE)
}
str(ldf[[1]]) 


name <- read_excel(listcsv[1], sheet = "CPR_CEB", range = "C3:AL3",col_names = FALSE )

data = do.call(rbind,ldf)

colnames(data) <- name
data$annee <- 1989:2014
colnames(data[,37]) <- c("annee")

data <- as.data.frame(data)
dff <- melt(data, id="annee")
dff <- na.omit(dff)
colnames(dff) <- c("annee","variable","value")
dffp <- dff

x11()
qplot(dff[,1], value, data = dff, colour = variable) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution des productions nationaux(en millions d'euros)") + geom_line()

}

#Evolution productivite
productivite_prod <- function(){

library(ggplot2)
library(xlsx)
library(here)
library(data.table)
library(readxl)

emplois <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/INSEE/emplois.xlsx"), range = "A1:AF27"))
#emplois <- as.data.table(lapply(emplois[,2:33], as.numeric))

production <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/INSEE/production.xlsx")))
#production <- as.data.table(lapply(production[,2:33], as.numeric))

#emplois <- emplois[ ,order(names(emplois))]
production <- production*1000
emplois <- emplois
productivite <- production/emplois
a <- c(1989:2014)
productivite$annee <- a

df <- melt(productivite, id="annee")
df1<-df
df1 <- df1[-(131:156),]#CD cokefaction
df1 <- df1[-(625:650),]#LZ activitees immo

x11()
qplot(annee, value, data = df1, colour = variable, group = variable) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution de la productivite (en euros)") + geom_line()

x11()
qplot(annee, value, data = df, colour = variable, group = variable) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution de la productivite (en euros)") + geom_line()


x11()
qplot(variable, value, data = df1, colour = annee, group=annee) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution de la productivite (en euros)") + geom_line()
}

#Evolution des VA

VA <- function(){

library(ggplot2)
library(xlsx)
library(here)
library(data.table)
library(readxl)
library(gghighlight)

setwd(here("..","data/Etude ratio monetaire/INSEE/TES"))
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.xls") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  ldf[[k]] <- read_excel(listcsv[k], sheet = "CPR_CEB", range = "C5:AL5",col_names = FALSE)
}
str(ldf[[1]]) 


name <- read_excel(listcsv[1], sheet = "CPR_CEB", range = "C3:AL3",col_names = FALSE )

data = do.call(rbind,ldf)

colnames(data) <- name
data$annee <- 1989:2014
colnames(data[,37]) <- c("annee")

data <- as.data.frame(data)
dff <- melt(data, id=c(37))
dff <- na.omit(dff)
colnames(dff) <- c("annee","variable","value")

x11()
qplot(dff[,1], value, data = dff, colour = variable) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution de la valeur ajoutee (en millions d'euros)") + geom_line()


}

#productivite en VA

Productivite_va <- function(){
  
library(ggplot2)
library(xlsx)
library(here)
library(data.table)
library(readxl)

emplois <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/INSEE/emplois.xlsx")))
emplois <- as.data.table(lapply(emplois[,2:33], as.numeric))

valeur_ajoutee <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/INSEE/valeur_ajoutee.xlsx")))
valeur_ajoutee <- as.data.table(lapply(valeur_ajoutee[,2:37], as.numeric))
valeur_ajoutee <- valeur_ajoutee[,-(31:34)]

valeur_ajoutee <-as.data.frame(valeur_ajoutee)
a <- as.data.frame(emplois)
emplois <- a[ ,order(names(a))]

valeur_ajoutee <- valeur_ajoutee
emplois <- emplois*1000
productivite <- valeur_ajoutee/emplois
b <- c(1989:2014)
productivite$annee <- b

df <- melt(productivite, id="annee")
df1<-df
df1 <- df1[-(651:676),]#LZ activitees immo

x11()
qplot(df$annee, value, data = df, colour = variable) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution de la productivite en VA (en millions €)") + geom_line()

x11()
qplot(df1$annee, value, data = df1, colour = variable) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution de la productivite en VA (en millions €)") + geom_line()

x11()
ggplot(data=df,
       aes(x=df$annee, y=value, colour=variable, group=variable)) +
  geom_line() + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(x=NULL, y="Evolution des emplois nationaux(en milliers)")

}


#Evolution des emissions CO2e

emissions <- function(){
  
  library(ggplot2)
  library(xlsx)
  library(here)
  library(data.table)
  library(readxl)
  library(gghighlight)

  #conversion a88->a38
naf <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/CITEPA/naf88_38.xlsx")))
co2e <- as.data.table(read_excel(here("..", "data/Etude ratio monetaire/CITEPA/Resultat_nameaCO2e.xlsx")))

res<-merge(naf,co2e, by="naf_88")
res <- res[,-4]
res <- res[,-(1:2)]

res <- res[,-2]
melted <- melt(res, id="a38")
melted2 <- melted[which(melted$value<5000)]

x11()
qplot(variable, value, data = melted, colour = a38, group=a38) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution des emissions en MtCO2e") + geom_line()

x11()
qplot(variable, value, data = melted2, colour = a38, group=a38) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution des emissions en MtCO2e") + geom_line()

#Evolution productivite & emissions
names(df1) <- c("variable","a38","value")

merged <- merge(df1,melted, by=c("a38", "variable"))

x11()
ggplot(data=merged,
       aes(x = variable, y = value.x, col = a38)) +
  geom_line() + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(x=NULL, y="Evolution des emplois nationaux(en milliers)")

x11()
qplot(variable, value.x, data = merged, colour = a38, group=a38, size=value.y) + theme(axis.text.x=element_text(angle=60, hjust=1))+ labs(y=NULL, x="Evolution de la productivité en fonction de leurs emissions de carbone") + geom_line()

}
#BDD
library(odbc)
library(DBI)


con <- dbConnect(odbc::odbc(),
                 dbname='DataBaseEgisCarbone',
                 host='item.cloud-egis.com',
                 port=8888,
                 user='UserBaseInies',
                 password='Readbaseinies')

data <- dbSendQuery(con, "SELECT * FROM myTable WHERE DATE >= '2018-01-01';")
dbDisconnect(con)














#Benchmark

#normalisation des valeurs


emissions_totales = c(43412,30555)
a=mean(emissions_totales)
emissions_totales = c(43412*50/a,30555*50/a)

Energie_totale = c(723174930,440853977)
b=mean(Energie_totale)
Energie_totale= c(723174930*50/b,440853977*50/b)

Intensite_carbone_m2 = c(25.15,32)
c=mean(Intensite_carbone_m2)
Intensite_carbone_m2=c(25.15*50/c,32*50/c)

Energie_m2 = c(334.35,391.90)
d=mean(Energie_m2)
Energie_m2= c(334.35*50/d,391.90*50/d)

ratio_monetaires = c(46.83,54.75)


data<-cbind(emissions_totales,Energie_totales,Intensite_carbone_m2,Energie_m2,ratio_monetaires)

labels = c("emissions totales","Energie totales","Intensite carbone","Energie par m2","ratio monetaires \n (kgCO2/k€)")

colnames(data)<-labels 
datx = as.data.frame(data)
datx=rbind(rep(100,10) , rep(0,10) , datx)
rownames(datx)=c("Gécina","Covivio","Min","Max")

x11()

radarchart(datx, axistype = 2, seg = 5, axislabcol = 10, plty = 1,title = "Concurrents")
legend("bottom", title="Benchmark",
       c("Gecina","Covivio"),
       fill=c("black","red"), cex=0.8, bg="#CCFFCC")

#new Radar

x11()
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( datx  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = rownames(datx), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

#Excel Price

prices <- read_excel("D:/DATA/r.martin/Desktop/Stage_Elioth/Data/Data Price/Origine_DPGF Marche/BATEG ARBONIS/DPGF - Origine - marche - indB.xls", 
                                       sheet = "GO T1 (2)", col_types = c("blank", 
                                                                          "blank", "blank", "text", "text", 
                                                                          "blank", "text", "blank"))

prices <- read_excel("D:/DATA/r.martin/Desktop/Stage_Elioth/Data/Data Price/Origine_DPGF Marche/BATEG ARBONIS/DPGF - Origine - marche - indB.xls", 
                                       sheet = "CVCD (2)", col_types = c("blank", 
                                                                   "blank", "blank", "text", "text", 
                                                                   "blank", "blank", "blank", "text", 
                                                                   "blank"))
prices <- read_excel("D:/DATA/r.martin/Desktop/Stage_Elioth/Data/Data Price/Origine_DPGF Marche/BATEG ARBONIS/DPGF - Origine - marche - indB.xls", 
                                       sheet = "VRD", col_types = c("blank", 
                                                                    "text", "text", "blank", "blank", 
                                                                    "blank", "text", "blank"))
prices <- na.omit(prices)
prices <- prices[which(prices$X__2=="m²"),]
prices <- prices[!duplicated(prices$X__1),]
names(prices) = c("Libelle", "Unite", "Prix")

setwd("D:/DATA/r.martin/Desktop/Stage_Elioth/Data/regroupement des chiffrage m²")
files <- list.files(pattern="*.xlsx")
write.xlsx(prices, file="Chiffrage(19++).xlsx")



x_espaces_verts <- read_excel("Data/Data Price/Marche Nexity N8B DPGF client/DPGF CLIENT - MARCHE.xlsx", 
                              sheet = "ESPACES VERTS", col_types = c("blank", 
                                                                     "text", "text", "blank", "text", 
                                                                     "text", "text"))

x_RIE <- read_excel("Data/Data Price/Marche Nexity N8B DPGF client/DPGF CLIENT - MARCHE.xlsx", 
                                 sheet = "RIE", col_types = c("blank", 
                                                                     "text","blank" "text", "text", "text", "text"))  


prices <- read_excel("D:/DATA/r.martin/Desktop/Stage_Elioth/Data/Data Price/Marche Nexity N8B DPGF client/DPGF CLIENT - MARCHE.xlsx", 
                        sheet = "CLOISONS DOUBLAGES", col_types = c("blank", 
                                                                    "blank", "text", "blank", "blank", 
                                                                    "blank", "text", "blank", "text", 
                                                                    "blank"))

 
prices <- na.omit(prices)
prices <- prices[which(prices$X__2=="M2"|prices$X__2=="m²"),]
prices <- prices[!duplicated(prices$X__1),]
names(prices) = c("Libelle", "Unite", "Prix")

setwd("D:/DATA/r.martin/Desktop/Stage_Elioth/Data/Data price/regroupement des chiffrage m²")
write.xlsx(prices, file="Chiffrage(PLOMBERIE).xlsx")  
  
#Merging all files
library(xlsx)
library(readxl)
library(data.table)


  
setwd("D:/DATA/r.martin/Desktop/Stage_Elioth/Data/Data price/regroupement des chiffrage m²/DPGF_Client")
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.xlsx") # creates the list of all the xlsx files in the directory
for (k in 1:length(listcsv)){
  ldf[[k]] <- read_excel(listcsv[k])
}
str(ldf[[1]]) 
data = do.call(rbind,ldf)

setwd("D:/DATA/r.martin/Desktop/Stage_Elioth/Data/Data price/regroupement des chiffrage m²/Origine_DPGF Marche")
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.xlsx") # creates the list of all the xlsx files in the directory
for (k in 1:length(listcsv)){
  ldf[[k]] <- read_excel(listcsv[k])
}
str(ldf[[1]]) 
data1 = do.call(rbind,ldf)



Chiffrage <-rbind(data,data1) 
