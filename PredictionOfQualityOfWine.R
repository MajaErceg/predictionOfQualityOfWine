library("readr") #za citanje csv fajla
library("corrplot") #za crtanje korelacionih matrica 
library("randomForest") #za machine learning random forest algoritam
library("dplyr") #za manipulisanje podacima
library("Boruta") #za pripremu podataka za random forest algorutam
library("ggplot2") #za vizualizaciju podataka
library("corrgram") #za crtanje korgrama
library("Rcpp")
library("cowplot")
library("MASS")
library("ggplot2")
library("naniar")
library("e1071")
library("lattice")
library("caret")
library("car")
library("caTools")
library("knitr")
library("GGally")
library("tidyverse")

data<- read.csv("C://Users//EX//Desktop//IspitNSI//winequality-red.csv")
View(data) #da prikaz tabele

glimpse(data) #prikazuje strukturu
head(data, 10) #prikaz kolona sa prvih 10 redova

#Ciscenje seta podataka

#posto je set podataka sa kojim radim mali, neopohni su mi svi podaci, tako da necu izbacivati nista, ali to bi izgledalo ovako npr.
# data <- data %>% select(-fixed.acidity)
# data <- data %>% select(-volatile.acidity)
# head(data, 10)

data <- unique(data) #uklanjamo duplikate
dim(data) #redovi su smanjeni na 1359 nakon uklanjanja dupikata


#proveravamo da li imamo NA vrednosti
sum(is.na(data)) #nemamo NA vrednosti
#colSums prikazuje i nazive kolona 

#Od karakteristika koje vidimo da je „kvalitet“ naša ciljna karakteristika. 
#I imamo ukupno 11 karakteristika koje treba koristiti kao prediktore.

table(data$quality) #podaci za quality ocena i broj
prop.table(table(data$quality))  #provera odnosa različitih oznaka u ciljnom svojstvu

#Vizualizacija podataka

attach(data)
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
truehist(residual.sugar, h = 5, col="slategray3")
mtext("Residual Sugar", side=1, outer=F, line=2, cex=0.8)
truehist(chlorides, h = 0.01, col="slategray3")
mtext("Chloride", side=1, outer=F, line=2, cex=0.8)
truehist(alcohol, h = 0.5, col="slategray3")
mtext("Alcohol", side=1, outer=F, line=2, cex=0.8)
truehist(density, h = 0.005, col="slategray3")
mtext("Density", side=1, outer=F, line=2, cex=0.8)

#Crtanje nezavisnih promenljivih u odnosu na zavisnu promenljivu
#Quality
qualityplot<-ggplot(data=data,aes(x=quality))+geom_histogram(color="black", fill="lightblue")+ggtitle("Quality")+xlab("Quality") + ylab("Number of Wines")+ theme(plot.title = element_text(size=10, face = "bold"))
#Fixed acidity 
acidplot<-ggplot(data=data,aes(x=fixed.acidity,y=quality))+geom_point()+ggtitle("Fixed acidity")+ theme(plot.title = element_text(size=10, face = "bold"))
#Volatile acidity 
volacidplot<-ggplot(data=data,aes(x=volatile.acidity,y=quality))+geom_point()+ggtitle("Volatile acidity") +theme(plot.title = element_text(size=10, face = "bold"))
#Citric acid  
citricacidplot<-ggplot(data=data,aes(x=citric.acid,y=quality))+geom_point()+ggtitle("Citric acid") +theme(plot.title = element_text(size=10, face = "bold"))
#Residual sugar 
residplot<-ggplot(data=data,aes(x=residual.sugar,y=quality))+geom_point()+ggtitle("Residual sugar")+ theme(plot.title = element_text(size=10, face = "bold"))
#Chlorides
chloridesplot<-ggplot(data=data,aes(x=chlorides,y=quality))+geom_point()+ggtitle("Chlorides")+ theme(plot.title = element_text(size=10, face = "bold"))
#FreeSulfur
freesulfplot<-ggplot(data=data,aes(x=free.sulfur.dioxide,y=quality))+geom_point()+ggtitle("Free SO2") +theme(plot.title = element_text(size=10, face = "bold"))
#TotalSulfur
totalsulfplot<-ggplot(data=data,aes(x=total.sulfur.dioxide,y=quality))+geom_point()+ggtitle("Total SO2")+ theme(plot.title = element_text(size=10, face = "bold"))
#Density 
totaldensplot<-ggplot(data=data,aes(x=density,y=quality))+geom_point()+ggtitle("Density") +theme(plot.title = element_text(size=10, face = "bold"))
#Sulphates 
sulphatesplot<-ggplot(data=data,aes(x=sulphates,y=quality))+geom_point()+ggtitle("Sulphates")+ theme(plot.title = element_text(size=10, face = "bold"))
#pH 
pHplot<- ggplot(data=data,aes(x=pH,y=quality))+geom_point()+ggtitle("pH")+ theme(plot.title = element_text(size=10, face = "bold"))
#Alcohol
alcoholplot<-ggplot(data=data,aes(x=alcohol,y=quality))+geom_point()+ggtitle("Alcohol")+ theme(plot.title = element_text(size=10, face = "bold"))
#All plots combined
plot_grid(acidplot,volacidplot,citricacidplot,residplot,chloridesplot,freesulfplot,totalsulfplot,totaldensplot,sulphatesplot,pHplot,alcoholplot)


#odnos izmedju kvaliteta i alkohola

a <- data$alcohol
b <- data$quality

plot(a, b, main = "Relationship between quality and alcohol", xlab = "Acohol", ylab = "Quality", pch = 8, frame = FALSE)
abline(lm(b ~ a, data = data), col = "red")
cor.test(a, b, method = c("pearson", "kendall", "spearman")) #pozitivna korelacija 0.48

summary(data) #prikaz statistickih podataka naseg skupa podataka

#Multivarijantna analiza
plot_alcohol = ggplot(data) + geom_point(aes(alcohol,quality),colour = "red", alpha =0.3) + theme(axis.title = element_text(size = 8.5)) 
+ ggtitle("Plot of wine quality based on alcohol content") + xlab("Alcohol") + ylab("Quality")
plot_alcohol

plot_chlorides = ggplot(data)+geom_point(aes(chlorides, quality),colour = "red", alpha =0.3) + theme(axis.title = element_text(size = 8.5))  
+ ggtitle("Plot of wine quality based on chlorides content") + xlab("Chlorides") + ylab("Quality")
plot_chlorides

plot_density = ggplot(data)+geom_point(aes(density, quality),colour = "red", alpha =0.3) + theme(axis.title = element_text(size = 8.5))
+ ggtitle("Plot of wine quality based on density content") + xlab("Density") + ylab("Quality")
plot_density


#Sada ćemo videti matricu korelacije da bismo odlučili da li treba da koristimo PCA algoritam za izbor karakteristika
data_numeric <- data %>% select_if(is.numeric) #Za ovu matricu su potrebni samo numericki atributi

corMatrix <- cor(data_numeric) #izracunavaju se sve međusobne korelacije i formiraju matricu korelacije
corrplot(corMatrix,order = "FPC",method = "color",type = "lower", tl.cex = 0.6, tl.col = "black")
#PCA nije neophodna jer nema previše jakih korelacija između ciljne vrednosti quality i ostalih atributa


#Priprema podataka za Logical Regression algoritam

dataLR = data %>%
  mutate(quality_bin = as.factor(ifelse(quality <= 5, 0,1))) %>% 
  dplyr::select(-quality) #konvertujemo quantity u binarnu klasu

p = round(prop.table(table(dataLR$quality_bin))*100,2)
p
#Nakon transformacije imamo 47,1% slučajeva klasifikovanih kao dobra vina naspram 52,9% kao loša vina

#Splitting Data
set.seed(123)

split = sample.split(dataLR$quality_bin, SplitRatio = 0.80)
training_set = subset(dataLR, split == TRUE)
test_set = subset(dataLR, split == FALSE)

#da proverimo ravnotežu podataka u podacima o obuci i testiranju.
prop.table(table(training_set$quality_bin))
prop.table(table(test_set$quality_bin))
 

#Treniranje modela sa Logical Regression
model_log = glm(quality_bin ~ ., data = training_set, family = 'binomial')
summary(model_log)

#crtamo promenljive sa najnižim p vrednostima/najvećom apsolutnom z vrednošću.
p = varImp(model_log) %>% data.frame() 
p = p %>% mutate(Features = rownames(p)) %>% arrange(desc(Overall)) %>% mutate(Features = tolower(Features))

p %>% ggplot(aes(x = reorder(Features, Overall), y = Overall)) + geom_col(width = .50, fill = 'darkred') + coord_flip() + 
  labs(title = "Importance of Features", subtitle = "Based on the value of individual z score") +
  xlab("Features") + ylab("Abs. Z Score") + 
  theme_minimal()

#Uporedićemo predviđeni ishod sa stvarnim ishodom i izračunati neke uobičajeno korišćene metrike merenja performansi modela binarne klasifikacije.
# predict target feature in test data
y_pred = as.data.frame(predict(model_log, type = "response", newdata = test_set)) %>% 
  structure( names = c("pred_prob")) %>%
  mutate(pred_cat = as.factor(ifelse(pred_prob > 0.5, "1", "0"))) %>% 
  mutate(actual_cat = test_set$quality_bin)
p = confusionMatrix(y_pred$pred_cat, y_pred$actual_cat, positive = "1")
p

##############################################

#Transformacija podataka da bi mogli da se koriste u Random Forest algoritmu


#Random Forest Algorithm

# klasifikujemo vina na dobra, loša i normalna na osnovu njihovog kvaliteta

data$taste <- ifelse(data$quality < 5, 'bad', 'good')
data$taste[data$quality == 5] <- 'normal'
data$taste <- as.factor(data$taste)
table(data$taste)

#test and train
set.seed(123)
samp <- sample(nrow(data), 0.8 * nrow(data)) #Ovo će staviti80% zapažanja iz originalnog skupa podataka u obuku, a preostalihih20% zapažanja u test
train <- data[samp, ]
test <- data[-samp, ]

#build model

model <- randomForest(taste ~ . - quality, data = train)
model


#Možemo videti da je izgrađeno 500 stabala, a model je nasumično uzorkovao 3 prediktora pri svakom razdvajanju. Takođe prikazuje matricu koja sadrži predviđanje u odnosu na stvarnu, kao i grešku u klasifikaciji za svaku klasu.
#sada testiramo model na skupu testnih podataka.

pred <- predict(model, newdata = test)
table(pred, test$taste)

#Možemo testirati tačnost na sledeći način:
(0 + 134 + 75) / nrow(test)

#Postigli smo 75%% tačnosti sa veoma jednostavnim modelom.
#Moglo bi se dodatno poboljšati izborom karakteristika, a možda i isprobavanjem različitih vrednosti mtry.