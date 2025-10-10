setwd("C:/Users/andre/OneDrive/Documenti/unimib/anno 3/data mining/progetto")
data <- read.csv("AppleStore1.csv", sep=",", dec = ".")


setwd("C:/Users/massi/Desktop/UNI/3° anno/Data Mining/Progetto2")
data <- read.csv("AppleStore.csv", sep=",", dec = ".",  
                 stringsAsFactors=TRUE, na.strings=c("NA","NaN"))

sapply(data , function(x)(sum(is.na(x))))
#non ci sono Na

library(funModeling)
library(dplyr)
library(psych)
library(caret)

dim(data)
head(data)
names(data)

describe(data) 

#PREPROCESSING GENERALE

summary(data$user_rating)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   3.500   4.000   3.527   4.500   5.000 

hist(data$user_rating,freq=TRUE, main = "Istogramma user_rating")

# "id": ID app
#
# "track_name": nome dell'app
# 
# "size_bytes": dimensione (in byte)
# 
# "currency": tipo di valuta
# 
# "price": importo del prezzo
# 
# "rating count tot": conta la valutazione dell'utente (per tutte le versioni)
# 
# "rating count ver": conta la valutazione dell'utente (per la versione corrente)
# 
# "user_rating": valore medio della valutazione utente (per tutte le versioni)
# 
# "user rating ver": valore medio della valutazione utente (per la versione corrente)
# 
# "ver": codice della versione più recente
# 
# "cont_rating": classificazione dei contenuti
# 
# "prime_genre": genere principale
# 
# "sup_devices.num": numero di dispositivi di supporto
# 
# "ipadSc_urls.num": numero di screenshot mostrati per la visualizzazione
# 
# "lang.num": numero di lingue supportate
# 
# "vpp_lic": licenza basata su dispositivo Vpp abilitata

table(data$currency)
# USD 
# 5042 
#uguale per tutte le oss

#X, rating count ver, user rating ver, ver, sup_devices.num, ipadSc_urls.num, vpp_lic
data=data[,-c(1,2,5,8,10,11,17)]
#rimangono 10 var

summary(data)

#tolgono le oss con meno di 50 recensioni e le oss con 0 lingue supportate

table(data$user_rating)
# 0    1  1.5    2  2.5    3  3.5    4  4.5    5 
# 929   44   56  106  196  383  702 1626 2663  492 

#abbiamo tolto le 0 lingue e quelle con minimo 50 recensioni
data=data[which(data$rating_count_tot>=50),]
data=data[which(data$lang.num!=0),]

table(data$user_rating)
# 1  1.5    2  2.5    3  3.5    4  4.5    5 
# 7   26   60  132  252  521 1352 2392  300 

hist=hist(data$user_rating,freq=TRUE) 
cuts=cut(hist$breaks, c(-Inf,3.9,Inf))
plot(hist,col=c("red","green")[cuts], ylim = c(0,3000), xlab = "user rating", 
     main = "distribuzione user rating")


# data$ur_dic=ifelse(data$user_rating>4,"c1","c0")
# table(data$user_rating)
# table(data$ur_dic)
# 0    1 
# 2350 2692 
#0 app brutta 
#1 app bella

str(data)
data$track_name=as.character(data$track_name)
data$rating_count_tot=as.numeric(data$rating_count_tot)
data$lang.num=as.numeric(data$lang.num)
data$sup_devices.num=as.numeric(data$sup_devices.num)
data$ipadSc_urls.num=as.numeric(data$ipadSc_urls.num)
#data$ur_dic=as.factor(data$ur_dic)
str(data)
# 'data.frame':	5042 obs. of  10 variables:
# $ track_name      : chr  "PAC-MAN Premium" "Evernote - stay organized" "WeatherBug - Local Weather, Radar, Maps, Alerts" "eBay: Best App to Buy, Sell, Save! Online Shopping" ...
# $ size_bytes      : num  1.01e+08 1.59e+08 1.01e+08 1.29e+08 9.28e+07 ...
# $ price           : num  3.99 0 0 0 0 0.99 0 0 9.99 3.99 ...
# $ rating_count_tot: num  21292 161065 188583 262241 985920 ...
# $ user_rating     : num  4 4 3.5 4 4.5 4 4 4 4.5 4 ...
# $ cont_rating     : Factor w/ 4 levels "12+","17+","4+",..: 3 3 3 1 3 3 3 1 3 3 ...
# $ prime_genre     : Factor w/ 23 levels "Book","Business",..: 8 16 23 18 17 8 6 12 22 8 ...
# $ sup_devices.num : num  38 37 37 37 37 47 37 37 37 38 ...
# $ ipadSc_urls.num : num  5 5 5 5 5 5 0 4 5 0 ...
# $ lang.num        : num  10 23 3 9 45 1 19 1 1 10 ...

#DIVISIONE DATASET TRAINING, VALIDATION, SCORE
library(caret)
set.seed(1234)
split <- createDataPartition(y=data$user_rating, p = 0.90, list = FALSE)
datan <- data[split,]
score <- data[-split,] 
#10% del dataset è lo score

#score<-score[,-5]  #504 obs of 9 var
#tolgo la variabile target nello score

set.seed(1234)
split <- createDataPartition(y=datan$user_rating, p = 0.70, list = FALSE)
train <- datan[split,]
test <- datan[-split,]
#divido il dataset in train e test (70% e 30%)

#PREPROCESSING

#Missing values
sapply(data , function(x)(sum(is.na(x))))
#0 Na

#Dividere le variabili in factor e numeric
factor <- sapply(train, function(x) is.factor(x))
factor
factordata <- train[, factor]
str(factordata)

numeric <- sapply(train, function(x) is.numeric(x))
numeric
numericdata <-train[, numeric]
str(numericdata)

#Collinearit�
numericdata_c=numericdata[,-4] #solo covariate nella collinearità
R=cor(numericdata_c)
R

correlatedPredictors = findCorrelation(R, cutoff = 0.95, names = TRUE)
correlatedPredictors  

correlatedPredictors = findCorrelation(R, cutoff = 0.9, names = TRUE)
correlatedPredictors

# Chi quadro
# se il chi quadro normalizzato ha valori >0.9, si elimina una delle due variabili #

library(plyr)
x_qual <- data[,c(6,7)]
combos <- combn(ncol(x_qual),2)
adply(combos, 2, function(x) {
  test <- chisq.test(x_qual[, x[1]], x_qual[, x[2]])
  tab  <- table(x_qual[, x[1]], x_qual[, x[2]])
  out <- data.frame("Row" = colnames(x_qual)[x[1]]
                    , "Column" = colnames(x_qual[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(x_qual[,x[1]], x_qual[,x[2]]))
                    , "u1" =length(unique(x_qual[,x[1]]))-1
                    , "u2" =length(unique(x_qual[,x[2]]))-1
                    , "nMinu1u2" =sum(table(x_qual[,x[1]], x_qual[,x[2]]))* min(length(unique(x_qual[,x[1]]))-1 , length(unique(x_qual[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(x_qual[,x[1]], x_qual[,x[2]]))* min(length(unique(x_qual[,x[1]]))-1 , length(unique(x_qual[,x[2]]))-1)) 
  )
  
  
  return(out)
  
}) 
rm(x_qual)








#Near zero variance
#Predittori con varianza zero e varianza vicino a zero 
nzv = nearZeroVar(train, saveMetrics = TRUE)
nzv 
#nessuna var vicino a zero

#vogliamo mettere la var target nella prima colonna
user_rating=train$user_rating
train=train[,-5] 
train=cbind(user_rating,train)
train=train[,-2] #tolgo track_name perchè chr 

#NORMALIZZARE TRAIN
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
train_norm <- as.data.frame(lapply(train[c(1,2,3,4,7,8,9)], min_max_norm))

head(train_norm)
summary(train_norm) #Median :0.8750

#vogliamo mettere la var target nella prima colonna
user_rating=test$user_rating
test=test[,-5]
test=cbind(user_rating,test)
test=test[,-2]

#NORMALIZZARE TEST
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
test_norm <- as.data.frame(lapply(test[c(1,2,3,4,7,8,9)], min_max_norm))

head(test_norm)
summary(test_norm) #Median :0.875

#BINARIZZARE LA VARIABILE TARGET DEL TRAIN_NORM
train_norm$user_rating=ifelse(train_norm$user_rating>=0.875, "c1", "c0")
table(train_norm$user_rating)
# c0   c1 
# 1481 1697

str(train_norm)
train_norm$user_rating=as.factor(train_norm$user_rating)

#RANDOM FOREST - IMPORTAZA VARIABILI TRAIN_NORM
set.seed(1)
cvCtrl_norm <- trainControl(method = "cv", number=5, search="grid", classProbs = TRUE,
                            summaryFunction = twoClassSummary)
rpartTuneCvA_norm <- train(user_rating ~ ., data = train_norm, method = "rpart",
                           tuneLength = 10,
                           trControl = cvCtrl_norm)
rpartTuneCvA_norm
getTrainPerf(rpartTuneCvA_norm)

# var imp of the tree
plot(varImp(object=rpartTuneCvA_norm),main="train_norm tuned - Variable Importance")

# VAR IMPORTANCE
Vimportance_norm <- varImp(rpartTuneCvA_norm)
head(Vimportance_norm)
plot(Vimportance_norm)

# save var imp as dataframe####
VImP_norm=as.data.frame(Vimportance_norm$importance)
head(VImP_norm)
dim(VImP_norm)

#..then drop least important var (<20%, <10% depends on the number M of x), 
# select them (V) and select V columns from M columns in the initial dataset

V_norm=subset(VImP_norm, Overall>10)
V2_norm=t(V_norm)

y_norm=train_norm[1] 

# select important vars
Xselected_norm=train_norm[,colnames(V2_norm)]

# add
train_norm_imp=cbind(Xselected_norm,y_norm)
head(train_norm_imp)
#data set preprocessato con solo le var importanti

#STANDARDIZZARE TRAIN
train_stand <- as.data.frame(scale(train[c(1,2,3,4,7,8,9)]))
#view first six rows of standardized dataset
head(train_stand)
summary(train_stand) #Median : 0.6135

#STANDARDIZZARE TEST
test_stand <- as.data.frame(scale(test[c(1,2,3,4,7,8,9)]))
summary(test_stand)  #Median : 0.6272

#BINARIZZARE VARIABILE TARGET DEL TRAIN_STAND
sogliastd <- (4.25-mean(train$user_rating))/sd(train$user_rating)
train_stand$user_rating=ifelse(train_stand$user_rating>=sogliastd, "c1", "c0")
table(train_stand$user_rating)
# c0   c1 
# 1481 1697 

#BINARIZZARE VARIABILE TARGET DEL TRAIN
train$user_rating=ifelse(train$user_rating>4, "c1", "c0")
#divido la variabile target in c0 <=4 e c1>4
table(train$user_rating)
# c0   c1 
# 1481 1697 

sapply(train_stand , function(x)(sum(is.na(x))))
#non ho var mancanti in train_stand

train_num=train[,-c(5,6)]

str(train)
train$user_rating=as.factor(train$user_rating)
str(train_stand)
train_stand$user_rating=as.factor(train_stand$user_rating)

#RANDOM FOREST - IMPORTAZA VARIABILI TRAIN
# select covariates with tree or rf or as you want #####
set.seed(1)
cvCtrl <- trainControl(method = "cv", number=5, search="grid", classProbs = TRUE,
                       summaryFunction = twoClassSummary)
rpartTuneCvA <- train(user_rating ~ ., data = train_num, method = "rpart",
                      tuneLength = 10,
                      trControl = cvCtrl)
rpartTuneCvA
getTrainPerf(rpartTuneCvA)

# var imp of the tree
plot(varImp(object=rpartTuneCvA),main="train tuned - Variable Importance")

# VAR IMPORTANCE
Vimportance <- varImp(rpartTuneCvA)
head(Vimportance)
plot(Vimportance)

# SAVE var IMP As DATAFRAME####
VImP=as.data.frame(Vimportance$importance)
head(VImP)
dim(VImP)

#..then drop least important var (<20%, <10% depends on the number M of x), 
# select them (V) and select V columns from M columns in the initial dataset

V=subset(VImP, Overall>10)
V2=t(V)

y=train[1] 

# select important vars
Xselected=train[,colnames(V2)]

# add
train_imp=cbind(Xselected,y)
head(train_imp)
#data set preprocessato con solo le var importanti

#RANDOM FOREST - IMPORTANZA VARIABILI TRAIN_STAND
set.seed(1)
cvCtrl_stand <- trainControl(method = "cv", number=5, search="grid", classProbs = TRUE,
                             summaryFunction = twoClassSummary)
rpartTuneCvA_stand <- train(user_rating ~ ., data = train_stand, method = "rpart",
                            tuneLength = 10,
                            trControl = cvCtrl_stand)
rpartTuneCvA_stand
getTrainPerf(rpartTuneCvA_stand)

# var imp of the tree
plot(varImp(object=rpartTuneCvA_stand),main="train_stand tuned - Variable Importance")

# VAR IMPORTANCE######
Vimportance_stand <- varImp(rpartTuneCvA_stand)
head(Vimportance_stand)
plot(Vimportance_stand)

# SAVE var IMP As DATAFRAME####
VImP_stand=as.data.frame(Vimportance_stand$importance)
head(VImP_stand)
dim(VImP_stand)

#..then drop least important var (<20%, <10% depends on the number M of x), 
# select them (V) and select V columns from M columns in the initial dataset

V_stand=subset(VImP_stand, Overall>5)
V2_stand=t(V_stand)

y_stand=train_stand[1] 

# select important vars
Xselected_stand=train_stand[,colnames(V2_stand)]

# add
train_stand_imp=cbind(Xselected_stand,y_stand)
head(train_stand_imp)
#data set preprocessato con solo le var importanti

#LOGISTICO TRAIN_STAND (NON SERVE PER LE ROC)
require(car)
train_stand$user_rating <- recode(train_stand$user_rating, recodes='"c1"=1; else=0')

metric<-"Spec"
controlGLM_stand=trainControl(method="cv",number=10,classProbs=TRUE,summaryFunction=twoClassSummary)
glm_stand <- glm(user_rating~ .,data=train_stand,family="binomial")
glm_stand

#OUTLIERS SUL TRAIN_STAND
#method="cv" tolto da glm_stand
#uso le distanze di cook per vedere gli outliers 
require(gghalfnorm)
cook=cooks.distance(glm_stand)
cutoff <- 4/((nrow(train_stand)-length(glm_stand$coefficients)-1))
no_out <- subset(train_stand,cook<max(cutoff))
require(car)
train_stand$user_rating<- recode(train_stand$user_rating,recode='1="c1";else="c0"')
require(car)
no_out$user_rating<- recode(no_out$user_rating,recode='1="c1";else="c0"')
dim(no_out)
dim(train_stand)
influencePlot(glm_stand,  main="Influence Plot")
#17 e 116 outliers

table(no_out$user_rating)
# c0   c1 
# 1425 1661
#tolte 92 osservazioni

#NAIVE BAYES TRAIN (50 problemi per lo zero problem)
metric<-"Spec"
controlNB=trainControl(method = "cv",number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
naivebayes=train(user_rating ~.,data=train,method="naive_bayes",
                 trControl=controlNB,tuneLength=5,trace=TRUE,metric=metric, na.action = na.pass)
naivebayes

#NAIVE BAYES TRAIN_STAND CON SOLO VAR IMPORTANTI (train_stand_imp)
controlNB_stand_imp=trainControl(method = "cv",number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
naivebayes_stand_imp<-train(user_rating ~.,data=train_stand_imp,method="nb",
                            trControl=controlNB_stand_imp,tuneLength=5,trace=TRUE,metric=metric,laplace = 50)
naivebayes_stand_imp
#train_stand_imp ha solo 1 variabile quindi non va

#NAIVE BAYES SUL TRAIN_STAND 
controlNB_stand=trainControl(method = "cv",number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
naivebayes_stand<-train(user_rating ~.,data=train_stand,method="nb",
                        trControl=controlNB_stand,tuneLength=5,trace=TRUE,metric=metric,laplace = 50)
naivebayes_stand

#NAIVE BAYES SUL TRAIN SENZA VARIABILI NON IMPORTANTI 
controlNB_imp=trainControl(method = "cv",number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
naivebayes_imp<-train(user_rating ~.,data=train_imp,method="nb",
                      trControl=controlNB_imp,tuneLength=5,trace=TRUE,metric=metric,laplace = 50)
naivebayes_imp

#NAIVE BAYES SUL TRAIN_NORM
metric<-"Spec"
controlNB_norm=trainControl(method = "cv",number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
naivebayes_norm=train(user_rating ~.,data=train_norm,method="naive_bayes",
                      trControl=controlNB_norm,tuneLength=5,trace=TRUE,metric=metric, na.action = na.pass)
naivebayes_norm

#NAIVE BAYES SUL TRAIN_NORM SENZA VARIABILI NON IMPORTANTI (newDAta_norm)
metric<-"Spec"
controlNB_norm_imp=trainControl(method = "cv",number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
naivebayes_norm_imp=train(user_rating ~.,data=train_norm_imp,method="naive_bayes",
                          trControl=controlNB_norm_imp,tuneLength=5,trace=TRUE,metric=metric, na.action = na.pass)
naivebayes_norm_imp

#NAIVE BAYES SUL DATASET SENZA OUTLIERS (no_out)
metric<-"Spec"
controlNB_no_out=trainControl(method = "cv",number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
naivebayes_no_out=train(user_rating ~.,data=no_out,method="naive_bayes",
                        trControl=controlNB_no_out,tuneLength=5,trace=TRUE,metric=metric, na.action = na.pass)
naivebayes_no_out

#LOGISTICO TRAIN (NON SERVE)
metric<-"Spec"
controlGLM=trainControl(method="cv",number=10,classProbs=TRUE,summaryFunction=twoClassSummary)
glm <- train(user_rating~ .,data=train,method="glm",trControl=controlGLM,
             tuneLength=5,trace=TRUE,metric=metric)
glm

#LOGISTICO TRAIN_STAND CON SOLO VAR IMPORTANTI (train_stand_imp)
metric<-"Spec"
controlGLM_stand_imp=trainControl(method="cv",number=10,classProbs=TRUE,summaryFunction=twoClassSummary)
glm_stand_imp <- train(user_rating~ .,data=train_stand_imp,method="glm",trControl=controlGLM_stand_imp,
                       tuneLength=5,trace=TRUE,metric=metric)
glm_stand_imp

#LOGISTICO SUL TRAIN SENZA VARIABILI NON IMPORTANTI (train_imp)
control_imp=trainControl(method="cv",number=10,classProbs=TRUE,summaryFunction=twoClassSummary)
glm_imp <- train(user_rating~ .,data=train_imp,method="glm",trControl=control_imp,
                 tuneLength=5,trace=TRUE,metric=metric)
glm_imp

#LOGISTICO SUL TRAIN_NORM (NON SERVE)
metric<-"Spec"
controlGLM_norm=trainControl(method="cv",number=10,classProbs=TRUE,summaryFunction=twoClassSummary)
glm_norm <- train(user_rating~ .,data=train_norm,method="glm",trControl=controlGLM_norm,
                  tuneLength=5,trace=TRUE,metric=metric)
glm_norm

#LOGISTICO SUL TRAIN_NORM SENZA VARIABILI NON IMPORTANTI (train_norm_imp)
metric<-"Spec"
controlGLM_norm_imp=trainControl(method="cv",number=10,classProbs=TRUE,summaryFunction=twoClassSummary)
glm_norm_imp <- train(user_rating~ .,data=train_norm_imp,method="glm",trControl=controlGLM_norm_imp,
                      tuneLength=5,trace=TRUE,metric=metric)
glm_norm_imp

#LOGISTICO SUL DATASET SENZA OUTLIERS (no_out)
metric<-"Spec"
controlGLM_no_out=trainControl(method="cv",number=10,classProbs=TRUE,summaryFunction=twoClassSummary)
glm_no_out <- train(user_rating~ .,data=no_out,method="glm",trControl=controlGLM_no_out,
                    tuneLength=5,trace=TRUE,metric=metric)
glm_no_out

#ALBERO SUL TRAIN
metric <- "Spec"  
controlTREE= trainControl(method="cv", number=10, classProbs=TRUE, summaryFunction=twoClassSummary)
tree <- train (user_rating ~ ., data=train, method="rpart",
               trControl=controlTREE,tuneLength=5,metric=metric)
tree

#ALBERO TRAIN_STAND CON SOLO VAR IMPORTANTI (newData_stand)
metric <- "Spec"  
controlTREE_stand_imp= trainControl(method="cv", number=10, classProbs=TRUE, summaryFunction=twoClassSummary)
tree_stand_imp <- train(user_rating ~ ., data=train_stand_imp, method="rpart",
                        trControl=controlTREE_stand_imp,tuneLength=5,metric=metric)
tree_stand_imp

#ALBERO TRAIN_STAND
metric <- "Spec"  
controlTREE_stand= trainControl(method="cv", number=10, classProbs=TRUE, summaryFunction=twoClassSummary)
tree_stand<- train (user_rating ~ ., data=train_stand, method="rpart",
                    trControl=controlTREE_stand,tuneLength=5,metric=metric)
tree_stand

#ALBERO SUL TRAIN SENZA VARIABILI NON IMPORTANTI
controlTREE_imp= trainControl(method="cv", number=10, classProbs=TRUE, summaryFunction=twoClassSummary)
tree_imp <- train (user_rating ~ ., data=train_imp, method="rpart",
                   trControl=controlTREE_imp,tuneLength=5,metric=metric)
tree_imp

#ALBERO SUL TRAIN_NORM
metric <- "Spec"  
controlTREE_norm= trainControl(method="cv", number=10, classProbs=TRUE, summaryFunction=twoClassSummary)
tree_norm <- train (user_rating ~ ., data=train_norm, method="rpart",
                    trControl=controlTREE_norm,tuneLength=5,metric=metric)
tree_norm

#ALBERO SUL TRAIN_NORM SENZA VARIABILI NON IMPORTANTI (train_norm_imp)
metric <- "Spec"  
controlTREE_norm_imp= trainControl(method="cv", number=10, classProbs=TRUE, summaryFunction=twoClassSummary)
tree_norm_imp <- train (user_rating ~ ., data=train_norm_imp, method="rpart",
                        trControl=controlTREE_norm_imp,tuneLength=5,metric=metric)
tree_norm_imp

#ALBERO SUL DATASET SENZA OUTLIERS (no_out)
metric <- "Spec"  
controlTREE_no_out= trainControl(method="cv", number=10, classProbs=TRUE, summaryFunction=twoClassSummary)
tree_no_out <- train (user_rating ~ ., data=no_out, method="rpart",
                      trControl=controlTREE_no_out,tuneLength=5,metric=metric)
tree_no_out

#RANDOM FOREST SUL TRAIN
metric <- "Spec"
controlRF=trainControl(method="cv", number=10,classProbs=TRUE, summaryFunction=twoClassSummary)
rf <- train(user_rating~ ., data=train, method="rf",trControl=controlRF,tuneLenght=5,
            trace=TRUE,metric=metric)
rf

#RANDOM FOREST TRAIN_STAND CON SOLO VAR IMPORTANTI (train_stand_imp)
metric <- "Spec"
controlRF_stand_imp=trainControl(method="cv", number=10,classProbs=TRUE, summaryFunction=twoClassSummary)
rf_stand_imp <- train(user_rating~ ., data=train_stand_imp, method="rf",trControl=controlRF_stand_imp,tuneLenght=5,
                      trace=TRUE,metric=metric)
rf_stand_imp

#RANDOM FOREST TRAIN_STAND
metric<-"Spec"
controlRF_stand=trainControl(method="cv", number=10,classProbs=TRUE, summaryFunction=twoClassSummary)
rf_stand <- train(user_rating~ ., data=train_stand, method="rf",trControl=controlRF_stand,tuneLenght=5,
                  trace=TRUE,metric=metric)
rf_stand

#RANDOM FOREST SUL TRAIN SENZA VARIABILI NON IMPORTANTI (train_imp)
controlRF_imp=trainControl(method="cv", number=10,classProbs=TRUE, summaryFunction=twoClassSummary)
rf_imp <- train(user_rating~ ., data=train_imp, method="rf",trControl=controlRF_imp,tuneLenght=5,
                trace=TRUE,metric=metric)

rf_imp

#RANDOM FOREST SUL TRAIN_NORM
metric <- "Spec"
controlRF_norm=trainControl(method="cv", number=10,classProbs=TRUE, summaryFunction=twoClassSummary)
rf_norm <- train(user_rating~ ., data=train_norm, method="rf",trControl=controlRF_norm,tuneLenght=5,
                 trace=TRUE,metric=metric)
rf_norm

#RANDOM FOREST SUL TRAIN_NORM SENZA VARIABILI NON IMPORTANTI (train_norm_imp)
metric <- "Spec"
controlRF_norm_imp=trainControl(method="cv", number=10,classProbs=TRUE, summaryFunction=twoClassSummary)
rf_norm_imp <- train(user_rating~ ., data=train_norm_imp, method="rf",trControl=controlRF_norm_imp,tuneLenght=5,
                     trace=TRUE,metric=metric)
rf_norm_imp

#RANDOM FOREST SUL DATSET SENZA OUTLIERS (no_out)
metric <- "Spec"
controlRF_no_out=trainControl(method="cv", number=10,classProbs=TRUE, summaryFunction=twoClassSummary)
rf_no_out <- train(user_rating~ ., data=no_out, method="rf",trControl=controlRF_no_out,tuneLenght=5,
                   trace=TRUE,metric=metric)
rf_no_out

#RETE NEURALE (NON SERVE)
metric <- "Spec"
controlNN=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
nnet<-train(user_rating~.,data=train,method="nnet",
            trControl=controlNN,tunelength=5,trace=TRUE,metric=metric)
nnet

#RETE NEURALE TRAIN_STAND CON SOLO VAR IMPORTANTI (train_stand_imp) (NON SERVE)
metric <- "Spec"
controlNN_stand_imp=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
nnet_stand_imp<-train(user_rating~.,data=train_stand_imp,method="nnet",
                      trControl=controlNN_stand_imp,tunelength=5,trace=TRUE,metric=metric)
nnet_stand_imp

#RETE NEURALE TRAIN_STAND (NON SERVE)
metric <- "Spec"
controlNN_stand=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
nnet_stand<-train(user_rating~.,data=train_stand,method="nnet",
                  trControl=controlNN_stand,tunelength=5,trace=TRUE,metric=metric)
nnet_stand

#RETE NEURALI SENZA VARIABILI NON IMPORTANTI (train_imp) (NON SERVE)
controlNN_imp=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
nnet_imp<-train(user_rating~.,data=train_imp,method="nnet",
                trControl=controlNN_imp,tunelength=5,trace=TRUE,metric=metric)
nnet_imp

#RETE NEURALE SUL TRAIN_NORM 
metric <- "Spec"
controlNN_norm=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
nnet_norm<-train(user_rating~.,data=train_norm,method="nnet",
                 trControl=controlNN_norm,tunelength=5,trace=TRUE,metric=metric)
nnet_norm

#RETE NEURALE SUL TRAIN_NORM SENZA VARIABILI NON IMPORTANTI (train_norm_imp)
metric <- "Spec"
controlNN_norm_imp=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
nnet_norm_imp<-train(user_rating~.,data=train_norm_imp,method="nnet",
                     trControl=controlNN_norm_imp,tunelength=5,trace=TRUE,metric=metric)
nnet_norm_imp

# #PLS TRAIN
# metric<-"Spec"
# controlPLS=trainControl(method="cv", number = 10, classProbs=TRUE, summaryFunction=twoClassSummary)
# pls <- train(user_rating~ ., data=train, method="pls",trControl=controlPLS,tuneLenght=5,
#             trace=TRUE,metric=metric)
# pls
# 
# #PLS TRAIN_STAND
# metric<-"Spec"
# controlPLS=trainControl(method="cv", number = 10, classProbs=TRUE, summaryFunction=twoClassSummary)
# pls <- train(user_rating~ ., data=train_stand, method="pls",trControl=controlPLS,tuneLenght=5,
#              trace=TRUE,metric=metric)
# pls
# 
# #PLS TRAIN_STAND_imp
# metric<-"Spec"
# controlPLS=trainControl(method="cv", number = 10, classProbs=TRUE, summaryFunction=twoClassSummary)
# pls <- train(user_rating~ ., data=train_stand_imp, method="pls",trControl=controlPLS,tuneLenght=5,
#              trace=TRUE,metric=metric)
# pls
# 
# #PLS TRAIN_imp
# metric<-"Spec"
# controlPLS=trainControl(method="cv", number = 10, classProbs=TRUE, summaryFunction=twoClassSummary)
# pls <- train(user_rating~ ., data=train_imp, method="pls",trControl=controlPLS,tuneLenght=5,
#              trace=TRUE,metric=metric)
# pls
# 
# #PLS TRAIN_norm
# metric<-"Spec"
# controlPLS=trainControl(method="cv", number = 10, classProbs=TRUE, summaryFunction=twoClassSummary)
# pls <- train(user_rating~ ., data=train_norm, method="pls",trControl=controlPLS,tuneLenght=5,
#              trace=TRUE,metric=metric)
# pls
# 
# #PLS TRAIN_norm_imp
# metric<-"Spec"
# controlPLS=trainControl(method="cv", number = 10, classProbs=TRUE, summaryFunction=twoClassSummary)
# pls <- train(user_rating~ ., data=train_norm_imp, method="pls",trControl=controlPLS,tuneLenght=5,
#              trace=TRUE,metric=metric)
# pls
# 
# #PLS no_out
# metric<-"Spec"
# controlPLS=trainControl(method="cv", number = 10, classProbs=TRUE, summaryFunction=twoClassSummary)
# pls <- train(user_rating~ ., data=no_out, method="pls",trControl=controlPLS,tuneLenght=5,
#              trace=TRUE,metric=metric)
# pls

#KNN (NON SERVE)
metric<-"Spec"
controlKNN=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
knn<-train(user_rating~ ., data=train,method="knn",trControl=controlKNN,tuneLength=5,metric=metric)
knn

#KNN TRAIN_STAND CON SOLO VAR IMPORTANTI (train_stand_imp)
metric<-"Spec"
controlKNN_stand_imp=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
knn_stand_imp<-train(user_rating~ ., data=train_stand_imp,method="knn",trControl=controlKNN_stand_imp,tuneLength=5,metric=metric)
knn_stand_imp

#KNN TRAIN_STAND
metric<-"Spec"
controlKNN_stand=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
knn_stand<-train(user_rating~ ., data=train_stand,method="knn",trControl=controlKNN_stand,tuneLength=5,metric=metric)
knn_stand

#KNN SENZA VARIABILI NON IMPORTANTI (train_imp) (NON SERVE)
controlKNN_imp=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
knn_imp<-train(user_rating~ ., data=train_imp,method="knn",trControl=controlKNN_imp,tuneLength=5,metric=metric)
knn_imp

#KNN SUL TRAIN_NORM (NON SERVE)
metric<-"Spec"
controlKNN_norm=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
knn_norm<-train(user_rating~ ., data=train_norm,method="knn",trControl=controlKNN_norm,tuneLength=5,metric=metric)
knn_norm

#KNN  SUL TRAIN_NORM SENZA VARIABILI NON IMPORTANTI (train_norm_imp) (NON SERVE)
metric<-"Spec"
controlKNN_norm_imp=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
knn_norm_imp<-train(user_rating~ ., data=train_norm_imp,method="knn",trControl=controlKNN_norm_imp,tuneLength=5,metric=metric)
knn_norm_imp

#KNN SUL DATASET SENZA OUTLIERS (no_out)
metric<-"Spec"
controlKNN_no_out=trainControl(method="cv",number=10,classProbs = TRUE,summaryFunction = twoClassSummary)
knn_no_out<-train(user_rating~ ., data=no_out,method="knn",trControl=controlKNN_no_out,tuneLength=5,metric=metric)
knn_no_out

#GRADIENT BOOSTING SUL TRAIN
metric <- "Spec"
controlGB=trainControl(method="cv", number=10, classProbs= TRUE, summaryFunction=twoClassSummary)
gb <- train (user_rating~ ., data=train, 
             method = "gbm",
             trControl = controlGB,
             verbose = FALSE,
             tuneGrid=data.frame(interaction.depth=4,
                                 n.trees = 100,
                                 shrinkage = .1,
                                 n.minobsinnode = 20),
             metric=metric)
gb

#GRADIENT BOOSTING TRAIN_STAND CON SOLO VAR IMPORTANTI (train_stand_imp)
metric <- "Spec"
controlGB_stand_imp=trainControl(method="cv", number=10, classProbs= TRUE, summaryFunction=twoClassSummary)
gb_stand_imp <- train (user_rating~ ., data=train_stand_imp, 
                       method = "gbm",
                       trControl = controlGB_stand_imp,
                       verbose = FALSE,
                       tuneGrid=data.frame(interaction.depth=4,
                                           n.trees = 100,
                                           shrinkage = .1,
                                           n.minobsinnode = 20),
                       metric=metric)
gb_stand_imp

#GRADIENT BOOSTING TRAIN_STAND
metric <- "Spec"
controlGB_stand=trainControl(method="cv", number=10, classProbs= TRUE, summaryFunction=twoClassSummary)
gb_stand <- train (user_rating~ ., data=train_stand, 
                   method = "gbm",
                   trControl = controlGB_stand,
                   verbose = FALSE,
                   tuneGrid=data.frame(interaction.depth=4,
                                       n.trees = 100,
                                       shrinkage = .1,
                                       n.minobsinnode = 20),
                   metric=metric)
gb_stand

#GRADIENT BOOSTING SENZA VARIABILI NON IMPORTANTI
controlGB_imp=trainControl(method="cv", number=10, classProbs= TRUE, summaryFunction=twoClassSummary)
gb_imp <- train (user_rating~ ., data=train_imp, 
                 method = "gbm",
                 trControl = controlGB_imp,
                 verbose = FALSE,
                 tuneGrid=data.frame(interaction.depth=4,
                                     n.trees = 100,
                                     shrinkage = .1,
                                     n.minobsinnode = 20),
                 metric=metric)
gb_imp

#GRADIENT BOOSTING SUL TRAIN_NORM
metric <- "Spec"
controlGB_norm=trainControl(method="cv", number=10, classProbs= TRUE, summaryFunction=twoClassSummary)
gb_norm <- train (user_rating~ ., data=train_norm, 
                  method = "gbm",
                  trControl = controlGB_norm,
                  verbose = FALSE,
                  tuneGrid=data.frame(interaction.depth=4,
                                      n.trees = 100,
                                      shrinkage = .1,
                                      n.minobsinnode = 20),
                  metric=metric)
gb_norm

#GRADIENT BOOSTING SUL TRAIN_NORM SENZA VARIABILI NON IMPORTANTI (train_norm_imp)
metric <- "Spec"
controlGB_norm_imp=trainControl(method="cv", number=10, classProbs= TRUE, summaryFunction=twoClassSummary)
gb_norm_imp <- train (user_rating~ ., data=train_norm_imp, 
                      method = "gbm",
                      trControl = controlGB_norm_imp,
                      verbose = FALSE,
                      tuneGrid=data.frame(interaction.depth=4,
                                          n.trees = 100,
                                          shrinkage = .1,
                                          n.minobsinnode = 20),
                      metric=metric)
gb_norm_imp

#GRADIENT BOOSTING SUL DATASET SENZA OUTLIERS (no_out)
metric <- "Spec"
controlGB_no_out=trainControl(method="cv", number=10, classProbs= TRUE, summaryFunction=twoClassSummary)
gb_no_out <- train (user_rating~ ., data=no_out, 
                    method = "gbm",
                    trControl = controlGB_no_out,
                    verbose = FALSE,
                    tuneGrid=data.frame(interaction.depth=4,
                                        n.trees = 100,
                                        shrinkage = .1,
                                        n.minobsinnode = 20),
                    metric=metric)
gb_no_out

#LASSO SUL TRAIN
set.seed(1)
ctrl =trainControl(method="cv", number = 10, classProbs = T,
                   summaryFunction=twoClassSummary)
grid = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.01))
lasso=train(user_rating~.,
            data=train,method = "glmnet",
            trControl = ctrl, tuneLength=5, na.action = na.pass,
            tuneGrid=grid)
lasso
plot(lasso)
confusionMatrix(lasso)

#LASSO TRAIN_STAND CON SOLO VAR IMPORTANTI (train_stand_imp) (NON SERVE)
set.seed(1)
ctrl_stand_imp =trainControl(method="cv", number = 10, classProbs = T,
                             summaryFunction=twoClassSummary)
grid_stand_imp = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.01))
lasso_stand_imp=train(user_rating~.,
                      data=train_stand_imp,method = "glmnet",
                      trControl = ctrl_stand_imp, tuneLength=5, na.action = na.pass,
                      tuneGrid=grid_stand_imp)
lasso_stand_imp
plot(lasso_stand_imp)
confusionMatrix(lasso_stand_imp)

#LASSO TRAIN_STAND
set.seed(1)
ctrl_stand =trainControl(method="cv", number = 10, classProbs = T,
                         summaryFunction=twoClassSummary)
grid_stand = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.01))
lasso_stand=train(user_rating~.,
                  data=train_stand,method = "glmnet",
                  trControl = ctrl_stand, tuneLength=5, na.action = na.pass,
                  tuneGrid=grid_stand)
lasso_stand
plot(lasso_stand)
confusionMatrix(lasso_stand)

#LASSO SENZA VARIABILI NON IMPORTANTI (NON SERVE)
ctrlLASSO_imp =trainControl(method="cv", number = 10, classProbs = T,
                            summaryFunction=twoClassSummary)
lasso_imp=train(user_rating~.,
                data=train_imp,method = "glmnet",
                trControl = ctrlLASSO_imp, tuneLength=5, na.action = na.pass)
lasso_imp

#LASSO SUL TRAIN_NORM 
set.seed(1)
ctrl_norm =trainControl(method="cv", number = 10, classProbs = T,
                        summaryFunction=twoClassSummary)
grid_norm = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.01))
lasso_norm=train(user_rating~.,
                 data=train_norm,method = "glmnet",
                 trControl = ctrl_norm, tuneLength=5, na.action = na.pass,
                 tuneGrid=grid_norm)
lasso_norm
plot(lasso_norm)
confusionMatrix(lasso_norm)

#LASSO SUL TRAIN_NORM SENZA VARIABILI NON IMPORTANTI (train_norm_imp) (NON SERVE)
set.seed(1)
ctrl_norm_imp =trainControl(method="cv", number = 10, classProbs = T,
                            summaryFunction=twoClassSummary)
grid_norm_imp = expand.grid(.alpha=1,.lambda=seq(0, 1, by = 0.01))
lasso_norm_imp=train(user_rating~.,
                     data=train_norm_imp,method = "glmnet",
                     trControl = ctrl_norm_imp, tuneLength=5, na.action = na.pass,
                     tuneGrid=grid_norm_imp)
lasso_norm_imp
plot(lasso_norm_imp)
confusionMatrix(lasso_norm_imp)

#BINARIZZARE LA VARIABILE user_rating in test_stand PER LA ROC
sogliastd2 <- (4.25-mean(test$user_rating))/sd(test$user_rating)
test_stand$user_rating=ifelse(test_stand$user_rating>=sogliastd2, "c1", "c0")
table(test_stand$user_rating)
# c0  c1 
# 634 726 

#BINARIZZARE LA VARIABILE user_rating in test_norm PER LA ROC
summary(test_norm) #Median :0.875
test_norm$user_rating=ifelse(test_norm$user_rating>=0.875, "c1", "c0")
table(test_norm$user_rating)
# c0  c1 
# 634 726 

#BINARIZZARE LA VARIABILE user_rating in test PER LA ROC
summary(test) #Median :0.875
test$user_rating=ifelse(test$user_rating>4, "c1", "c0")
table(test$user_rating)
# c0  c1 
# 634 726 

library(ROCR)
library(pROC)
library(funModeling)
library(dplyr)
library(psych)
library(mice)
library(caret)
library(Hmisc)
library(tidyr)
library(ggplot2)

#ROC FINALE 
#choose second column of probs: event c1
knn_pred=test_stand$knn_no_out=predict(knn_no_out,newdata=test_stand, type="prob")[,2]
glm_pred=test_stand$glm_no_out=predict(glm_no_out,newdata=test_stand, type="prob")[,2]
lasso_pred=test$lasso=predict(lasso,newdata=test, type="prob")[,2]
naivebayes_pred=test$naivebayes=predict(naivebayes,newdata=test, type="prob")[,2]
#test$pls=predict(pls,test, "prob")[,2]
tree_pred=test_stand$tree_no_out=predict(tree_no_out,newdata=test_stand, type="prob")[,2]
nnet_pred=test_norm$nnet_norm_imp=predict(nnet_norm_imp,newdata=test_norm, type="prob")[,2]
gb_pred=test$gb=predict(gb,newdata=test, type="prob")[,2]
rf_pred=test$rf=predict(rf,newdata=test, type="prob")[,2]

pred_ROC_knn = prediction(knn_pred, test_stand$user_rating)
plot(performance(pred_ROC_knn, "tpr", "fpr"))
abline(0, 1, lty = 2)
pred_ROC_glm = prediction(glm_pred, test_stand$user_rating)
plot(performance(pred_ROC_glm, "tpr", "fpr"))
abline(0, 1, lty = 2)
pred_ROC_lasso = prediction(lasso_pred, test$user_rating)
plot(performance(pred_ROC_lasso, "tpr", "fpr"))
abline(0, 1, lty = 2)
pred_ROC_naivebayes = prediction(naivebayes_pred, test$user_rating)
plot(performance(pred_ROC_naivebayes, "tpr", "fpr"))
abline(0, 1, lty = 2)
pred_ROC_tree = prediction(tree_pred, test_stand$user_rating)
plot(performance(pred_ROC_tree, "tpr", "fpr"))
abline(0, 1, lty = 2)
pred_ROC_nnet = prediction(nnet_pred, test_norm$user_rating)
plot(performance(pred_ROC_nnet, "tpr", "fpr"))
abline(0, 1, lty = 2)
pred_ROC_gb = prediction(gb_pred, test$user_rating)
plot(performance(pred_ROC_gb, "tpr", "fpr"))
abline(0, 1, lty = 2)
pred_ROC_rf = prediction(rf_pred, test$user_rating)
plot(performance(pred_ROC_rf, "tpr", "fpr"))
abline(0, 1, lty = 2)

library(ROCR)
title("CURVE ROC")
plot(performance(pred_ROC_knn, "tpr", "fpr"),add=TRUE,col="YELLOW")
plot(performance(pred_ROC_glm, "tpr", "fpr"),add=TRUE,col="GREEN")
plot(performance(pred_ROC_lasso, "tpr", "fpr"),add=TRUE,col="BLUE")
plot(performance(pred_ROC_naivebayes, "tpr", "fpr"),add=TRUE,col="PINK")
plot(performance(pred_ROC_tree, "tpr", "fpr"),add=TRUE,col="RED")
plot(performance(pred_ROC_nnet, "tpr", "fpr"),add=TRUE,col="CYAN")
plot(performance(pred_ROC_gb, "tpr", "fpr"),add=TRUE,col="PURPLE")
plot(performance(pred_ROC_rf, "tpr", "fpr"),add=TRUE,col="BLACK")
legend("bottomright", cex=0.5, c("Logistic","KNN","Neural Network", "Tree", "Random Forest", 
                        "Lasso", "Gradient Boosting","Naive Bayes"), lty=1, 
       col = c("GREEN", "YELLOW", "CYAN", "RED","BLACK","BLUE","PURPLE","PINK"))


#SENSITIVITY,SPECIFICITY E SOGLIA
# cambiamo nomi per riusare un codice dove il target era M ed R
df=test
df$Class=test$user_rating
head(df)
df$Class=ifelse(df$Class=="c1","M","R") # il nostro event c1 ? ora M #PROVARE A FARLO CON C1 E C0 PER RESTARE COERENTI CON IL TRAINING
head(df)
df$ProbM=test$gb
head(df)

library(dplyr)
# for each threshold, find tp, tn, fp, fn and the sens=prop_true_M, spec=prop_true_R, precision=tp/(tp+fp)

thresholds <- seq(from = 0, to = 1, by = 0.01)
prop_table <- data.frame(threshold = thresholds, prop_true_M = NA,  prop_true_R = NA, true_M = NA,  true_R = NA ,fn_M=NA)

for (threshold in thresholds) {
  pred <- ifelse(df$ProbM > threshold, "M", "R")  # be careful here!!!
  pred_t <- ifelse(pred == df$Class, TRUE, FALSE)
  
  group <- data.frame(df, "pred" = pred_t) %>%
    group_by(Class, pred) %>%
    dplyr::summarise(n = n())
  
  group_M <- filter(group, Class == "M")
  
  true_M=sum(filter(group_M, pred == TRUE)$n)
  prop_M <- sum(filter(group_M, pred == TRUE)$n) / sum(group_M$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_M"] <- prop_M
  prop_table[prop_table$threshold == threshold, "true_M"] <- true_M
  
  fn_M=sum(filter(group_M, pred == FALSE)$n)
  # true M predicted as R
  prop_table[prop_table$threshold == threshold, "fn_M"] <- fn_M
  
  
  group_R <- filter(group, Class == "R")
  
  true_R=sum(filter(group_R, pred == TRUE)$n)
  prop_R <- sum(filter(group_R, pred == TRUE)$n) / sum(group_R$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_R"] <- prop_R
  prop_table[prop_table$threshold == threshold, "true_R"] <- true_R
  
}

head(prop_table, n=10)

# prop_true_M = sensitivity
# prop_true_R = specificicy

######################################################################
#  the program can be re-used only changing:
#   dataset=df,   target=Class, target modalities, M, R
######################################################################


# now think to your best cell in the matrix ad decide the metric of interest##########
##########
#pred	
#true	M	   R
#M  	 TP	  FN
#R	   FP	  TN
##########

# calculate other missing measures

# n of observations of the validation set    
prop_table$n=nrow(df)

# false positive (fp_M) by difference of   n and            tn,                 tp,         fn, 
prop_table$fp_M=nrow(df)-prop_table$true_R-prop_table$true_M-prop_table$fn_M

# find accuracy
prop_table$acc=(prop_table$true_R+prop_table$true_M)/nrow(df)

# find precision
prop_table$prec_M=prop_table$true_M/(prop_table$true_M+prop_table$fp_M)

# find F1 =2*(prec*sens)/(prec+sens)
# prop_true_M = sensitivity

prop_table$F1=2*(prop_table$prop_true_M*prop_table$prec_M)/(prop_table$prop_true_M+prop_table$prec_M)

# verify not having NA metrics at start or end of data 
tail(prop_table)
head(prop_table)
# we have typically some NA in the precision and F1 at the boundary..put,impute 1,0 respectively 

library(Hmisc)
#impute NA as 0, this occurs typically for precision
prop_table$prec_M=impute(prop_table$prec_M, 1)
prop_table$F1=impute(prop_table$F1, 0)
tail(prop_table)
colnames(prop_table)
# drop counts, PLOT only metrics
prop_table2 = prop_table[,-c(4:8)] 
head(prop_table2)

# plot measures vs soglia
# before we must impile data vertically: one block for each measure
library(dplyr)
library(tidyr)
gathered=prop_table2 %>%
  gather(x, y, prop_true_M:prop_true_R)
head(gathered)
#da f1 guardiamo che la soglia è 0.696069

##########################################################################
# grafico con tutte le misure 
library(ggplot2)
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "specificity",
       color = "Buono: event\nNon buono: nonevent")

# zoom
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "specificity",
       color = "Buono: event\n Non buono: nonevent") +
  coord_cartesian(xlim = c(0.4, 0.6))

#MATRICE DI CONFUSIONE SU TEST_STAND
library(gmodels)
pred <- ifelse(test$gb> 0.696069, "c1", "c0")
table(actual=test$user_rating,pred)

CrossTable(test$user_rating, pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#SCORE 
require(descr)
gb_score <- predict(gb, score)
gb_score #tutti i valori previsti user_rating da parte del modello
score$score_previsto=gb_score

summary(score) #median >4
score$user_rating=ifelse(score$user_rating>4, "c1", "c0")
table(score$user_rating)
# c0  c1 
# 235 269 
table(score$score_previsto)
# c0  c1 
# 234 270 

#MATRICE DI CONFUSIONE
score$user_rating=as.factor(score$user_rating)
CrossTable(score$user_rating, gb_score,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


