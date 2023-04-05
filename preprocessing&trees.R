library(pROC)
library(caret)
library(rpart)
library(ggplot2)
library(tidyverse)
library(mice)
library(imbalance)
library(randomForest)
#original data
dataO = read.csv("heart.csv")
data = read.csv("heart_data.csv")
clean = read.csv("clean.csv")
trainFS1 = read.csv("trainFS1.csv")
rf = read.csv("rf.csv") ##rf_1 is the same dataset

###############################detect null values####################
which(is.na(dataO)) #0
which(is.na(data)) #0
sum(is.na(data)) #0

################removing the dead person################
data <- data[-c(450),]

###################trasforma in factors#################
data$FastingBS <- as.factor(data$FastingBS)
#st_slope 1 = UP, 0 = DOWN
data$ST_Slope <- factor(data$ST_Slope, levels = c("Flat", "Up", "Down"))
#Heart disease 1 = HD, 0 = Normal
data$HeartDisease <- as.factor(ifelse(data$HeartDisease == "1", "HD", "Normal"))
data$HeartDisease <- as.factor(data$HeartDisease)
#exercise angina 1 = Yes, 0 = No
data$ExerciseAngina <- as.factor(ifelse(data$ExerciseAngina == "Y",1, 0))
data$ExerciseAngina <- as.factor(data$ExerciseAngina)
#sex
data$Sex <- as.factor(data$Sex)
#chestpain
data$ChestPainType <- factor(data$ChestPainType, levels = c("ATA", "NAP", "ASY", "TA"))
#restingecg
data$RestingECG <- factor(data$RestingECG, levels = c("Normal", "ST", "LVH"))
write.csv(data, file = "heart_data.csv", row.names = FALSE)

rf$FastingBS <- as.factor(rf$FastingBS)
rf$ST_Slope <- factor(rf$ST_Slope, levels = c("Up", "Flat", "Down"))
rf$HeartDisease <- as.factor(rf$HeartDisease)
rf$ExerciseAngina <- as.factor(rf$ExerciseAngina)
rf$Sex <- as.factor(rf$Sex)
rf$ChestPainType <- factor(rf$ChestPainType, levels = c("ATA", "NAP", "ASY", "TA"))
rf$RestingECG <- factor(rf$RestingECG, levels = c("Normal", "ST", "LVH"))

clean$FastingBS <- as.factor(clean$FastingBS)
clean$ST_Slope <- factor(clean$ST_Slope, levels = c("Up", "Flat", "Down"))
clean$HeartDisease <- as.factor(clean$HeartDisease)
clean$ExerciseAngina <- as.factor(clean$ExerciseAngina)
clean$Sex <- as.factor(clean$Sex)
clean$ChestPainType <- factor(clean$ChestPainType, levels = c("ATA", "NAP", "ASY", "TA"))
clean$RestingECG <- factor(clean$RestingECG, levels = c("Normal", "ST", "LVH"))

trainFS1$FastingBS <- as.factor(trainFS1$FastingBS)
trainFS1$ST_Slope <- factor(trainFS1$ST_Slope, levels = c("Up", "Flat", "Down"))
trainFS1$HeartDisease <- as.factor(trainFS1$HeartDisease)
trainFS1$ExerciseAngina <- as.factor(trainFS1$ExerciseAngina)
trainFS1$Sex <- as.factor(trainFS1$Sex)
trainFS1$ChestPainType <- factor(trainFS1$ChestPainType, levels = c("ATA", "NAP", "ASY", "TA"))
trainFS1$RestingECG <- factor(trainFS1$RestingECG, levels = c("Normal", "ST", "LVH"))

###################analisi degli outlier#############################
par(mfrow = c(2, 3))
boxplot(data$Age, main = "Age")
#resting blood pressure = 0 (1 occurance)
boxplot(data$RestingBP, main="RestingBP")
min(data$RestingBP)
max(data$RestingBP)
sum(data$RestingBP > 140) #220
min(boxplot.stats(data$RestingBP)$out)
mean(data$RestingBP > 180)
sum(data$RestingBP > 160) #43
sum(data$RestingBP == 200) #4
sum(data$RestingBP < 100) #12
boxplot.stats(clean$RestingBP)$out
length(boxplot.stats(clean$RestingBP)$out) #
length(boxplot.stats(clean$RestingBP)$out)/length(clean$RestingBP) #0.0300
#oldpeak
boxplot(data$Oldpeak, main="Oldpeak")
#maxhr
boxplot(data$MaxHR, main="MaxHR") # non ha valori particolarmente outlier
boxplot.stats(data$MaxHR)$out
sum(data$MaxHR > 63 & data$MaxHR < 70)

#############################colesterolo elemento disturbante####################
mean(data$Cholesterol == 0) #0.1864776 -> 0.19
sum(data$Cholesterol == 0) #171
sum(data$Cholesterol < 43) #171
sum(data$Cholesterol > 43) #746
sum(data$Cholesterol > 200) #596
sum(data$Cholesterol < 200) #317
sum(data$Cholesterol > 300) #103
sum(clean$Cholesterol > 300) #109
sum(data$Cholesterol > 400) #13
mean(clean$Cholesterol> 200) #0.8
sum(trainFS1$Cholesterol > 300) #1
max(clean$Cholesterol) #394
summary(data$Cholesterol)

set.seed(1)
data2 <- data
data2 <- data[!(data$Cholesterol == 0),]
View(data2)
summary(data2$Cholesterol)
###data3 con la mediana 
set.seed(1)
data3 <- data
data3$Cholesterol[data3$Cholesterol == 0] <- median(data3$Cholesterol)
summary(data3$Cholesterol)
###datapmm sostituire i valori con l'algoritmo pmm in Mice
#regress Cholesterol on the other variables
set.seed(1)
datapmm <- data
datapmm$Cholesterol[datapmm$Cholesterol == 0] <- NA
imp <- mice(datapmm, method = "pmm")
datapmm<- complete(imp)

min(datapmm$Cholesterol) #85
max(datapmm$Cholesterol) #603
median(datapmm$Cholesterol) #237
summary(datapmm$Cholesterol)
###try diff imputation methods
#using random forest
set.seed(1)
rf_1 <- data
rf_1$Cholesterol[rf_1$Cholesterol == 0] <- NA
sum(is.na(rf_1$Cholesterol)) #171
imp3 <- mice(rf_1, method = "rf")
rf_1<- complete(imp3)
head(rf_1)
summary(rf_1$Cholesterol)
#############now visualize the different imputations of Cholesterol == 0#############à

#all imputations
ggplot() +
  geom_density(data = data.frame(cholesterol = data$Cholesterol), aes(x = data$Cholesterol, y = after_stat(density)), color = "black", fill = "grey", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = datapmm$Cholesterol), aes(x = datapmm$Cholesterol, y = after_stat(density)), color = "blue", fill = "lightblue", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = data3$Cholesterol), aes(x = data3$Cholesterol, y = after_stat(density)), color = "red", fill = "pink", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = rf$Cholesterol), aes(x = rf$Cholesterol, y = after_stat(density)), color = "green", fill = "lightgreen", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = data2$Cholesterol), aes(x = data2$Cholesterol, y = after_stat(density)), color = "purple", fill = "#E6E6FA", alpha = 0.3) +
  xlab("Cholesterol") +
  ylab("Density") +
  ggtitle("Comparison of Different Imputation Methods") +
  scale_fill_manual(values = c("gray80", "lightblue", "pink", "lightgreen", "#E6E6FA")) +
  scale_color_manual(values = c("black", "blue", "red", "green", "purple")) + theme_bw() + ylab(NULL)+
  theme(plot.title = element_text(hjust = 0, size = 15, margin = margin(b = 10)), text = element_text(family = "Georgia", color = "grey7"),
        axis.ticks = element_blank(), axis.text.y = element_text(color = "grey70"), axis.title.y = element_text(margin = margin(r = 20), color = "grey7"),
        axis.title.x = element_text(margin = margin(t = 20), color = "grey7"), axis.text.x = element_text(color = "grey7"), axis.line = element_line(colour = "grey70"))


#removing the original distribution of the observations + the median imputation
ggplot() +
  geom_density(data = data.frame(cholesterol = datapmm$Cholesterol), aes(x = datapmm$Cholesterol, y = after_stat(density)), color = "blue", fill = "lightblue", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = rf$Cholesterol), aes(x = rf$Cholesterol, y = after_stat(density)), color = "green", fill = "lightgreen", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = data2$Cholesterol), aes(x = data2$Cholesterol, y = after_stat(density)), color = "purple", fill = "#E6E6FA", alpha = 0.3) +
  xlab("Cholesterol") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "#E6E6FA")) +
  scale_color_manual(values = c("blue", "green", "purple"))+ theme_bw() + ylab(NULL)+
  theme(plot.title = element_text(hjust = 0, size = 15, margin = margin(b = 10)), text = element_text(family = "Georgia", color = "grey7"),
        axis.ticks = element_blank(), axis.text.y = element_text(color = "grey70"), axis.title.y = element_text(margin = margin(r = 20), color = "grey7"),
        axis.title.x = element_text(margin = margin(t = 20), color = "grey7"), axis.text.x = element_text(color = "grey7"), axis.line = element_line(colour = "grey70"))

#I prefer using random forest for imputation of data, rather than pmm or median
###########analisi outlier colesterolo###############
min(data$Cholesterol) #0
max(data$Cholesterol) #603
median(data$Cholesterol) #223
boxplot(datapmm$Cholesterol)
boxplot(rf$Cholesterol)

###############analisi dei valori#############
min(rf$Cholesterol) #85
max(rf$Cholesterol) #603
median(rf$Cholesterol) #236
mean(rf$Cholesterol) #245.3152
sd(rf$Cholesterol) #60.72569
c = boxplot(rf$Cholesterol, ylab = "Cholesterol levels") #tanti outlier
length(boxplot.stats(rf$Cholesterol)$out) #29
a = boxplot(rf$Age, ylab = "Age")
rb = boxplot(rf$RestingBP, ylab = "Resting Blood Pressure") 
length(boxplot.stats(rf$RestingBP)$out) #27
op = boxplot(rf$Oldpeak, ylab = "Old Peak")


###############removing the outliers###############
set.seed(1)
quartiles <- quantile(rf$Cholesterol, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(rf$Cholesterol)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
rf_clean <- rf
rf_clean <- rf_clean[!(rf_clean$Cholesterol < Lower | rf_clean$Cholesterol > Upper),]
View(rf_clean)
min(rf_clean$Cholesterol) #110
max(rf_clean$Cholesterol) #369
median(rf_clean$Cholesterol) #234
plot(density(rf_clean$Cholesterol), main = "Cholesterol")

## 2 attempt
set.seed(1)
quartiles1 <- quantile(rf$Cholesterol, probs=c(.15, .85), na.rm = FALSE)
IQR1 <- IQR(rf$Cholesterol)
Lower1 <- quartiles1[1] - 1.5*IQR1
Upper1 <- quartiles1[2] + 1.5*IQR1

rf_clean1 <- rf
rf_clean1 <- rf_clean1[!(rf_clean1$Cholesterol < Lower1 | rf_clean1$Cholesterol > Upper1),]
View(rf_clean1)
min(rf_clean1$Cholesterol) #100
max(rf_clean1$Cholesterol) #394
median(rf_clean1$Cholesterol) #236
plot(density(rf_clean1$Cholesterol), main = "Cholesterol")
summary(rf_clean1)
rf_clean1
### 3 attempt
set.seed(1)
quartiles2 <- quantile(rf$Cholesterol, probs=c(.20, .80), na.rm = FALSE)
IQR2 <- IQR(rf$Cholesterol)
Lower2 <- quartiles2[1] - 1.5*IQR2
Upper2 <- quartiles2[2] + 1.5*IQR2

rf_clean2 <- rf
rf_clean2 <- rf_clean2[!(rf_clean2$Cholesterol < Lower2 | rf_clean2$Cholesterol > Upper2),]
View(rf_clean2)
min(rf_clean2$Cholesterol) #100
max(rf_clean2$Cholesterol) #384
median(rf_clean2$Cholesterol) #236
plot(density(rf_clean2$Cholesterol), main = "Cholesterol")

############comparison of the overall densities###############
ggplot() +
  geom_density(data = data.frame(cholesterol = data$Cholesterol), aes(x = data$Cholesterol, y = after_stat(density)), color = "black", fill = "grey", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = rf_clean$Cholesterol), aes(x = rf_clean$Cholesterol, y = after_stat(density)), color = "blue", fill = "lightblue", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = data3$Cholesterol), aes(x = data3$Cholesterol, y = after_stat(density)), color = "red", fill = "pink", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = rf$Cholesterol), aes(x = rf$Cholesterol, y = after_stat(density)), color = "green", fill = "lightgreen", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = data2$Cholesterol), aes(x = data2$Cholesterol, y = after_stat(density)), color = "purple", fill = "#E6E6FA", alpha = 0.3) +
  xlab("Cholesterol") +
  ylab("Density") +
  ggtitle("Comparison of Different Imputation Methods") +
  scale_fill_manual(values = c("grey","lightblue","pink", "lightgreen", "#E6E6FA")) +
  scale_color_manual(values = c("black", "blue", "red", "green", "purple")) +
  theme_classic()

ggplot() +
  geom_density(data = data.frame(cholesterol = data$Cholesterol), aes(x = data$Cholesterol, y = after_stat(density)), color = "black", fill = "grey", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = rf_clean$Cholesterol), aes(x = rf_clean$Cholesterol, y = after_stat(density)), color = "blue", fill = "lightblue", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = rf$Cholesterol), aes(x = rf$Cholesterol, y = after_stat(density)), color = "green", fill = "lightgreen", alpha = 0.3) +
  xlab("Cholesterol") +
  ylab("Density") +
  ggtitle("Comparison of Different Imputation Methods") +
  scale_fill_manual(values = c("grey","lightblue", "lightgreen")) +
  scale_color_manual(values = c("black", "blue", "green")) +
  theme_classic()


ggplot() +
  geom_density(data = data.frame(cholesterol = data$Cholesterol), aes(x = data$Cholesterol, y = after_stat(density)), color = "black", fill = "grey", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = rf_clean$Cholesterol), aes(x = rf_clean$Cholesterol, y = after_stat(density)), color = "blue", fill = "lightblue", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = rf$Cholesterol), aes(x = rf$Cholesterol, y = after_stat(density)), color = "green", fill = "lightgreen", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = rf_clean1$Cholesterol), aes(x = rf_clean1$Cholesterol, y = after_stat(density)), color = "red", fill = "pink", alpha = 0.3) +
  geom_density(data = data.frame(cholesterol = rf_clean2$Cholesterol), aes(x = rf_clean2$Cholesterol, y = after_stat(density)), color = "purple", fill = "#E6E6FA", alpha = 0.3) +
  xlab("Cholesterol") +
  ylab("Density") +
  ggtitle("Comparison of Different Imputation Methods") +
  scale_fill_manual(values = c("grey","lightblue", "lightgreen", "pink", "#E6E6FA")) +
  scale_color_manual(values = c("black", "blue", "green", "red", "purple")) +
  theme_classic()

#fasting blood sugar (quantità di sugar nel sangue dopo una notte di fasting)
# [1: if FastingBS > 120 mg/dl, 0: otherwise]

#max hr: maximum heart rate achieved (between 60 and 202)
max(data$MaxHR) #202
min(data$MaxHR) #60

#Oldpeak: oldpeak = ST [Numeric value measured in depression]
min(data$Oldpeak) #-2.6
max(data$Oldpeak) #6.2

#st_slope
ggplot(rf) + geom_bar(aes(x = ST_Slope), color = "darkgreen", fill="green", alpha=0.3) +
  labs(y = "", x = "ST Slope") 

maxhr = boxplot(rf$MaxHR, ylab = "Maximum Heart Rate", add = FALSE)

########### BUILDING THE TREES ################################
set.seed(123)
set.seed(42)
set.seed(1)
###rf
trainIndex <- createDataPartition(rf$HeartDisease, p = 0.8, list = FALSE)
training <- rf[trainIndex, ]
testing <- rf[-trainIndex, ]
train_tree_rf<-rpart(HeartDisease~. -HeartDisease, data=training)
treerf = rpart.plot::prp(train_tree_rf, digits = 4)
x_test<-testing[, 1:11]
y_test<-testing[,12]
test_test_rf <- predict(train_tree_rf, x_test, type = "class")
cf <- confusionMatrix(data=factor(test_test_rf), reference = y_test)
cf
## roc curves ##
y_test = ifelse(y_test == "HD", 0, 1)
test_test_rf = ifelse(test_test_rf == "HD", 1, 0)
roc.rf = roc(y_test, test_test_rf)
plot.roc(roc.rf)

#test accuracy set.seed(123) = 0.765
#test accuracy set.seed(42) = 0.8743
#test accuracy set.seed(1) = 0.8579


#### dataset rf_clean
set.seed(1)
trainIndex_clean <- createDataPartition(rf_clean$HeartDisease, p = 0.8, list = FALSE)
training_clean <- rf_clean[trainIndex_clean, ]
testing_clean <- rf_clean[-trainIndex_clean, ]
train_tree_rf_clean<-rpart(HeartDisease~. -HeartDisease, data=training_clean)
tree_clean = rpart.plot::prp(train_tree_rf_clean, digits = 4)
x_test_clean<-testing_clean[, 1:11]
y_test_clean<-testing_clean[,12]
test_test_rf_clean <- predict(train_tree_rf_clean, x_test_clean, type = "class")
cf_clean = confusionMatrix(test_test_rf_clean, as.factor(y_test_clean)) 
cf_clean 

##test accuracy set.seed(123) = 0.8125
#test accuracy set.seed(42) = 0.8407
#test accuracy set.seed(42) = 0.8182
#test accuracy set.seed(42) = 0.8531

y_test_clean = ifelse(y_test_clean == "HD", 0, 1)
test_test_rf_clean = ifelse(test_test_rf_clean == "HD", 1, 0)
roc.rf_c = roc(y_test_clean, test_test_rf_clean, add = TRUE)
plot.roc(roc.rf_c, add = FALSE, col = "green")

### dataset rf_clean1
set.seed(1)
trainIndex_clean1 <- createDataPartition(rf_clean1$HeartDisease, p = 0.8, list = FALSE)
training_clean1 <- rf_clean1[trainIndex_clean1, ]
testing_clean1 <- rf_clean1[-trainIndex_clean1, ]
train_tree_rf_clean1<-rpart(HeartDisease~. -HeartDisease, data=training_clean1)
tree_clean1 = rpart.plot::prp(train_tree_rf_clean1, digits = 4)
x_test_clean1<-testing_clean1[, 1:11]
y_test_clean1<-testing_clean1[,12]
test_test_rf_clean1 <- predict(train_tree_rf_clean1, x_test_clean1, type = "class")
cf_clean1 = confusionMatrix(test_test_rf_clean1, as.factor(y_test_clean1)) ## accuracy 0.8962
cf_clean1 

##test accuracy set.seed(123) = 0.7865
##test accuracy set.seed(42) = 0.8187
##test accuracy set.seed(1) = 0.8436

## roc curve ###
y_test_clean1 = ifelse(y_test_clean1 == "HD", 1, 0)
test_test_rf_clean1 = ifelse(test_test_rf_clean1 == "HD", 1, 0)
roc.rf_c1 = roc(y_test_clean1, test_test_rf_clean1)
plot.roc(roc.rf_c1, add = TRUE, col = "red")

## dataset rf_clean2
set.seed(1)
trainIndex_clean2 <- createDataPartition(rf_clean2$HeartDisease, p = 0.8, list = FALSE)
training_clean2 <- rf_clean2[trainIndex_clean2, ]
testing_clean2 <- rf_clean2[-trainIndex_clean2, ]
train_tree_rf_clean2<-rpart(HeartDisease~. -HeartDisease, data=training_clean2)
tree_clean2 = rpart.plot::prp(train_tree_rf_clean2, digits = 4)
x_test_clean2<-testing_clean2[, 1:11]
y_test_clean2<-testing_clean2[,12]
test_test_rf_clean2 <- predict(train_tree_rf_clean2, x_test_clean2, type = "class")
cf_clean2 = confusionMatrix(test_test_rf_clean2, as.factor(y_test_clean2)) ## accuracy 0.8962
cf_clean2 

## roc curve ###
y_test_clean12 = ifelse(y_test_clean2 == "HD", 1, 0)
test_test_rf_clean2 = ifelse(test_test_rf_clean2 == "HD", 1, 0)
roc.rf_c2 = roc(y_test_clean2, test_test_rf_clean2, add = TRUE)
plot.roc(roc.rf_c2, add = TRUE, col = "purple")

##test accuracy set.seed(123) = 0.8371
##test accuracy set.seed(42) = 0.8077
# test accuracy set.seed(1) = 0.8579

########### class imbalance ################
head(data)
### Sex ### IMBALANCED
barchart(data$Sex) 
mean(data$Sex == "M") #0.789
mean(data$Sex == "F") #0.21
length(which(data$Sex=="F")) #193
length(which(data$Sex=="M")) #724
### Chest Pain Type ### IMBALANCED
barchart(data$ChestPainType)
mean(data$ChestPainType== "ASY") #0.5409
mean(data$ChestPainType== "ATA") #0.1886587
mean(data$ChestPainType== "TA") # 0.05016358
mean(data$ChestPainType== "NAP") # 0.2202835
## Fasting BS ## IMBALANCED
barchart(data$FastingBS)
mean(data$FastingBS == 0) #0.7666303
length(which(rf$FastingBS == 0)) #703

## ST_Slope ##
barchart(data$ST_Slope)
mean(data$ST_Slope== "Up") #0.4307525
mean(data$ST_Slope== "Flat") #0.5005453
mean(data$ST_Slope== "Down") #0.06870229
## Resting ECG ##
barchart(data$RestingECG)
mean(data$RestingECG == "Normal") #0.6008724
mean(data$RestingECG == "LVH") #0.2050164
mean(data$RestingECG == "ST") #0.1941112
### Exercise Angina ###
barchart(data$ExerciseAngina)
mean(data$ExerciseAngina==0) #0.5954198

### le variabili che risultano maggiormente imbalanced sono SEX, CHEST PAIN TYPE, FASTING ###
## since our classes present imbalance, we can use the "oversampling" method for fixing the problem
#Compute imbalance ratio of a binary dataset
imbalanceRatio(rf_clean, classAttr = "Sex") #0.266
imbalanceRatio(rf_clean, classAttr = "FastingBS") #0.3044

set.seed(1)
training = downSample(rf, rf$Sex, list = FALSE)
training = training[-13]
mean(training$Sex=="F")
sum(training$Sex=="F")
sum(training$Sex == "M")

trainFS = upSample(training, training$FastingBS, list = FALSE)
trainFS = trainFS[-13]


#### from clean1
set.seed(1)
training1 = downSample(clean, clean$Sex, list = FALSE)
training1 = training1[-13]
mean(training1$Sex=="F")
sum(training1$Sex=="F")
sum(training1$Sex == "M")

trainFS1 = upSample(training1, training1$FastingBS, list = FALSE)
trainFS1 = trainFS1[-13]
summary(trainFS1)

### random forest and bagging ###
set.seed(1)
set.seed(123) #train = 0.80 of the total sample
set.seed(12) #train = 0.60 of the total sample
set.seed(13)
#test and train BAGGING
train <- sample (1: nrow (rf), nrow (rf)*0.80)
bag.heart <- randomForest(HeartDisease~ ., data = rf, subset = train, mtry = 11, importance = TRUE)
bag.heart

y.test <- rf[-train, "HeartDisease"]
yhat.bag <- predict (bag.heart , newdata = rf[-train , ])
plot(yhat.bag, y.test)
confusionMatrix(yhat.bag, y.test)
confusionMatrix(yhat.bag, y.test, mode = "prec_recall")
#set.seed(12) 0.8256
#set.seed(13) 0.8696
#0.8478 

#test and train RANDOM FOREST
train1 <- sample (1: nrow (rf), nrow (rf)*0.80)
rt.heart <- randomForest(HeartDisease~ ., data = rf, subset = train1, importance = TRUE)
rt.heart
y.test.rt <- rf[-train1, "HeartDisease"]
yhat.rt <- predict (rt.heart , newdata = rf[-train1 , ])
plot(yhat.rt, y.test.rt)
confusionMatrix(yhat.rt, y.test.rt, mode = "prec_recall") #0.875
confusionMatrix(yhat.rt, y.test.rt) #0.875
#set.seed(12) 0.8229
#set.seed(13) 0.8696

y.test.rt = ifelse(y.test.rt == "HD", 1,0)
yhat.rt = ifelse(yhat.rt == "HD", 1,0)
roc.rff = roc(y.test.rt, yhat.rt)
plot.roc(roc.rff, col = "violet", add = FALSE, grid = TRUE)


#### RT E BAGGING SU RF_CLEAN CON ELIMINAZIONE DEGLI OUTLIER
set.seed(1)
train_clean <- sample (1: nrow (clean), nrow (clean)*0.80)
bag.heart.clean <- randomForest(HeartDisease~ ., data = clean, subset = train_clean, mtry = 11, importance = TRUE)
bag.heart.clean
y.test.clean <- clean[-train_clean, "HeartDisease"]
yhat.bag.clean <- predict (bag.heart.clean , newdata = clean[-train_clean , ])
plot(yhat.bag.clean, y.test.clean)
confusionMatrix(yhat.bag.clean, y.test.clean) 
confusionMatrix(yhat.bag.clean, y.test.clean, mode = "prec_recall") 

#set.seed(123) 0.8596
#set.seed(12) 0.8258
#set.seed(13) 0.8539

#### RT E BAGGING SU RF_CLEAN CON ELIMINAZIONE DEGLI OUTLIER
set.seed(1)
set.seed(3)
train_clean <- sample (1: nrow (clean), nrow (clean)*0.80)
length(train_clean)
rt.heart.clean <- randomForest(HeartDisease~ ., data = clean, subset = train_clean, importance = TRUE)
rt.heart.clean
y.test.rt.clean <- clean[-train_clean, "HeartDisease"]
yhat.rt.clean <- predict (rt.heart.clean , newdata = clean[-train_clean , ])
plot(yhat.rt.clean, y.test.rt.clean)
confusionMatrix(yhat.rt.clean, y.test.rt.clean, mode = "prec_recall") 
confusionMatrix(yhat.rt.clean, y.test.rt.clean) 
#set.seed(123) 0.8708
#set.seed(12) 0.8596
#set.seed(13) 0.8596
#rf_clean1, set.seed(13) 0.9602

## roc curve ###
y.test.rt.clean = ifelse(y.test.rt.clean == "HD", 1, 0)
yhat.rt.clean = ifelse(yhat.rt.clean == "HD", 1, 0)
roc.rf_c1 = roc(y.test.rt.clean, yhat.rt.clean, add = TRUE)
plot.roc(roc.rf_c1, col = "green")

#trainFS1 -> bagging e random forests
set.seed(4)
rt.heartFS1 <- randomForest(HeartDisease~ ., data = trainFS1, importance = TRUE)
rt.heartFS1
y.test.rtFS1 <- rf[, "HeartDisease"]
yhat.rtFS1 <- predict (rt.heartFS1 , newdata = rf)
plot(yhat.rtFS1, y.test.rtFS1)
confusionMatrix(yhat.rtFS1, y.test.rtFS1) #
confusionMatrix(yhat.rtFS1, y.test.rtFS1, mode = "prec_recall") #
#set.seed(1)0.9166

set.seed(1)
rt.heartFS1B <- randomForest(HeartDisease~ ., data = trainFS1, importance = TRUE, mtry = 11)
rt.heartFS1B
y.test.rtFS1B <- rf[, "HeartDisease"]
yhat.rtFS1B <- predict (rt.heartFS1B , newdata = rf)
plot(yhat.rtFS1B, y.test.rtFS1B)
confusionMatrix(yhat.rtFS1B, y.test.rtFS1B) #
confusionMatrix(yhat.rtFS1B, y.test.rtFS1B, mode = "prec_recall") #


## roc curves for bagging
y.test = ifelse(y.test == "HD",1,0)
yhat.bag = ifelse(yhat.bag == "HD",1,0)
roc.rfb = roc(y.test, yhat.bag)
plot.roc(roc.rfb, col = "violet", add = FALSE)

y.test.clean = ifelse(y.test.clean == "HD", 1,0)
yhat.bag.clean = ifelse(yhat.bag.clean == "HD", 1,0)
roc.bc = roc(y.test.clean, yhat.bag.clean)
plot.roc(roc.bc, col = "lightblue", add = TRUE)

y.test.rtFS1B = ifelse(y.test.rtFS1B == "HD", 1,0)
yhat.rtFS1B = ifelse(yhat.rtFS1B == "HD", 1,0)
roc.bt = roc(y.test.rtFS1B, yhat.rtFS1B)
plot.roc(roc.bt, col = "lightgreen", add = TRUE, grid = TRUE)

## roc curves for random forests
y.test.rt = ifelse(y.test.rt == "HD", 1,0)
yhat.rt = ifelse(yhat.rt == "HD", 1,0)
roc.rff = roc(y.test.rt, yhat.rt)
plot.roc(roc.rff, col = "purple", add = FALSE, grid = TRUE)

y.test.rt.clean = ifelse(y.test.rt.clean == "HD", 1,0)
yhat.rt.clean = ifelse(yhat.rt.clean == "HD", 1,0)
roc.rtc = roc(y.test.rt.clean, yhat.rt.clean)
plot.roc(roc.rtc, col = "blue", add = TRUE)

y.test.rtFS1 = ifelse(y.test.rtFS1 == "HD", 1,0)
yhat.rtFS1 = ifelse(yhat.rtFS1 == "HD", 1,0)
roc.rfFS1 = roc(y.test.rtFS1, as.numeric(yhat.rtFS1))
plot.roc(roc.rfFS1, col = "darkgreen", add = TRUE, print.auc = TRUE)
