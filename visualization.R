library(ggplot2)
library(caret)
library(lattice)
library(beeswarm)
library(ggcorrplot)
library(extrafont)
library(patchwork)
library(dplyr)          # for data manipulation
library(tidyr)          # for data manipulation
library(magrittr) 
library(hrbrthemes)
library(viridis)
library(graphics)
suppressPackageStartupMessages(library(dendextend))

### VISUALIZATION ###
dataO = read.csv("heart.csv")
data = read.csv("heart_data.csv")
rf = read.csv("rf.csv")
clean = read.csv("clean.csv")
trainFS1 = read.csv("trainFS1.csv")
###################trasforma in factors#################
dataO$FastingBS <- as.factor(dataO$FastingBS)
dataO$ST_Slope <- factor(dataO$ST_Slope, levels = c("Up", "Flat", "Down"))
dataO$HeartDisease <- as.factor(dataO$HeartDisease)
dataO$ExerciseAngina <- as.factor(dataO$ExerciseAngina)
dataO$Sex <- as.factor(dataO$Sex)
dataO$ChestPainType <- factor(dataO$ChestPainType, levels = c("ATA", "NAP", "ASY", "TA"))
dataO$RestingECG <- factor(dataO$RestingECG, levels = c("Normal", "ST", "LVH"))

data$FastingBS <- as.factor(data$FastingBS)
data$ST_Slope <- factor(data$ST_Slope, levels = c("Up", "Flat", "Down"))
data$HeartDisease <- as.factor(data$HeartDisease)
data$ExerciseAngina <- as.factor(data$ExerciseAngina)
data$Sex <- as.factor(data$Sex)
data$ChestPainType <- factor(data$ChestPainType, levels = c("ATA", "NAP", "ASY", "TA"))
data$RestingECG <- factor(data$RestingECG, levels = c("Normal", "ST", "LVH"))

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

## scatter plots ##########
age.hd = beeswarm(Age ~ HeartDisease, data = clean,
         pch = 19, 
         col = c("#B8B7ED", "#77C3EC"))
age.hd = beeswarm(Age ~ HeartDisease, data = dataO,
                  pch = 19, 
                  col = c("#B8B7ED", "#77C3EC"), corral = "random")

maxhr.hd = beeswarm(MaxHR ~ HeartDisease, data = clean,
                    pch = 19, 
                    col = c("#F37777", "#1989AC"), corral = "random")
maxhr.hd
maxhr.hd = beeswarm(MaxHR ~ HeartDisease, data = dataO,
                    pch = 19, 
                    col = c("#F37777", "#1989AC"), corral = "random")

chol.hd = beeswarm(Cholesterol ~ HeartDisease, data = clean,
                   pch = 19, 
                   col = c("#52A849", "#f78c03"), corral = "random")

oldp.hd = beeswarm(Oldpeak ~ HeartDisease, data = clean,
                   pch = 19, 
                   col = c("#fd6165", "#fe96a0"), corral = "random")


## correlation matrix
df = clean[-12]
dmy <- dummyVars(" ~ .", data = df)
ds <- data.frame(predict(dmy, newdata = df))
View(ds)

corrm = ggcorrplot(cor(ds), title = "Correlation Matrix")
corrm

########## ORIGINAL DATA REPRESENTATION ###########
sum(unique(dataO$Age))
sum((dataO$Age >= 28 & dataO$Age < 40)) #80
sum((dataO$Age >= 40 & dataO$Age < 50)) #211
sum((dataO$Age >= 50 & dataO$Age < 60)) #374
sum((dataO$Age >= 60 & dataO$Age < 70)) #222
sum(((dataO$Age >= 60))) #253

head(dataO)
age.d <- ggplot(dataO,aes(x = Age)) +
  geom_histogram(aes(y = ..density..), bins = 30, colour = "lightblue", fill = "white") +
  geom_density(color = "#B8B7ED", fill="#B8B7ED", alpha=0.3) +
  ggtitle("Density and histogram distributions across patients") + xlab("Age")+ylab("Frequency")+ theme_bw() 
age.d
restingbp.d <- ggplot(dataO, aes(x = RestingBP))+
  geom_density(color = "#6aa84f", fill="#d9ead3", alpha=0.3) + xlab("Resting Blood Pressure")+ylab(NULL)+ theme_bw()

restingbp.d

chol.d <- ggplot(dataO,aes(x = Cholesterol)) +
  geom_histogram(aes(y = ..density..), bins = 20,, fill = "lightblue") + xlab("Total serum cholesterol levels")+ylab("Frequency")+ theme_bw()

chol.d

maxhr.d <- ggplot(dataO, aes(x = MaxHR))+ 
  geom_density(color = "green", fill="lightgreen", alpha=0.3) + xlab("Maximum heart rate")+ylab(NULL)+ theme_bw() 
maxhr.d

oldp.d <- ggplot(dataO,aes(x = Oldpeak)) +
  geom_density(color = "#B8B7ED", fill="#B8B7ED", alpha=0.3) + xlab("Exercise-induced levels of ST depression")+ ylab(NULL) +theme_bw()
oldp.d

## barplots
head(dataO)
sex.p <- ggplot(dataO, aes(x = Sex)) + geom_bar(aes(y = ..count..))
sex.p

cpt.p <- ggplot(dataO, aes(x = ChestPainType)) + geom_bar(aes(y = ..count..))
cpt.p

fBS.p <- ggplot(dataO, aes(x = FastingBS)) + geom_bar(aes(y = ..count..))
fBS.p

exan.p <- ggplot(dataO, aes(x = ExerciseAngina)) + geom_bar(aes(y = ..count..))
exan.p

slope.p <- ggplot(dataO, aes(x = ST_Slope)) + geom_bar(aes(y = ..count..))
slope.p

hd.p <- ggplot(dataO, aes(x = HeartDisease)) + geom_bar(aes(y = ..count..))
hd.p
meanhd1 = mean(dataO$HeartDisease==1)
sum(dataO$HeartDisease == 1)/sum(dataO$HeartDisease== 1 | dataO$HeartDisease == 0)
sum(dataO$HeartDisease== 1 | dataO$HeartDisease == 0)
## sex, chestpaintype, fastingBS, RestingECG, ExerciseAngina, ST_slope

## box plots ######
rf %>%
  ggplot(aes(x=NULL, y = Age, fill = HeartDisease)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")

rf %>%
  ggplot(aes(x=NULL, y = Age, fill = HeartDisease)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#999999", "#E69F00"))+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")

## box plots 
head(clean)
a.c = boxplot(clean$Age, ylab = "Years")
r.bp = boxplot(clean$RestingBP, ylab = "Resting Blood Pressure")
c.c = boxplot(clean$Cholesterol, ylab = "Cholesterol")
maxhr.c = boxplot(clean$MaxHR, ylab = "Maximum Heart Rate")
oldp.c = boxplot(clean$Oldpeak, ylab = "Oldpeak")
par(mfrow = c(2,3))
length(boxplot.stats(clean$Cholesterol)$out) #11
length(boxplot.stats(clean$RestingBP)$out) #27
length(boxplot.stats(clean$Oldpeak)$out) #15
length(boxplot.stats(data$Oldpeak)$out) #15

par(mfrow = c(2,3))
a = boxplot(rf$Age, ylab = "Years")
r = boxplot(rf$RestingBP, ylab = "Resting Blood Pressure")
c = boxplot(rf$Cholesterol, ylab = "Cholesterol")
maxhr = boxplot(rf$MaxHR, ylab = "Maximum Heart Rate")
oldp = boxplot(rf$Oldpeak, ylab = "Oldpeak")

## clusters
avg_dend_obj <- as.dendrogram(hc_t)
avg_col_dend <- color_branches(avg_dend_obj, k = 2)
plot(avg_col_dend)

avg_dend_obj <- as.dendrogram(hc_t.av)
avg_col_dend <- color_branches(avg_dend_obj, k = 2)
plot(avg_col_dend)

Esgower <- numeric(10)
for(i in 2:10){
  pames <- pam(gower_dist, diss = TRUE, k = i)
  Esgower[i] <- pames$silinfo$avg.width}
plot(1:10, Esgower, type = "b", ylab = "Silhouette", xlab = "Number of Clusters", main = "Optimal Number of Clusters", col = "blue") 
Esgower <- numeric(10)
for(i in 2:10){
  pames <- pam(gower_dist_t, diss = TRUE, k = i)
  Esgower[i] <- pames$silinfo$avg.width
plot(1:10, Esgower, type = "b", ylab = "Silhouette", xlab = "Number of Clusters", main = "Optimal Number of Clusters", col = "blue") 
