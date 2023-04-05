set.seed(1680) # for reproducibility
library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(philentropy)
library(BBmisc)
library(factoextra)

### UNSUPERVISED LEARNING ###
data = read.csv("heart_data.csv")
rf = read.csv("rf.csv")
clean = read.csv("clean.csv")
trainFS1 = read.csv("trainFS1.csv")
###################trasforma in factors#################
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

## hierarchical clustering for mixed data ############
## normalize numerical data: CLEAN DF
num = clean[c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")]
head(num)
norm = normalize(num, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
summary(norm)
hc.c = select(clean, subset = -c("Age", "RestingBP", "Cholesterol", "Oldpeak", "MaxHR"))
hc.c = cbind(hc.c, norm)
head(hc.c)

## computing the gower's distance #####

gower_dist <- daisy(hc.c,
                    metric = "gower",
                    type = list(logratio = 3))

# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# Output most similar pair
clean[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair
clean[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

## building the cluster ###
hc <- hclust(gower_dist, method = "complete")
hc.av <- hclust(gower_dist, method = "average")
plot(hc)
plot(hc.av)
plot(hc.av, abline(h=15, col="red", xlab = "The clusters have been created using \"complete\" linkage method"))
groups <- cutree(hc, k=2)
groups.av <- cutree(hc.av, k=3)
group <- cutree(hc, k = 2)
group4 <- cutree(hc, k  = 4)
rect.hclust(hc, k=3, border="red") 
table(groups, hc.c$HeartDisease)

clusplot(hc.c, groups, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=1, lines=0)
clusplot(hc.c, groups.av, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
clusplot(hc.c, group, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

## normalize numerical data: TRAINFS1
num.t = trainFS1[c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")]
head(num)
norm.t = normalize(num.t, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
summary(norm.t)
hc.t = select(trainFS1, subset = -c("Age", "RestingBP", "Cholesterol", "Oldpeak", "MaxHR"))
hc.t = cbind(hc.t, norm.t)
head(hc.t)

## computing the gower's distance #####
gower_dist_t <- daisy(hc.t,
                    metric = "gower",
                    type = list(logratio = 3))
summary(gower_dist_t)
gower_mat_t <- as.matrix(gower_dist_t)

trainFS1[
  which(gower_mat_t == min(gower_mat_t[gower_mat_t != min(gower_mat_t)]),
        arr.ind = TRUE)[1, ], ]


trainFS1[
  which(gower_mat_t == max(gower_mat_t[gower_mat_t != max(gower_mat_t)]),
        arr.ind = TRUE)[1, ], ]
## building the cluster 
hc_t <- hclust(gower_dist_t, method = "complete")
hc_t
hc_t.av <- hclust(gower_dist_t, method = "average")
plot(hc_t) 
plot(hc_t.av)
group_t <- cutree(hc_t, k = 2)

group_t.av = cutree(hc_t.av, k = 2)
rect.hclust(hc_t.av, k=2, border="red")


clusplot(hc.t, group_t, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=1, lines=0)

table(group_t.av, hc.t$HeartDisease)

c = cbind(hc.t, group_t)
head(c)
which(c$group_t == 1)
c_1 = c[which(group_t == 1),]
head(c_1,20)
tail(c_1,10)


x = cbind(hc.c, groups)
head(x)
x_1 = c[which(groups == 1),]
sum(c_1$FastingBS == "1")
head(x_1, 20)
tail(x_1, 10)

summary(c_1)
summary(x_1)
