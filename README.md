# Supervised-and-Unsupervised-Learning
The project is based on the popular "Heart" dataset from the UCI Machine Learning Repository.
The aim of the project is to showcase the main usefult steps to carry out a statistical analysis. 
For this purpose, I've focused on a dataset provided by the University of California, containing several qualitative and quantitative variables relative to 918 patients, with the aim of predicting heart diseases.
To do so, I've first conducted an extensive data pre-processing, which resulted in a final training set where the missing values of \textbf{Cholesterol} have been replaced using the "random forest" algorithm, and where the classes of \textbf{Sex} and \textbf{FastingBS} were re-balanced. 
Then, I've applied three supervised algorithms, such as decision trees, random trees, and bagging to provide predictions on the outcome variable \textbf{HeartDisease}.
For each, I've provided few performance metrics, such as accuracy, precision, recall, sensitivity and sensibility to evaluate their performance. \\ The results show that models trained on balanced sets obtain improved predictive accuracy, and overall better performances, even when tested on unbalanced data.\\ Moreover, decision trees and random forests highlight the importance of the variables ST-Slope and ChestPainType in detecting the presence of heart diseases.\\
Finally, I've applied hierarchical clustering on the unbalanced and the balanced datasets, obtaing for both two optimal number of clusters.
