##clear your working directory
rm(list=ls())

##libraries
library(readxl)
library(cluster)
library(dplyr)

##### Agglomerative Clustering 


foods <-read.csv("/Users/AustinTheBoss/Documents/Documents - User’s MacBook Pro (2)/COSC 6520/Final Project/nutrition_contentlegacy.csv")


##z-score to scale the data (only numeric columns 3 through 12)
foods2 <- scale(foods[, 3:12])


##similarity measures
##"Euclidean" distances calculated first 
foodsE <- dist(foods2, method = "euclidean")


##utilize the agnes function to perform agglomerative clustering
ResultEuc <- agnes(foodsE, diss = TRUE, method = "ward")

##print the result
ResultEuc

##produce the banner plot and dendrogram
plot(ResultEuc)

##similarity measures
##"Manhattan" distances calculated next 
foodsM <- dist(foods2, method = "manhattan")


##utilize the agnes function to perform agglomerative clustering
ResultMan <- agnes(foodsM, diss = TRUE, method = "ward")

##print the result
ResultMan


##note the agglomerative coefficient = 0.9983333 with euclidean distance, very strong clustering
##note the agglomerative coefficient = 0.9984644 with manhattan distance, slightly better clustering
#Also dendrogram shows more clear and clean breaks

##produce the banner plot and dendrogram
plot(ResultMan)

aClusters <- cutree(ResultMan, k = 4)

##append the result to the original data frame
new_foods <- data.frame(foods, aClusters)
View(new_foods)

## Summary statistics for each cluster, categorized by means
summary(subset(new_foods, aClusters == 1))
#1106 Calories, 65 Carbs, 8 Protein, 11 Fat, 0 Trans Fat, 3.5 Sat. Fat, 3 Monoun. Fat, 2 Polyun Fat, 5 Fiber,  20 Sugar


summary(subset(new_foods, aClusters == 2))
#191 Calories, 12 Carbs, 2.5 Protein, 1 Fat, 0 Trans Fat, 0 Sat Fat, 0 Monoun, 0 Polyun, 
#1.5 Fiber, 4 Sugar

summary(subset(new_foods, aClusters == 3))
#620 Calories, 4 Carbs, 21 Protein, 12 Fat, 0 Trans Fat, 4 Sat Fat, 4.5 Monoun, 1.5 Polyun, 0 Fiber, 1 Sugar

summary(subset(new_foods, aClusters == 4))
#1903 Calories, 10 Carbs, 8 Protein, 70 Fat, 2 Trans Fat, 19 Sat Fat, 27 Monoun, 19 Polyun, 3 Fiber, 2 Sugar


##to identify the number of observations in each cluster type
summary(as.factor(aClusters))


#Group by Clusters and categories  

recommendations <- new_foods %>%
  group_by(aClusters, Food.Category) %>%
  summarise(Total = n())

# Top 5 food categories per cluster
top5 <- recommendations %>%
  group_by(aClusters) %>%
  arrange(desc(Total)) %>%  
  slice(1:5)  

###### K Means Clustering 



##set k equal to 3 (agglomerative K - 1)
set.seed(1)
kResult3 <- pam(foods2, k = 3)
summary(kResult3)
plot(kResult3)
#0.2896871



##set k equal to 4
set.seed(1)
kResult4 <- pam(foods2, k = 4)
summary(kResult4)
plot(kResult4)
#0.3296839

##set k equal to 5 (agglomerative K + 1)
set.seed(1)
kResult5 <- pam(foods2, k = 5)
summary(kResult5)
plot(kResult5)
#0.3541174



## Better viz for each result


# K = 3
clusplot(foods2, kResult3$cluster, color = TRUE,
         shade = TRUE, 
         labels = 0, 
         lines = 0)


#K = 4
clusplot(foods2, kResult4$cluster, color = TRUE,
         shade = TRUE, 
         labels = 0, 
         lines = 0)


#K = 5
clusplot(foods2, kResult5$cluster, color = TRUE,
         shade = TRUE, 
         labels = 0, 
         lines = 0)
