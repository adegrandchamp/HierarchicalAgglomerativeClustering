#Clustering for Final Project
#research question: what are the clusters of behavior women exhibit when it comes to preventative health?

#libraries
library(readr)
library(tidyverse)
library(stats)
library(caret)
library(factoextra)
library(reshape2)
library(ggthemes)

#data set - precleaned
clusteringSet <- read_csv("Documents/GradSchool/DSC424/Final Project/SelfAnalysis/clusteringSet.csv")
summary(clusteringSet)

#variable types
#binary - hadmam, hadhyst2, hlthpln1, medcost, internet, smoke100, flushot6, hivtst6, caregiv1,
#hadhpvorpap, activlvl, aerorecs
#interval - howlong, lastpap2, checkup1
#ordinal - genhlth, educa, employ1, seatbelt, _ageg5yr, incomg
#continuous - physhlth, mentlhlth, poorhlth, children, htin4, weightlbs, alcdaysclean,
#sumfruit, sumveg, strenclean, exertimes, maxvo2, funccap

#mostly binary and continuous variables - all pre-cleaned to scale similarly

#want to try a couple of distances and linkages before settling on a final answer
#distances: binary, euclidean, manhattan
#linkages: complete, median, Ward-D

#binary/complete
dist_bin1 <- dist(clusteringSet, method = 'binary')
hfit1 <- hclust(dist_bin1, method = 'complete')
plot(hfit1)

#binary/median
hfit2 <- hclust(dist_bin1, method = 'median')
plot(hfit2)

#binary/ward-D
hfit3 <- hclust(dist_bin1, method = 'ward.D')
plot(hfit3)

#euclidean/complete
dist_euc1 <- dist(clusteringSet, method = 'euclidean')
hfit4 <- hclust(dist_euc1, method = 'complete')
plot(hfit4)

#euclidean/median
hfit5 <- hclust(dist_euc1, method = 'median')
plot(hfit5)

#euclidean/ward-D
hfit6 <- hclust(dist_euc1, method = 'ward.D')
plot(hfit6, horiz = TRUE)
title(xlab = 'Euclidean Distance + Ward-D linkage')
title(ylab = 'Cluster Height')

#manhattan/complete
dist_man1 <- dist(clusteringSet, method = 'manhattan')
hfit7 <- hclust(dist_man1, method = 'complete')
plot(hfit7)

#manhattan/median
hfit8 <- hclust(dist_man1, method = 'median')
plot(hfit8)

#manhattan/ward-D
hfit9 <- hclust(dist_man1, method = 'ward.D')
plot(hfit9)

#initial results
#median linkage resulted in skewed dendrograms, will not use
#euclidean complete also had a high degree of skew, will not use
#manhattan complete is skewed, will not use
#binary complete is a lot of noise

#ward-d linkage has the best noise and skew control
#Euclidean is less skewed than binary/manhattan
#hfit6

#pca for visualization
pca <- prcomp(clusteringSet)
rotated_pca <- as.data.frame(pca$x)

#modeling various cluster sizes - want to look at between 4 and 7
h4_eucD <- cutree(hfit6, k = 4)

rotated_pca$ClusterEucWardD <- as.factor(h4_eucD)

ggplot(rotated_pca, aes(x = PC1, y = PC2, col = ClusterEucWardD)) +
  geom_point(alpha = 0.5)

h5_eucD <- cutree(hfit6, k = 5)
h6_eucD <- cutree(hfit6, k = 6)
h7_eucD <- cutree(hfit6, k = 7)

rotated_pca$MD4 <- as.factor(h4_eucD)
rotated_pca$MD5 <- as.factor(h5_eucD)
rotated_pca$MD6 <- as.factor(h6_eucD)
rotated_pca$MD7 <- as.factor(h7_eucD)

ggplot(rotated_pca, aes(x = PC1, y = PC2, col = MD4)) +
  geom_point(alpha = 0.5)

ggplot(rotated_pca, aes(x = PC1, y = PC2, col = MD5)) +
  geom_point(alpha = 0.5)

ggplot(rotated_pca, aes(x = PC1, y = PC2, col = MD6)) +
  geom_point(alpha = 0.5)

ggplot(rotated_pca, aes(x = PC1, y = PC2, col = MD7)) +
  geom_point(alpha = 0.5)

#4 results in the most distinct clusters

#adding clusters to original data set to understand cluster characteristics

numberedSet <- clusteringSet %>% mutate(rowNum = row_number())
dataWithClusters <- lapply(1:4, function(nc) numberedSet$rowNum[h4_eucD==nc])  

#most observations fall in cluster 2 (8542), with 1 having 6779, 3 3436, and 4 2875

numberedSet <- numberedSet %>% rename('AGEGROUP' = `_AGEG5YR`)

#plotting density plots of all variables by cluster to understand unique characteristics

cluster1 <- numberedSet %>% filter(rowNum %in% dataWithClusters[[1]]) %>% select(-rowNum)
cluster2 <- numberedSet %>% filter(rowNum %in% dataWithClusters[[2]]) %>% select(-rowNum)
cluster3 <- numberedSet %>% filter(rowNum %in% dataWithClusters[[3]]) %>% select(-rowNum)
cluster4 <- numberedSet %>% filter(rowNum %in% dataWithClusters[[4]]) %>% select(-rowNum)

drivingVars <- c('AGEGROUP', 'WEIGHTNORM','NORMPOOR','CAREGIV1','NORMMENT',
                 'NORMEXER','HOWLONG','LASTPAP2','INCOMG','FUNCCAP','WEIGHTNORM',
                 'HADMAM','CHILDREN','SMOKE100','HIVTST6','HADHPVORPAP', 'EMPLOY1')

meltClust1 <- melt(cluster1)
meltClust2 <- melt(cluster2)
meltClust3 <- melt(cluster3)
meltClust4 <- melt(cluster4)

filtMelt1 <- meltClust1 %>% filter(variable %in% drivingVars)
filtMelt2 <- meltClust2 %>% filter(variable %in% drivingVars)
filtMelt3 <- meltClust3 %>% filter(variable %in% drivingVars)
filtMelt4 <- meltClust4 %>% filter(variable %in% drivingVars)

#main density plots

ggplot(data = meltClust1, aes(x = value)) + 
  stat_density() +
  facet_wrap(~variable, scales = 'free') + 
  theme_clean() + 
  ggtitle('Cluster 1 Densities')

ggplot(data = meltClust2, aes(x = value)) + 
  stat_density() +
  facet_wrap(~variable, scales = 'free') + 
  theme_clean() +
  ggtitle('Cluster 2 Densities')

ggplot(data = meltClust3, aes(x = value)) + 
  stat_density() +
  facet_wrap(~variable, scales = 'free') + 
  theme_clean() +
  ggtitle('Cluster 3 Densities')

ggplot(data = meltClust4, aes(x = value)) + 
  stat_density() +
  facet_wrap(~variable, scales = 'free') + 
  theme_clean() +
  ggtitle('Cluster 4 Densities')

#most significant density plots
ggplot(data = filtMelt1, aes(x = value)) + 
  stat_density() +
  facet_wrap(~variable, scales = 'free') + 
  theme_clean() + 
  ggtitle('Young Retirees\' Densities')

ggplot(data = filtMelt2, aes(x = value)) + 
  stat_density() +
  facet_wrap(~variable, scales = 'free') + 
  theme_clean() +
  ggtitle('Mid-Life Workers\' Densities')

ggplot(data = filtMelt3, aes(x = value)) + 
  stat_density() +
  facet_wrap(~variable, scales = 'free') + 
  theme_clean() +
  ggtitle('Older Retirees\' Densities')

ggplot(data = filtMelt4, aes(x = value)) + 
  stat_density() +
  facet_wrap(~variable, scales = 'free') + 
  theme_clean() +
  ggtitle('Young Professionals\' Densities')

#four characteristics can be loosely defined on weight, health, and activity level:
#Cluster 1 - Younger Retirees
#Cluster 2 - Mid-Life Workers
#Cluster 3 - Older Retirees
#Cluster 4 - Young Professionals

#cleaning up a presentation version of 4 clusters scatter plot
rotated_pca <- rotated_pca %>% rename('MD4' = 'Clusters')
rotated_pca <- rotated_pca %>%
  mutate(Clusters = case_when(MD4 == 1 ~ 'Younger Retirees',
                              MD4 == 2 ~ 'Mid-Life Workers',
                              MD4 == 3 ~ 'Older Retirees',
                              MD4 == 4 ~ 'Young Professionals'
  ))

ggplot(rotated_pca, aes(x = PC1, y = PC2, col = Clusters)) +
  geom_point(alpha = 0.5) + 
  theme_clean() +
  theme(legend.position = 'bottom') +
  ggtitle('Clusters by PC')
