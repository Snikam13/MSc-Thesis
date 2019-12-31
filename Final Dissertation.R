## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = F,warning = F,message = F,cache = T,fig.align = 'center')
library(knitr)
library(readxl)
library(DMwR)
library(factoextra)
library(NbClust)
library(ggthemes)
library(openxlsx)
library(ggplot2)
library(ggmap)
library(dplyr)
library(pander)
library(knitr)
library(kableExtra)
library(cowplot)
library(rlang)
library(tidyr)
options(scipen = 0)



## Loading and cleaning the Dataset
## ------------------------------------------------------------------------
arrivals <- read.xlsx('world bank international arrivals islands v2.xlsx')

arrivals <- data.frame(gsub('\\.',NA,as.matrix(arrivals)))
arrivals <- arrivals[!(arrivals$country == 'country'),]
arrivals <- arrivals[!(arrivals$ovnarriv == 'world bank'),]
arrivals <- arrivals[!(rowMeans(is.na(arrivals)) > 0.95),]
arrivals <- data.frame(sapply(arrivals,as.character),stringsAsFactors = F)
arrivals <- data.frame(sapply(arrivals,as.numeric))
arrivals$country <- as.factor(arrivals$country)
countries <- c('Mauritius','Seychelles','Antigua and Barbuda',
               'Grenada','Bahrain','Barbados','Bermuda','Cape Verde',
               'Comoros','Dominica','Kiribati','Maldives','Malta','Marshall Islands',
               'Micronesia','Samoa','Sao Tome and Principe','Saint Kitts and Nevis',
               'Saint Lucia','Cayman Islands','Saint Vincent & Grenadines','Tonga',
               'Tuvalu','Palau','Singapore','Trinidad and Tobago','Solomon Islands')
arrivals$country <-as.character(countries[arrivals$country])
arrivals$year <- arrivals$year + 2000


## Name of countries present
## ------------------------------------------------------------------------
unique(arrivals$country)

## Check column names
## ------------------------------------------------------------------------
names(arrivals)



## Source Data Cleaning functions and
## use to calculate missing proportions for each variable
## ----include = F---------------------------------------------------------

source('Data Cleaning Functions.R')
missing_prop <- missing(arrivals)

## Define function to generate heatmaps for Missing values
## ------------------------------------------------------------------------
miss<-function(i,x){
  dt<-arrivals %>%
    select(country,year,!!sym(x))
  names(dt)[3]<- 'Missing'
  dt <- complete(dt,country,year)
  dt$Missing <- is.na(dt$Missing)
  title <- paste0(i,'. Heat map of missing values by year and country for the variable ',x)
  p <- ggplot(dt,aes(x=year,y = country,fill = Missing)) +
    geom_tile(alpha = 0.6) +
    labs(fill = 'Value Missing')+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 300,hjust = -0.5),
          plot.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_blank(),
          legend.position = 'bottom') +
    scale_fill_manual(values = c('limegreen','tomato'),labels = c('Present','Absent'),na.value = 'tomato')
  return(p)
}



## Heatmap f missing values for gdpnom
## ----fig.width = 12,fig.height = 8---------------------------------------
miss('','gdpnom')

## Heatmap f missing values for arram
## ----fig.width = 12,fig.height = 8---------------------------------------
miss('','arram') 


## Check missing proportions for all data vs data after 2006
## ----fig.height = 8,fig.width = 10---------------------------------------
arrivals_2007 <- arrivals[arrivals$year > 2006,]
missing_prop_2007 <- missing(arrivals_2007)
p1 <- ggplot(missing_prop_2007,aes(missing_prop)) +
  geom_histogram(fill = 'tomato',alpha = 0.6) +
  theme_bw() +
  labs(subtitle = 'Total 67 variables',caption = 'Year > 2006')

p <- ggplot(missing_prop,aes(missing_prop)) +
  geom_histogram(fill = 'tomato',alpha = 0.6) +
  theme_bw() +
  labs(subtitle = 'Total 67 variables')

plot_grid(p,p1,ncol = 1,nrow = 2)


## Clean up variables with 75 % + missing data
## ------------------------------------------------------------------------
library(missCompare)
arrivals_raw <- arrivals
arrivals <- arrivals %>% filter(!country %in% c('Palau','Tuvalu','Cayman Islands','Micronesia','Marshall Islands','Singapore'))
clustering2 <- arrivals %>%
  filter(year > 2006) 

clustering2$country <- as.factor(clustering2$country)
clustering2_1 <- clean(clustering2,
                 var_removal_threshold = 0.75, 
                 ind_removal_threshold = 0.8,
                 missingness_coding = -9)

metadata <- missCompare::get_data(clustering2_1,
                                  matrixplot_sort = T,
                                  plot_transform = T)
clustering2_1$country <- levels(clustering2$country)[clustering2_1$country]

## Missing data dendrogram
## ----fig.height = 5,fig.width = 8----------------------------------------
metadata$Cluster_plot

## Clustering Experiment 1

## 2 clusters

## Selecting Variables
## ------------------------------------------------------------------------
missing_prop[missing_prop$missing_prop < 15,1]

clustering1 <- arrivals %>%
  select(country,receipt,gdpnom,ovnarriv,pop)

## KNN Imputation
## ------------------------------------------------------------------------
country_unique <- unique(clustering1$country)
clustering1[,-1] <- knnImputation(clustering1[,-1],k = 2)
clustering1_1_raw<- clustering1 %>% group_by(country) %>%
  summarise_all(funs(mean(.,na.rm = T)))



## Scatterplot to visualise relationships between core variables
## ----fig.height = 4,fig,width = 8----------------------------------------
ggplot(clustering1_1_raw,aes(receipt,gdpnom))+
  geom_point(aes(size = ovnarriv,col = log10(pop))) +
  scale_color_continuous_tableau() +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = 'bottom')


## Check variable distributions
## ----fig.height = 6------------------------------------------------------
clustering1_1_raw<- clustering1 %>% group_by(country) %>%
  summarise_all(funs(mean(.,na.rm = T)))

par(mfrow = c(2,2))
hist(clustering1_1_raw$receipt,main = "",xlab = 'Receipt')
hist(clustering1_1_raw$gdpnom,main = '',xlab = 'GDPNOM')
hist(clustering1_1_raw$ovnarriv,main = '',xlab = 'ovnarriv')
hist(clustering1_1_raw$pop,main = '',xlab = 'Population')

## Perform log transformation and check variable distributions
## ----fig.height = 6------------------------------------------------------


clustering1_1 <- as.data.frame(sapply(clustering1_1_raw[,-1],log10))

par(mfrow = c(2,2))
hist(clustering1_1$receipt,main = "",xlab = 'Receipt')
hist(clustering1_1$gdpnom,main = '',xlab = 'GDPNOM')
hist(clustering1_1$ovnarriv,main = '',xlab = 'ovnarriv')
hist(clustering1_1$pop,main = '',xlab = 'Population')

## Calculate number of optimal clusters
## ----fig.height = 8------------------------------------------------------
#clustering1_1 <- clustering1 %>% group_by(country) %>%
  #summarise_all(funs(mean(.,na.rm = T)))
clustering1_1 <- scale(clustering1_1)
silhoutte <- fviz_nbclust(clustering1_1, hkmeans, method = "silhouette") +
    geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Silhouette method") + theme(plot.title = element_blank(),plot.subtitle = element_text(hjust = 0.5))

elbow <- fviz_nbclust(clustering1_1, hkmeans, method = "wss") +
    geom_vline(xintercept = c(3,4), linetype = 2)+
  labs(subtitle = "Elbow method") + theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

plot_grid(elbow,silhoutte,ncol = 1)

## Show cluster average profile
## ------------------------------------------------------------------------
library(scales)
hcluster <- hkmeans(clustering1_1,2)
clustering1_1_raw$cluster <- as.character(hcluster$cluster)
clustering1_1_raw <- clustering1_1_raw[,c(1,6,2:5)]
cluster_avg<-clustering1_1_raw[,-1] %>%
  mutate(cluster = as.character(cluster)) %>%
  group_by(cluster) %>%
  summarise_all(mean) %>%
  mutate_if(is.numeric,funs(scientific(.,digits = 3)))



pander(cluster_avg,booktabs = T,align = 'c',caption = 'Cluster profiles for clustering experiment 1 with 2 clusters',split.table = Inf)

## Correlate clusters with core variables
## ------------------------------------------------------------------------
ggplot(clustering1_1_raw,aes(x = receipt,y = gdpnom)) +
  geom_point(aes(col = cluster,size = ovnarriv),alpha = 0.6) +
  theme_bw() +
  theme(legend.position = 'bottom') 

## Cluster Memberships
## ------------------------------------------------------------------------
cluster_members <- clustering1_1_raw %>%
  group_by(cluster)%>%
  summarise(countries = paste(country,collapse = ', '))
pander(cluster_members,caption = 'Cluster memberships obtained from clustering experiment 1 with 2 clusters')

## Cluster separation
## ------------------------------------------------------------------------
fviz_cluster(hcluster,clustering1_1) +
  theme_bw() +
  coord_cartesian(ylim = c(-1.7,1.7)) +
  theme(legend.position = 'bottom')
  

## 3 Clusters

## ------------------------------------------------------------------------
hcluster <- hkmeans(clustering1_1,3)
clustering1_1_raw$cluster <- as.character(hcluster$cluster)

clustering1_1_raw <- clustering1_1_raw[,c(1,6,2:5)]
cluster_avg<-clustering1_1_raw[,-1] %>%
  mutate(cluster = as.character(cluster)) %>%
  group_by(cluster) %>%
  summarise_all(mean) %>%
  mutate_if(is.numeric,funs(scientific(.,digits = 3)))

pander(cluster_avg,booktabs = T,caption = 'Cluster profiles for clustering experiment 1 with 3 clusters',digits = 3,split.table = Inf)


## ------------------------------------------------------------------------
ggplot(clustering1_1_raw,aes(x = receipt,y = gdpnom)) +
  geom_point(aes(col = cluster,size = ovnarriv),alpha = 0.6) +
  theme_bw() +
  theme(legend.position = 'bottom') 


## ------------------------------------------------------------------------
cluster_members <- clustering1_1_raw %>%
  group_by(cluster)%>%
  summarise(countries = paste(country,collapse = ', '))
pander(cluster_members,caption = 'Cluster memberships obtained from clustering experiment 1 with 3 clusters')


## ----fig.height = 4------------------------------------------------------
fviz_cluster(hcluster,clustering1_1) +
  theme_bw()+
  coord_cartesian(ylim = c(-1.7,1.7))+
  theme(legend.position = 'bottom')

## 4 Clusters

## ------------------------------------------------------------------------
hcluster <- hkmeans(clustering1_1,4)
clustering1_1_raw$cluster <- as.character(hcluster$cluster)
clustering1_1_raw <- clustering1_1_raw[,c(1,6,2:5)]
cluster_avg<-clustering1_1_raw[,-1] %>%
  mutate(cluster = as.character(cluster))%>%
  group_by(cluster) %>%
  summarise_all(mean)%>%
  mutate_if(is.numeric,funs(scientific(.,digits = 3)))

pander(cluster_avg,booktabs = T,split.table = Inf,caption = 'Cluster profiles obtained from clustering experiment 1 with 4 clusters')


## ------------------------------------------------------------------------
ggplot(clustering1_1_raw,aes(x = receipt,y = gdpnom)) +
  geom_point(aes(col = cluster,size = ovnarriv),alpha = 0.6) +
  theme_bw() +
  theme(legend.position = 'bottom') 


## ------------------------------------------------------------------------
cluster_members <- clustering1_1_raw %>%
  group_by(cluster)%>%
  summarise(countries = paste(country,collapse = ', '))
pander(cluster_members,caption = 'Cluster memberships obtained from clustering experiment 1 with 4 clusters')


## ------------------------------------------------------------------------
fviz_cluster(hcluster,clustering1_1) +
  theme_bw()+
  coord_cartesian(ylim = c(-1.7,1.7))+
  theme(legend.position = 'bottom')

## Clustering Experiment 2

## ------------------------------------------------------------------------
clustering3 <- clustering2_1%>%
  select(country,pop,gdpnom,arram,arreur,receipt,ovnarriv)


## ----fig.height = 8------------------------------------------------------
clustering3 <- clustering3[complete.cases(clustering3[,c('arram','arreur')]),]
clustering3[,-1] <- knnImputation(clustering3[,-1],k = 2)
clustering3_1 <- clustering3 %>%
  group_by(country)%>%
  summarise_all(mean)
clustering3_2 <- sapply(clustering3_1[,c('arram','arreur')],log2)
silhoutte <- fviz_nbclust(clustering3_2, hkmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method") + theme(plot.title = element_blank(),plot.subtitle = element_text(hjust = 0.5))

elbow <- fviz_nbclust(clustering3_2, hkmeans, method = "wss") +
    geom_vline(xintercept = c(3,4), linetype = 2)+
  labs(subtitle = "Elbow method") + theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

plot_grid(elbow,silhoutte,ncol = 1)

hcluster <- hkmeans(clustering3_2,4)

clustering3_1$cluster <- as.character(hcluster$cluster)


## ------------------------------------------------------------------------
fviz_cluster(hcluster) +
  theme_bw() +
  coord_cartesian(ylim = c(-2.5,1.7))+
  theme(legend.position = 'bottom')


## ------------------------------------------------------------------------
cluster_avg<-clustering3_1[,-1] %>%
  mutate(cluster = as.character(cluster))%>%
  group_by(cluster) %>%
  summarise_all(mean)%>%
  mutate_if(is.numeric,funs(scientific(.,digits = 3)))

pander(cluster_avg,caption = 'Cluster profiles obtained from clustering experiment 2',split.table = Inf)


cluster_members <- clustering3_1 %>%
  group_by(cluster)%>%
  summarise(countries = paste(country,collapse = ', '))
pander(cluster_members,caption = 'Cluster memberships obtained from clustering experiment 2')



## ------------------------------------------------------------------------
clustering3_1$cluster <- factor(clustering3_1$cluster)
ggplot(clustering3_1,aes(receipt,gdpnom))+
  geom_point(aes(col = cluster),alpha = 0.6,size = 5) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = 'bottom')

## Clustering Experiment 3

## ------------------------------------------------------------------------
clustering3 <- clustering2_1%>%
  select(country,pop,gdpnom,arrswe,arrbel,arrspa,receipt,ovnarriv)


## ----fig.height = 8------------------------------------------------------
clustering3 <- clustering3[complete.cases(clustering3[,c('arrswe','arrbel','arrspa')]),]
clustering3[,-1] <- knnImputation(clustering3[,-1],k = 2)
clustering3_1 <- clustering3 %>%
  group_by(country)%>%
  summarise_all(mean)
clustering3_2 <- sapply(clustering3_1[,c('arrswe','arrbel','arrspa')],log2)
silhoutte <- fviz_nbclust(clustering3_2, hkmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method") + theme(plot.title = element_blank(),plot.subtitle = element_text(hjust = 0.5))

elbow <- fviz_nbclust(clustering3_2, hkmeans, method = "wss") +
    geom_vline(xintercept = c(2), linetype = 2)+
  labs(subtitle = "Elbow method") + theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

plot_grid(elbow,silhoutte,ncol = 1)


## ------------------------------------------------------------------------

hcluster <- hkmeans(clustering3_2,2)

clustering3_1$cluster <- as.character(hcluster$cluster)
fviz_cluster(hcluster) +
  theme_bw() +
  coord_cartesian(ylim = c(-0.6,0.5))+
  theme(legend.position = 'bottom')


## ------------------------------------------------------------------------

cluster_avg<-clustering3_1[,-1] %>%
  mutate(cluster = as.character(cluster))%>%
  group_by(cluster) %>%
  summarise_all(mean)%>%
  mutate_if(is.numeric,funs(scientific(.,digits = 3)))

pander(cluster_avg,caption = 'Cluster profiles obtained from clustering experiment 3',split.table = Inf)


cluster_members <- clustering3_1 %>%
  group_by(cluster)%>%
  summarise(countries = paste(country,collapse = ', '))
pander(cluster_members,caption = 'Cluster memberships obtained from clustering experiment 3')


## ------------------------------------------------------------------------
clustering3_1$cluster <- factor(clustering3_1$cluster)
ggplot(clustering3_1,aes(receipt,gdpnom))+
  geom_point(aes(col = cluster),alpha = 0.6,size = 5) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = 'bottom')


## Clustering Experiment 4

## ------------------------------------------------------------------------
clustering3 <- clustering2_1%>%
  select(country,pop,gdpnom,arrausl,receipt,ovnarriv)


## ----fig.height = 8------------------------------------------------------
clustering3 <- clustering3[complete.cases(clustering3[,c('arrausl')]),]
clustering3[,-1] <- knnImputation(clustering3[,-1],k = 2)
clustering3_1 <- clustering3 %>%
  group_by(country)%>%
  summarise_all(mean)
clustering3_2 <- sapply(clustering3_1[,c('arrausl')],log2)
silhoutte <- fviz_nbclust(clustering3_2, hkmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method") + theme(plot.title = element_blank(),plot.subtitle = element_text(hjust = 0.5))

elbow <- fviz_nbclust(clustering3_2, hkmeans, method = "wss") +
    geom_vline(xintercept = c(2), linetype = 2)+
  labs(subtitle = "Elbow method") + theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

plot_grid(elbow,silhoutte,ncol = 1)


## ------------------------------------------------------------------------

hcluster <- hkmeans(clustering3_2,2)

clustering3_1$cluster <- as.character(hcluster$cluster)
#fviz_cluster(hcluster)

cluster_avg<-clustering3_1[,-1] %>%
  mutate(cluster = as.character(cluster))%>%
  group_by(cluster) %>%
  summarise_all(mean)%>%
  mutate_if(is.numeric,funs(scientific(.,digits = 3)))

pander(cluster_avg,caption = 'Cluster profiles obtained from clustering experiment 4',split.table = Inf)


cluster_members <- clustering3_1 %>%
  group_by(cluster)%>%
  summarise(countries = paste(country,collapse = ', '))
pander(cluster_members,caption = 'Cluster memberships obtained from clustering experiment 4')


## ------------------------------------------------------------------------
clustering3_1$cluster <- factor(clustering3_1$cluster)
ggplot(clustering3_1,aes(receipt,gdpnom))+
  geom_point(aes(col = cluster),alpha = 0.6,size = 5) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = 'bottom')

## Clustering Experiment 5
## ------------------------------------------------------------------------
clustering3 <- clustering2_1%>%
  select(country,pop,gdpnom,arrfra,arrit,receipt,ovnarriv)


## ----fig.height = 8------------------------------------------------------
clustering3 <- clustering3[complete.cases(clustering3[,c('arrfra','arrit')]),]
clustering3[,-1] <- knnImputation(clustering3[,-1],k = 2)
clustering3_1 <- clustering3 %>%
  group_by(country)%>%
  summarise_all(mean)
clustering3_2 <- sapply(clustering3_1[,c('arrfra','arrit')],log2)
silhoutte <- fviz_nbclust(clustering3_2, hkmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method") + theme(plot.title = element_blank(),plot.subtitle = element_text(hjust = 0.5))

elbow <- fviz_nbclust(clustering3_2, hkmeans, method = "wss") +
    geom_vline(xintercept = c(3,4), linetype = 2)+
  labs(subtitle = "Elbow method") + theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

plot_grid(elbow,silhoutte,ncol = 1)


## ------------------------------------------------------------------------

hcluster <- hkmeans(clustering3_2,3)

clustering3_1$cluster <- hcluster$cluster
fviz_cluster(hcluster) +
  theme_bw()+
  coord_cartesian(ylim = c(-2,1.7))+
  theme(legend.position = 'bottom')


## ------------------------------------------------------------------------

cluster_avg<-clustering3_1[,-1] %>%
  mutate(cluster = as.character(cluster))%>%
  group_by(cluster) %>%
  summarise_all(mean)%>%
  mutate_if(is.numeric,funs(scientific(.,digits = 3)))

pander(cluster_avg,caption = 'Cluster profiles obtained from clustering experiment 5',split.table = Inf)


cluster_members <- clustering3_1 %>%
  group_by(cluster)%>%
  summarise(countries = paste(country,collapse = ', '))
pander(cluster_members,caption = 'Cluster memberships obtained from clustering experiment 5')


## ------------------------------------------------------------------------
clustering3_1$cluster <- factor(clustering3_1$cluster)
ggplot(clustering3_1,aes(receipt,gdpnom))+
  geom_point(aes(col = cluster),alpha = 0.6,size = 5) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = 'bottom')

