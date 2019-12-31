library(DataExplorer)
library(dplyr)
library(knitr)


missing <- function(data){
  missing_proportions <- data.frame(missing_prop = round(colMeans(is.na(data))*100,2))
  missing_proportions$attribute <- rownames(missing_proportions)
  missing_proportions <- missing_proportions[,c(2,1)]
  rownames(missing_proportions) <- NULL
  kable(missing_proportions,booktabs=T,longtable = T) %>%
    kable_styling() %>%
    row_spec(which(missing_proportions$missing_prop >= 30),
             color = 'white',
             background = 'red') %>%
    row_spec(which(missing_proportions$missing_prop > 0 & missing_proportions$missing_prop < 10),
             color = 'white',
             background = 'lime') %>%
    row_spec(which(missing_proportions$missing_prop > 10 & missing_proportions$missing_prop < 30),
             color = 'black',
             background = 'yellow')
  return(missing_proportions)
}

summarised <- function(data,x){
  variable <- unlist(data[,x])
  total <- length(variable)
  missing <- sum(is.na(variable))
  mode <- sort(table(variable),decreasing = T)[1]
  mode_value <- names(mode)
  mode_prop <- round(as.numeric(mode)*100/total,2)
  unique_vals <- length(unique(variable))
  m_prop <- round(missing*100/total,1)
  outliers <- outlierKD(data,x)
  quantiles <- data.frame(t(data.frame(value = round(quantile(variable,
                                                              probs = c(0,0.01,0.05,0.1,0.2500,0.5,0.75,0.9,0.95,0.99,1),
                                                              na.rm = T),2))))
  results <- data.frame(variable = x,
                        observations = total,
                        unique_vals = unique_vals,
                        mode_value = mode_value,
                        mode_prop = as.numeric(mode_prop),
                        quantiles,
                        outliers)
  row.names(results) <- NULL
  return(results)
}

outlierKD <- function(dt, var) {
  color_tr <- adjustcolor('limegreen',alpha.f = 0.6)
  var_name <- unlist(dt[,var])
  na1 <- sum(is.na(var_name))
  m1 <- round(mean(var_name, na.rm = T),2)
  #par(mfrow=c(2, 2), oma=c(0,0,3,0))
  #boxplot(var_name, main="With outliers",col = color_tr)
  #hist(var_name, main="With outliers", xlab=NA, ylab=NA,col = color_tr)
  outlier <- boxplot.stats(var_name)$out
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  #boxplot(var_name, main="Without outliers",col = color_tr)
  #hist(var_name, main="Without outliers", xlab=NA, ylab=NA,col = color_tr)
  #title(paste("Outlier Check for",var), outer=TRUE)
  na2 <- sum(is.na(var_name))
  num_outliers <- na2 - na1
  prop_outliers <- round((na2 - na1) / sum(!is.na(var_name))*100, 1)
  m2 <- round(mean(var_name, na.rm = T),2)
  result <- data.frame(num_outliers = num_outliers,
                       prop_outliers = as.numeric(prop_outliers),
                       mean_w_outliers = m1,
                       mean_wo_outliers = m2)
  return(result)
}


univariate_numerical <- function(data,x,y){
  library(dplyr)
  numerical <- select(data,
                      which(sapply(data,is.numeric) & sapply(data,
                                                             function(x) length(unique(x)) > 20)))
  resulted <- matrix(ncol = 20,nrow = ncol(numerical))
  for (i in 1:ncol(numerical)){
    name <- names(numerical)[i]
    result <- as.matrix(summarised(numerical,name))
    resulted[i,] <- result
  }
  
  resulted <- as.data.frame(resulted)
  resulted[,-1] <- sapply(sapply(resulted[-1],as.character),as.numeric)
  names(resulted) <- names(summarised(numerical,name))
  
  if(x == 1){
    resulted <- resulted[,c(1:5,17:20)]
    kabled<- kable(resulted,
                   caption = paste('Univariate analysis for numerical variables in',y,'dataset -',x),
                   align = 'c') %>%
      row_spec(which(resulted$mode_prop > 25 
                     | resulted$prop_outliers > 10),
               color = 'black',
               background = 'gold') %>%
      kable_styling(position = 'center') %>%
      scroll_box(height = '500px',width = '1400px') 
  }
  
  else{
    resulted <- resulted[,c(1,6:16)]
    dupli <- c()
    for(i in 1:nrow(resulted)){
         rowed <- unlist(resulted[i,])
         dup<- sum(duplicated(rowed)) > 0
         dupli <- c(dupli,dup)
       }
    names(resulted) <- gsub('X','Percentile_',names(resulted))
    names(resulted) <- gsub('\\.','',names(resulted))
    kabled<- kable(resulted,
                   caption = paste('Univariate analysis for numerical variables in',y,'dataset -',x),
                   align = 'c') %>%
      row_spec(which(dupli),
               color = 'black',
               background = 'gold') %>%
      kable_styling(position = 'center') %>%
      scroll_box(height = '500px',width = '1400px') 
  }
  
  
  return(kabled)
}




univariate_categorical <- function(data){
  numerical <- sapply(data,is.numeric)
  categorical <- select(data,which(!numerical))
  numerical <- select(data,which(numerical))
  cat_as_num <- sapply(numerical, function(x) length(unique(x)) < 50)
  cat_as_num <- select(numerical,which(cat_as_num))
  chars <- sapply(categorical, function(x) length(unique(x)) == 1 | length(unique(x)) > 50)
  categorical <- select(categorical,which(!chars))
  categorical <- cbind(cat_as_num,categorical)
  tabled <- matrix(ncol = 4)
  for(i in 1:length(categorical)){
    tab <- as.data.frame(table(categorical[,i]))
    names(tab) <- c('level','frequency')
    tab$variable <- names(categorical)[i]
    tab$proportion <- round(tab$frequency*100/sum(tab$frequency),2)
    tab <- as.matrix(tab[,c(3,1,2,4)])
    tabled <- rbind(tabled,tab)
  }
  
  tabled <- as.data.frame(tabled)
  tabled <- tabled[-1,]
  tabled$proportion <- as.numeric(as.character(tabled$proportion))
  kable(tabled,caption = 'Contingency table for categorical variables',align = 'c') %>%
    row_spec(which(tabled$proportion > 25),
             color = 'black',
             background = 'tomato') %>%
    kable_styling(position = 'center') %>%
    scroll_box(height = '500px',width = '1400px') 
}

outlier_rows <- function(data){
  data <- data[,sapply(data,is.numeric)]
  outliers <- sapply(data,function(x) boxplot.stats(x)$out)
  applications <- c()
  for(i in 1:ncol(data)){
    outs <- unlist(outliers[[i]])
    rows <- unlist(data[,i]) %in% outs
  }
}



