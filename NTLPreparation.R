#Binning NightLight Luminosity Levels & Preparing NTL Dataset

#GMMs assume that the distribution of univariate night light intensities comes from the mixture of k-underlying normal or
#Gaussian distributions and find the set of normal distributions that best fit the data. Based on these, the
#probability of each observation belonging to each group is derived.
#Splitting of datasets is done by performing random sampling within each luminosity bin to preserve overall
#class distribution

library(mclust)
library(tidyverse)

#select csv file containing mean luminority values
NTL_csv_path <- ff #paste
wd_path > dirname(NTL_csv_path)
setwd(wd_path)

#Check CSV data & take note of name of column containing luminosity values
head(datapoints)
#specify colmn name containing avg luminosity
ntl_col = ""
#subset column containing average luminosity values
avector <- datapoints[,ntl_col]
class(avector) #numeric


fit=Mclust(avector, G=3, model ="V") #run GMM model to produce 3 clusters
#view summary of model
summary(fit)

#Check is Mclust yields results
if (is.null(fit)==FALSE) {
  #display bin classification to check if initiial calculation produced results
  fit$classification 
  
  #merge bin results to the original dataframe & select relevant columns 
  df_bin <- data.frame(datapoints, bin_GMM = fit$classification) %>%
    select(id,      #grid ID
           lon,lat, #centroid coordinates
           geocode, #adm boundary code
           avg_rad = all_of(ntl_col), #luminosity column
           bin_GMM, #bin column
           filename) #imagery filename
}else{ #if resulting mclust is null 
  
  #filter luminosity levels to extract all positive non-0 lum. levels
  non_zero_datapoints <- datapoints %>%
    filter(get(ntl_col)>0)
  #generate another subset column containing avg lum values  & store in var
  non_zero_avector <- non_zero_datapoints[,ntl_col]
  
  #run GMM
  fit=Mclust(non_zero_avector, G=3, model="V")
  print(summary(fit))
  
  #merge non-zero luminosity data with bin classification 
  df_non_zero <- data.frame(non_zero_datapoints, bin_GMM = fit$classification) %>%
    select(id, bin_GM) #retain only id & bin column for ease of merging 
  
  #merge binned data with rest of data
  df_bin<- left_join(datapoints, df_non_zero, by="id") %>%
    mutate(bin_GMM = ifelse(is.na(bin_GMM), 1,bin_GMM)) #classify zero luminosity values into bin category 1
    select(id, #grid id
           lon,lat, #centroid coordinates
           geocode, 
           avg_rad = all_of(ntl_col), #luminosity values, change column name based on input csv
           bin_GMM, #bins
           filename)  #jpeg filenames
  }
}

#Determine cutoff values for each bin 
df_cutoff <- df_bin %>%
  group_by(bin_GMM) %>%
  summarise(min_cutoff = min(avg_rad),
            max_cutoff = max(avg_rad), 
            n_samples = n())

#view cutoff table 
view(df_cutoff)

#Merge gov published poverty & population data with dataset
SAE_csv_path <- ff #paste
df_sae <- read.csv(SAE_csv_path)
head(df_sae) #check structure of data & identify common var for joining

#check here!
#merge the dataframe containing binned NTL & published poverty data
df <- left_join(df_bin, df_sae, by = c("geocode" = "PSGC_code"))

#view merged dataframe
head(df)

#Dataset splitting 
#90% for training & validation and 10% for holdout dataset
library(caret)

#Generate index of 90% of training and validation dataset
splitIndex <- createDataPartition(df$bin_GMM,#coloumn of dataset for basis of split
                                  times = 1, #no. of split to perform
                                  p = 0.9, #split ratio
                                  list = FALSE) #output dataset as matrix

#subset dataset to extract training & validation dataset
df_Train <- df[ splitIndex,]
#subset dataset to extract holdout dataset
df_Test <- df[-splitIndex,]

#check the resulting datasets
head(df_Train)
head(df_Test)

nrow(df_Train)
NROW(df_Test)

#output results to as csv files
#generate filename
train_file_name <- str_replace(basename(NTL_csv_path), "full", "train90")
test_file_name <- str_replace(basename(NTL_csv_path), "full", "test10")

write.csv(df_Train, train_file_name, row.names = F)
write.csv(df_Test, test_file_name, row.names = F)
