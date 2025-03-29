#*****Loading the Data and exploring it 
library("ggplot2")
library("dplyr")
library("car")
file_path <- "C:/Users/oscal/Downloads/project-files-housing-market-data-analysis-in-r-project/housing_data.csv"
data <- read.csv(file_path)#Importation of dataset
head(data)#Verification de la bonne importation des données
str(data)#Exploration de la structure de la dataset
summary(data)#Inclut le min,max,mean,median
#*****Computing descriptive statistics
means<- sapply(data,mean,na.rm=TRUE)#sapply applique une fonction sur une liste 
#na.rm ignore les valeurs manquantes
medians<- sapply(data,median,na.rm=TRUE)
#La fonction mode statique etant n'est pas définie
f_mode <- function(x) {
  x <- na.omit(x)  # Supprimer les valeurs manquantes
  uniq_values <- unique(x)  # Valeurs uniques
  uniq_values[which.max(tabulate(match(x, uniq_values)))]  # Valeur la plus fréquente
}
modes<- sapply(data,f_mode)# pas la peine d'ajouter na.rm dans ce cas
print("Means of variables")
print(means)
print("Medians of variables")
print(medians)
print("Modes of variabels")
print(modes)
#*****Correlation Analysis
cor_matrix<- cor(data,use="complete.obs")#complete.obs pour ignorer NA valeurs
print("Correlation matrix")
print(cor_matrix)
#*****Handling missing values
data[is.na(data)] <- sapply(data, median, na.rm = TRUE)
head(data) 

