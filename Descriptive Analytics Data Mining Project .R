###################################################
#####DESCRIPTIVE ANALYTICS DATA MINING PROJECT#####
###################################################

##IMPORTING DATA##

read.csv('/Users/skyepetersen/Desktop/dataset2.0.csv') #import data as CSV
dataset<-read.csv('/Users/skyepetersen/Desktop/dataset2.0.csv') #store data as a variable called'dataset' 
View(dataset) #open data in new R Script tab 

##CLEANING DATA##
#delete unnecessary columns and rows#

library(dplyr) #download dplyr package to take out rows 
dataset_B<-dataset #start a new dataset to see how many variables are lost 
dataset_B<- dataset %>% slice(-c(1,2,51,52,53,54,55,56))
dataset_B$X.7<-NULL #delete Crude Oil Production column 
dataset_B$X.5<-NULL #delete Shares of Renewable Energy columnn
View(dataset_B) #new dataset has 2 less variables 

#delete NA's#

dataset_C<-dataset_B #start a new dataset 
dataset_C<-na.omit(dataset_B) #deletes the rows with NA's 
View(dataset_C) #new dataset has 4 less observations 

#rename column headers#

colnames(dataset_C) #look at existing column names 
dataset_D<-dataset_C #make a new dataset 
dataset_D<-dataset_C %>% rename(Energy.Consumption = Domestic.Energy.Consumption.Per.Country..2019., Year = X, Oil = X.1, Elec = X.2, Coal = X.3, Gas = X.4, CO2 = X.6) #download dplyr grammar of data manipulation package to get rename function
View(dataset_D)

##EXPLORATORY ANALYSIS## 

install.packages('ggplot2')
library(ggplot2)
dataset_E<-read.table('/Users/skyepetersen/Desktop/fixed_dataset.txt') #use dataset from Dr. Levkoff because mine wasn't working
View(dataset_E)   

#Plotting Histograms#  

ggplot(dataset_E, aes(Oil)) + geom_histogram()
ggplot(dataset_E, aes(Elec)) + geom_histogram()
ggplot(dataset_E, aes(Coal)) + geom_histogram()
ggplot(dataset_E, aes(Gas)) + geom_histogram()
ggplot(dataset_E, aes(CO2)) + geom_histogram() 

summary(dataset_E) #prove that mean is much larger than the median 

#Scatter Plots with Smoothing per Variable relating to CO2#

library(MASS) #use this to load the method=rlm smoothing kit 
ggplot(dataset_E, aes(x = Oil, y = CO2))  #builds structure of the plot 
ggplot(dataset_E, aes(x = Oil, y = CO2)) + geom_point()  #renders the data using geom_point 
ggplot(dataset_E, aes(x = Oil, y=CO2)) + geom_point() + geom_smooth() #adds shaded area showing confidence interval 
ggplot(dataset_E, aes(x = Oil, y=CO2)) + geom_point() + geom_smooth(method = 'rlm') 

ggplot(dataset_E, aes(x = Elec, y = CO2))  
ggplot(dataset_E, aes(x = Elec, y = CO2)) + geom_point()   
ggplot(dataset_E, aes(x = Elec, y=CO2)) + geom_point() + geom_smooth()
ggplot(dataset_E, aes(x = Elec, y=CO2)) + geom_point() + geom_smooth(method = 'rlm')

ggplot(dataset_E, aes(x = Coal, y = CO2))  
ggplot(dataset_E, aes(x = Coal, y = CO2)) + geom_point()   
ggplot(dataset_E, aes(x = Coal, y=CO2)) + geom_point() + geom_smooth()
ggplot(dataset_E, aes(x = Coal, y=CO2)) + geom_point() + geom_smooth(method = 'rlm')

ggplot(dataset_E, aes(x = Gas, y = CO2))  
ggplot(dataset_E, aes(x = Gas, y = CO2)) + geom_point()   
ggplot(dataset_E, aes(x = Gas, y=CO2)) + geom_point() + geom_smooth()
ggplot(dataset_E, aes(x = Gas, y=CO2)) + geom_point() + geom_smooth(method = 'rlm')




       