# Remove all the objects stored
rm(list = ls())

#Setting working directory
setwd("E:/edWisor/Project")

#Loading libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced","dummies", "e1071", "Information",
      "MASS", "rpart","rpart.plot","gbm", "ROSE","sampling", "DataCombine", "xgboost","inTrees","usdm","doSNOW","stats")

#Install packages
lapply(x,require, character.only = TRUE)

rm(x)

## Read the data
cab_train = read.csv("train_cab.csv", header = T, na.strings = c(" ", "", "NA"))
cab_test =read.csv("test.csv",header = T)

########################################EXPLORE THE DATA########################################
# Check number of rows and columns
dim(cab_train)
dim(cab_test)

# Column names
names(cab_train)
names(cab_test)

# Observe top 5 rows
head(cab_train,5)
head(cab_test,5)

# Structure of variables
str(cab_train)
str(cab_test)

#Transform data types
cab_train$fare_amount = as.numeric(as.character(cab_train$fare_amount))
cab_train$passenger_count = round(cab_train$passenger_count)

# Summary of the data
summary(cab_train)
summary(cab_test)

test_pickup_datetime = cab_test["pickup_datetime"]

# 1) Unique values ofpassenger_count
unique(cab_train$passenger_count)
table(cab_train$passenger_count)
table(cab_train$passenger_count>6) # 20 observations of passenger_count > 6
sum(is.na(cab_train$passenger_count)) # 55 observations of passenger_count == NA

# removing passenger counts '0' and above '6' 
nrow(cab_train[which(cab_train$passenger_count > 6 ),])
nrow(cab_train[which(cab_train$passenger_count < 1 ),])
cab_train = cab_train[-which(cab_train$passenger_count<1),]
cab_train = cab_train[-which(cab_train$passenger_count>6),]

# 2) fare amount - Values below 1 and - ve values replaced with NA
cab_train[which(cab_train$fare_amount < 1),]
nrow(cab_train[which(cab_train$fare_amount < 1),])#checking number of values below 1

cab_train = cab_train[-which(cab_train$fare_amount < 1),]
nrow(cab_train[which(cab_train$fare_amount < 1),])#checking number of values below 1
sum(is.na(cab_train$fare_amount))

# 3) A latitude-longitude map displays data for any region in the world
# Singed degrees format data is used for analysis
# Latitudes range from -90 to 90, Longitudes range from -180 to 180
# In this format South latitudes and West longitudes preceded by a minus sign
print(paste('pickup_longitude above 180', nrow(cab_train[which(cab_train$pickup_longitude > 180),])))
print(paste('pickup_longitude below -180', nrow(cab_train[which(cab_train$pickup_longitude < -180),])))
print(paste('pickup_latitude above 90', nrow(cab_train[which(cab_train$pickup_latitude > 90),])))
print(paste('pickup_latitude below -90', nrow(cab_train[which(cab_train$pickup_latitude < -90),])))
print(paste('dropoff_longitude above 180', nrow(cab_train[which(cab_train$dropoff_longitude > 180),])))
print(paste('dropoff_longitude below -180', nrow(cab_train[which(cab_train$dropoff_longitude < -180),])))
print(paste('dropoff_latitude above 180', nrow(cab_train[which(cab_train$dropoff_latitude > 90),])))
print(paste('dropoff_latitude below -180', nrow(cab_train[which(cab_train$dropoff_latitude < -90),])))

# One oulier in variable 'pickup_latitute" would be removed
cab_train = cab_train[-which(cab_train$pickup_latitude >90),]

# Checking any values equal to '0'
nrow(cab_train[which(cab_train$pickup_longitude == 0),])
nrow(cab_train[which(cab_train$pickup_latitude == 0),])
nrow(cab_train[which(cab_train$dropoff_longitude == 0),])
nrow(cab_train[which(cab_train$dropoff_latitude == 0),])

# Removing values with '0'
cab_train = cab_train[-which(cab_train$pickup_longitude == 0),]
cab_train = cab_train[-which(cab_train$dropoff_longitude == 0),]

# deleting incorrect data
unique(cab_train$pickup_longitude[cab_train$pickup_longitude>=0])
unique(cab_train$pickup_latitude[cab_train$pickup_latitude<=0])
unique(cab_train$dropoff_longitude[cab_train$dropoff_longitude>=0])
unique(cab_train$dropoff_latitude[cab_train$dropoff_latitude<=0])

cab_train = cab_train[-which(cab_train$pickup_longitude >= 0),]
cab_train = cab_train[-which(cab_train$dropoff_longitude >= 0),]

# Make a copy
df= cab_train
# cab_train = df

############################# Missing value analysis ###########################
missing_val = data.frame(apply(cab_train,2,function(x){sum(is.na(x))}))

#Converting row names into column names
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL
# Rename the variable
names(missing_val)[1]= "Missing_percentage"
# Calculating percentage
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(cab_train))*100
# Arrange in descending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
# Rearrange the column
missing_val= missing_val[,c(2,1)]

# Writing output result back to disk
write.csv(missing_val,"cab_train_mp.csv",row.names = F)

# Visualization of missing value
ggplot(data = missing_val[1:6,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "greenyellow")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()

# Identifying imputation method
cab_train[["fare_amount"]][15]
cab_train[["fare_amount"]][15]= NA

# Actual value = 12.5(fare amount),1(passenger_count)
# Mean imputed value = 15.11(fare amount),1.65(passenger_count)
# Median imputed value = 8.5(fare amount),1(passenger_count)
# KNN imputed value = 10(fare amount),1.33(passenger_count)

# Mean method
#cab_train$fare_amount[is.na(cab_train$fare_amount)] = median(cab_train$fare_amount, na.rm = T)
#cab_train$passenger_count[is.na(cab_train$passenger_count)] = median(cab_train$passenger_count, na.rm = T)

# Median method
#cab_train$fare_amount[is.na(cab_train$fare_amount)] = median(cab_train$fare_amount, na.rm = T)
#cab_train$passenger_count[is.na(cab_train$passenger_count)] = median(cab_train$passenger_count, na.rm = T)

# KNN imputation
cab_train = knnImputation(cab_train, k=3)
sum(is.na(cab_train))

#write.csv(cab_train, 'cab_train_missing.csv', row.names = F)
##################################### Outlier analysis ###################################
cab_train$passenger_count = round(cab_train$passenger_count)
# Boxplot for outliers
pl1 = ggplot(cab_train,aes(passenger_count,fare_amount))
pl1 +geom_boxplot(aes(group= cut_width(passenger_count,0.25)),outlier.colour="red", 
                  fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)

# Replace all outliers with NA and impute
vals = cab_train[,"fare_amount"] %in% boxplot.stats(cab_train[,"fare_amount"])$out
cab_train[which(vals),"fare_amount"] = NA

# check the NA
fare_amount_missing_val = sum(is.na(cab_train$fare_amount))
fare_amount_missing_percentage = (fare_amount_missing_val/nrow(cab_train))*100
fare_amount_missing_percentage # 8.7%

#median imputation
#cab_train$fare_amount[is.na(cab_train$fare_amount)] = median(cab_train$fare_amount, na.rm = T)
# KNN imputation
cab_train = knnImputation(cab_train, k=3)
sum(is.na(cab_train))

df1 = cab_train
# cab_train = df1

############################# Feature Engineering ##################################
# 1. Feature engineering for 'pickup_datetime' variable

# Convering 'pickup_datetime' factor into date and time for cab_train
cab_train$pickup_date = as.Date(as.character(cab_train$pickup_datetime))
cab_train$pickup_weekday = as.factor(format(cab_train$pickup_date,"%u"))
cab_train$pickup_month = as.factor(format(cab_train$pickup_date,"%m"))
cab_train$pickup_year = as.factor(format(cab_train$pickup_date,"%Y"))
pickup_time = strptime(cab_train$pickup_datetime,"%Y-%m-%d %H:%M:%S")
cab_train$pickup_hour = as.factor(format(pickup_time,"%H"))

# Convering 'pickup_datetime' factor into date and time for cab_test
cab_test$pickup_date = as.Date(as.character(cab_test$pickup_datetime))
cab_test$pickup_weekday = as.factor(format(cab_test$pickup_date,"%u"))
cab_test$pickup_month = as.factor(format(cab_test$pickup_date,"%m"))
cab_test$pickup_year = as.factor(format(cab_test$pickup_date,"%Y"))
pickup_time = strptime(cab_test$pickup_datetime,"%Y-%m-%d %H:%M:%S")
cab_test$pickup_hour = as.factor(format(pickup_time,"%H"))

sum(is.na(cab_train))
cab_train = na.omit(cab_train)

cab_train = subset(cab_train,select=-c(pickup_datetime,pickup_date))
cab_test = subset(cab_test,select=-c(pickup_datetime,pickup_date))

# 2.Calculating distance travelled using latitude and longitude
deg_to_rad = function(deg){
  (deg*pi)/180
}
harversine = function(long1,lat1,long2,lat2){
  phi1 = deg_to_rad(lat1)
  phi2 = deg_to_rad(lat2)
  delphi = deg_to_rad(lat2 - lat1)
  dellamda = deg_to_rad(long2 - long1)
  
  a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
    sin(dellamda/2) * sin(dellamda/2)
  
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  R = 6371e3
  R * c/1000 #1000 is used to convert to KM
}
# Harvestine formula is used to calculate distance for both cab_train and cab_test
cab_train$distance = harversine(cab_train$pickup_longitude,cab_train$pickup_latitude,cab_train$dropoff_longitude,cab_train$dropoff_latitude)
cab_test$distance = harversine(cab_test$pickup_longitude,cab_test$pickup_latitude,cab_test$dropoff_longitude,cab_test$dropoff_latitude)

# Removing variables used for feature enginerring the new variables
cab_train = subset(cab_train,select=-c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
cab_test = subset(cab_test,select=-c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

str(cab_train)
summary(cab_train)

nrow(cab_train[which(cab_train$distance >130 ),]) 
# considering the distance 130 as max and considering rest as outlier.
cab_train=cab_train[-which(cab_train$distance >130 ),] # removing 2 data
nrow(cab_test[which(cab_test$distance >130 ),])
#nrow(cab_train[which(cab_train$distance == 0),])
#nrow(cab_test[which(cab_test$distance == 0),])

#removing values with 0
# cab_train = cab_train[-which(cab_train$distance ==0),]
# cab_test = cab_test[-which(cab_test$distance ==0),]

########################################### Graph ######################################################

b1<-ggplot(data=cab_train, aes_string(x=cab_train$passenger_count),fill =cab_train$passenger_count) + geom_bar(stat ='count')+ ggtitle("Count of passenger_count") + theme_bw()
b1
b2<-ggplot(data=cab_train, aes_string(x=cab_train$pickup_weekday),fill=cab_train$pickup_weekday) + geom_bar(stat = "count")+ ggtitle("Count of pickup_weekday")+ theme_bw()
b2
b3<-ggplot(data=cab_train, aes_string(x=cab_train$pickup_month),fill=cab_train$pickup_month) + geom_bar(stat = "count")+ ggtitle("Count of pickup_month") + theme_bw()
b3
b4<-ggplot(data=cab_train, aes_string(x=cab_train$pickup_year),fill=cab_train$pickup_year) + geom_bar(stat = "count")+ ggtitle("Count of pickup_year") + theme_bw()
b4
b5<-ggplot(data=cab_train, aes_string(x=cab_train$pickup_hour),fill=cab_train$pickup_hour) + geom_bar(stat = "count")+ ggtitle("Count of pickup_hour") + theme_bw()
b5
################                             Feature selection                 ###################
cab_train$passenger_count = as.factor(as.numeric(cab_train$passenger_count))
numeric_index = sapply(cab_train,is.numeric) #selecting only numeric

numeric_data = cab_train[,numeric_index]

# Selecting categorical data
categorical_data = cab_train[,!numeric_index]

cnames = colnames(numeric_data)
catnames = colnames(categorical_data)
#Correlation analysis for numeric variables
corrgram(cab_train[,numeric_index],upper.panel=panel.pie, main = "Correlation Plot")

#Check for multicollinearity using VIF
vifcor(numeric_data)



##################################             Feature Scaling         ################################################
#Normality check
qqnorm(cab_train$distance)
Ht1<-truehist(cab_train$fare_amount) # truehist() scales the counts to give an estimate of the probability density.
lines(density(cab_train$fare_amount))

Ht2<-truehist(cab_train$distance)
lines(density(cab_train$distance))

#Normalisation
# for (i in cnames) {
#   print(i)
#   cab_train[,i] = (cab_train[,i] - min(cab_train[,i]))/
#     (max(cab_train[,i]-min(cab_train[,i])))
#   
# }
#Normalisation

print('distance')
cab_train[,'distance'] = (cab_train[,'distance'] - min(cab_train[,'distance']))/
  (max(cab_train[,'distance'] - min(cab_train[,'distance'])))

sum(is.na(cab_train))
sum(is.na(cab_test))
#################### Splitting train into train and validation subsets ###################
# Creating dummy variables for categorical variables
categorical_names = names(categorical_data)
cab_train = dummy.data.frame(cab_train,categorical_names)
cab_test = dummy.data.frame(cab_test,categorical_names)

#Splitting data into train and test data
set.seed(1000)
train_index = sample(1:nrow(cab_train), 0.8*nrow(cab_train))        
train = cab_train[train_index,]
test = cab_train[-train_index,]

#############            Linear regression               #################
lm_model = lm(fare_amount ~.,data=train)

summary(lm_model)
str(train)
plot(lm_model$fitted.values,rstandard(lm_model),main = "Residual plot",
     xlab = "Predicted values of fare_amount",
     ylab = "standardized residuals")


lm_predictions = predict(lm_model,test[,2:58])

qplot(x = test[,1], y = lm_predictions, data = test, color = I("blue"), geom = "point")

regr.eval(test[,1],lm_predictions)
# mae        mse       rmse       mape 
# 0.13432102 0.02976247 0.17251803 0.45597995 
#Calculate MAE, RMSE, R-squared for testing data 
print(postResample(pred = lm_predictions, obs = test$fare_amount))



#############                             Decision Tree            #####################

Dt_model = rpart(fare_amount ~ ., data = train, method = "anova")

summary(Dt_model)
#Predict for new test cases
predictions_DT = predict(Dt_model, test[,2:58])

qplot(x = test[,1], y = predictions_DT, data = test, color = I("blue"), geom = "point")

regr.eval(test[,1],predictions_DT)
# mae        mse       rmse       mape 
# 0.09313101 0.01580352 0.12571206 0.28612238 
print(postResample(pred = predictions_DT, obs = test$fare_amount))

#############                             Random forest            #####################
rf_model = randomForest(fare_amount ~.,data=train,importance = TRUE, ntree = 500)

summary(rf_model)

rf_predictions = predict(rf_model,test[,2:58])

qplot(x = test[,1], y = rf_predictions, data = test, color = I("blue"), geom = "point")

regr.eval(test[,1],rf_predictions)
# mae        mse       rmse       mape 
# 0.07947964 0.01226177 0.11073290 0.23835453 
print(postResample(pred = rf_predictions, obs = test$fare_amount))


############          Improving Accuracy by using Ensemble technique ---- XGBOOST             ###########################
train_data_matrix = as.matrix(sapply(train[-1],as.numeric))
test_data_data_matrix = as.matrix(sapply(test[-1],as.numeric))

xgboost_model = xgboost(data = train_data_matrix,label = train$fare_amount,nrounds = 15,verbose = FALSE)

summary(xgboost_model)
xgb_predictions = predict(xgboost_model,test_data_data_matrix)

qplot(x = test[,1], y = xgb_predictions, data = test, color = I("blue"), geom = "point")

regr.eval(test[,1],xgb_predictions)
# regr.eval(test[,1],xgb_predictions)
# mae        mse       rmse       mape 
# 0.07954457 0.01219059 0.11041099 0.23978363 
print(postResample(pred = xgb_predictions, obs = test$fare_amount))

plot(test$fare_amount,type="l",lty=1.8,col="Green")
lines(xgb_predictions,type="l",col="Blue")
#############                         Finalizing and Saving Model for later use                         ####################
# In this step we will train our model on whole training Dataset and save that model for later use
train_data_matrix2 = as.matrix(sapply(cab_train[-1],as.numeric))
test_data_matrix2 = as.matrix(sapply(cab_test,as.numeric))

xgboost_model2 = xgboost(data = train_data_matrix2,label = cab_train$fare_amount,nrounds = 15,verbose = FALSE)

# Saving the trained model
saveRDS(xgboost_model2, "./final_Xgboost_model_using_R.rds")

# loading the saved model
super_model <- readRDS("./final_Xgboost_model_using_R.rds")
print(super_model)

# Lets now predict on test dataset
xgb = predict(super_model,test_data_matrix2)
test_final = data.frame(xgb) 


# Now lets write(save) the predicted fare_amount in disk as .csv format 
write.csv(test_final,"xgb_predictions_R.csv",row.names = FALSE)
