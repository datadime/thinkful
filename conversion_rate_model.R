#install multiple packages
install.packages(c("dplyr", "ggplot2", "rpart", "randomForest"))

#load multiple packages at a time
ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

package = c("dplyr", "ggplot2", "rpart", "randomForest")

ipak(package)

#import the conversion_data.csv into R from the path where you downloaded it.
#take a sample to look at the data
head(conversion_data)

#Look at the summary statistics to examin the data
summary(conversion_data)

#Notice that you have outliers in age. check out all the unique values for age in a decending order
sort(unique(conversion_data$age), decreasing=TRUE)

#Create a subset eliminating the outliers
data = subset(conversion_data, age<80)

#Visualize relationships between each of the independent and the dependent variable.

#group by country
data_country = data%>%
  group_by(country)%>%
  summarise(conversion_rate = mean(converted))
# plot
ggplot(data=data_country, aes(x=country, y=conversion_rate))+
  geom_bar(stat = "identity", aes(fill = country))

#group by age
data_age = data%>%
  group_by(age)%>%
  summarise(conversion_rate = mean(converted))
#plot
qplot(age, conversion_rate, data=data_age, geom="line")
#or
ggplot(data=data_age, aes(x=age, y=conversion_rate))+
  geom_line()

#group by new user
data_user = data%>%
  group_by(new_user)%>%
  summarise(conversion_rate = mean(converted))
# plot
ggplot(data=data_user, aes(x=new_user, y=conversion_rate))+
  geom_bar(stat = "identity", aes(fill = new_user))

#group by source
data_user = data%>%
  group_by(source)%>%
  summarise(conversion_rate = mean(converted))
# plot
ggplot(data=data_user, aes(x=source, y=conversion_rate))+
  geom_bar(stat = "identity", aes(fill = source))

#group by total_pages_visited
data_pages = data%>%
  group_by(total_pages_visited)%>%
  summarise(conversion_rate = mean(converted))
# plot
qplot(total_pages_visited, conversion_rate, data=data_pages, geom="line")
#or
ggplot(data=data_pages, aes(x=total_pages_visited, y=conversion_rate))+
  geom_line()

#Lets build different models to predict conversion rate

#Random Forest
#factors have to be explicitly labeled as factors
data$converted = as.factor(data$converted)
data$new_user = as.factor(data$new_user)
data$country = as.factor(data$country)
data$source = as.factor(data$source)

#Create test/training sets with a standard 66% split
train_sample = sample(nrow(data), size = nrow(data)*0.66)
train_data = data[train_sample,]
test_data = data[-train_sample,]
#or
train_sample = sample(2,nrow(data),prob = c(0.66,0.34),replace = TRUE)
train_data = data[id==1,]
test_data = data[id==2,]

#optimize the Value of mtry
bestmtry = tuneRF(train_data,train_data$converted,stepFactor = 1.2,improve = 0.01,trace = T,plot = T)

# build the forest with standard values for the 3 most important parameters (100 trees, 3 random variables selected at each split).
rf = randomForest(y=train_data$converted, x = train_data[, -ncol(train_data)],  ytest = test_data$converted, xtest = test_data[, -ncol(test_data)], ntree = 100, mtry = 3, keep.forest = TRUE)
rf


           
           