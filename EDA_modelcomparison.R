## EDA & Model Comparison 

data = read.csv("autoswapper.csv")
head(data)
# Confirm null values in the provided dataset
sum(is.null(data))



# Confirm the distribution of price and year for each
p1 = ggplot(data, aes(x=price)) + geom_histogram(fill="steelblue",color="blue",binwidth=20)+theme_minimal()
p2 = ggplot(data=data, aes(x=year, y=price)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('year')+ylab('price')+theme_minimal()
ggarrange(p1, p2, ncol = 2, nrow = 1)

summary(data$price)
summary(data$year)


# Confirm the distribution of mileage and mpg for each
p1 = ggplot(data=data, aes(x=mileage, y=price)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + xlab('mileage')+ylab('price')+theme_minimal()
p2 = ggplot(data=data, aes(x=mpg, y=price)) + geom_point(alpha = 0.4,color=rgb(0.2,0.4,0.6,0.6)) + 
  xlab('mileage')+ylab('price')+theme_minimal()
ggarrange(p1, p2, ncol = 2, nrow = 1)

summary(data$mileage)
summary(data$mpg)



p1 = ggplot(data, aes(x = make)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_histogram(fill = "cornflowerblue", color = "black", stat="count") + 
  labs(title="Price by Make", x = "Make") 

p2 = ggplot(data, 
            aes(x = make, 
                y = price)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "white",
               outlier.color = "orange",
               outlier.size = 0.1) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Price Distribution by Make") 

ggarrange(p1, p2, ncol = 2, nrow = 1)





length(unique(data$model))
df1 = data.frame(data %>% group_by(model, .drop = FALSE) %>% count())
sum(df1$n<10)
# There are 194 different models in the dataset and 35 models have less than 10 counts. 



p1 = ggplot(data, aes(x = transmission)) +
  geom_histogram(fill = "cornflowerblue", color = "black", stat="count") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Price by Transmission", x = "Transmission")

p2 = ggplot(data, 
            aes(x = transmission, 
                y = price)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "white",
               outlier.color = "orange",
               outlier.size = 0.1) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Price Distribution by Transmission") 

ggarrange(p1, p2, ncol = 2, nrow = 1)


df1 = data.frame(data %>% group_by(transmission, .drop = FALSE) %>% count())
df1
# The `Transmission` variable is a categorical variable with the following categories: `Automatic`, `Manual`, `Semi-Auto`, `Other` categories. 
# Most of categories produced more than 19000 vehicles, and majority of the vehicles in each category cost less than GBP50000. 
# There are only 9 vehicles in category `Other`, but the distribution of the `price` for category `Other` is similar to that of the other three categories.



p1 = ggplot(data, aes(x = fuelType)) +
  geom_histogram(fill = "cornflowerblue", color = "black", stat="count") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Price by fuelType", x = "fuelType") 

p2 = ggplot(data, 
            aes(x = fuelType, 
                y = price)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "white",
               outlier.color = "orange",
               outlier.size = 0.1) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Price Distribution by fuelType") 

ggarrange(p1, p2, ncol = 2, nrow = 1)


df1 = data.frame(data %>% group_by(fuelType, .drop = FALSE) %>% count())
df1

# The `fuelType` variable is a categorical variable with the following categories: `Diesel`, `Petrol`, `Hybrid`, `Other`, and `Electric`. 
# More than 90% of vehicles belong to the `Diesel` and `Petrol` while there are only 6 vehicles in the `Electric` category. 
# There are less than 3000 vehicles in categories `Hybrid`, `Other`, and `Electric`, respectively, but the distribution of the `price` for each of three categories is similar to that of the other two categories.


ggplot(data, aes(x = engineSize)) +
  geom_histogram(fill = "cornflowerblue", color = "black", stat="count") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Price by engineSize", x = "engineSize") 

df1 = data.frame(data %>% group_by(engineSize, .drop = FALSE) %>% count())
summary(df1$engineSize)
dim(df1)
sum(df1$n<10)

# There are 40 different `engineSize`, The median and mean of `enginSize` are 2.85 and 3.17, respectively. 
# According to the barplots, the majority of `engienSize` are concentrated between 1 and 3. 
# 10 out of 40 categories in `engienSize` have less than 10 counts. The vehicles with a big `engienSize` are more likely to have less counts. 



## Three candidate models 

# Linear Model
m1 = lm((price)^(1/2) ~ make+model+transmission+fuelType+mpg+engineSize+year*mileage, data=data)


# Smoothing Spline
ss = gam(price ~ make + model + transmission + fuelType + s(mpg,k=15) 
         + s(engineSize,k=15) + s(year,k=5) + s(mileage,k=12) 
         + ti(year, mileage,k=18), data = data)


# Random Forest
rf = randomForest(price~make+model+transmission+fuelType+mpg+engineSize+year+mileage,
                  mtry = 4, ntree=1000, maxnodes= 500, nodesize = 50, sampsize = 50000,
                  data = data, importance=TRUE) 



## outliers & testdata
dtest=read.csv("autoswapper_test.csv")

data=data[data$year>1998,]
data=data[data$year<2021,]
data=data[data$engineSize<5.6,]


Model <- function(dtrain, dtest){
  # Train the model
  fit = randomForest(price~make+model+transmission+fuelType+mpg+engineSize+year+mileage,
                     mtry = 4, ntree=1000, maxnodes= 500, nodesize = 50, sampsize = 50000,
                     data = data, importance=TRUE) 
  pred = predict(fit, newdata=dtest)
  res = data.frame(Id=dtest$testID, price=pred)
  return(res)
} 

res = Model(data, dtest)
write.csv(res, file="username_price_suggestions.csv", row.names=FALSE)



