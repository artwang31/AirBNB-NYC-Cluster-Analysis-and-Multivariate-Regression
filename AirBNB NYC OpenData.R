# AirBNB NYC Open Data
library(tidyverse)
library(data.table)

# UPLOADING DATASET
airbnb <- fread(paste0("AirBNB_NYC_2019.csv"), header = T, stringsAsFactors = F, data.table = T)
airbnb$borough <- airbnb$neighbourhood_group

# VARIABLE SELECTION
airbnb$borough <- as.factor(as.character(airbnb$borough))
airbnb$neighbourhood <- as.factor(as.character(airbnb$neighbourhood))
airbnb$room_type <- as.factor(as.character(airbnb$room_type))
airbnb <- select(airbnb, 'host_id', 'neighbourhood','room_type','price','number_of_reviews','borough')         
colnames(airbnb)
str(airbnb)
# [1] "host_id" "neighbourhood" "room_type" "price" "number_of_reviews" "borough" 
  
# STATEN ISLAND KMEANS CLUSTER ANALYSIS
staten <- airbnb[borough == 'Staten Island']
staten <- staten[complete.cases(staten),]
sum(sapply(staten, is.infinite))
str(staten)
unique(staten$neighbourhood) # 43 neighbourhoods

# DIMENSIONALITY REDUCTION
staten <- staten %>% filter(price < 300)

# VISUALIZATION OF ALL < 300
library(ggplot2)
ggplot(staten, aes(number_of_reviews, price, color = room_type)) +
  geom_point(alpha = 1, size = 1) +
  scale_x_continuous() +
  scale_y_continuous()

# KMEANS ALGORITHIM FOR PRICE AND NUMBER OF REVIEWS
set.seed(123)
staten_subset <- staten[,4:5]
scaled_data <- as.matrix(scale(staten_subset))
staten_cluster <- kmeans(scaled_data, centers = 10, nstart = 100, iter.max = 15)
print(staten_cluster)
# ELBOW METHOD, COMPUTE AND PLOT FOR K = 2 TO K = 15
set.seed(123)
k.max <- 20
elbow <- sapply(1:k.max, function(k)
{kmeans(scaled_data, centers = 15, nstart = 50)$tot.withinss})
plot(1:k.max, elbow,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# VISUALIZATION OF CLUSTERS
library(cluster)
clusplot(scaled_data, staten_cluster$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0,
         main = 'STATEN ISLAND CLUSTER',
         xlab = 'NUMBER OF REVIEWS',
         ylab = 'PRICE PER NIGHT')


# ELBOW METHOD
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

##
# MANHATTAN
##
# UNDERSTANDING DATA
which(is.na(airbnb)) # no NAs
manhattan <- airbnb[borough == 'Manhattan']
manhattan <- manhattan[complete.cases(manhattan),]
sum(sapply(manhattan, is.infinite))
str(manhattan)
unique(manhattan$neighbourhood) # 32 neighbourhoods 

# DIMENSIONALITY REDUCTION
manhattan <- manhattan %>% filter(price < 1000)
scale(manhattan[,4:5])
# VISUALIZING DATA
ggplot(manhattan, aes(number_of_reviews, price, color = room_type)) +
  geom_point(size = 0.5, alpha = 0.6) +
  scale_x_continuous() +
  scale_y_continuous()

# REGRESSION MODEL PRICE AND NUMBER OF REVIEWS
library(caTools)
set.seed(123)
split <- sample.split(manhattan$price, SplitRatio = 0.8)
training_set <- subset(manhattan, split == TRUE)
test_set <- subset(manhattan, split == FALSE)

# APPLYING REGRESSION FORMULA
regressor <- lm(formula = price ~ number_of_reviews,
               data = training_set)
regressor1 <- lm(formula = price ~ number_of_reviews,
                data = test_set)
regressor2 <- lm(formula = price ~ number_of_reviews,
                 data = manhattan)
glm_regressor <- glm(formula = price ~ number_of_reviews + room_type,
                 data = manhattan)

# PREDICTING TEST RESULTS
prediction <- predict(regressor, newdata = test_set)
prediction

# VISUALIZING TRAINING SET RESULTS
legend_title <- "Room Type"
training_set %>%
  ggplot() +
    geom_boxplot(aes(x = number_of_reviews, y = price, color = training_set$room_type), 
               alpha = 0.7, size = .25) +
    geom_line(aes(x = number_of_reviews, y = predict(regressor2, newdata = training_set)), color = 'blue') +
    ggtitle('Price vs Number of Reviews (Training set)') +
    xlab('Number of Reviews') +
    ylab('Price') +
    labs(color = 'Room Type')

# VISUALIZING TEST RESULTS
test_set %>%
  ggplot() +
  geom_jitter(aes(x = number_of_reviews, y = price, color = test_set$room_type), 
             alpha = 0.7, size = .5, show.legend = TRUE) +
  geom_line(aes(x = number_of_reviews, y = predict(regressor2, newdata = test_set)),
            colour = 'blue') +
  ggtitle('Price vs Number of Reviews (Test set)') +
  xlab('Number of Reviews') +
  ylab('Price') +
  labs(color = 'Room Type')

# PREDICTS PRICE BASED ON REVIEWS
pred_price <- data.frame(number_of_reviews = c(25,50,235,6,14,307)) 
prediction <- predict(regressor, pred_price)
prediction

