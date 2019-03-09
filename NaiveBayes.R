# Libraries
#install.packages("naviebayes")
#install.packages("naviebayes", dependencies=TRUE, repos='http://cran.rstudio.com/')
#library(na)
install.packages("e1071", repos = "https://cran.rstudio.com")
library(e1071)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("psych")
library(psych)
install.packages("caret")
library(caret)

# Data
ndata <- read.csv(file.choose(), header = T)
str(ndata)
xtabs(~admit+rank, data = ndata)

ndata$rank <- as.factor(ndata$rank)
ndata$admit <- as.factor(ndata$admit)
str(ndata)
# Visualization
pairs.panels(ndata[-1])
ndata %>%
         ggplot(aes(x=admit, y=gpa, fill = admit)) +
         geom_boxplot() +
         ggtitle("Box Plot")

ndata %>% ggplot(aes(x=gpa, fill = admit)) +
         geom_density(alpha=0.8, color= 'black') +
         ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(ndata), replace = T, prob = c(0.8, 0.2))
train <- ndata[ind == 1,]

test <- ndata[ind == 2,]

# Naive Bayes Model
model <- naiveBayes(admit ~ ., data = train, usekernel = T)
model

train %>%
         filter(admit == "1") %>%
         summarise(mean(gre), sd(gre))
install.packages("Caret", repos="https://cran.rstudio")
library(caret)
#plot(model)

# Predict
p <- predict(model, train)
head(cbind(p, train))

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$admit))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$admit))
1 - sum(diag(tab2)) / sum(tab2)
