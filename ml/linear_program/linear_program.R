#install.packages('Hmisc')
library(ggplot2)
longevity <- read.csv('linear_program/data/longevity.csv')

testData <- data.frame(data = rnorm(1000))
ggplot(data = testData, aes(x = data)) + 
  #geom_histogram() + 
  geom_density()


heights_weights <- read.csv("linear_program/data/01_heights_weights_genders.csv")

ggplot(data = heights_weights, aes(x = Height, y = Weight)) + 
  geom_point() +
  geom_smooth(methods = "lm")

hw <- lm(Height~Weight, data = heights_weights)
predict(hw, 60)
xishu <- coef(hw)
r <- xishu[1] + 60*xishu[2]


####### 网站排名前1000的网站 #######
#注意 logp 可以避免对 0 取对数
top.1000.sites <- read.csv("linear_program/data/top_1000_sites.tsv", 
                           sep = '\t', stringsAsFactors = FALSE)

ggplot(data = top.1000.sites, aes(x = PageViews , y = UniqueVisitors)) +
  geom_point()

ggplot(data = top.1000.sites, aes(x = PageViews)) +
  geom_density()

ggplot(data = top.1000.sites, aes(x = log(PageViews))) +
  geom_density()

ggplot(data = top.1000.sites, aes(x = log(PageViews) , y = log(UniqueVisitors))) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)


pv.lift <- lm(log(PageViews) ~ log(UniqueVisitors), data = top.1000.sites)
coef(pv.lift)
residuals(pv.lift)
summary(pv.lift)


#####GAM Generalized Addictive Model ####
set.seed(1)
x <- seq(-10, 10, by = 0.01)
y <- 1 - x^2 + rnorm(length(x), 0, 5)
ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(se = FALSE)

####多项式拟合，多项式拟合####
set.seed(1)
x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)
df <- data.frame( X = x, Y = y)
ggplot(data = df, aes(x = X, y = Y)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

summary(lm(Y ~ X, data = df))


#### 文本回归 #####
ranks <- read.csv(file = 'linear_program/data/oreilly.csv', stringsAsFactors = FALSE)
library('tm')
documents <- data.frame(Text = ranks$Long.Desc.)
row.names(documents) <- 1:nrow(documents)
corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, PlainTextDocument)
dtm <- DocumentTermMatrix(corpus)
x <- as.matrix(dtm)

y <- rev(1:100)


set.seed(1)
#install.packages("glmnet")
library(glmnet)
row.names(x) <- 1:nrow(x)
#下面记录表现评分
performance <- data.frame()

for (lambda in c(0.1, 0.25, 0.5, 1, 2, 5)) {
  for (i in 1:50) {
    indices <- sample(1:100, 80)
    training.x <- x[indices, ]
    training.y <- y[indices]
    test.x <- x[-indices, ]
    test.y <- y[-indices]
    glm.fit <- glmnet(training.x, training.y)
    predicted.y <- predict(glm.fit, test.x, s = lambda)
    rmse <- sqrt(mean((predicted.y - test.y) ^ 2))
    performance <- rbind(performance,
                         data.frame(Lambda = lambda,
                                    Iteration = i,
                                    RMSE = rmse))
  }
  
}

ggplot(performance, aes(x = Lambda, y = RMSE)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point')


##### 使用logit回归预测书本排名能否进入前50 #####
y <- rep(0:1, 1, each = 50)
re.fit <- glmnet(x, y, family = 'binomial')
predict(re.fit, newx = x, s = 0.001)

performance <- data.frame()
for (i in 1:250) {
  #进行250次预测，每次都是取不同的训练样本
  indices <- sample(1:100, 80)
  training.x <- x[indices, ]
  training.y <- y[indices]
  test.x <- x[-indices, ]
  test.y <- y[-indices]
  for (lambda in c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.5, 0.1)) {
    lambda <- 0.0001
    #对上面这么多的参数，计算每一个参数预测得到的模型交叉验证的误差
    glm.fit <- glmnet(training.x, training.y, family = 'binomial')
    predicted.y <- ifelse(predict(glm.fit, test.x, s = lambda) > 0, 1, 0)
    error.rate <- length(predicted.y[which(predicted.y!= test.y)]) / length(test.y)
    performance  <- rbind (performance, 
                           data.frame(
                             Lambda = lambda,
                             Iteration = i,
                             ErrorRate = error.rate
                             ))
  }
}

ggplot(performance, aes(x = Lambda, y = ErrorRate)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point')
