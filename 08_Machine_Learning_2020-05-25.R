library(tidyverse)
library(dslabs)
library(data.table)
#install.packages("caret")
library(caret)

# Prerequisite Knowledge --------------------------------------------------
data(heights)
head(heights)

# Q1
str(heights)
class(75.000)

# Q2
nrow(heights)

# Q3
heights[777,]

# Q5
max(heights$height)
which.min(heights$height)

# Q6
summary(heights)

# Q7
heights %>% filter(sex == "Male") %>% nrow
812/1050

# Q8
heights %>% filter(height > 78) %>% nrow

# Q9
heights %>% filter(height > 78 & sex == "Female") %>% nrow


# Section 2: Machine Learning Basics --------------------------------------
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Q1
head(dat)
dat %>% filter(type == "inclass") %>% nrow
dat %>% filter(type == "online") %>% nrow
dat %>% filter(sex == "Female") %>% count(type)
26/39 # 0.667 (females inclass)
42/111 # 0.378 (females online)

# Q2
y_hat <- ifelse(x == "inclass", "Female", "Male") %>% 
  factor(levels = levels(y))
mean(y_hat == y)

# Q3
table(y_hat, y)

# Q4
cm <- confusionMatrix(y_hat, y)
cm
sensitivity(y_hat,y)

# Q5
specificity(y_hat,y)

# Q6
nrow(dat[dat$sex == "Female",])/nrow(dat)

# Q7
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
set.seed(2)    
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Q8
head(train)
train %>% count(Species)
accuracy <- lapply(c(colnames(train)[1:4]), function(i){
  cutoff <- seq(min(train[,i]), max(train[,i]), 0.1)
  acc <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(train[,i] > x, "virginica", "versicolor") %>% 
      factor(levels = levels(test$Species))
    mean(y_hat == train$Species)
  })
  acc_feat <- data.frame(cutoff, acc)
})
names(accuracy) <- colnames(train)[1:4]

accuracy$Sepal.Length %>% filter(acc == max(accuracy$Sepal.Length$acc))
accuracy$Sepal.Width %>% filter(acc == max(accuracy$Sepal.Width$acc))
accuracy$Petal.Length %>% filter(acc == max(accuracy$Petal.Length$acc))
accuracy$Petal.Width %>% filter(acc == max(accuracy$Petal.Width$acc))
# best accuracy with petal length of 4.7 (acc = 0.96)

# Q9
y_hat <- ifelse(test$Petal.Length > 4.7, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

# Q10
accuracy_test <- lapply(c(colnames(test)[1:4]), function(i){
  cutoff <- seq(min(test[,i]), max(test[,i]), 0.1)
  acc <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(test[,i] > x, "virginica", "versicolor") %>% 
      factor(levels = levels(test$Species))
    mean(y_hat == test$Species)
  })
  acc_feat <- data.frame(cutoff, acc)
})
names(accuracy_test) <- colnames(test)[1:4]

accuracy_test$Sepal.Length %>% filter(acc == max(accuracy_test$Sepal.Length$acc))
accuracy_test$Sepal.Width %>% filter(acc == max(accuracy_test$Sepal.Width$acc))
accuracy_test$Petal.Length %>% filter(acc == max(accuracy_test$Petal.Length$acc))
accuracy_test$Petal.Width %>% filter(acc == max(accuracy_test$Petal.Width$acc))

# Q11
plot(iris,pch=21,bg=iris$Species)

accuracy$Petal.Length %>% filter(acc == max(accuracy$Petal.Length$acc))
# best accuracy of 0.96 at 4.7
accuracy$Petal.Width %>% filter(acc == max(accuracy$Petal.Width$acc))
# best accuracy of 0.94 at 1.5

y_hat <- ifelse(test$Petal.Length > 4.7 | test$Petal.Width > 1.5, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)


# Part2.2: Conditional probabilities
# Q6
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

# Q7
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

# Q8
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)


# Section 3: Linear regression for prediction, smooting + working  --------

# *Part 1: lin reg ----
# Q1
set.seed(1) # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1)
models <- replicate(100, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x, data = train)
  y_hat <- predict(fit, test)
  sqrt(mean((y_hat - test$y)^2))
})
mean(models)
sd(models)

# Q2
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)

fun_q2 <- function(n){
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  models <- replicate(100, {
    test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    test <- dat[test_index,]
    train <- dat[-test_index,]
    fit <- lm(y ~ x, data = train)
    y_hat <- predict(fit, test)
    sqrt(mean((y_hat - test$y)^2))
  })
  c(mean = mean(models), sd = sd(models))
}

n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
res_q2 <- map(n, fun_q2)

# Q4
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1)
models <- replicate(100, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x, data = train)
  y_hat <- predict(fit, test)
  sqrt(mean((y_hat - test$y)^2))
})
mean(models)
sd(models)

# Q6
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]

y_hat_1 <- predict(lm(y ~ x_1, data = train), test)
rmse_1 <- sqrt(mean((y_hat_1 - test$y)^2))
y_hat_2 <- predict(lm(y ~ x_2, data = train), test)
rmse_2 <- sqrt(mean((y_hat_2 - test$y)^2))
y_hat_1_2 <- predict(lm(y ~ x_1 + x_2, data = train), test)
rmse_1_2 <- sqrt(mean((y_hat_1_2 - test$y)^2))

rmse_1
rmse_2
rmse_1_2

# Q8
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]

y_hat_1 <- predict(lm(y ~ x_1, data = train), test)
rmse_1 <- sqrt(mean((y_hat_1 - test$y)^2))
y_hat_2 <- predict(lm(y ~ x_2, data = train), test)
rmse_2 <- sqrt(mean((y_hat_2 - test$y)^2))
y_hat_1_2 <- predict(lm(y ~ x_1 + x_2, data = train), test)
rmse_1_2 <- sqrt(mean((y_hat_1_2 - test$y)^2))

rmse_1
rmse_2
rmse_1_2

# Logistic regression
# Q1
set.seed(2) #if you are using R 3.5 or earlier
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
dat$train %>% ggplot(aes(x, color = y)) + geom_density()

set.seed(1)
mu_1 <- seq(0, 3, len=25)
dat_q1 <- sapply(seq(1,25), function(i){
  dat_i <- make_data(mu_1 = mu_1[i])
  fit_glm <- glm(y ~ x, data = dat_i$train, family = "binomial")
  p_hat_glm <- predict(fit_glm, dat_i$test, type="response")
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
  cm <- confusionMatrix(y_hat_glm, dat_i$test$y)
  accuracy <- cm$overall["Accuracy"]
  c(accuracy = unname(accuracy), mu_1 = mu_1[i])
})
dat_q1_plot <- transpose(as.data.frame(dat_q1))
colnames(dat_q1_plot) <- c("res", "delta")
dat_q1_plot

ggplot(dat_q1_plot, aes(x = delta, y = res)) +
  geom_point()



# *Part 2: Smooting ----
# Q1
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")
head(dat)
dat_mod <- dat %>% filter(!is.na(deaths)) 

total_time <- max(dat_mod$date) - min(dat_mod$date)
span <- duration(2, units = "months")/total_time
span <- 61/as.numeric(total_time)
fit <- loess(deaths ~ as.numeric(date), span = span, data = dat_mod, degree = 1)

dat_mod %>% mutate(smooth = fit$fitted, day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, deaths, col = year)) +
  geom_point() +
  geom_line(aes(day, smooth), color="red", lwd = 2)

# Q3
library(broom)
library(dslabs)
data(mnist_27)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

qplot(x_2, y, data = mnist_27$train)

head(mnist_27$train)
dat_q3 <- mnist_27$train
fit_q3 <- loess(as.numeric(levels(y)[y]) ~ x_2, degree = 1, span = 0.05, data = dat_q3)

dat_q3 %>% mutate(smooth = fit_q3$fitted) %>%
  ggplot(aes(x_2, y)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(x_2, smooth), col = "red", lwd = 2)
  #geom_smooth(method.args = list(degree = 1))

# correct answer:
mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")

# *Part 3: Matrices ----
# Q6
mnist <- read_mnist()
dat_q6 <- mnist$train$images
ind_q6 <- mnist$train$labels
class(dat_q6)
class(ind_q6)

vec_q6 <- as.vector(dat_q6)
length(vec_q6[vec_q6 > 50 & vec_q6 < 206])/length(vec_q6)
dim(dat_q6[dat_q6 > 50 & dat_q6 < 205])

# proportion of grey pixels per observation
grey <- apply(dat_q6, 1, function(x){
  length(x[x > 50 & x < 206])/length(x)
})
# df w/ numbers (indices) als observations and proportion of grey as values
grey_df <- data.frame(num = as.factor(ind_q6), prop = grey)

grey_df %>% 
  ggplot(aes(num,prop,fill=num)) +
  geom_boxplot()

# correct answer:
mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")



# Section 4: Distances, Ken, Cross-validation, and Generative Mode --------
# *Part 1: Nearest neighbors ----
# 1.1 Distance
# Q1
library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
d <- dist(tissue_gene_expression$x)

# Q2
as.matrix(d)[1,2]
as.matrix(d)[39,40]
as.matrix(d)[73,74]

as.matrix(d)[1,40]
image(as.matrix(d))

# correct answer:
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

# Q3
image(as.matrix(d))

# comprehension check
# Q1
set.seed(1)
data(heights)
test_index <- createDataPartition(heights$sex, times=1,p=0.5,list=FALSE)
q1_test <- heights[test_index,]
q1_train <- heights[-test_index,]
ks <- seq(1, 101, 3)
names(ks) <- ks
knn_q1 <- sapply(ks, function(k){
  knn_fit <- knn3(sex ~ height, data = q1_train, k = k)
  y_hat <- predict(knn_fit, q1_test, type = "class") %>% 
    factor(levels = levels(q1_train$sex))
  F1 <- F_meas(data = y_hat, reference = q1_test$sex)
  F1
})
plot(ks, knn_q1)
q1 <- data.frame(k = ks, F1 = knn_q1)
q1 %>% arrange(desc(F1,k)) %>% head

# Q2
data("tissue_gene_expression")
set.seed(1)
q2_test_index <- createDataPartition(tissue_gene_expression$y, times=1,p=0.5,list=FALSE)
q2_test_x <- tissue_gene_expression$x[q2_test_index,]
q2_test_y <- tissue_gene_expression$y[q2_test_index]
q2_train_x <- tissue_gene_expression$x[-q2_test_index,]
q2_train_y <- tissue_gene_expression$y[-q2_test_index]

q2_ks <- c(1, 3, 5, 7, 9, 11)
#set.seed(1)
q2_acc <- sapply(q2_ks, function(k){
  q2_fit <- knn3(q2_train_x, q2_train_y, k = k)
  q2_yhat <- predict(q2_fit, q2_test_x, type = "class") %>% 
    factor(levels = levels(q2_test$y))
  q2_cm <- confusionMatrix(data = q2_yhat, reference = q2_test_y)
  q2_cm$overall["Accuracy"]
})

data.frame(k = q2_ks, Acc = q2_acc)

# correct answer:
set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})


# *Part 2: Cross validation ----
# 2.1 k-fold cross validation
# Q1
set.seed(1996) #if you are using R 3.5 or earlier
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
fit <- train(x_subset, y, method = "glm")
fit$results

# Q2
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value

# Q3
ind <- which(pvals < 0.01)
length(ind)

# Q4
x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

# Q5
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

# Q7
data("tissue_gene_expression")
str(tissue_gene_expression)

fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn",
             tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fit)
fit$results


# 2.2 bootstrap
# Q1
data(mnist_27)
set.seed(1995) # if R 3.6 or later, set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)
length(indexes$Resample01[indexes$Resample01 == 3])
length(indexes$Resample01[indexes$Resample01 == 4])
length(indexes$Resample01[indexes$Resample01 == 7])

# Q2
is_3 <- sapply(1:10, function(i){
  sum(indexes[[i]] == 3)
})
sum(is_3)
# correct answer:
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

# Q3
y <- rnorm(100, 0, 1)
quantile(y, 0.75)

set.seed(1)
q75 <- replicate(10000, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q75)
sd(q75) # SE of a sample statistic is SD of sampling distribution

# Q4
set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
ind <- createResample(y, 10)
q4 <- sapply(ind, function(ind){
  sample <- y[ind]
  quantile(sample, 0.75)
})
mean(q4)
sd(q4)

# Q5
set.seed(1)
ind <- createResample(y, 10000)
q5 <- sapply(ind, function(ind){
  sample <- y[ind]
  quantile(sample, 0.75)
})
mean(q5)
sd(q5)

# *Part 3: Generative models ----
# Q1
data("tissue_gene_expression")

set.seed(1993) #if using R 3.6 or later set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

q1_data <- data.frame(x = x, y = y)
p_lda <- train(y ~ ., method = "lda", data = q1_data)
p_lda$results

# Q2
p_lda$finalModel
means <- data.frame(genes = names(p_lda$finalModel$means[1,]), cerrebellum = p_lda$finalModel$means[1,], hippocampes = p_lda$finalModel$means[2,])
means %>% gather("tissue", "expr", -genes) %>% 
  mutate(genes = str_split(genes, "x.", simplify = TRUE)[,2]) %>%
  ggplot(aes(x=genes, y=expr, col=tissue)) +
  geom_point() +
  geom_line(aes(x=genes, y=expr, group=tissue)) +
  theme(axis.text.x = element_text(angle= 45, hjust=1))

# Q3
q1_data <- data.frame(x = x, y = y)
p_qda <- train(y ~ ., method = "qda", data = q1_data)
p_qda$results

# Q4
q4_means <- data.frame(genes = names(p_qda$finalModel$means[1,]), 
                       cerrebellum = p_qda$finalModel$means[1,], 
                       hippocampes = p_qda$finalModel$means[2,])
q4_means %>% gather("tissue", "expr", -genes) %>% 
  mutate(genes = str_split(genes, "x.", simplify = TRUE)[,2]) %>%
  ggplot(aes(x=genes, y=expr, col=tissue)) +
  geom_point() +
  geom_line(aes(x=genes, y=expr, group=tissue)) +
  theme(axis.text.x = element_text(angle= 45, hjust=1))

# Q5
q5_p_lda <- train(y ~ ., method = "lda", preProcess = "center", data = q1_data)
q5_p_lda$results
means <- data.frame(genes = names(q5_p_lda$finalModel$means[1,]), 
                    cerrebellum = q5_p_lda$finalModel$means[1,], 
                    hippocampes = q5_p_lda$finalModel$means[2,])
means %>% gather("tissue", "expr", -genes) %>% 
  mutate(genes = str_split(genes, "x.", simplify = TRUE)[,2]) %>%
  ggplot(aes(x=genes, y=expr, col=tissue)) +
  geom_point() +
  geom_line(aes(x=genes, y=expr, group=tissue)) +
  theme(axis.text.x = element_text(angle= 45, hjust=1))

# Q6
set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

q6_data <- data.frame(x = x, y = y)
q6_lda <- train(y ~ ., method = "lda", preProcess = "center", data = q6_data)
q6_lda$results
confusionMatrix(q6_lda)



# Section 5: Classification with >2 Classes + Caret package ---------------

# *Part 1: Classification with >2 classes ----
# Q1
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1) #set.seed(1, sample.kind = "Rounding") if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat) 

# Q2
plot(fit)

# Q3
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

# Q4
library(randomForest)
fit <-  randomForest(y ~ x, data = dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

# Q5
plot(fit)

# Q6
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")


# *Part 2: Caret package ----
# Q1
library(rpart)
data("tissue_gene_expression")
dat <- as.data.frame(tissue_gene_expression)
modelLookup("rpart")
set.seed(1991)
train_q1 <- train(y ~ ., data = dat, method = "rpart", 
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
train_q1$results %>% 
  ggplot(aes(x = cp, y = Accuracy), highlight = TRUE) + 
  geom_line() + 
  geom_point()

# Q2
set.seed(1991)
train_q2 <- train(y ~ ., data = dat, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                  control = rpart.control(minsplit = 0))
ggplot(train_q2, highlight = TRUE)
confusionMatrix(train_q2)

# Q3
plot(train_q2$finalModel, margin = 0.1)
text(train_q2$finalModel, cex = 0.75)

# Q4
modelLookup("rf")
set.seed(1991)
train_q4 <- train(y ~ ., data = dat, method = "rf",
                  tuneGrid = data.frame(mtry = seq(50, 200, 25)),
                  nodesize = 1)
ggplot(train_q4, highlight = TRUE)

# Q5
imp <- varImp(train_q4)
imp

# Q6
tree_terms <- as.character(unique(train_q2$finalModel$frame$var[!(train_q2$finalModel$frame$var == "<leaf>")]))
tree_terms

imp <- data.frame(gene = row.names(imp$importance), imp = imp$importance$Overall)
imp <- imp %>% arrange(desc(imp))
imp %>% filter(gene %in% tree_terms) 
which(imp$gene == "x.CFHR4")


# *Part 3: Titanic exercises ----
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Q1
set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
titanic_test <- titanic_clean[test_index,]
nrow(titanic_test)
titanic_train <- titanic_clean[-test_index,]
nrow(titanic_train)
mean(titanic_train$Survived == 1)

# Q2
set.seed(3)
rdm <- sample(c(0,1), nrow(titanic_test), replace = TRUE)
confusionMatrix(as.factor(rdm), titanic_test$Survived)$overall[["Accuracy"]]
mean(rdm == titanic_test$Survived)

# Q3
# a
sum(titanic_train$Sex == "female" & titanic_train$Survived == 1)/sum(titanic_train$Sex == "female")
sum(titanic_train$Sex == "male" & titanic_train$Survived == 1)/sum(titanic_train$Sex == "male")

# b
titanic_test %>%
  mutate(surv_sex = case_when(
    Sex == "female" ~ 1,
    Sex == "male" ~ 0
  )) %>%
  summarize(acc = mean(surv_sex == Survived))

model_sex <- as.factor(ifelse(titanic_test$Sex == "female", 1, 0))
mean(model_sex == titanic_test$Survived)

# Q4
# a
titanic_train %>%
  group_by(Pclass) %>%
  summarize(surv = mean(Survived == 1))

# b
model_class <- as.factor(ifelse(titanic_test$Pclass == 1, 1, 0))
mean(model_class == titanic_test$Survived)

titanic_test %>%
  mutate(surv_class = case_when(
    Pclass == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  summarize(acc = mean(surv_class == Survived))

# c
titanic_train %>%
  group_by(Pclass, Sex) %>%
  summarize(surv = mean(Survived == 1))

# d
model_comb <- as.factor(ifelse(titanic_test$Pclass %in% c(1,2) & titanic_test$Sex == "female", 1, 0))
mean(model_comb == titanic_test$Survived)

# Q5
# a
conf_sex <- confusionMatrix(model_sex, titanic_test$Survived)
conf_class <- confusionMatrix(model_class, titanic_test$Survived)
conf_comb <- confusionMatrix(model_comb, titanic_test$Survived)

conf_sex$byClass[["Sensitivity"]]
conf_class$byClass[["Sensitivity"]]
conf_comb$byClass[["Sensitivity"]]

conf_sex$byClass[["Specificity"]]
conf_class$byClass[["Specificity"]]
conf_comb$byClass[["Specificity"]]

conf_sex$byClass[["Balanced Accuracy"]]
conf_class$byClass[["Balanced Accuracy"]]
conf_comb$byClass[["Balanced Accuracy"]]

# Q6
F_meas(model_sex, titanic_test$Survived)
F_meas(model_class, titanic_test$Survived)
F_meas(model_comb, titanic_test$Survived)

# Q7
set.seed(1)
model_q7_lda <- train(Survived ~ Fare, data = titanic_train, method = "lda")
confusionMatrix(predict(model_q7_lda, titanic_test), titanic_test$Survived)

set.seed(1)
model_q7_qda <- train(Survived ~ Fare, data = titanic_train, method = "qda")
confusionMatrix(predict(model_q7_qda, titanic_test), titanic_test$Survived)

# Q8
set.seed(1)
model_q8_1 <- train(Survived ~ Age, data = titanic_train, method = "glm")
mean(predict(model_q8_1, titanic_test) == titanic_test$Survived)

set.seed(1)
model_q8_2 <- train(Survived ~ Sex + Age + Pclass + Fare, data = titanic_train, method = "glm")
mean(predict(model_q8_2, titanic_test) == titanic_test$Survived)

set.seed(1)
model_q8_3 <- train(Survived ~ ., data = titanic_train, method = "glm")
mean(predict(model_q8_3, titanic_test) == titanic_test$Survived)

# Q9
# a
set.seed(6)
model_q9 <- train(Survived ~ ., 
                  data = titanic_train, 
                  method = "knn", 
                  tuneGrid = data.frame(k = seq(3, 51, 2)))
model_q9$bestTune

# b
ggplot(model_q9, highlight = TRUE)

# c
mean(predict(model_q9, titanic_test) == titanic_test$Survived)

# Q10
set.seed(8)
model_q10 <- train(Survived ~ .,
                   data = titanic_train,
                   method = "knn",
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   trControl = trainControl(method = "cv", number = 10, p = 0.9))
ggplot(model_q10, highlight = TRUE)
model_q10$bestTune
mean(predict(model_q10, titanic_test) == titanic_test$Survived)


# Q11 
# a
set.seed(10)
model_q11 <- train(Survived ~ .,
                   data = titanic_train,
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
model_q11$bestTune
mean(predict(model_q11, titanic_test) == titanic_test$Survived)
plot(model_q11$finalModel, margin = 0.1)
text(model_q11$finalModel, cex = 0.75)

# Q12
set.seed(14)
model_q12 <- train(Survived ~ .,
                   data = titanic_train,
                   method = "rf",
                   tuneGrid = data.frame(mtry = seq(1, 7)),
                   ntree = 100)
model_q12$bestTune
mean(predict(model_q12, titanic_test) == titanic_test$Survived)
varImp(model_q12)



# Section 6: Model fitting & recommendation systems -----------------------
# *Part 1: Case study MNIST ----
# Model fitting
# Q1
library(caret)
library(dslabs)
set.seed(1)
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# Q2
q2_pred <- map_df(fits, function(fit){
  predict(fit, mnist_27$test)
})

names(q2_pred) <- models

length(mnist_27$test$y)
dim(q2_pred)

# Q3
q3_acc <- sapply(q2_pred, function(y_hat)
  confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]])
mean(q3_acc)

# Q4
head(q2_pred)
q4_ens <- as.factor(apply(q2_pred, 1, function(x) ifelse(sum(match(x, "7", nomatch = 0)) > 5, "7","2")))
mean(q4_ens == mnist_27$test$y)
# given answer:
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

# Q5
q3_acc[which(q3_acc > 0.815)]

# Q6
q6_acc <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(q6_acc)

# Q7
models_ens <- models[which(q6_acc >= 0.8)]
q7_ens <- as.factor(apply(q2_pred[,models_ens], 1, function(x) ifelse(sum(match(x, "7", nomatch = 0)) >= 3, "7","2")))
mean(q7_ens == mnist_27$test$y)
mean(q3_acc[models_ens])


# Dimension reduction
# Q1
library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
pca <- prcomp(tissue_gene_expression$x)
data.frame(pca$x) %>%
  mutate(tissue_type = str_match(rownames(.), "(\\D*)_(\\d+)")[,2]) %>% 
  ggplot(aes(x = PC1, y = PC2, col = tissue_type)) +
  geom_point()
# given answer:
data.frame(pc_1 = pca$x[,1], pc_2 = pca$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Q2
data.frame(tissue_gene_expression$x) %>%
  mutate(avg = rowMeans(.)) %>% head
obs_means <- rowMeans(data.frame(tissue_gene_expression$x))
cor (obs_means, pca$x[,1])

data.frame(means = obs_means, PC1 = pca$x[,1] , tissue = tissue_gene_expression$y) %>%
  ggplot(aes(x = means, y = PC1, col = tissue)) +
  geom_point()

# Q3
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Q4
data.frame(pc_7 = pc$x[,7], tissue = tissue_gene_expression$y) %>%
  group_by(tissue) %>%
  summarize(median = median(pc_7)) %>%
  arrange(desc(median))

data.frame(pc_7 = pc$x[,7], tissue = tissue_gene_expression$y) %>%
  ggplot(aes(x = tissue, y = pc_7)) +
  geom_boxplot()
# given answer:
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

# Q5
summary(pca)
# given answer:
plot(summary(pc)$importance[3,])


# *Part 2: Recommendation systems ----
library(lubridate)
data("movielens")

# Q1
head(movielens)
movielens %>%
  count(movieId, year) %>% 
  group_by(year) %>%
  summarize(median = median(n)) %>%
  arrange(desc(median))
# given answer:
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Q2
class(movielens$timestamp) = c('POSIXt','POSIXct')
q2_rt_year <- movielens %>% 
  mutate(rate_year = year(timestamp)) %>%
  filter(rate_year <= 2018 & year >= 1993) %>%
  group_by(movieId, rate_year) %>%
  summarize(n_year = n()) %>% 
  left_join(select(movielens, movieId, year)) %>% 
  group_by(movieId) %>% 
  summarize(sum_rt = sum(n_year), years_rt = 2018-first(year)+1, avg_rt_year = sum_rt/years_rt) %>%
  arrange(desc(avg_rt_year)) %>% head(25)
q2_avg_rt <- q2_rt_year %>%
  left_join(select(movielens,movieId, rating)) %>%
  group_by(movieId) %>%
  summarize(avg_rt_overall = mean(rating))
movielens %>%
  filter(title == "Shawshank Redemption, The") %>%
  left_join(q2_avg_rt) %>% head(1)
movielens %>%
  mutate(rate_year = year(timestamp)) %>%
  filter(title == "Forrest Gump" & rate_year <= 2018) %>% 
  summarize(avg_rt_year = n()/(2018-first(year)))
  
#given answer:
q2_movies <- movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

# Q3
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(x = rating, y = rate)) +
  geom_point()

# Q5
movielens <- mutate(movielens, date = as_datetime(timestamp))

# Q6
movielens %>%
  mutate(week = round_date(date, unit = "week")) %>% 
  group_by(week) %>%
  summarize(rating =mean(rating)) %>% 
  ggplot(aes(x = week, y = rating)) +
  geom_point() +
  geom_smooth()

# Q7
movielens %>%
  mutate(week = round_date(date, unit = "day")) %>% 
  group_by(week) %>%
  summarize(rating =mean(rating)) %>% 
  ggplot(aes(x = week, y = rating)) +
  geom_point() +
  geom_smooth()

# Q8
movielens %>%
  group_by(genres) %>%
  summarize(n = n(), rating_avg = mean(rating), rating_se = sd(rating)/sqrt(n)) %>%
  filter(n >= 1000) %>%
  ggplot(aes(reorder(genres, rating_avg), rating_avg)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = rating_avg - rating_se, ymax = rating_avg + rating_se)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# *Part 3: Regularization ----
# Regularization
options(digits=7)
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1) 
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% 
  mutate(score = sapply(scores, mean), 
         score_se = sapply(scores, function(x) sd(x)/sqrt(length(x))))

# Q1
top_ten <- schools %>% arrange(desc(score)) %>% head(10)

# Q2
median(schools$size)
median(top_ten$size)

# Q3
schools %>% arrange(score) %>% head(10) %>%
  summarize(median = median(size))

# Q4
top_ten_true <- schools %>% arrange(desc(quality)) %>% head(10)
schools %>%
  mutate(topten = as.factor(ifelse(id %in% top_ten_true$id, 1, 0))) %>% #arrange(desc(quality)) %>% head(11)
  ggplot(aes(x = score, y = size, col = topten)) +
  geom_point()
schools %>% 
  arrange(size) %>%
  head(10)
schools %>% 
  arrange(desc(size)) %>%
  head(10)

# Q5
overall <- mean(sapply(scores, mean))
alpha <- 25
schools %>% 
  mutate(b_i = sapply(scores, function(scores) sum(scores-overall)/(length(scores)+alpha)),
         scores_reg = overall + b_i) %>%
  arrange(desc(scores_reg)) %>%
  head(10)

# Q6
alpha <- seq(10, 250)
rmse <- sapply(alpha, function(alpha){
  estimate <- sapply(scores, function(scores) overall+sum(scores-overall)/(length(scores)+alpha))
  rmse <- sqrt((1/1000)*sum((schools$quality - estimate)^2))
  #rmse <- RMSE(schools$quality, estimate)
  rmse
})
data.frame(alpha = alpha, rmse = rmse) %>%
  ggplot(aes(alpha, rmse)) +
  geom_point() 
data.frame(alpha = alpha, rmse = rmse) %>% arrange(rmse) %>% head(1)

# Q7
schools %>%
  mutate(scores_reg = sapply(scores, function(scores) overall+sum(scores-overall)/(length(scores)+135))) %>%
  arrange(desc(scores_reg)) %>%
  head(10)

# Q8
rmse <- sapply(alpha, function(alpha){
  estimate <- sapply(scores, function(scores) sum(scores)/(length(scores)+alpha))
  #rmse <- sqrt(mean(estimate^2))
  rmse <- RMSE(schools$quality, estimate)
  rmse
})
data.frame(alpha = alpha, rmse = rmse) %>%
  ggplot(aes(alpha, rmse)) +
  geom_point() 
alpha[which.min(rmse)]


# Matrix Factorization -> singluar value decomposition
set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

# Q1
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

# Q2
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Q3
s <- svd(y)
names(s)
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- apply(y, 2, function(x) sum(x^2))
yv <- y %*% s$v
ss_yv <- apply(yv, 2, function(x) sum(x^2))
sum(ss_y)
sum(ss_yv)

# Q4
plot(ss_y)
plot(ss_yv)

# Q5
plot(sqrt(ss_yv), s$d)

# Q6
ss_yv_var <- ss_yv/sum(ss_yv)
sum(ss_yv_var[1:3])

# Q7
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

# Q8
avg_stu <- rowMeans(y)
head(sweep(s$u, 2, s$d, FUN = "*")[,1])
head(-s$u[,1]*s$d[1])
plot(avg_stu, sweep(s$u, 2, s$d, FUN = "*")[,1])

# Q9
my_image(s$v)

# Q10
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)

# Q11
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)

# Q12
resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)

#Q13
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))


# comprehension check: clustering
data("tissue_gene_expression")

# Q1
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

# Q2
h <- hclust(d)
plot(h, cex = 0.65, main = "", xlab = "")

# Q3
k_cluster <- replicate(10, {
  k <- kmeans(tissue_gene_expression$x, centers = 7)
  k$cluster
})
head(k_cluster)
q3 <- cbind(k_cluster, as.integer(tissue_gene_expression$y))
rownames(q3) <- tissue_gene_expression$y
head(q3)
heatmap(q3, col = RColorBrewer::brewer.pal(11, "Spectral"))
data.frame(data.frame(k_cluster), tissue = tissue_gene_expression$y) %>%
  filter(tissue == "liver") 
# given answer:
table(k_cluster[,1], tissue_gene_expression$y)

# Q4
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)



# Final assessment --------------------------------------------------------

options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(RColorBrewer)
library(reshape2)
data(brca)

# Q1
head(brca)
length(brca$y[brca$y == "M"])/length(brca$y)
which.max(colMeans(brca$x))
which.min(colSds(brca$x))

# Q2
x_sc <- sweep(brca$x, 2, STATS = colMeans(brca$x), FUN = "-")
x_sc <- sweep(x_sc, 2, STATS = colSds(x_sc), FUN = "/")
sd(x_sc[,1])
median(x_sc[,1])

# Q3
d <- as.matrix(dist(x_sc))
dim(d)
head(brca$y)
benign <- which(brca$y == "B")
malign <- which(brca$y == "M")
mean(d[1,2:length(benign)])
mean(d[1,malign])

# Q4
q4_melt <- melt(x_sc)
ggplot(q4_melt, aes(Var1, Var2, fill=value)) +
  geom_tile() 
# given answer:
d_features <- dist(t(x_sc))
heatmap(as.matrix(d_features))#, labRow = NA, labCol = NA)

# Q5
h <- hclust(d_features)
groups <- cutree(h, k = 5)
groups_names <- sapply(c(1:5), function(i){
  names(groups)[groups == i]
})
groups_names
# given answer:
split(names(groups), groups)

# Q6
pca <- prcomp(x_sc)
pca_sum <- summary(pca)
cumsum(pca_sum$importance[2,])

# Q7
ggplot(data.frame(pca$x), aes(PC1, PC2, col = brca$y)) +
  geom_point()

# Q8
data.frame(data.frame(pca$x[,1:10]), type = brca$y) %>% 
  gather(pc, val, -type) %>% 
  ggplot(aes(x = pc, y = val, col = type)) +
    geom_boxplot()

# Q9
set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_sc[test_index,]
test_y <- brca$y[test_index]
train_x <- x_sc[-test_index,]
train_y <- brca$y[-test_index]

length(train_y[test_y == "B"])/length(train_y)
length(test_y[test_y == "B"])/length(test_y)
# given answer:
mean(train_y == "B")
mean(test_y == "B")

# Q10
# a
set.seed(3)
k <- kmeans(train_x, 2)

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

q10_pred <- as.factor(predict_kmeans(test_x, k))
levels(q10_pred) <- levels(test_y)
confusionMatrix(q10_pred, test_y)
q10_acc <- mean(q10_pred == test_y)

# b
test_b <- which(test_y == "B")
test_m <- which(test_y == "M")
mean(q10_pred[test_b] == test_y[test_b])
mean(q10_pred[test_m] == test_y[test_m])
# given answer:
sensitivity(q10_pred, test_y, positive = "B")
sensitivity(q10_pred, test_y, positive = "M")

# Q11
q11_fit <- train(train_x, train_y, method = "glm")
q11_pred <- predict(q11_fit, test_x)
confusionMatrix(q11_pred, test_y)$overall
q11_acc <- mean(q11_pred == test_y)

# Q12
q12_lda <- train(train_x, train_y, method = "lda")
q12_lda_pred <- predict(q12_lda, test_x)
q12_lda_acc <- mean(q12_lda_pred == test_y)
q12_qda <- train(train_x, train_y, method = "qda")
q12_qda_pred <- predict(q12_qda, test_x)
q12_qda_acc <- mean(q12_qda_pred == test_y)

# Q13
#install.packages("gam")
library(gam)
set.seed(5)
q13_loess <- train(train_x, train_y, method = "gamLoess")
q13_pred <- predict(q13_loess, test_x)
q13_acc <- mean(q13_pred == test_y)

# Q14
set.seed(7)
q14_knn <- train(train_x, train_y, 
                 method = "knn",
                 tuneGrid = data.frame(k = seq(3, 21, 2)))
q14_knn$bestTune
q14_pred <- predict(q14_knn, test_x)
q14_acc <- mean(q14_pred == test_y)

# Q15
# a
set.seed(9)
q15_rf <- train(train_x, train_y, 
                method = "rf", 
                tuneGrid = data.frame(mtry = c(3)), 
                importance = TRUE)
q15_rf$bestTune
q15_pred <- predict(q15_rf, test_x)
q15_acc <- mean(q15_pred == test_y)
confusionMatrix(q15_pred, test_y)$overall
q15_varimp <- varImp(q15_rf)


# b
q15_varimp

# Q16
# a
preds <- data.frame(cbind(q10_pred, q11_pred, q12_lda_pred, q12_qda_pred, q13_pred, q14_pred, q15_pred))
head(preds)
pred_ens <- preds %>%
  mutate(avg = as.factor(ifelse(rowSums(.) > 10, "M", "B"))) %>% 
  .$avg
levels(pred_ens) <- levels(test_y)
acc_pred <- mean(pred_ens == test_y)
# given answer:
ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

# b
data.frame(method = c("k-means", "log reg", "LDA", "QDA", "loess", "knn", "rf", "ensemble"),
           acc = c(q10_acc, q11_acc, q12_lda_acc, q12_qda_acc, q13_acc,
                     q14_acc, q15_acc, acc_pred))

