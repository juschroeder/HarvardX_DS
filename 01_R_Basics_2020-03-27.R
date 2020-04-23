#install.packages("dslabs")
library(tidyverse)
library(dslabs)



# Section 1: R Basics, functions, and data types --------------------------

data("murders")
class(murders)
str(murders)
head(murders)
names(murders)

pop <- murders$population
length(pop)
class(pop)

class(murders$state)

z <- 3 == 2
class(z)

class(murders$region)
levels(murders$region)


# Assessment
# Q1: quadratic function
a <- 2
b <- -1
c <- -4

# positive quadratic function
(-b + sqrt(b^2 - 4*a*c))/(2*a)

# negative quadratic function
(-b - sqrt(b^2 - 4*a*c))/(2*a)


# Q2: log
log(1024,base=4)


# Q3 movielens dataset
data(movielens)
# a. rows
nrow(movielens)
# b. cols
ncol(movielens)
# c. class title
class(movielens$title)
# d. class genres
class(movielens$genres)


# Q4. n of levels in genres
length(levels(movielens$genres))




# Section 2: Vectors, sorting ---------------------------------------------

# Q1 rank/sort/order
x <- c(2, 43, 27, 96, 18)

sort(x)
rank(x)
order(x)

# Q2
min(x)
which.min(x)
max(x)
which.max(x)

# Q3
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

df <- data.frame(name, distance, time)
df

# 1. How many hours did Olivia run?
time[4]/60
# 2. What was Mandi's speed in miles per hour?
distance[1]/(time[1]/60)
# 3. Which runner had the fastest speed
speed <- distance/(time/60)
df <- cbind(df, speed)
df



# Section 3: Data wrangling -----------------------------------------------
library(dslabs)
data(heights)
options(digits = 3)
head(heights)

# Q1: How many individuals are above average height
avg <- mean(heights$height)
heights %>% filter(height > avg) %>% nrow()

# Q2: above avg height + female
heights %>% filter(height > avg & sex == "Female") %>% nrow()

# Q3: proportion of females in dataset
mean(heights$sex == "Female")

# Q4
# a: minimum height
min(heights$height)
# b: index
which.min(heights$height)
# c: sex
heights[which.min(heights$height),]

# Q5
# a: max height
max(heights$height)
# b: integers b/w min and max height
x <- 51:82
# c: how many integers not in dataset
sum(!(x %in% heights$height))

# Q6: add column of heights in cm (1 in = 2.54 cm) and save in heights2
heights2 <- heights %>% mutate(ht_cm = height*2.54)
head(heights2)
# a: height of 18th individual
heights2[18,]
# b: mean height in cm
mean(heights2$ht_cm)

# Q7
# a: how many females
bla <- heights2 %>% filter(sex == "Female") 
nrow(bla)
# b: mean height of females
mean(bla$ht_cm)


library(dslabs)
data(olive)
head(olive)
# Q8
plot(olive$palmitic, olive$palmitoleic)

# Q9
hist(olive$eicosenoic)

# Q10
boxplot(palmitic ~region, data=olive)


# Section 4: Final assessment ---------------------------------------------

# Q1:
data("heights")
head(heights)
heights %>% mutate(gender = ifelse(sex == "Female",1,2)) %>% summarize(sum= sum(gender)) 

# Q2:
heights %>% mutate(height = ifelse(height > 72,height,0)) %>% summarize(mean = mean(height)) 

# Q3:
inches_to_ft <- function(n){
  n/12
}
inches_to_ft(144)
heights %>% mutate(height_in = inches_to_ft(height)) %>% filter(height_in < 5) %>% nrow()

# Q4:
any(TRUE,TRUE,TRUE)
any(TRUE,TRUE,FALSE)
any(FALSE,FALSE,FALSE)
all(TRUE,TRUE,TRUE)
all(TRUE,TRUE,FALSE)
all(FALSE,FALSE,FALSE)

# Q5:
# define a vector of length m
m <- 10
f_n <- vector(length = m)

# make a vector of factorials
_________{
  f_n[n] <- factorial(n)
}

# inspect f_n
f_n




