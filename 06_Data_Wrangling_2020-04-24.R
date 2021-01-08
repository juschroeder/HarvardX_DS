library(tidyverse)
library(data.table)


# Section 1: Data import --------------------------------------------------

# Q14
url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_lines(url, n_max = 3)

wdbc <- read_csv(url, col_names = F)


# Section 2: Tidy data ----------------------------------------------------
co2

# Q10
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
head(co2_wide)

co2_tidy <- gather(co2_wide, month, co2, -year)
head(co2_tidy)

# Q11
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

# Q12
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
head(dat)

dat_tidy <- spread(dat, gender, admitted)
head(dat_tidy)

# Q13
tmp <- gather(admissions, key, value, admitted:applicants)
head(tmp)
tmp2 <- unite(tmp, column_name, c(key, gender))
head(tmp2)

# Q14
spread(tmp2, column_name, value)

# Part 2: Combining data
# Q5
install.packages("Lahman")
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
head(top_names)

# Q6
head(Salaries)
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>% select(nameFirst, nameLast, teamID, HR, salary)
head(top_salary)

# Q7
awards_2016 <- AwardsPlayers %>% filter(yearID == 2016) 
length(unique(awards_2016$playerID))


top_award <- AwardsPlayers %>% filter(yearID == 2016) %>% 
  inner_join(top_names) 
length(unique(top_award$playerID))

award_notop <- AwardsPlayers %>% filter(yearID == 2016) %>% 
  anti_join(top_names)
length(unique(award_notop$playerID))


## Web Scraping
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])

# Q1:
t1 <- html_table(nodes[[1]])
t2 <- html_table(nodes[[2]])
t3 <- html_table(nodes[[3]])
t4 <- html_table(nodes[[4]])

# Q2: 
html_table(nodes[[21]])
html_table(nodes[[20]])
html_table(nodes[[19]])

 # Q3:
tab_1 <- html_table(nodes[[10]], header = TRUE)
tab_1 <- tab_1 %>% select(-`No.`)
head(tab_1)
tab_2 <- html_table(nodes[[19]], header = TRUE)
head(tab_2)
tab_12 <- full_join(tab_1, tab_2, by="Team")

# Q4:
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
html <- read_html(url)
tab <- html_nodes(html, "table")

# Q5:
list <- list()
for (i in 1:9){
  tabs <- html_table(tab[[i]], header=TRUE, fill=TRUE)
  list <- append(list,tabs)
  print("Tab: ")
  print(i)
  print("No of cols:")
  print(ncol(tabs))
  print("First colname:")
  print(colnames(tabs)[1])
}



# Section 3: String Processing --------------------------------------------

# Q1:
not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}

not_inches(c(70))


# Q5:
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

# Q6:
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)



## Part 3
# grab data from pdf
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)
raw_data_research_funding_rates <- txt[2]
raw_data_research_funding_rates %>% head
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
tab %>% head
the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()


# Assessment 
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

# Q5
colnames(polls)
head(polls)
colnames(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls <- polls %>% filter(str_detect(remain, pattern = "%")) %>% nrow()

# Q6
a <- "48.1%"
as.numeric(str_replace(polls$remain, "%", ""))/100
str_remove(polls$remain, "%")/100
parse_number(polls$remain)/100

# Q7
str_replace(polls$undecided, "N/A", "0")
head(polls$undecided)




# Section 4: Dates and times ----------------------------------------------
library(dslabs)
library(lubridate)
library(scales)

data("trump_tweets")
head(trump_tweets)
names(trump_tweets)

trump_tweets %>% count(source) %>% arrange(desc(n))

trump_tweets %>% 
  tidyr::extract(source, into = "source_mod", regex = "Twitter for (.*)") %>% 
  count(source_mod) 

# get tweets from campaign trail sent from Android or iPhone
campaign_tweets <- trump_tweets %>% 
  tidyr::extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

# extract hour (EST) for each tweet and compute proportions of tweets tweeted at each hour for each device
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>% # set every time to EST
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")
# two different peaks -> two different entities tweeting from Android and iPhone separately 

# explore tweet content
#install.packages("tidytext")
library(tidytext)
#install.packages("textdata")
library(textdata)

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)
# deletes important characters # and @

# twitter tokens can also start with # or @ followed by any combination of letters or digits
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>% # remove links to pictures
  unnest_tokens(word, text, token = "regex", pattern = pattern)

tweet_words %>% 
  count(word) %>%
  arrange(desc(n))
# most common words are the, to, and, ...
# tidytext contains library of those commonly used words: stop_words

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word) 

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

# remove words that are just numbers and apostrophes in quotes
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))
head(tweet_words)

# compute odds ratio to find out if its more likely to come from Android or iPhone tweet
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

# get sentiment of different tweet origins
# tidytext provides lexica of sentiments
sentiments
# bing lexicon gives positive/negative evaluation
get_sentiments("bing")
# AFINN lexicon gives score [-5,5] -> -5 negative, 5 positive
get_sentiments("afinn") #%>% filter(value == 5) %>% head
# other: loughran or nrc
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)

tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)

# count and compare frequencies of each sentiments on each device
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

tweet_words %>% group_by(source) %>% summarize(n = n())
# more words on Android than on iPhone

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))
# largest differences in disgust, anger, negative -> significant?

# compute OR + CR for each sentiments
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or)) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 
# strong evidence for disgust, anger, netative, sadness, fear coming from Android

android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# Assessment Part 1
library(dslabs)
library(lubridate)
options(digits = 3) 

# Q3
data(brexit_polls)
head(brexit_polls)
brexit_polls %>% 
  filter(month(startdate) == 4) %>% 
  nrow


brexit_polls %>% 
  mutate(enddate_week = round_date(enddate, unit="week")) %>% 
  filter(enddate_week == "2016-06-12") %>%
  nrow

# Q4
brexit_polls %>%
  mutate(endday = weekdays(enddate)) %>%
  count(endday) %>%
  arrange(desc(n))

# Q5
data(movielens)
movielens %>%
  mutate(time_date = as_datetime(timestamp), time_year = year(time_date)) %>% 
  count(time_year) %>%
  arrange(desc(n))

movielens %>%
  mutate(time_date = as_datetime(timestamp), time_hour = hour(time_date)) %>% 
  count(time_hour) %>%
  arrange(desc(n))

# part 2
#install.packages("gutenbergr")
library(gutenbergr)
gutenberg_metadata

# Q6
gutenberg_metadata[grepl("Pride and Prejudice", gutenberg_metadata$title),]

# Q7
gutenberg_works(title == "Pride and Prejudice")

# Q8
pp <- gutenberg_download(gutenberg_id = 1342)
words <- pp %>% unnest_tokens(word, text)
nrow(words)

# Q9
words <- words %>% filter(!word %in% stop_words$word)
nrow(words)

# Q10
words <- words %>%
  filter(!grepl("\\d", words$word))
nrow(words)

# Q11
wordcount <- words %>% count(word) %>% arrange(desc(n))
head(wordcount)
wordcount %>% filter(n > 100) %>% nrow

# Q12
afinn <- get_sentiments("afinn")
words_sent <- inner_join(words, afinn, by = "word")
head(words_sent)
nrow(words_sent)
words_sent %>% filter(value >= 1) %>% nrow
words_sent %>% filter(value == 4) %>% nrow




# Comprehensive assessment ------------------------------------------------

library(tidyverse)
library(pdftools)
options(digits = 3)

# Q1
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open", args = fn)

# Q2
txt <- pdf_text(fn)
head(txt)

# Q3
x <- txt[9] %>% str_split("\n")
x

# Q4
s <- x[[1]]
s

# Q5
s <- str_trim(s)
s[1]

# Q6
header_index <- str_which(s, "2015")[1]
header_index

# Q7
s[header_index]
month <- s[header_index] %>% str_extract("[A-Z]*")
month
header <- str_split(s[header_index], "\\s+")
header <- header[[1]][2:5]
header

# Q8
s
tail_index <- str_which(s, "Total")[1]

# Q9
n <- str_count(s, "\\d+")
length(which(n == 1))

# Q10
s <- s[c(-1:-header_index,-which(n == 1), -tail_index:-40)]
s

# Q11
s <- str_remove_all(s, "[^\\d\\s]")

# Q12
tab <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
tab <- apply(tab, 2, as.numeric)
tab <- as.data.frame(tab)
colnames(tab) <- c("day",header)
head(tab)
mean(tab$`2015`)
mean(tab$`2016`)
mean(tab[1:19,"2017"])
mean(tab[20:30,"2017"])

# Q13
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
head(tab)

# Q14
tab %>%
  filter(year != 2018) %>%
  ggplot(aes(x=day, y=deaths, color=year)) +
  geom_line() +
  geom_vline(xintercept = 20)





