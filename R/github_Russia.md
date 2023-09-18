Media Source: Russia Analysis
================
Ayse D Lokmanoglu
2023-09-17

``` r
# List of packages to install
packages <- c(
  "quanteda"          = "Text analysis package",
  "seededlda"         = "Latent Dirichlet Allocation (LDA) modeling package",
  "lubridate"         = "Date and time manipulation package",
  "readr"             = "Data import package",
  "dplyr"             = "Data manipulation package",
  "tidyr"             = "Data tidying package",
  "tidyverse"         = "Data science ecosystem package",
  "scales"            = "Graphical scales package",
  "ggplot2"           = "Data visualization package",
  "wesanderson"       = "Color palettes package",
  "ggthemes"          = "Additional themes for ggplot2",
  "tidyquant"         = "Financial analysis package",
  "quanteda.textplots"= "Text visualization package",
  "quanteda.textstats"= "Text statistics package",
  "rgexf"             = "GEXF graph file format package",
  "openxlsx"          = "Excel file manipulation package",
  "tidytext"          = "Text mining package",
  "readxl"            = "Excel file import package",
  "ldatuning"         = "LDA topic model tuning package",
  "writexl"           = "Excel file export package",
  "forecast"          = "Forecasting package",
  "tseries"           = "Time series",
  "segmented"         = "Structural Time Series",
  "osfr"              = "Download data from OSF"
)

# Install packages
for (package_name in names(packages)) {
  message(paste0("Installing ", package_name, "..."))
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
  } else {
    message(paste0(package_name, " is already installed."))
  }
  message(packages[[package_name]])
  message("")
}

message("All packages installed.")
```

``` r
library(quanteda)
library(seededlda)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(scales)
library(ggplot2)
library(wesanderson)
library(ggthemes)
library(tidyquant)
library(quanteda.textplots)
library(quanteda.textstats)
library(rgexf)
library(openxlsx)
library(tidytext)
library(readxl)
library(ldatuning)
library(writexl)
library(modelsummary)
library(forecast)
library(tseries)
library(modelsummary)
library(flextable)
library(segmented)
library(osfr)
```

## Data Collection

``` r
num_articles <- 5000
api_key <- 'XXXXXXX'
api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)
get_newswhip_articles <- function(api_key, limit, start_time, end_time) {
  api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)          
  data <- paste0('{\"filters\": [\"language:ru\"],
                           \"size\": ', limit, ', 
                           \"from\": ', start_time, ',
                           \"to\": ', end_time, ',
                           \"search_full_text\": true,
                           \"find_related\": false}')
  r <- httr::POST(api_endpoint, body = data)
  httr::stop_for_status(r)         
  jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), flatten = TRUE)$articles          
}

days<-as.character(as.Date(as.Date("2021-09-01"):as.Date("2023-02-04"), origin="1970-01-01"))
##Changed dates above for our time frame 2021-09-01 till 2023-02-04

mylist <- list() 

for (i in days) {
  print("now running days:")
  print (i)
  start_time <- as.numeric(as.POSIXct(paste(i, "00:00:00 EST", sep=" "))) * 1000
  end_time <- as.numeric(as.POSIXct(paste(as.Date(paste(i))+1,  "00:00:00 EST", sep=" "))) * 1000 - 1
  data_temp <- get_newswhip_articles(api_key = api_key, limit = num_articles, start_time = start_time, end_time = end_time)
  data_temp$date_time <- as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01")
  data_temp$date <- as.Date(as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01"))
  data_temp$relatedStories <- NULL
  data_temp$topics <- NULL
  data_temp$authors <- NULL
  data_temp$entities <- NULL
  data_temp$videos <- NULL
  try(data_temp<- data_temp |>  dplyr::select(delta_time, 
                                              recent_fb_counts, 
                                              recent_tw_counts, 
                                              predicted_interactions, 
                                              predicted_timestamp, 
                                              uuid, 
                                              publication_timestamp, 
                                              link, 
                                              headline, 
                                              excerpt, 
                                              keywords, 
                                              image_link, 
                                              has_video, 
                                              nw_score, 
                                              max_nw_score, 
                                              fb_data.total_engagement_count, 
                                              fb_data.likes, 
                                              fb_data.comments, 
                                              fb_data.shares, 
                                              fb_data.total_count_delta, 
                                              fb_data.delta_period, 
                                              fb_data.delta_period_unit, 
                                              tw_data.tw_count, 
                                              tw_data.total_count_delta, 
                                              tw_data.delta_period, 
                                              tw_data.delta_period_unit, 
                                              li_data.li_count, 
                                              li_data.total_count_delta, 
                                              li_data.delta_period, 
                                              li_data.delta_period_unit, 
                                              pi_data.pi_count, 
                                              pi_data.delta_period, 
                                              pi_data.delta_period_unit, 
                                              source.publisher, 
                                              source.domain, 
                                              source.link, 
                                              source.country, 
                                              source.country_code, 
                                              source.language, 
                                              date_time, 
                                              date))
  mylist[[i]] <- data_temp
}

data_temp1 <- do.call("rbind",mylist) |> data.frame()

save(data_temp1, file="Output/RussianWarAllRus.Rda") ##changed the title of the file 

dataRussiaMaster<- data_temp1 |> 
  distinct(link,.keep_all = TRUE)|> 
  distinct(uuid,.keep_all = TRUE) |> 
  tibble::rowid_to_column("master_index") |> 
  rowwise() |> 
  mutate(engagement = sum(fb_data.total_engagement_count, 
                          tw_data.tw_count,
                          li_data.li_count,
                          pi_data.pi_count)) 
dataRussiaMaster$dateMONTH <- format(as.Date(dataRussiaMaster$date), "%Y-%m")

min(dataRussiaMaster$date)
max(dataRussiaMaster$date)

dataRussiaMaster |>  count(dateMONTH)

save(dataRussiaMaster, file="Output/Russia_Newswhip_MasterAllRus.Rda")
```

Prepare the data for scraping in Python

``` r
myscrape <- dataRussiaMaster |> 
  dplyr::select(master_index, uuid, link, date, dateMONTH)

myscrape1 <-  myscrape |> 
  filter(dateMONTH=='2021-09' | dateMONTH=='2021-10') ##Changed date to Sept2021, but for twitter our timeframe was 2021-09-04 till 2023-02-04, also I did 2 months, but shall we do one?
min(myscrape1$date)
max(myscrape1$date)
write.csv(myscrape1, file="Output/Scrape_1.csv", row.names = FALSE)
rm(myscrape1)

myscrape2 <-  myscrape |> 
  filter(dateMONTH=='2021-11' | dateMONTH=='2021-12')
min(myscrape2$date)
max(myscrape2$date)
write.csv(myscrape2, file="Output/Scrape_2.csv", row.names = FALSE)
rm(myscrape2)

myscrape3 <-  myscrape |> 
  filter(dateMONTH=='2022-01' | dateMONTH=='2022-02')
min(myscrape3$date)
max(myscrape3$date)
write.csv(myscrape3, file="Output/Scrape_3.csv", row.names = FALSE)
rm(myscrape3)

myscrape4 <-  myscrape |> 
  filter(dateMONTH=='2022-03' | dateMONTH=='2022-04')
min(myscrape4$date)
max(myscrape4$date)
write.csv(myscrape4, file="Output/Scrape_4.csv", row.names = FALSE)
rm(myscrape4)

myscrape5 <-  myscrape |> 
  filter(dateMONTH=='2022-05' | dateMONTH=='2022-06')
min(myscrape5$date)
max(myscrape5$date)
write.csv(myscrape5, file="Output/Scrape_5.csv", row.names = FALSE)
rm(myscrape5)

 ## added more scrapes for more months
myscrape6 <-  myscrape |> 
  filter(dateMONTH=='2022-07' | dateMONTH=='2022-08')
min(myscrape6$date)
max(myscrape6$date)
write.csv(myscrape6, file="Output/Scrape_6.csv", row.names = FALSE)
rm(myscrape6)

myscrape7 <-  myscrape |> 
  filter(dateMONTH=='2022-09' | dateMONTH=='2022-10')
min(myscrape7$date)
max(myscrape7$date)
write.csv(myscrape7, file="Output/Scrape_7.csv", row.names = FALSE)
rm(myscrape7)

myscrape8 <-  myscrape |> 
  filter(dateMONTH=='2022-11' | dateMONTH=='2022-12')
min(myscrape8$date)
max(myscrape8$date)
write.csv(myscrape8, file="Output/Scrape_8.csv", row.names = FALSE)
rm(myscrape8)

myscrape9 <-  myscrape |> 
  filter(dateMONTH=='2023-01' | dateMONTH=='2023-02')
min(myscrape9$date)
max(myscrape9$date)
write.csv(myscrape9, file="Output/Scrape_9.csv", row.names = FALSE)
rm(myscrape9)
```

After scrape upload data and pre-process it

``` r
path <- c("Input/Scraped/Russia")

scraped <- list.files(path = path,  # Identify all CSV files
                      pattern = "*.csv", full.names = TRUE) |> 
  lapply(read_csv) |>                              
  bind_rows 

scraped |>  count(dateMONTH)


## load master file to join
load("Output/Russia_Newswhip_MasterAllRus.Rda")


dataRussiaMaster |>  count(dateMONTH)

temp <- left_join(dataRussiaMaster, scraped[, c("uuid", "link", "text")])
rm(dataRussiaMaster)
rm(scraped)
gc()

temp2 <- temp |> 
  distinct(uuid,.keep_all = TRUE) |> 
  distinct(text,.keep_all = TRUE) |> 
  distinct(link,.keep_all = TRUE)

temp2 |>  count(dateMONTH)
save(temp2, file="Output/Russia_NewsWhip_MasterAllRussia_with_text.Rda")
rm(temp)
gc()
```

Filter with search words

``` r
search_words <- readxl::read_excel("Data/russian-search-words.xlsx")
```

    ## New names:
    ## • `` -> `...5`
    ## • `` -> `...6`
    ## • `` -> `...9`

``` r
search_words_russia <- search_words[1:49,]
search_words_russia <- search_words_russia$`Russia 50`
print(search_words_russia)
```

    ##  [1] "Украина"      "Россия"       "Беларусь"     "Казахстан"    "Польша"      
    ##  [6] "Турция"       "США"          "ЕС"           "НАТО"         "Китай"       
    ## [11] "ДНР"          "Донбасс"      "Крым"         "Зеленский"    "Путин"       
    ## [16] "Байден"       "Эрдоган"      "Макрон"       "Джонсон"      "американцы"  
    ## [21] "китайцы"      "европейцы"    "русские"      "украинцы"     "поляки"      
    ## [26] "турки"        "белоруссы"    "беларусы"     "казахи"       "СМИ"         
    ## [31] "Телеграм"     "Фейсбук"      "блокировка"   "протест"      "война"       
    ## [36] "СВО"          "спецоперация" "победа"       "отступление"  "наступление" 
    ## [41] "переговоры"   "армия"        "оружие"       "помощь"       "коррупция"   
    ## [46] "мобилизация"  "беженцы"      "ядерное"      "язык"

``` r
df_filtered <- temp2 |> 
  filter(grepl(paste(search_words_russia, collapse="|"), text))

print(c("filter done", format(Sys.time(), "%a %b %d %X %Y")))

df_filtered |>  count(dateMONTH)

# Create new column with search words
df_filtered$search_words <- sapply(str_extract_all(df_filtered$text, paste(search_words_russia, collapse="|")), paste, collapse=", ")

print(c("search words done", format(Sys.time(), "%a %b %d %X %Y")))

df_filtered$number_sw <- str_count(df_filtered$search_words, "\\w+")
print(c("search words count done", format(Sys.time(), "%a %b %d %X %Y")))

df_filtered$number_words <- str_count(df_filtered$text, "\\w+")
print(c("text count done", format(Sys.time(), "%a %b %d %X %Y")))

save(df_filtered, file="Output/Russia_NewsWhip_Filtered_with_text.Rda")

beepr::beep()
rm(temp2)
rm(search_words)
rm(search_words_russia)
gc()
```

## SeededLDA

SeededLDA from [Watanabe,
2019/2023](https://github.com/koheiw/seededlda)

``` r
df_filtered <- as.data.frame(df_filtered)
RU_sample <- df_filtered |> 
  dplyr::select(master_index, 
                link,
                uuid,
                date,
                dateMONTH, 
                engagement,
                text) 

rm(df_filtered)

### Start Cleaning
#remove NAs
RU_sample<-RU_sample[!is.na(RU_sample$text),] 

#remove duplicates
RU_sample <- RU_sample |> 
  distinct(text, .keep_all = TRUE)

RU_sample$nwords_text <- str_count(RU_sample$text, "\\w+")
summary(RU_sample$nwords_text)
hist(RU_sample$nwords_text, breaks = 10000)

#remove extra short texts
dim(RU_sample)

quartiles <- quantile(RU_sample$nwords_text, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(RU_sample$nwords_text)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

RU_sample2<- subset(RU_sample, RU_sample$nwords_text  > Lower & RU_sample$nwords_text  < Upper)

dim(RU_sample2)
summary(RU_sample2$nwords_text)

table(RU_sample2$nwords_text)

rm(RU_sample)
rm(IQR)
rm(Lower)
rm(quartiles)
rm(Upper)
gc()

#create an original text column
RU_sample2$original_tweet <- RU_sample2$text

stopwords <- read_csv("Input/stop_words_russian.txt", col_names = FALSE)
stopwords_remove <- c(stopwords("ru", source = "marimo"), 
                      tm::stopwords(kind = "ru"),
                      stopwords$X1,
                      "internet-group",
                      "новости",
                      "риа",
                      "миа",
                      "html",
                      "645-6601",
                      "meta",
                      "head",
                      "true",
                      "фгуп",
                      "og:title",
                      "og:description",
                      "ru-ru",
                      "the",
                      "ru")
rm(stopwords)

RU_sample2$text <- tolower(RU_sample2$text)
RU_sample2$text <- gsub("@\\w+"," ",RU_sample2$text) #removes all after @ sign
url_pattern <- "(http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+)"

# Remove URLs from the text column
RU_sample2$text <- str_replace_all(RU_sample2$text, url_pattern, "")

## tokenize it
toks <- tokens(RU_sample2$text,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE,
               remove_url = TRUE,
               remove_separators = TRUE,
               split_hyphens = FALSE,
               include_docvars = TRUE,
               padding = FALSE) |> 
  tokens_remove(stopwords_remove) |> 
  tokens_select(min_nchar = 2) 


dfm_counts<- dfm(toks) 
rm(toks) #remove unused files to save space

docnames(dfm_counts)<-RU_sample2$master_index

sparsity(dfm_counts)


dfm_counts2<-dfm_trim(dfm_counts, 
                      max_docfreq = nrow(RU_sample2)*0.5,
                      min_docfreq= nrow(RU_sample2)*0.0002,
                      docfreq_type="count")
sparsity(dfm_counts2)
dfm3 <- dfm_wordstem(dfm_counts2, language = "ru")


#rm(dfm_counts) #remove for space

#use word cloud to add more stopwords
textstat_frequency(dfm3)
quanteda.textplots::textplot_wordcloud(dfm3, 
                                       min_size = 2, max_size = 5,
                                       max_words = 5000)

rm(dfm_counts2)
rm(dfm_counts)
gc()
```

Upload grouped search words to create the dictionary

``` r
## Search words
Grouped_search_wordsRU <- read_excel("Data/Grouped_search_words.xlsx", 
                                     sheet = "Russian")
```

``` r
dictRU <- dictionary(list(Ukraine = na.omit(Grouped_search_wordsRU$Ukraine),
                          Insurgents = na.omit(Grouped_search_wordsRU$Insurgents),
                          Russia = na.omit(Grouped_search_wordsRU$Russia),
                          USA = na.omit(Grouped_search_wordsRU$USA),
                          Belarus = na.omit(Grouped_search_wordsRU$Belaus),
                          Turkey = na.omit(Grouped_search_wordsRU$Turkey),
                          Kazakhstan = na.omit(Grouped_search_wordsRU$Kazakhstan),
                          Europe = na.omit(Grouped_search_wordsRU$Europe),
                          Poland = na.omit(Grouped_search_wordsRU$Poland),
                          China = na.omit(Grouped_search_wordsRU$China),
                          NATO = na.omit(Grouped_search_wordsRU$NATO),
                          UN = na.omit(Grouped_search_wordsRU$UN)))

rm(Grouped_search_wordsRU)

print(dictRU)

start <- Sys.time()
tmod_slda <- textmodel_seededlda(dfm3, 
                                 dictionary = dictRU,
                                 residual = TRUE)
end <- Sys.time()
print(end-start)


# assign topics from seeded LDA as a document-level variable to the dfm
RU_sample$topic2 <- topics(tmod_slda)

# cross-table of the topic frequency
table(RU_sample$topic2)

save.image("Output/Russia_NW_SeededLDA_Test.Rdata")

terms <- terms(tmod_slda, 20)

# assign topics from seeded LDA as a document-level variable to the dfm
RU_sample2$topic2 <- topics(tmod_slda)

# cross-table of the topic frequency
table(RU_sample2$topic2)

terms <- as.data.frame(terms)

openxlsx::write.xlsx(terms, 
                     file="Output/Russia_Stemmed_SeededLDA_Top_Terms.xlsx",
                     rowNames = FALSE)

data_excel <- split(RU_sample2, RU_sample2$topic2)

size <- length(data_excel)


#creating number of lists equivalent
# to the size of the generated groups 
lapply(1:size, 
       function(i) 
         write.xlsx(data_excel[[i]],
                    file = paste0("Output/",
                                  names(data_excel[i]), ".xlsx")))

### add more analysis

dfm3$topic2 <- topics(tmod_slda)
topic_frequency<-data.frame(table(dfm3$topic2))
openxlsx::write.xlsx(topic_frequency,
                     file="Output/Russia_Stem_SeededLDA_Topic_Freq.xlsx",
                     rowNames = FALSE)

###write top unique words#####
topterms<-data.frame(terms(tmod_slda, 100))
openxlsx::write.xlsx(topterms,
                     "Output/Russia_Stemmed_SeededLDA_Top_Terms.xlsx",
                     rowNames = FALSE)
####the functions to get document topic probabilities and topic probabilities#####
### get document topic probabilities
get_doc_topic_probs <- function(tmod_slda) {
  out <- tmod_slda$theta |>  
    as_tibble(rownames = "doc_id")
  return(out)
}

### get word topic probabilities
get_word_topic_probs <- function(tmod_slda) {
  out <- tmod_slda$phi |>  
    as_tibble(rownames = "topic") |> 
    pivot_longer(cols = !matches("topic"), 
                 names_to = "token", 
                 values_to = "prob")
  return(out)
}

#####use them and save them#####
topicprob<-get_doc_topic_probs(tmod_slda)

topicprob$doc_id <- as.numeric(topicprob$doc_id)

temp <- left_join(RU_sample2, topicprob, 
                  by = c("master_index" = "doc_id"))

url_pattern <- "(http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+)"

# Remove URLs from the text column
temp$text <- str_replace_all(temp$text, url_pattern, " ")

save(temp, file = "Output/Russia_NewsWhip_Filtered_with_topics_prob.Rda")
```

### Sentiment Analysis

``` r
###first get the sentiment dictionary
load("C:/Users/adl6244/Dropbox/ayse-and-olga-coding/Input/NRC_Russian.Rda")

##create a stopword dataframe
stopwords <- read_csv("Input/stop_words_russian.txt", col_names = FALSE)
stopwords_remove <- c(stopwords("ru", source = "marimo"), 
                      tm::stopwords(kind = "ru"),
                      stopwords$X1,
                      "internet-group",
                      "новости",
                      "риа",
                      "миа",
                      "html",
                      "645-6601",
                      "meta",
                      "head",
                      "true",
                      "фгуп",
                      "og:title",
                      "og:description",
                      "ru-ru",
                      "the",
                      "ru")
rm(stopwords)
stopwords<-data.frame(stopwords_remove)
names(stopwords)[1]<-"word"

#####antijoin by stopwords join by sentiment and counts
temp_test<-temp |> 
  unnest_tokens(word, text) |> 
  anti_join(stopwords) |> 
  inner_join(nrcRU) |> 
  group_by(master_index) |> 
  count(sentiment) |> 
  spread(sentiment, n, fill=0) 
temp_test<-full_join(temp, temp_test, by="master_index")
##create a net sentiment column
temp_test<-temp_test |>  mutate(sentiment=positive-negative)
save(temp_test, file = "Output/Russia_NewsWhip_Filtered_with_topics_prob_sentiment.Rda")
gc()
beepr::beep()
rm(temp)
gc()


#### Top 100 for each -----

writeSentimentDataToExcel <- function(data, sentiment, prefix, sheetname_prefix, output_file) {
  # Create a new Excel workbook
  wb <- createWorkbook()
  result_dfs <- list()
  
  # Get unique topics from the 'topic2' column
  topics <- unique(data$topic2)
  
  # Loop through each topic
  for (topic in topics) {
    # Filter dataframe for the current topic and select top 100 texts based on the specified sentiment
    filtered_data <- data |> 
      select(master_index, topic2, link, uuid, date, engagement, text, anger:sentiment) |> 
      filter(topic2 == topic) |> 
      slice_max(!!sym(sentiment), n = 100)
    
    # Store the filtered dataframe in the list with a dynamically created name
    result_dfs[[paste0(prefix, "_", topic)]] <- filtered_data
    
    # Create a new worksheet in the workbook
    addWorksheet(wb, sheetName = paste0(sheetname_prefix, "_", topic))
    
    # Write the dataframe to the worksheet
    writeData(wb, sheet = paste0(sheetname_prefix, "_", topic), x = filtered_data)
  }
  
  # Save the Excel workbook
  saveWorkbook(wb, output_file)
}



writeSentimentDataToExcel(temp_test, "anger", "anger", "anger", "Output/Anger_Texts_PerTopic.xlsx")
writeSentimentDataToExcel(temp_test, "disgust", "disgust", "disgust", "Output/Disgust_Texts_PerTopic.xlsx")
writeSentimentDataToExcel(temp_test, "fear", "fear", "fear", "Output/Fear_Texts_PerTopic.xlsx")
writeSentimentDataToExcel(temp_test, "joy", "joy", "joy", "Output/Joy_Texts_PerTopic.xlsx")
writeSentimentDataToExcel(temp_test, "sadness", "sadness", "sadness", "Output/Sadness_Texts_PerTopic.xlsx")
writeSentimentDataToExcel(temp_test, "surprise", "surprise", "surprise", "Output/Surprise_Texts_Russia_PerTopic.xlsx")
writeSentimentDataToExcel(temp_test, "trust", "trust", "trust", "Output/Trust_Texts_Russia_PerTopic.xlsx")
writeSentimentDataToExcel(temp_test, "sentiment", "sentiment", "sentiment", "Output/Sentiment_Texts_Russia_PerTopic.xlsx")

rm(filtered_data, wb, result_dfs)
gc()

# Function to multiply emotion values with respective columns

multiplyEmotionValues <- function(data, emotion_columns) {
  prefixes <- emotion_columns
  
  for (i in 1:length(emotion_columns)) {
    emotions <- select(data, Ukraine:other)
    colnames(emotions) <- paste(prefixes[i], colnames(emotions), sep = "_")
    
    for (j in 1:ncol(emotions)) {
      emotions[, j] <- emotions[, j] * data[[emotion_columns[i]]]
    }
    
    data <- cbind(data, emotions)
  }
  
  return(data)
}



emotion_columns <- c("anger", "anticipation", 
                     "disgust", "fear",
                     "joy", "negative",
                     "positive", "sadness",
                     "surprise", "trust",
                     "sentiment")  

data <- multiplyEmotionValues(temp_test, emotion_columns)

save(data, file = "Output/Russia_NewsWhip_Filtered_with_topics_prob_sentiment.Rda")

## create daily data for regression
## first create long dataframes for topics and different sentiments anger, 
## fear, joy, sadness, trust
topics <- data |> 
  dplyr::select(master_index, 
                        date,
                        Ukraine:other) |> 
  pivot_longer(
    cols = Ukraine:other,
    names_to = c("topics"),
    values_to = "value"
  )

anger <- data |> 
  dplyr::select(master_index, 
                date,
                anger_Ukraine:anger_other) |> 
  pivot_longer(
    cols = anger_Ukraine:anger_other,
    names_to = c("topics"),
    values_to = "value"
  )

fear <- data |> 
  dplyr::select(master_index, 
                date,
                fear_Ukraine:fear_other) |> 
  pivot_longer(
    cols = fear_Ukraine:fear_other,
    names_to = c("topics"),
    values_to = "value"
  )
  
joy <- data |> 
  dplyr::select(master_index, 
                date,
                joy_Ukraine:joy_other) |> 
  pivot_longer(
    cols = joy_Ukraine:joy_other,
    names_to = c("topics"),
    values_to = "value"
  )

sadness <- data |> 
  dplyr::select(master_index, 
                date,
                sadness_Ukraine:sadness_other) |> 
  pivot_longer(
    cols = sadness_Ukraine:sadness_other,
    names_to = c("topics"),
    values_to = "value"
  )

trust <- data |> 
  dplyr::select(master_index, 
                date,
                trust_Ukraine:trust_other) |> 
  pivot_longer(
    cols = trust_Ukraine:trust_other,
    names_to = c("topics"),
    values_to = "value"
  )

rm(data)
gc()

save.image("Output/Smaller_Df.Rdata")

write.csv(anger, file = "Output/Anger_Russia.csv",
          row.names = FALSE)

write.csv(fear, file = "Output/Fear_Russia.csv",
          row.names = FALSE)

write.csv(joy, file = "Output/Joy_Russia.csv",
          row.names = FALSE)

write.csv(sadness, file = "Output/Sadness_Russia.csv",
          row.names = FALSE)

write.csv(topics, file = "Output/Topics_Russia.csv",
          row.names = FALSE)

write.csv(trust, file = "Output/Trust_Russia.csv",
          row.names = FALSE)

### create a daily average of columns
data_daily_RU <- data |> 
  dplyr::select(-engagement) |> 
  rename('engagement' = "tw_data.tw_count") |> 
  group_by(date) |> 
  summarise(across(everything(), mean, na.rm=TRUE))

cor(data_daily_RU$fb_data.total_engagement_count, data_daily_RU$engagement, 
    method = "pearson")

save(data_daily_RU, file="Output/Data_for_regression_Russia.Rda")
```

## Statistical Analysis

ADD making them wider

``` r
load("Data/Smaller_Df.Rdata")

## download to R from your project directory

# anger <- read_csv("Output/Aggregate_IV_DV_for_pub.csv",
#     col_types = cols(date = col_date(format = "%Y-%m-%d")))


anger <- anger |> 
  mutate(topics = gsub("Insurgents", "Seccessionist", topics))

fear <- fear |> 
  mutate(topics = gsub("Insurgents", "Seccessionist", topics))

joy <- joy |> 
  mutate(topics = gsub("Insurgents", "Seccessionist", topics))

sadness <- sadness |> 
  mutate(topics = gsub("Insurgents", "Seccessionist", topics))

topics <- topics |> 
  mutate(topics = gsub("Insurgents", "Seccessionist", topics))

trust <- trust |> 
  mutate(topics = gsub("Insurgents", "Seccessionist", topics))
```

### ANOVA and Tukey-HSD Post Hoc

#### Topic Probabilities

``` r
# Perform ANOVA
anova_topics <- aov(value ~ topics, data = topics)
anova_summary <- summary(anova_topics)

# Check if p-value is less than 0.05
if (anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  message("ANOVA p<0.05 means are different")
  
  # Perform Tukey's post-hoc analysis
  tukey_topics <- TukeyHSD(anova_topics, conf.level=.95)
  table_topics <- as.data.frame(tukey_topics$topics)
  table_topics <- janitor::clean_names(table_topics)
  table_topics <- table_topics |> 
    mutate_if(is.numeric, round, 5)  |> 
    tibble::rownames_to_column() |> 
    rename('Topics' = 'rowname') |> 
    mutate(status = case_when(
      p_adj < 0.05 ~ 'significant',
      #p_adj >= 0.05 ~ 'not_significant'
      .default = "not_significant"
    ))
  
  # Filter for significant differences
  message("Significant Differences")
  significant_differences <- subset(table_topics, p_adj < 0.05)
  print(significant_differences$Topics)
  
  message("Not Significant Differences")
  not_significant_differences <- subset(table_topics, p_adj > 0.05)
  print(not_significant_differences$Topics)
} else {
  message("ANOVA p>=0.05 means are not significantly different")
}
```

    ## ANOVA p<0.05 means are different

    ## Significant Differences

    ##  [1] "China-Belarus"            "Europe-Belarus"          
    ##  [3] "Kazakhstan-Belarus"       "NATO-Belarus"            
    ##  [5] "other-Belarus"            "Poland-Belarus"          
    ##  [7] "Russia-Belarus"           "Seccessionist-Belarus"   
    ##  [9] "Turkey-Belarus"           "Ukraine-Belarus"         
    ## [11] "UN-Belarus"               "USA-Belarus"             
    ## [13] "Europe-China"             "Kazakhstan-China"        
    ## [15] "NATO-China"               "other-China"             
    ## [17] "Poland-China"             "Russia-China"            
    ## [19] "Seccessionist-China"      "Turkey-China"            
    ## [21] "Ukraine-China"            "UN-China"                
    ## [23] "USA-China"                "Kazakhstan-Europe"       
    ## [25] "NATO-Europe"              "other-Europe"            
    ## [27] "Poland-Europe"            "Russia-Europe"           
    ## [29] "Seccessionist-Europe"     "Turkey-Europe"           
    ## [31] "Ukraine-Europe"           "USA-Europe"              
    ## [33] "NATO-Kazakhstan"          "other-Kazakhstan"        
    ## [35] "Poland-Kazakhstan"        "Russia-Kazakhstan"       
    ## [37] "Seccessionist-Kazakhstan" "Turkey-Kazakhstan"       
    ## [39] "Ukraine-Kazakhstan"       "UN-Kazakhstan"           
    ## [41] "USA-Kazakhstan"           "other-NATO"              
    ## [43] "Poland-NATO"              "Russia-NATO"             
    ## [45] "Seccessionist-NATO"       "Turkey-NATO"             
    ## [47] "Ukraine-NATO"             "UN-NATO"                 
    ## [49] "USA-NATO"                 "Poland-other"            
    ## [51] "Russia-other"             "Seccessionist-other"     
    ## [53] "Turkey-other"             "Ukraine-other"           
    ## [55] "UN-other"                 "USA-other"               
    ## [57] "Seccessionist-Poland"     "Turkey-Poland"           
    ## [59] "Ukraine-Poland"           "UN-Poland"               
    ## [61] "USA-Poland"               "Seccessionist-Russia"    
    ## [63] "Turkey-Russia"            "Ukraine-Russia"          
    ## [65] "UN-Russia"                "USA-Russia"              
    ## [67] "Turkey-Seccessionist"     "Ukraine-Seccessionist"   
    ## [69] "UN-Seccessionist"         "USA-Seccessionist"       
    ## [71] "UN-Turkey"                "USA-Turkey"              
    ## [73] "UN-Ukraine"               "USA-Ukraine"             
    ## [75] "USA-UN"

    ## Not Significant Differences

    ## [1] "UN-Europe"      "Russia-Poland"  "Ukraine-Turkey"

<figure>
<img src="github_Russia_files/figure-gfm/figure1-1.jpeg"
alt="Figure 1. Topic Probabilities Tukey-HSD 95% Confidence Intervals" />
<figcaption aria-hidden="true">Figure 1. Topic Probabilities Tukey-HSD
95% Confidence Intervals</figcaption>
</figure>

``` r
rm(list=ls()[! ls() %in% c("anger","fear", "joy", "sadness", "topics", "trust")])
```

#### Anger Probabilities per Topic

``` r
# Perform ANOVA
anova_anger <- aov(value ~ topics, data = anger)
anova_summary <- summary(anova_anger)

# Check if p-value is less than 0.05
if (anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  message("ANOVA p<0.05 means are different")
  
  # Perform Tukey's post-hoc analysis
  tukey_topics <- TukeyHSD(anova_anger)
  table_topics <- as.data.frame(tukey_topics$topics)
  table_topics <- janitor::clean_names(table_topics)
  table_topics <- table_topics |> 
    mutate_if(is.numeric, round, 4) |> 
    tibble::rownames_to_column() |> 
    rename('Topics' = 'rowname') |> 
    mutate(status = case_when(
      p_adj < 0.05 ~ 'significant',
      #p_adj >= 0.05 ~ 'not_significant'
      .default = "not_significant"
    ))
  
  # Filter for significant differences
  message("Significant Differences")
  significant_differences <- subset(table_topics, p_adj < 0.05)
  print(significant_differences$Topics)
  
  message("Not Significant Differences")
  not_significant_differences <- subset(table_topics, p_adj > 0.05)
  print(not_significant_differences$Topics)
} else {
  message("ANOVA p>=0.05 means are not significantly different")
}
```

    ## ANOVA p<0.05 means are different

    ## Significant Differences

    ##  [1] "anger_Europe-anger_Belarus"          
    ##  [2] "anger_Kazakhstan-anger_Belarus"      
    ##  [3] "anger_NATO-anger_Belarus"            
    ##  [4] "anger_other-anger_Belarus"           
    ##  [5] "anger_Poland-anger_Belarus"          
    ##  [6] "anger_Russia-anger_Belarus"          
    ##  [7] "anger_Seccessionist-anger_Belarus"   
    ##  [8] "anger_Turkey-anger_Belarus"          
    ##  [9] "anger_Ukraine-anger_Belarus"         
    ## [10] "anger_UN-anger_Belarus"              
    ## [11] "anger_USA-anger_Belarus"             
    ## [12] "anger_Europe-anger_China"            
    ## [13] "anger_Kazakhstan-anger_China"        
    ## [14] "anger_NATO-anger_China"              
    ## [15] "anger_other-anger_China"             
    ## [16] "anger_Poland-anger_China"            
    ## [17] "anger_Russia-anger_China"            
    ## [18] "anger_Seccessionist-anger_China"     
    ## [19] "anger_Turkey-anger_China"            
    ## [20] "anger_Ukraine-anger_China"           
    ## [21] "anger_UN-anger_China"                
    ## [22] "anger_USA-anger_China"               
    ## [23] "anger_Kazakhstan-anger_Europe"       
    ## [24] "anger_NATO-anger_Europe"             
    ## [25] "anger_other-anger_Europe"            
    ## [26] "anger_Poland-anger_Europe"           
    ## [27] "anger_Russia-anger_Europe"           
    ## [28] "anger_Seccessionist-anger_Europe"    
    ## [29] "anger_Ukraine-anger_Europe"          
    ## [30] "anger_UN-anger_Europe"               
    ## [31] "anger_USA-anger_Europe"              
    ## [32] "anger_NATO-anger_Kazakhstan"         
    ## [33] "anger_other-anger_Kazakhstan"        
    ## [34] "anger_Poland-anger_Kazakhstan"       
    ## [35] "anger_Russia-anger_Kazakhstan"       
    ## [36] "anger_Seccessionist-anger_Kazakhstan"
    ## [37] "anger_Turkey-anger_Kazakhstan"       
    ## [38] "anger_Ukraine-anger_Kazakhstan"      
    ## [39] "anger_UN-anger_Kazakhstan"           
    ## [40] "anger_USA-anger_Kazakhstan"          
    ## [41] "anger_other-anger_NATO"              
    ## [42] "anger_Poland-anger_NATO"             
    ## [43] "anger_Russia-anger_NATO"             
    ## [44] "anger_Seccessionist-anger_NATO"      
    ## [45] "anger_Turkey-anger_NATO"             
    ## [46] "anger_Ukraine-anger_NATO"            
    ## [47] "anger_UN-anger_NATO"                 
    ## [48] "anger_USA-anger_NATO"                
    ## [49] "anger_Poland-anger_other"            
    ## [50] "anger_Russia-anger_other"            
    ## [51] "anger_Seccessionist-anger_other"     
    ## [52] "anger_Turkey-anger_other"            
    ## [53] "anger_Ukraine-anger_other"           
    ## [54] "anger_UN-anger_other"                
    ## [55] "anger_USA-anger_other"               
    ## [56] "anger_Russia-anger_Poland"           
    ## [57] "anger_Seccessionist-anger_Poland"    
    ## [58] "anger_Turkey-anger_Poland"           
    ## [59] "anger_Ukraine-anger_Poland"          
    ## [60] "anger_USA-anger_Poland"              
    ## [61] "anger_Seccessionist-anger_Russia"    
    ## [62] "anger_Turkey-anger_Russia"           
    ## [63] "anger_Ukraine-anger_Russia"          
    ## [64] "anger_UN-anger_Russia"               
    ## [65] "anger_USA-anger_Russia"              
    ## [66] "anger_Turkey-anger_Seccessionist"    
    ## [67] "anger_Ukraine-anger_Seccessionist"   
    ## [68] "anger_UN-anger_Seccessionist"        
    ## [69] "anger_USA-anger_Seccessionist"       
    ## [70] "anger_Ukraine-anger_Turkey"          
    ## [71] "anger_UN-anger_Turkey"               
    ## [72] "anger_USA-anger_Turkey"              
    ## [73] "anger_UN-anger_Ukraine"              
    ## [74] "anger_USA-anger_Ukraine"             
    ## [75] "anger_USA-anger_UN"

    ## Not Significant Differences

    ## [1] "anger_China-anger_Belarus" "anger_Turkey-anger_Europe"
    ## [3] "anger_UN-anger_Poland"

<figure>
<img src="github_Russia_files/figure-gfm/figure2-1.jpeg"
alt="Figure 2. Anger Probabilities Tukey-HSD 95% Confidence Intervals" />
<figcaption aria-hidden="true">Figure 2. Anger Probabilities Tukey-HSD
95% Confidence Intervals</figcaption>
</figure>

``` r
rm(list=ls()[! ls() %in% c("anger","fear", "joy", "sadness", "topics", "trust")])
```

#### Fear Probabilities per Topic

``` r
# Perform ANOVA
anova_fear <- aov(value ~ topics, data = fear)
anova_summary <- summary(anova_fear)

# Check if p-value is less than 0.05
if (anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  message("ANOVA p<0.05 means are different")
  
  # Perform Tukey's post-hoc analysis
  tukey_topics <- TukeyHSD(anova_fear)
  table_topics <- as.data.frame(tukey_topics$topics)
  table_topics <- janitor::clean_names(table_topics)
  table_topics <- table_topics |> 
    mutate_if(is.numeric, round, 4) |> 
    tibble::rownames_to_column() |> 
    rename('Topics' = 'rowname') |> 
    mutate(status = case_when(
      p_adj < 0.05 ~ 'significant',
      #p_adj >= 0.05 ~ 'not_significant'
      .default = "not_significant"
    ))
  
  # Filter for significant differences
  message("Significant Differences")
  significant_differences <- subset(table_topics, p_adj < 0.05)
  print(significant_differences$Topics)
  
  message("Not Significant Differences")
  not_significant_differences <- subset(table_topics, p_adj > 0.05)
  print(not_significant_differences$Topics)
} else {
  message("ANOVA p>=0.05 means are not significantly different")
}
```

    ## ANOVA p<0.05 means are different

    ## Significant Differences

    ##  [1] "fear_China-fear_Belarus"            "fear_Europe-fear_Belarus"          
    ##  [3] "fear_Kazakhstan-fear_Belarus"       "fear_NATO-fear_Belarus"            
    ##  [5] "fear_other-fear_Belarus"            "fear_Poland-fear_Belarus"          
    ##  [7] "fear_Russia-fear_Belarus"           "fear_Seccessionist-fear_Belarus"   
    ##  [9] "fear_Turkey-fear_Belarus"           "fear_Ukraine-fear_Belarus"         
    ## [11] "fear_UN-fear_Belarus"               "fear_USA-fear_Belarus"             
    ## [13] "fear_Europe-fear_China"             "fear_Kazakhstan-fear_China"        
    ## [15] "fear_NATO-fear_China"               "fear_other-fear_China"             
    ## [17] "fear_Poland-fear_China"             "fear_Russia-fear_China"            
    ## [19] "fear_Seccessionist-fear_China"      "fear_Turkey-fear_China"            
    ## [21] "fear_Ukraine-fear_China"            "fear_UN-fear_China"                
    ## [23] "fear_USA-fear_China"                "fear_Kazakhstan-fear_Europe"       
    ## [25] "fear_NATO-fear_Europe"              "fear_other-fear_Europe"            
    ## [27] "fear_Poland-fear_Europe"            "fear_Russia-fear_Europe"           
    ## [29] "fear_Seccessionist-fear_Europe"     "fear_Turkey-fear_Europe"           
    ## [31] "fear_Ukraine-fear_Europe"           "fear_UN-fear_Europe"               
    ## [33] "fear_USA-fear_Europe"               "fear_NATO-fear_Kazakhstan"         
    ## [35] "fear_other-fear_Kazakhstan"         "fear_Poland-fear_Kazakhstan"       
    ## [37] "fear_Russia-fear_Kazakhstan"        "fear_Seccessionist-fear_Kazakhstan"
    ## [39] "fear_Turkey-fear_Kazakhstan"        "fear_Ukraine-fear_Kazakhstan"      
    ## [41] "fear_UN-fear_Kazakhstan"            "fear_USA-fear_Kazakhstan"          
    ## [43] "fear_other-fear_NATO"               "fear_Poland-fear_NATO"             
    ## [45] "fear_Russia-fear_NATO"              "fear_Seccessionist-fear_NATO"      
    ## [47] "fear_Turkey-fear_NATO"              "fear_Ukraine-fear_NATO"            
    ## [49] "fear_UN-fear_NATO"                  "fear_USA-fear_NATO"                
    ## [51] "fear_Poland-fear_other"             "fear_Russia-fear_other"            
    ## [53] "fear_Seccessionist-fear_other"      "fear_Turkey-fear_other"            
    ## [55] "fear_Ukraine-fear_other"            "fear_UN-fear_other"                
    ## [57] "fear_USA-fear_other"                "fear_Russia-fear_Poland"           
    ## [59] "fear_Seccessionist-fear_Poland"     "fear_Turkey-fear_Poland"           
    ## [61] "fear_Ukraine-fear_Poland"           "fear_USA-fear_Poland"              
    ## [63] "fear_Seccessionist-fear_Russia"     "fear_Turkey-fear_Russia"           
    ## [65] "fear_Ukraine-fear_Russia"           "fear_UN-fear_Russia"               
    ## [67] "fear_USA-fear_Russia"               "fear_Turkey-fear_Seccessionist"    
    ## [69] "fear_Ukraine-fear_Seccessionist"    "fear_UN-fear_Seccessionist"        
    ## [71] "fear_USA-fear_Seccessionist"        "fear_Ukraine-fear_Turkey"          
    ## [73] "fear_UN-fear_Turkey"                "fear_USA-fear_Turkey"              
    ## [75] "fear_UN-fear_Ukraine"               "fear_USA-fear_Ukraine"             
    ## [77] "fear_USA-fear_UN"

    ## Not Significant Differences

    ## [1] "fear_UN-fear_Poland"

<figure>
<img src="github_Russia_files/figure-gfm/figure3-1.jpeg"
alt="Figure 3. Fear Probabilities Tukey-HSD 95% Confidence Intervals" />
<figcaption aria-hidden="true">Figure 3. Fear Probabilities Tukey-HSD
95% Confidence Intervals</figcaption>
</figure>

``` r
rm(list=ls()[! ls() %in% c("anger","fear", "joy", "sadness", "topics", "trust")])
```

#### Joy Probabilities per Topic

``` r
# Perform ANOVA
anova_joy <- aov(value ~ topics, data = joy)
anova_summary <- summary(anova_joy)

# Check if p-value is less than 0.05
if (anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  message("ANOVA p<0.05 means are different")
  
  # Perform Tukey's post-hoc analysis
  tukey_topics <- TukeyHSD(anova_joy)
  table_topics <- as.data.frame(tukey_topics$topics)
  table_topics <- janitor::clean_names(table_topics)
  table_topics <- table_topics |> 
    mutate_if(is.numeric, round, 4) |> 
    tibble::rownames_to_column() |> 
    rename('Topics' = 'rowname') |> 
    mutate(status = case_when(
      p_adj < 0.05 ~ 'significant',
      #p_adj >= 0.05 ~ 'not_significant'
      .default = "not_significant"
    ))
  
  # Filter for significant differences
  message("Significant Differences")
  significant_differences <- subset(table_topics, p_adj < 0.05)
  print(significant_differences$Topics)
  
  message("Not Significant Differences")
  not_significant_differences <- subset(table_topics, p_adj > 0.05)
  print(not_significant_differences$Topics)
} else {
  message("ANOVA p>=0.05 means are not significantly different")
}
```

    ## ANOVA p<0.05 means are different

    ## Significant Differences

    ##  [1] "joy_China-joy_Belarus"            "joy_Kazakhstan-joy_Belarus"      
    ##  [3] "joy_NATO-joy_Belarus"             "joy_Poland-joy_Belarus"          
    ##  [5] "joy_Russia-joy_Belarus"           "joy_Seccessionist-joy_Belarus"   
    ##  [7] "joy_Turkey-joy_Belarus"           "joy_Ukraine-joy_Belarus"         
    ##  [9] "joy_UN-joy_Belarus"               "joy_USA-joy_Belarus"             
    ## [11] "joy_Europe-joy_China"             "joy_Kazakhstan-joy_China"        
    ## [13] "joy_NATO-joy_China"               "joy_other-joy_China"             
    ## [15] "joy_Poland-joy_China"             "joy_Russia-joy_China"            
    ## [17] "joy_Seccessionist-joy_China"      "joy_Turkey-joy_China"            
    ## [19] "joy_Ukraine-joy_China"            "joy_UN-joy_China"                
    ## [21] "joy_USA-joy_China"                "joy_Kazakhstan-joy_Europe"       
    ## [23] "joy_NATO-joy_Europe"              "joy_Poland-joy_Europe"           
    ## [25] "joy_Russia-joy_Europe"            "joy_Seccessionist-joy_Europe"    
    ## [27] "joy_Turkey-joy_Europe"            "joy_Ukraine-joy_Europe"          
    ## [29] "joy_UN-joy_Europe"                "joy_USA-joy_Europe"              
    ## [31] "joy_NATO-joy_Kazakhstan"          "joy_other-joy_Kazakhstan"        
    ## [33] "joy_Poland-joy_Kazakhstan"        "joy_Russia-joy_Kazakhstan"       
    ## [35] "joy_Seccessionist-joy_Kazakhstan" "joy_Turkey-joy_Kazakhstan"       
    ## [37] "joy_Ukraine-joy_Kazakhstan"       "joy_UN-joy_Kazakhstan"           
    ## [39] "joy_USA-joy_Kazakhstan"           "joy_other-joy_NATO"              
    ## [41] "joy_Poland-joy_NATO"              "joy_Russia-joy_NATO"             
    ## [43] "joy_Seccessionist-joy_NATO"       "joy_Ukraine-joy_NATO"            
    ## [45] "joy_UN-joy_NATO"                  "joy_USA-joy_NATO"                
    ## [47] "joy_Poland-joy_other"             "joy_Russia-joy_other"            
    ## [49] "joy_Seccessionist-joy_other"      "joy_Turkey-joy_other"            
    ## [51] "joy_Ukraine-joy_other"            "joy_UN-joy_other"                
    ## [53] "joy_USA-joy_other"                "joy_Russia-joy_Poland"           
    ## [55] "joy_Seccessionist-joy_Poland"     "joy_Turkey-joy_Poland"           
    ## [57] "joy_Ukraine-joy_Poland"           "joy_UN-joy_Poland"               
    ## [59] "joy_USA-joy_Poland"               "joy_Turkey-joy_Russia"           
    ## [61] "joy_Ukraine-joy_Russia"           "joy_UN-joy_Russia"               
    ## [63] "joy_Turkey-joy_Seccessionist"     "joy_Ukraine-joy_Seccessionist"   
    ## [65] "joy_UN-joy_Seccessionist"         "joy_Ukraine-joy_Turkey"          
    ## [67] "joy_UN-joy_Turkey"                "joy_USA-joy_Turkey"              
    ## [69] "joy_UN-joy_Ukraine"               "joy_USA-joy_Ukraine"             
    ## [71] "joy_USA-joy_UN"

    ## Not Significant Differences

    ## [1] "joy_Europe-joy_Belarus"       "joy_other-joy_Belarus"       
    ## [3] "joy_other-joy_Europe"         "joy_Turkey-joy_NATO"         
    ## [5] "joy_Seccessionist-joy_Russia" "joy_USA-joy_Russia"          
    ## [7] "joy_USA-joy_Seccessionist"

<figure>
<img src="github_Russia_files/figure-gfm/figure4-1.jpeg"
alt="Figure 4. Joy Probabilities Tukey-HSD 95% Confidence Intervals" />
<figcaption aria-hidden="true">Figure 4. Joy Probabilities Tukey-HSD 95%
Confidence Intervals</figcaption>
</figure>

``` r
rm(list=ls()[! ls() %in% c("anger","fear", "joy", "sadness", "topics", "trust")])
```

#### Sadness Probabilities per Topic

``` r
# Perform ANOVA
anova_sadness <- aov(value ~ topics, data = sadness)
anova_summary <- summary(anova_sadness)

# Check if p-value is less than 0.05
if (anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  message("ANOVA p<0.05 means are different")
  
  # Perform Tukey's post-hoc analysis
  tukey_topics <- TukeyHSD(anova_sadness)
  table_topics <- as.data.frame(tukey_topics$topics)
  table_topics <- janitor::clean_names(table_topics)
  table_topics <- table_topics |> 
    mutate_if(is.numeric, round, 4) |> 
    tibble::rownames_to_column() |> 
    rename('Topics' = 'rowname') |> 
    mutate(status = case_when(
      p_adj < 0.05 ~ 'significant',
      #p_adj >= 0.05 ~ 'not_significant'
      .default = "not_significant"
    ))
  
  # Filter for significant differences
  message("Significant Differences")
  significant_differences <- subset(table_topics, p_adj < 0.05)
  print(significant_differences$Topics)
  
  message("Not Significant Differences")
  not_significant_differences <- subset(table_topics, p_adj > 0.05)
  print(not_significant_differences$Topics)
} else {
  message("ANOVA p>=0.05 means are not significantly different")
}
```

    ## ANOVA p<0.05 means are different

    ## Significant Differences

    ##  [1] "sadness_China-sadness_Belarus"           
    ##  [2] "sadness_Europe-sadness_Belarus"          
    ##  [3] "sadness_Kazakhstan-sadness_Belarus"      
    ##  [4] "sadness_NATO-sadness_Belarus"            
    ##  [5] "sadness_other-sadness_Belarus"           
    ##  [6] "sadness_Poland-sadness_Belarus"          
    ##  [7] "sadness_Russia-sadness_Belarus"          
    ##  [8] "sadness_Seccessionist-sadness_Belarus"   
    ##  [9] "sadness_Turkey-sadness_Belarus"          
    ## [10] "sadness_Ukraine-sadness_Belarus"         
    ## [11] "sadness_UN-sadness_Belarus"              
    ## [12] "sadness_USA-sadness_Belarus"             
    ## [13] "sadness_Europe-sadness_China"            
    ## [14] "sadness_Kazakhstan-sadness_China"        
    ## [15] "sadness_NATO-sadness_China"              
    ## [16] "sadness_other-sadness_China"             
    ## [17] "sadness_Poland-sadness_China"            
    ## [18] "sadness_Russia-sadness_China"            
    ## [19] "sadness_Seccessionist-sadness_China"     
    ## [20] "sadness_Turkey-sadness_China"            
    ## [21] "sadness_Ukraine-sadness_China"           
    ## [22] "sadness_UN-sadness_China"                
    ## [23] "sadness_USA-sadness_China"               
    ## [24] "sadness_Kazakhstan-sadness_Europe"       
    ## [25] "sadness_NATO-sadness_Europe"             
    ## [26] "sadness_other-sadness_Europe"            
    ## [27] "sadness_Poland-sadness_Europe"           
    ## [28] "sadness_Seccessionist-sadness_Europe"    
    ## [29] "sadness_Turkey-sadness_Europe"           
    ## [30] "sadness_Ukraine-sadness_Europe"          
    ## [31] "sadness_UN-sadness_Europe"               
    ## [32] "sadness_USA-sadness_Europe"              
    ## [33] "sadness_NATO-sadness_Kazakhstan"         
    ## [34] "sadness_other-sadness_Kazakhstan"        
    ## [35] "sadness_Poland-sadness_Kazakhstan"       
    ## [36] "sadness_Russia-sadness_Kazakhstan"       
    ## [37] "sadness_Seccessionist-sadness_Kazakhstan"
    ## [38] "sadness_Ukraine-sadness_Kazakhstan"      
    ## [39] "sadness_UN-sadness_Kazakhstan"           
    ## [40] "sadness_USA-sadness_Kazakhstan"          
    ## [41] "sadness_other-sadness_NATO"              
    ## [42] "sadness_Poland-sadness_NATO"             
    ## [43] "sadness_Russia-sadness_NATO"             
    ## [44] "sadness_Seccessionist-sadness_NATO"      
    ## [45] "sadness_Turkey-sadness_NATO"             
    ## [46] "sadness_Ukraine-sadness_NATO"            
    ## [47] "sadness_UN-sadness_NATO"                 
    ## [48] "sadness_USA-sadness_NATO"                
    ## [49] "sadness_Poland-sadness_other"            
    ## [50] "sadness_Russia-sadness_other"            
    ## [51] "sadness_Seccessionist-sadness_other"     
    ## [52] "sadness_Turkey-sadness_other"            
    ## [53] "sadness_Ukraine-sadness_other"           
    ## [54] "sadness_UN-sadness_other"                
    ## [55] "sadness_USA-sadness_other"               
    ## [56] "sadness_Russia-sadness_Poland"           
    ## [57] "sadness_Seccessionist-sadness_Poland"    
    ## [58] "sadness_Turkey-sadness_Poland"           
    ## [59] "sadness_Ukraine-sadness_Poland"          
    ## [60] "sadness_UN-sadness_Poland"               
    ## [61] "sadness_USA-sadness_Poland"              
    ## [62] "sadness_Seccessionist-sadness_Russia"    
    ## [63] "sadness_Turkey-sadness_Russia"           
    ## [64] "sadness_Ukraine-sadness_Russia"          
    ## [65] "sadness_UN-sadness_Russia"               
    ## [66] "sadness_USA-sadness_Russia"              
    ## [67] "sadness_Turkey-sadness_Seccessionist"    
    ## [68] "sadness_Ukraine-sadness_Seccessionist"   
    ## [69] "sadness_UN-sadness_Seccessionist"        
    ## [70] "sadness_USA-sadness_Seccessionist"       
    ## [71] "sadness_Ukraine-sadness_Turkey"          
    ## [72] "sadness_UN-sadness_Turkey"               
    ## [73] "sadness_UN-sadness_Ukraine"              
    ## [74] "sadness_USA-sadness_Ukraine"             
    ## [75] "sadness_USA-sadness_UN"

    ## Not Significant Differences

    ## [1] "sadness_Russia-sadness_Europe"     "sadness_Turkey-sadness_Kazakhstan"
    ## [3] "sadness_USA-sadness_Turkey"

<figure>
<img src="github_Russia_files/figure-gfm/figure5-1.jpeg"
alt="Figure 5. Sadness Probabilities Tukey-HSD 95% Confidence Intervals" />
<figcaption aria-hidden="true">Figure 5. Sadness Probabilities Tukey-HSD
95% Confidence Intervals</figcaption>
</figure>

``` r
rm(list=ls()[! ls() %in% c("anger","fear", "joy", "sadness", "topics", "trust")])
```

#### Trust Probabilities per Topic

``` r
# Perform ANOVA
anova_trust <- aov(value ~ topics, data = trust)
anova_summary <- summary(anova_trust)

# Check if p-value is less than 0.05
if (anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  message("ANOVA p<0.05 means are different")
  
  # Perform Tukey's post-hoc analysis
  tukey_topics <- TukeyHSD(anova_trust)
  table_topics <- as.data.frame(tukey_topics$topics)
  table_topics <- janitor::clean_names(table_topics)
  table_topics <- table_topics |> 
    mutate_if(is.numeric, round, 4) |> 
    tibble::rownames_to_column() |> 
    rename('Topics' = 'rowname') |> 
    mutate(status = case_when(
      p_adj < 0.05 ~ 'significant',
      #p_adj >= 0.05 ~ 'not_significant'
      .default = "not_significant"
    ))
  
  # Filter for significant differences
  message("Significant Differences")
  significant_differences <- subset(table_topics, p_adj < 0.05)
  print(significant_differences$Topics)
  
  message("Not Significant Differences")
  not_significant_differences <- subset(table_topics, p_adj > 0.05)
  print(not_significant_differences$Topics)
} else {
  message("ANOVA p>=0.05 means are not significantly different")
}
```

    ## ANOVA p<0.05 means are different

    ## Significant Differences

    ##  [1] "trust_China-trust_Belarus"           
    ##  [2] "trust_Europe-trust_Belarus"          
    ##  [3] "trust_Kazakhstan-trust_Belarus"      
    ##  [4] "trust_NATO-trust_Belarus"            
    ##  [5] "trust_other-trust_Belarus"           
    ##  [6] "trust_Poland-trust_Belarus"          
    ##  [7] "trust_Russia-trust_Belarus"          
    ##  [8] "trust_Seccessionist-trust_Belarus"   
    ##  [9] "trust_Turkey-trust_Belarus"          
    ## [10] "trust_Ukraine-trust_Belarus"         
    ## [11] "trust_UN-trust_Belarus"              
    ## [12] "trust_USA-trust_Belarus"             
    ## [13] "trust_Europe-trust_China"            
    ## [14] "trust_Kazakhstan-trust_China"        
    ## [15] "trust_NATO-trust_China"              
    ## [16] "trust_other-trust_China"             
    ## [17] "trust_Poland-trust_China"            
    ## [18] "trust_Russia-trust_China"            
    ## [19] "trust_Seccessionist-trust_China"     
    ## [20] "trust_Turkey-trust_China"            
    ## [21] "trust_Ukraine-trust_China"           
    ## [22] "trust_UN-trust_China"                
    ## [23] "trust_USA-trust_China"               
    ## [24] "trust_Kazakhstan-trust_Europe"       
    ## [25] "trust_NATO-trust_Europe"             
    ## [26] "trust_other-trust_Europe"            
    ## [27] "trust_Poland-trust_Europe"           
    ## [28] "trust_Russia-trust_Europe"           
    ## [29] "trust_Seccessionist-trust_Europe"    
    ## [30] "trust_Turkey-trust_Europe"           
    ## [31] "trust_Ukraine-trust_Europe"          
    ## [32] "trust_UN-trust_Europe"               
    ## [33] "trust_USA-trust_Europe"              
    ## [34] "trust_NATO-trust_Kazakhstan"         
    ## [35] "trust_other-trust_Kazakhstan"        
    ## [36] "trust_Poland-trust_Kazakhstan"       
    ## [37] "trust_Russia-trust_Kazakhstan"       
    ## [38] "trust_Seccessionist-trust_Kazakhstan"
    ## [39] "trust_Turkey-trust_Kazakhstan"       
    ## [40] "trust_Ukraine-trust_Kazakhstan"      
    ## [41] "trust_UN-trust_Kazakhstan"           
    ## [42] "trust_USA-trust_Kazakhstan"          
    ## [43] "trust_other-trust_NATO"              
    ## [44] "trust_Poland-trust_NATO"             
    ## [45] "trust_Russia-trust_NATO"             
    ## [46] "trust_Seccessionist-trust_NATO"      
    ## [47] "trust_Turkey-trust_NATO"             
    ## [48] "trust_Ukraine-trust_NATO"            
    ## [49] "trust_UN-trust_NATO"                 
    ## [50] "trust_USA-trust_NATO"                
    ## [51] "trust_Poland-trust_other"            
    ## [52] "trust_Russia-trust_other"            
    ## [53] "trust_Seccessionist-trust_other"     
    ## [54] "trust_Turkey-trust_other"            
    ## [55] "trust_Ukraine-trust_other"           
    ## [56] "trust_UN-trust_other"                
    ## [57] "trust_USA-trust_other"               
    ## [58] "trust_Russia-trust_Poland"           
    ## [59] "trust_Seccessionist-trust_Poland"    
    ## [60] "trust_Turkey-trust_Poland"           
    ## [61] "trust_Ukraine-trust_Poland"          
    ## [62] "trust_UN-trust_Poland"               
    ## [63] "trust_USA-trust_Poland"              
    ## [64] "trust_Seccessionist-trust_Russia"    
    ## [65] "trust_Turkey-trust_Russia"           
    ## [66] "trust_Ukraine-trust_Russia"          
    ## [67] "trust_UN-trust_Russia"               
    ## [68] "trust_Ukraine-trust_Seccessionist"   
    ## [69] "trust_UN-trust_Seccessionist"        
    ## [70] "trust_USA-trust_Seccessionist"       
    ## [71] "trust_Ukraine-trust_Turkey"          
    ## [72] "trust_UN-trust_Turkey"               
    ## [73] "trust_USA-trust_Turkey"              
    ## [74] "trust_UN-trust_Ukraine"              
    ## [75] "trust_USA-trust_Ukraine"             
    ## [76] "trust_USA-trust_UN"

    ## Not Significant Differences

    ## [1] "trust_USA-trust_Russia"           "trust_Turkey-trust_Seccessionist"

<figure>
<img src="github_Russia_files/figure-gfm/figure6-1.jpeg"
alt="Figure 6. Sadness Probabilities Tukey-HSD 95% Confidence Intervals" />
<figcaption aria-hidden="true">Figure 6. Sadness Probabilities Tukey-HSD
95% Confidence Intervals</figcaption>
</figure>

``` r
rm(list=ls())
```

### Segmented Regression

``` r
load("Data/Data_for_regression_Russia.Rda")
# Find the indices of columns containing the word "other"
cols_to_drop <- grep("other", colnames(data_daily_RU))

# Drop the columns from the data frame
data_daily_RU <- data_daily_RU[, -cols_to_drop]
rm(cols_to_drop)

colnames(data_daily_RU) <- gsub("Insurgents", "Seccessionist", colnames(data_daily_RU))

data_daily_RU <- data_daily_RU |> 
  rename('tw_eng' = "engagement",
         'fb_eng' = 'fb_data.total_engagement_count') |> 
  mutate(Anger_Russia_Ukraine_Dif = anger_Russia - anger_Ukraine)
```

    ## Dependent variables with significant change in slope:

    ## Ukraine(positive) 
    ## Seccessionist(negative) 
    ## Russia(negative) 
    ## USA(negative) 
    ## Belarus(positive) 
    ## Turkey(negative) 
    ## Kazakhstan(positive) 
    ## Europe(negative) 
    ## China(negative) 
    ## NATO(positive) 
    ## UN(positive) 
    ## anger_Ukraine(positive) 
    ## anger_Seccessionist(negative) 
    ## anger_Russia(positive) 
    ## anger_USA(negative) 
    ## anger_Belarus(positive) 
    ## anger_Turkey(negative) 
    ## anger_Kazakhstan(positive) 
    ## anger_Europe(negative) 
    ## anger_China(negative) 
    ## anger_NATO(positive) 
    ## anger_UN(positive) 
    ## fear_Ukraine(positive) 
    ## fear_Seccessionist(positive) 
    ## fear_USA(negative) 
    ## fear_Belarus(positive) 
    ## fear_Turkey(negative) 
    ## fear_Kazakhstan(positive) 
    ## fear_Europe(negative) 
    ## fear_Poland(positive) 
    ## fear_China(negative) 
    ## fear_NATO(positive) 
    ## fear_UN(positive) 
    ## joy_Ukraine(positive) 
    ## joy_Seccessionist(negative) 
    ## joy_Russia(positive) 
    ## joy_USA(negative) 
    ## joy_Belarus(negative) 
    ## joy_Turkey(negative) 
    ## joy_Kazakhstan(positive) 
    ## joy_Europe(negative) 
    ## joy_Poland(positive) 
    ## joy_China(negative) 
    ## joy_NATO(positive) 
    ## joy_UN(positive) 
    ## sadness_Ukraine(positive) 
    ## sadness_Seccessionist(negative) 
    ## sadness_Russia(positive) 
    ## sadness_USA(negative) 
    ## sadness_Belarus(positive) 
    ## sadness_Turkey(negative) 
    ## sadness_Kazakhstan(positive) 
    ## sadness_Europe(negative) 
    ## sadness_Poland(positive) 
    ## sadness_China(negative) 
    ## sadness_NATO(positive) 
    ## sadness_UN(positive) 
    ## trust_Ukraine(positive) 
    ## trust_Seccessionist(negative) 
    ## trust_Russia(positive) 
    ## trust_USA(negative) 
    ## trust_Belarus(negative) 
    ## trust_Turkey(negative) 
    ## trust_Kazakhstan(positive) 
    ## trust_Europe(negative) 
    ## trust_Poland(positive) 
    ## trust_China(negative) 
    ## trust_NATO(positive) 
    ## trust_UN(positive) 
    ## fb_eng(negative) 
    ## tw_eng(negative) 
    ## Ukraine_tw_eng(negative) 
    ## Ukraine_fb_eng(negative) 
    ## Seccessionist_tw_eng(negative) 
    ## Seccessionist_fb_eng(negative) 
    ## Russia_tw_eng(negative) 
    ## Russia_fb_eng(negative) 
    ## USA_tw_eng(negative) 
    ## USA_fb_eng(negative) 
    ## Belarus_tw_eng(negative) 
    ## Belarus_fb_eng(negative) 
    ## Turkey_tw_eng(negative) 
    ## Turkey_fb_eng(negative) 
    ## Kazakhstan_tw_eng(negative) 
    ## Kazakhstan_fb_eng(negative) 
    ## Europe_tw_eng(negative) 
    ## Europe_fb_eng(negative) 
    ## Poland_tw_eng(negative) 
    ## Poland_fb_eng(negative) 
    ## China_fb_eng(negative) 
    ## NATO_tw_eng(negative) 
    ## NATO_fb_eng(negative) 
    ## UN_tw_eng(negative) 
    ## UN_fb_eng(negative)

    ## Dependent variables that are not significant change in slope:

    ## Poland 
    ## anger_Poland 
    ## fear_Russia 
    ## Anger_Russia_Ukraine_Dif 
    ## China_tw_eng

``` r
# Create a data frame to store the table data
table_data <- data.frame(
  Model = character(),
  Significance = character(),
  Direction = character(),
  `P Value` = character(),
  stringsAsFactors = FALSE
)

# Populate the data frame with significant models data
for (dep_var_direction in significant_vars) {
  model_name <- sub("\\(.*", "", dep_var_direction)  # Extract everything before the parenthesis
  direction <- str_extract(dep_var_direction, "\\((.*?)\\)")
  direction <- substr(direction, 2, nchar(direction) - 1)  # Remove the leading and trailing parentheses

  model_info <- significant_models[[model_name]]
  p_value <- summary(model_info[[1]])$coefficients["date", "Pr(>|t|)"]
  significance <- ifelse(p_value < 0.001, "p < 0.001", ifelse(p_value < 0.01, "p < 0.01", "p < 0.05"))
  p_value <-  round(p_value, 4)
  table_data <- rbind(table_data, data.frame(Model = model_name, `P Value` = p_value, Significance = significance, Direction = direction))
}

table_data <- table_data |> 
  arrange(Model)
table <- flextable(table_data)
table <- theme_vanilla(table)
table_caption = c("Table 1", "Segmented Regression Results")
table <- table |>
  add_header_lines(values = table_caption) |>
  bold(part = "header", i = 1) |>
  italic(part = "header", i = c(2:length(table_caption))) |>
  align(part = "header", i = c(1:length(table_caption)), align = "left") |>
  border(part = "head", i = c(1:length(table_caption)),
         border = list("width" = 0, color = "black", style = "solid")) |>
  set_table_properties(align = "left", layout = "autofit") |>
  line_spacing(space = 0.8, part = "all") |>
  paginate(init = FALSE, hdr_ftr = TRUE)
table
```

<div class="tabwid tabwid_left"><style>.cl-50011c52{table-layout:auto;}.cl-4ffc71b6{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4ffc71c0{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4ffc71de{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4ffe4342{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 0.8;background-color:transparent;}.cl-4ffe4360{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 0.8;background-color:transparent;}.cl-4ffe4361{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 0.8;background-color:transparent;}.cl-4ffe4374{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 0.8;background-color:transparent;}.cl-4ffe50e4{background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4ffe50e5{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4ffe50ee{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4ffe510c{background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4ffe5116{background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4ffe5117{background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4ffe5148{background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4ffe5152{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4ffe515c{background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-50011c52'><thead><tr style="overflow-wrap:break-word;"><th  colspan="4"class="cl-4ffe50e4"><p class="cl-4ffe4342"><span class="cl-4ffc71b6">Table 1</span></p></th></tr><tr style="overflow-wrap:break-word;"><th  colspan="4"class="cl-4ffe50e4"><p class="cl-4ffe4342"><span class="cl-4ffc71c0">Segmented Regression Results</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-4ffe50e5"><p class="cl-4ffe4342"><span class="cl-4ffc71b6">Model</span></p></th><th class="cl-4ffe50ee"><p class="cl-4ffe4360"><span class="cl-4ffc71b6">P.Value</span></p></th><th class="cl-4ffe50e5"><p class="cl-4ffe4342"><span class="cl-4ffc71b6">Significance</span></p></th><th class="cl-4ffe50e5"><p class="cl-4ffe4342"><span class="cl-4ffc71b6">Direction</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-4ffe510c"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Belarus</span></p></td><td class="cl-4ffe5116"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe510c"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe510c"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Belarus_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Belarus_tw_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">China</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">China_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Europe</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Europe_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Europe_tw_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Kazakhstan</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Kazakhstan_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Kazakhstan_tw_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0122</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.05</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">NATO</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">NATO_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">NATO_tw_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Poland_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Poland_tw_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Russia</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0007</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Russia_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Russia_tw_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Seccessionist</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Seccessionist_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Seccessionist_tw_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Turkey</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Turkey_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Turkey_tw_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">UN</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">UN_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">UN_tw_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">USA</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">USA_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">USA_tw_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Ukraine</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Ukraine_fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">Ukraine_tw_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">anger_Belarus</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0005</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">anger_China</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">anger_Europe</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">anger_Kazakhstan</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">anger_NATO</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">anger_Russia</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">anger_Seccessionist</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">anger_Turkey</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">anger_UN</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">anger_USA</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">anger_Ukraine</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fb_eng</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fear_Belarus</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0036</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.01</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fear_China</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fear_Europe</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fear_Kazakhstan</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fear_NATO</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fear_Poland</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0023</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.01</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fear_Seccessionist</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fear_Turkey</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fear_UN</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fear_USA</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">fear_Ukraine</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_Belarus</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0037</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.01</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_China</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_Europe</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_Kazakhstan</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_NATO</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_Poland</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_Russia</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_Seccessionist</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_Turkey</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_UN</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_USA</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">joy_Ukraine</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_Belarus</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_China</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_Europe</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_Kazakhstan</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_NATO</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_Poland</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_Russia</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_Seccessionist</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_Turkey</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_UN</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_USA</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">sadness_Ukraine</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_Belarus</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_China</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_Europe</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_Kazakhstan</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_NATO</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_Poland</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_Russia</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_Seccessionist</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_Turkey</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_UN</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_USA</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">trust_Ukraine</span></p></td><td class="cl-4ffe5148"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5117"><p class="cl-4ffe4361"><span class="cl-4ffc71de">positive</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-4ffe5152"><p class="cl-4ffe4361"><span class="cl-4ffc71de">tw_eng</span></p></td><td class="cl-4ffe515c"><p class="cl-4ffe4374"><span class="cl-4ffc71de">0.0000</span></p></td><td class="cl-4ffe5152"><p class="cl-4ffe4361"><span class="cl-4ffc71de">p &lt; 0.001</span></p></td><td class="cl-4ffe5152"><p class="cl-4ffe4361"><span class="cl-4ffc71de">negative</span></p></td></tr></tbody></table></div>

``` r
# Function to plot a segmented model
# Loop over each significant segmented model and create ggplot plots
# selected_models <- significant_models[grepl(paste(c("Russia", "Ukraine", "Insurgents", "China", "USA", "fb_eng", "tw_eng"), collapse = "|"), names(significant_models))]

selected_models <- significant_models2[
  grepl(paste(c("Russia", "Ukraine"), collapse = "|"), names(significant_models2)) |
  names(significant_models2) %in% c("fb_eng", "tw_eng")
]


for (dep_var in names(selected_models)) {
  # Extract segmented model data
  model_data <- data.frame(date = data_daily_RU$date,
                           y = data_daily_RU[[dep_var]],
                           fitted = fitted(significant_models2[[dep_var]]),
                           segment = predict(significant_models2[[dep_var]]))

  # Create ggplot plot
  plot <- ggplot(model_data, aes(x = date)) +
    geom_line(aes(y = y), color = "blue") +
    geom_line(aes(y = fitted), color = "purple", linetype = "dashed", linewidth = 2) +
    geom_line(aes(y = segment), color = "darkgray", linetype = "dashed", linewidth = 2) +
    geom_vline(xintercept = as.Date("2022-02-24"),
             col = "red", lwd = 2, linetype = "dotted") +
    labs(title = paste("Predicted Segmented Model Russia Media Source for", dep_var),
         x = "Date", y = "Value") +
    scale_x_date(date_breaks = "1 month", date_labels = "%d %b %y", date_minor_breaks = "15 days") +
    theme_hc() +
    theme(text=element_text(size=12,family="sans"),
        title=element_text(size=12,family="sans"),
        axis.text.x=element_text(size=12, angle = 60, hjust=1, family = "sans"),
        axis.text.y=element_text(family="sans"),
        axis.title.x=element_text(vjust=-0.25, size=12, family="sans"),
        axis.title.y=element_text(vjust=-0.25, size=12, family="sans"),
        legend.position="none", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=12, family="sans")) 
  print(plot)
  filename <- paste0("Russia_Images/", dep_var, ".jpeg")
  ggsave(filename, plot, bg = "white",
         width = 12, height = 10, dpi = 300)
}
```

![](github_Russia_files/figure-gfm/fig-1.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-2.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-3.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-4.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-5.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-6.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-7.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-8.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-9.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-10.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-11.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-12.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-13.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-14.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-15.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-16.jpeg)<!-- -->![](github_Russia_files/figure-gfm/fig-17.jpeg)<!-- -->
