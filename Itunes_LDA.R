#1BM20-Lecture REM Review Analysis Demo: This R script is based on 
#https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25?gi=8b45150a968a
#https://datascienceplus.com/analysing-ios-app-store-itunes-reviews-in-r/


# main steps in the script :
# Load libraries
# Retrieve data
# Prepare data for LDA topic analysis
# Perform the LDA Analysis 
#------------Load Libraries
# Install and load latest itunesr version 
devtools::install_github("amrrs/itunesr")
library(itunesr)
# Load other libraries
# Packages needed for LDA analysis---------------------------------------------------------
packages <- c( "magrittr",
               "scales",
               "tidyverse",
               "sentimentr",
               "stringr",
              "data.table",
              "tidytext",
              "textclean",
              "topicmodels",
              "colorspace",
              "ldatuning",
              "gmp",
              "wordcloud",
              "RColorBrewer",
              "lubridate","reshape2","textmineR")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#----------Retrieve itunes data


# App ID in Apple Store, for example Uber, WhatsApp  (google search will whow you app ids)
appstoreID = 368677368
# Create this dataframe for the aggregation of results 
df_App_allCtry = NULL
#  App Store Country Codes to get latest reviews, you can add as many , here we select apple markets in English speaking countries
appMarkets <- c("us", "gb", "au")

# Retrieve data
# Itereate all countries to retrieve review data , if a market does not include  the product , try block will skip the error and continue with the next market in the list.
for (ctry in appMarkets) {
  df_Appctry= NULL
  for( ind in c(1:10) )  {   
    try(df_App_ctry_pg<-getReviews(appstoreID,ctry,ind))
    # Add all country reviews  into one single dataframe
    df_Appctry<-rbind(df_Appctry,df_App_ctry_pg)
  } 
  df_Appctry$market<-ctry
  df_App_allCtry <- rbind(df_App_allCtry,df_Appctry)
  
}
#Format data types and values  
df_App_allCtry$Date<-as.Date(df_App_allCtry$Date)
df_App_allCtry$Rating<-as.numeric(df_App_allCtry$Rating)
#export data for further future analysis
write_excel_csv(df_App_allCtry,  "export_demo_csv.csv")

#----------Prepare data for LDA  Analysis


data <- df_App_allCtry

data<- data %>% mutate(id = row_number()) %>% rename(text=Review) %>%filter(market=="us") %>% select(text,id)
data <- data %>% mutate(noemtext=replace_non_ascii(text, replacement = ''))

#text cleaning
data$text <- sub("RT.*:", "", data$text)
data$text <- sub("@.* ", "", data$text)
text_cleaning_tokens <- data %>% 
  tidytext::unnest_tokens(word, text)
text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>% 
  anti_join(stop_words)
tokens <- text_cleaning_tokens %>% filter(!(word==""))



#----------Perform LDA  Analysis

tokens <- tokens %>% mutate(ind = row_number())
tokens <- tokens %>% group_by(id) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens [is.na(tokens)] <- ""
tokens <- tidyr::unite(tokens, text,-id,sep =" " )
tokens$text <- trimws(tokens$text)

#create DTM
dtm <- CreateDtm(tokens$text, 
                 doc_names = tokens$ID, 
                 ngram_window = c(1, 2))


#explore the basic frequency
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)

# Eliminate words appearing less than 2 times or in more than half of the
# documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]

dtm = dtm

# Running LDA -----------------------------------------------------------
k_list <- seq(1, 20, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines

#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")

#select models based on max average
model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]

#1. Top 20 terms based on phi  ---------------------------------------------
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
top20_wide <- as.data.frame(model$top_terms)

#3. word, topic relationship ---------------------------------------------
#looking at the terms allocated to the topic and their pr(word|topic)
allterms <-data.frame(t(model$phi))
allterms$word <- rownames(allterms)
rownames(allterms) <- 1:nrow(allterms)
allterms <- melt(allterms,idvars = "word") 
allterms <- allterms %>% rename(topic = variable)
FINAL_allterms <- allterms %>% group_by(topic) %>% arrange(desc(value))


# 2. Topic,word,freq ------------------------------------------------------
final_summary_words <- data.frame(top_terms = t(model$top_terms))
final_summary_words$topic <- rownames(final_summary_words)
rownames(final_summary_words) <- 1:nrow(final_summary_words)
final_summary_words <- final_summary_words %>% melt(id.vars = c("topic"))
final_summary_words <- final_summary_words %>% rename(word = value) %>% select(-variable)
final_summary_words <- left_join(final_summary_words,allterms)
final_summary_words <- final_summary_words %>% group_by(topic,word) %>%
  arrange(desc(value))
final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
  ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))

#4. per-document-per-topic probabilities ----------------------------------------------
#trying to see the topic in each document
theta_df <- data.frame(model$theta)
theta_df$document <-rownames(theta_df) 
rownames(theta_df) <- 1:nrow(theta_df)
theta_df$document <- as.numeric(theta_df$document)
theta_df <- melt(theta_df,id.vars = "document")
theta_df <- theta_df %>% rename(topic = variable) 
theta_df <- theta_df %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
FINAL_document_topic <- theta_df %>% group_by(document) %>% 
  arrange(desc(value)) %>% filter(row_number() ==1)

#5. Visualising of topics in a dendrogram ----------------------------------------------
#probability distributions called Hellinger distance, distance between 2 probability vectors
model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
plot(model$hclust)


#visualising topics of words based on the max value of phi
set.seed(1234)
pdf("cluster.pdf")
for(i in 1:length(unique(final_summary_words$topic)))
{  wordcloud(words = subset(final_summary_words ,topic == i)$word, freq = subset(final_summary_words ,topic == i)$value, min.freq = 1,
             max.words=200, random.order=FALSE, rot.per=0.35, 
             colors=brewer.pal(8, "Dark2"))}

dev.off()
