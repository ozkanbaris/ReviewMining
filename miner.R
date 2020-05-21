#Based on 
#https://towardsdatascience.com/spotify-app-review-mining-with-r-google-cloud-machine-learning-feb6f9c3b75f
#https://datascienceplus.com/analysing-ios-app-store-itunes-reviews-in-r/
#https://rpubs.com/grigory/PodCruncher

# Install and load libraries
devtools::install_github("amrrs/itunesr")
library(itunesr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(tidyverse)
library(sentimentr)
library(DataExplorer)

# App ID in Apple Store, for example Spotify 
appstoreID = 368677368
# Get information about the Spotify App
# Create this df for later purpose
df_App_allCtry = NULL
#  App Store Country Codes to get latest reviews
appMarkets <- c("us", "gb")



for (ctry in appMarkets) {
  df_Appctry= NULL
  for( ind in c(1:10) )  {   
    df_App_ctry_pg<-getReviews(appstoreID,ctry,ind)
      # Create a for loop and merge all tables in to one single df
    df_Appctry<-rbind(df_Appctry,df_App_ctry_pg)
  } 
  df_Appctry$mArket<-ctry
  df_App_allCtry <- rbind(df_App_allCtry,df_Appctry)
  
}

df_App_allCtry$Date<-as.Date(df_App_allCtry$Date)
df_App_allCtry$Rating<-as.numeric(df_App_allCtry$Rating)

# Show Ratings
ggplot(df_App_allCtry, aes(x=Rating)) + geom_histogram()

# Calculate sentiment , extract polarity
sentiment_scores <-  sentiment_by(df_App_allCtry$Review,by=NULL)
df_App_allCtry$review_sentiment <-sentiment_scores$ave_sentiment

# Add an index and give review authors an unique ID 
df_App_allCtry$ID <- seq.int(nrow(df_App_allCtry))
df_App_allCtry$ID <- as.factor(df_App_allCtry$ID)

# plot sentiment for each review, group by app version and market
ggplot(df_App_allCtry, aes(ID, review_sentiment, fill = mArket)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~App_Version, ncol = 2, scales = "free_x")
