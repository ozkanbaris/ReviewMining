#1BM20-Lecture REM Review Analysis Demo: This R script is based on 
#https://towardsdatascience.com/spotify-app-review-mining-with-r-google-cloud-machine-learning-feb6f9c3b75f
#https://datascienceplus.com/analysing-ios-app-store-itunes-reviews-in-r/
#https://rpubs.com/grigory/PodCruncher


# main steps in the script :
# Load libraries
# Retrieve data
# Sentiment Analysis 



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
              "data.table")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#----------Retrieve itunes data


# App ID in Apple Store, for example Uber, WhatsApp  (google search will whow you app ids)
appstoreID = 310633997
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



#----------Sentiment Analysis


# Calculate sentiment , extract polarity
sentiment_scores <-  sentiment_by(df_App_allCtry$Review,by=NULL)
df_App_allCtry$review_sentiment <-sentiment_scores$ave_sentiment
df_App_allCtry<-arrange(df_App_allCtry,market)

# Show Ratings in a histogram
ggplot(df_App_allCtry, aes(Rating, fill=market)) + geom_histogram()


# Add an index and give review authors an unique ID , I am not using this data in the analysis,though.
df_App_allCtry$ID <- seq.int(nrow(df_App_allCtry))
df_App_allCtry$ID <- as.factor(df_App_allCtry$ID)

df_App_allCtry <-df_App_allCtry %>% mutate ( App_Version_short = substr(App_Version,1,4) )

# plot sentiment for each review, group by app version and market
ggplot(df_App_allCtry, aes(ID, review_sentiment, fill = App_Version_short)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~market, ncol = 2, scales = "free_x")+
  theme(legend.position = c(0.8, 0.1), legend.direction = "horizontal" )


# plot superimposed density for each market 
ggplot(df_App_allCtry, aes(review_sentiment, fill = market)) + geom_density(alpha = 0.2)


