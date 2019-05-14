
##############################################
# Name: 03_explore.R
# Author: Adam Walker
# Date: May 2019
# Description: Explore some YouTube trending video data
# Includes some initial data summaries and plots, with more fleshed out visuals later on
##############################################

rm(list=ls())
gc()

library(ggplot2)
library(dplyr)

library(cld3)
library(textcat)

library(tidytext)
library(tidyr)
library(widyr)
library(igraph)
library(ggraph)

# Disable scientific notation
options(scipen=999)

# Your path here!
path <- '/Users/walkerag/Documents/youtube_trending/data/'


########################
# READ IN DATA
########################

# Read in data from 02 code
video.df <- readRDS(file = paste0(path,"video_df.rds"))
tag.df <- readRDS(file = paste0(path,"tag_df.rds"))


########################
# INITIAL SUMMARIES
########################

# Exploratory checks
# glimpse more informative than summary initially
glimpse(video.df)
summary(video.df)
video.df %>% head(30)
View( video.df %>% head(30) )

glimpse(tag.df)
summary(tag.df)

names(video.df)
table(video.df$definition)
table(video.df$licensedContent)
table(video.df$category)

table(video.df$favoriteCount)
# Invariant, so drop
video.df %>% select(-favoriteCount) -> video.df


############################
# MISSING DATA, FORMATTING
############################

# Check NAs by column
na_count <- sapply(video.df, function(y) sum(length(which(is.na(y)))) )
na_count
View( video.df %>% filter(is.na(video_id)) )

# Remove NA ids
# Often would want to rename df here
video.df %>% filter( !is.na(video_id) ) -> video.df

# Check NAs by column
na_count <- sapply(video.df, function(y) sum(length(which(is.na(y)))) )
na_count

# Convert character variables to numeric
head( video.df[,1:15] )
video.df$viewCount <- as.numeric( video.df$viewCount )
video.df$likeCount <- as.numeric( video.df$likeCount )
video.df$dislikeCount <- as.numeric( video.df$dislikeCount )
video.df$favoriteCount <- as.numeric( video.df$favoriteCount )
video.df$commentCount <- as.numeric( video.df$commentCount )
video.df$category <- as.numeric( video.df$category )

View( video.df %>% filter( is.na(likeCount) ) )

# Flag if likes/dislikes disabled
video.df$likes_disabled <- ifelse( is.na(video.df$likeCount), 1, 0 )
summary( video.df$likes_disabled )

# Coalesce missing likes/dislikes to zero
video.df$likeCount <- ifelse( is.na(video.df$likeCount), 0, video.df$likeCount )
video.df$dislikeCount <- ifelse( is.na(video.df$dislikeCount), 0, video.df$dislikeCount )

# Flag if comments disabled
video.df$comments_disabled <- ifelse( is.na(video.df$commentCount), 1, 0 )
summary( video.df$comments_disabled )

# Coalesce missing comments to zero
video.df$commentCount <- ifelse( is.na(video.df$commentCount), 0, video.df$commentCount )
summary( video.df$commentCount )

# One video has a missing view count
# Looks like a video created by Google
video.df %>% filter(is.na(viewCount))
# https://www.youtube.com/watch?v=jYdKqVUd3GQ
# OK removing this one...
video.df %>% filter( !is.na(viewCount) ) -> video.df


############################
# QUICK PLOTS
############################

# Once we've got our data in a usable format, want to start plotting the data ASAP

# Numeric columns
video.df %>% select_if(is.numeric) %>% head(10)

# Extremely skewed
ggplot( video.df, aes(viewCount) ) +
  geom_histogram()

# Top videos by view count
video.df %>%
  arrange( desc(viewCount) ) %>%
  select( viewCount, title ) %>%
  head(10)

ggplot( video.df, aes(likeCount) ) +
  geom_histogram()

video.df %>%
  arrange( desc(likeCount) ) %>%
  select( viewCount, title ) %>%
  head(10)

# With a skewed distribution, how to summarize?
# Likely want to focus on medians and percentiles over something like mean
summary( video.df$viewCount )
round( quantile( video.df$viewCount, c(0.1,0.2,0.5,0.8,0.9,0.95,0.99,0.999)), 0 )

ggplot( video.df, aes(dislikeCount) ) +
  geom_histogram()

# Simple favorability
video.df$like_ratio <- video.df$likeCount / video.df$dislikeCount

ggplot( video.df, aes( like_ratio ) ) +
  geom_histogram()

# Take the natural log
video.df$like_ratio_log <- log( video.df$like_ratio )
summary( video.df$like_ratio_log )

ggplot( video.df, aes( like_ratio_log ) ) +
  geom_histogram()
# That's looking like a distribution we can work with!

# Most positive
video.df %>%
  arrange( desc(like_ratio_log) ) %>%
  select( title ) %>%
  head(10)
# BTS are dominating the favorability rankings


############################
# FORMAT VIDEO DURATION
############################

# VIDEO DURATION

sum(is.na(video.df$duration))

#Format duration
video.df$duration2<-gsub("PT","",video.df$duration)

# Get location of hours, minutes, and seconds indicators in string
video.df$H <- as.numeric(regexpr("H",video.df$duration2))
video.df$M <- as.numeric(regexpr("M",video.df$duration2))
video.df$S <- as.numeric(regexpr("S",video.df$duration2))

# A very convoluted process to convert video time into seconds
video.df$HN<-as.numeric( ifelse(video.df$H>0, substr(video.df$duration2, 1, (video.df$H-1)), 0) )

video.df$MN<-as.numeric(ifelse(video.df$H>0 & video.df$M>0,substr(video.df$duration2,(video.df$H+1),(video.df$M-1)),0))
video.df$MN<-as.numeric(ifelse(video.df$H<0 & video.df$M>0,substr(video.df$duration2,1,(video.df$M-1)),video.df$MN))
summary(video.df$MN)

video.df$SN<-as.numeric(ifelse(video.df$M>0 & video.df$S>0,substr(video.df$duration2,(video.df$M+1),(video.df$S-1)),0))
video.df$SN<-as.numeric(ifelse(video.df$M<0 & video.df$S>0,substr(video.df$duration2,1,(video.df$S-1)),video.df$SN))
summary(video.df$SN)

#Final time in seconds
video.df$HN <- coalesce(video.df$HN, 0)
video.df$MN <- coalesce(video.df$MN, 0)
video.df$SN <- coalesce(video.df$SN, 0)
video.df$seconds<-(video.df$HN*3600)+(video.df$MN*60)+(video.df$SN)
summary(video.df$seconds)

# Trying out histograms vs density plots
ggplot(video.df, aes(seconds)) +
  geom_density()
ggplot(video.df, aes(seconds)) +
  geom_histogram()

ggplot(video.df, aes( log(seconds) )) +
  geom_density()
ggplot(video.df, aes( log(seconds) )) +
  geom_histogram()

# Check some of the outliers
video.df %>% filter( seconds<10 )
video.df %>% filter( seconds>34000 )


############################
# VIDEO LANGUAGE
############################

# Look at listed languages
sort( table( video.df$defaultAudio ) )
# Can see that English is coded multiple ways: en, en-US, en-GB

video.df %>% filter( defaultAudio=='ru' ) %>% select( title )

# Try out textcat
textcat(c("This is an english sentence.", "Das ist ein deutscher satz."))
video.df$detected_language_textcat <- textcat( video.df$description )

# Try out cld3
detect_language(c("This is an english sentence.", "Das ist ein deutscher satz."))
video.df$detected_language_cld3 <- detect_language( as.character(video.df$description) )

sort( table( video.df$detected_language ) )
sort( table( video.df$detected_language_cld3 ) )

View( video.df %>% select(title, defaultAudio, detected_language_textcat, detected_language_cld3 ) %>% sample_n(100) )


#####################
# TAGS
#####################

# Most popular tags
tag.df %>%
  group_by( tag ) %>%
  summarise(
    total=n()
    ) %>%
  arrange(desc(total))

# Convert to character
tag.df$tag <- as.character(tag.df$tag) 
tag.df$video_id <- as.character(tag.df$video_id) 

# Tokenize, restrict to english language
video.df %>% filter(detected_language_cld3=="en") %>% pull(video_id) -> english.language.videos

tag.df.token <- tag.df %>% 
  filter( video_id %in% english.language.videos ) %>%
  unnest_tokens( input=tag, word, to_lower=TRUE,drop=FALSE )
head( tag.df.token )

tag.df.token$word <- gsub( "[^[:alnum:][:space:]]","", tag.df.token$word )
tag.df.token %>% filter( nchar(word) > 0 ) -> tag.df.token

 #Remove stop words
tag.df.token <- tag.df.token %>%
  anti_join(stop_words)

tag.df.token %>%
  group_by( word ) %>%
  summarise(
    total=n()
  ) %>%
  arrange(desc(total))

# Summarize how many videos the word appears in
tags.distinct <- tag.df.token %>% 
  group_by(word) %>% 
  mutate(
    count=n()
    ,videos=n_distinct(video_id)
  )

head( tags.distinct )

# Create correlation graph

keyword_cors <- tags.distinct %>% 
  filter( videos >= 30 ) %>%
  group_by( word ) %>%
  pairwise_cor(word, video_id, sort = TRUE, upper = FALSE)

keyword_cors %>%
  filter(correlation > .5)

keyword_cors$Correlation <- keyword_cors$correlation

set.seed(1234)
keyword_cors %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = Correlation, edge_width = Correlation), edge_colour = "#4ebdc0") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, size=9.5, 
                 point.padding = unit(0.2, "lines")) +
  theme(text = element_text(size = 9.5,family="Arial Bold")
        ,legend.key.size = unit(2.5,"line")
        ,legend.text = element_text(size=20)
        ,legend.title = element_text(size=20)
        ,panel.border = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_blank()
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,axis.title = element_blank()
        ,legend.box.background = element_blank()
        ,legend.key=element_blank()
        ,title = element_text(size=35)
  )


tag.df %>%
  filter( grepl('sp:dt',tag) ) %>% pull(video_id) -> vid.00

video.df %>%
  filter( video_id %in% vid.00 ) %>%
  select( title ) %>%
  head(20)

