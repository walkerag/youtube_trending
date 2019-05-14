
##############################################
# Name: 02_data_wrangling.R
# Author: Adam Walker
# Date: May 2019
# Description: Wrangle API data into dataframe for analysis
# Creates two dataframes from 01_data_pull results:
# video.df: At video ID level. Contains stats such as likes, comments, also has title, description
# tag.df: Each row is a video ID - tag, where videos can have many tags
##############################################

rm(list=ls())

# Disable scientific notation
options(scipen=999)

# Your path here!
path <- '/Users/walkerag/Documents/youtube_trending/data/'


########################
# READ IN DATA
########################

# Read in data from 01 code
vid_stats_all <- readRDS(file = paste0(path,"vid_stats_all.rds"))
vid_details_all <- readRDS(file = paste0(path,"vid_details_all.rds"))
vid_content_all <- readRDS(file = paste0(path,"vid_content_all.rds"))


########################
# VIDEO DATAFRAME
########################

# Get duration
durationFunction <- function (x) {
  return(tryCatch(x[[4]][[1]]$contentDetails$duration, error=function(e) NA))
}
duration <- lapply(vid_content_all, durationFunction )
duration[sapply(duration, is.null)] <- NA
duration <- unlist( duration )

# Get title
titleFunction <- function (x) {
  return(tryCatch(x[[4]][[1]]$snippet$localized$title, error=function(e) NA))
}
title <- lapply(vid_details_all, titleFunction )
title[sapply(title, is.null)] <- NA
title <- unlist( title )

# Get description
descriptionFunction <- function (x) { return(tryCatch(x[[4]][[1]]$snippet$localized$description, error=function(e) NA)) }
description <- lapply(vid_details_all, descriptionFunction )
description[sapply(description, is.null)] <- NA
description <- unlist( description )

# Get default audio language
defaultAudioFunction <- function (x) { return(tryCatch(x[[4]][[1]]$snippet$defaultAudioLanguage, error=function(e) NA)) }
defaultAudio <- lapply(vid_details_all, defaultAudioFunction )
defaultAudio[sapply(defaultAudio, is.null)] <- NA
defaultAudio <- unlist( defaultAudio )

# Get category ID
categoryFunction <- function (x) { return(tryCatch(x[[4]][[1]]$snippet$categoryId, error=function(e) NA)) }
category <- lapply(vid_details_all, categoryFunction )
category[sapply(category, is.null)] <- NA
category <- unlist( category )
head(category)

# Get channel ID
channelFunction <- function (x) { return(tryCatch(x[[4]][[1]]$snippet$channelId, error=function(e) NA)) }
channel <- lapply(vid_details_all, channelFunction )
channel[sapply(channel, is.null)] <- NA
channel <- unlist( channel )
head(channel)

# Get definition
definitionFunction <- function (x) { return(tryCatch(x[[4]][[1]]$contentDetails$definition, error=function(e) NA)) }
definition <- lapply(vid_content_all, definitionFunction )
definition[sapply(definition, is.null)] <- NA
definition <- unlist( definition )
head(definition)

# Get caption
captionFunction <- function (x) { return(tryCatch(x[[4]][[1]]$contentDetails$caption, error=function(e) NA)) }
caption <- lapply(vid_content_all, captionFunction )
caption[sapply(caption, is.null)] <- NA
caption <- unlist( caption )
head(caption)

# Get dimension
dimensionFunction <- function (x) { return(tryCatch(x[[4]][[1]]$contentDetails$dimension, error=function(e) NA)) }
dimension <- lapply(vid_content_all, dimensionFunction )
dimension[sapply(dimension, is.null)] <- NA
dimension <- unlist( dimension )
head(dimension)

# Get licensedContent
licensedContentFunction <- function (x) { return(tryCatch(x[[4]][[1]]$contentDetails$licensedContent, error=function(e) NA)) }
licensedContent <- lapply(vid_content_all, licensedContentFunction )
licensedContent[sapply(licensedContent, is.null)] <- NA
licensedContent <- unlist( licensedContent )
head(licensedContent)

# Get video statistics
video_id <- sapply(vid_stats_all,function(x) return(tryCatch(x$id,simplify=FALSE,error=function(e) NA)))
video_id[sapply(video_id, is.null)] <- NA
video_id <- unlist(video_id)

viewCount <- sapply(vid_stats_all,function(x) return(tryCatch(x$viewCount,simplify=FALSE,error=function(e) NA)))
viewCount[sapply(viewCount, is.null)] <- NA
viewCount <- unlist(viewCount)

likeCount <- sapply(vid_stats_all,function(x) return(tryCatch(x$likeCount,simplify=FALSE,error=function(e) NA)))
likeCount[sapply(likeCount, is.null)] <- NA
likeCount <- unlist(likeCount)

dislikeCount <- sapply(vid_stats_all,function(x) return(tryCatch(x$dislikeCount,simplify=FALSE,error=function(e) NA)))
dislikeCount[sapply(dislikeCount, is.null)] <- NA
dislikeCount <- unlist(dislikeCount)

favoriteCount <- sapply(vid_stats_all,function(x) return(tryCatch(x$favoriteCount,simplify=FALSE,error=function(e) NA)))
favoriteCount[sapply(favoriteCount, is.null)] <- NA
favoriteCount <- unlist(favoriteCount)

commentCount <- sapply(vid_stats_all,function(x) return(tryCatch(x$commentCount,simplify=FALSE,error=function(e) NA)))
commentCount[sapply(commentCount, is.null)] <- NA
commentCount <- unlist(commentCount)

# Combine all statistics into a df
comb <- data.frame(video_id, viewCount, likeCount, dislikeCount, favoriteCount, commentCount, stringsAsFactors = FALSE)
rm( video_id, viewCount, likeCount, dislikeCount, favoriteCount, commentCount)

# Video level dataset
row_max <- min(nrow(comb)
               , length(duration)
               , length(defaultAudio)
               , length(category)
               , length(definition)
               , length(caption)
               , length(dimension)
               , length(licensedContent)
               , length(title)
               , length(description)
               , length(channel)
               )
video.df <- cbind( comb[1:row_max,]
                    , duration = duration[1:row_max]
                    , defaultAudio = defaultAudio[1:row_max]
                    , category = category[1:row_max]
                    , definition = definition[1:row_max]
                    , caption = caption[1:row_max]
                    , dimension = dimension[1:row_max]
                    , licensedContent = licensedContent[1:row_max]
                    , title = title[1:row_max]
                    , description = description[1:row_max]
                    , channel = channel[1:row_max]
                    )

rm( comb, duration, defaultAudio, category, definition, caption, dimension, licensedContent, title, description, channel )

tail( video.df )
View( head(video.df) )

# Save out
saveRDS(video.df, file = paste0(path,"video_df.rds"))


########################
# TAGS DATAFRAME
########################

# Pull video tags
tag_all <- NULL
tag_id_all <- NULL
for(i in 1:length(vid_details_all)){
  
  # Get video
  vid <- vid_details_all[[i]]

  # See if video pulled correctly
  if( length(vid) > 0 ){

    # Pull tags
    tag <- vid[[4]][[1]]$snippet$tags
    tag <- unlist( tag )
    
    # Get corresponding video ID
    tag_id <- rep( vid[[4]][[1]]$id, length(tag) )
    
    #Concat the tags together, as well as the video IDs
    tag_all <- append( tag_all, tag )
    tag_id_all <- append( tag_id_all, tag_id )
    
    rm( x, tag, tag_id )

  } else {
    
    print(i)
    print("Missing video")
    
  }
  
}

# Combine as datafram
tag.df <- data.frame( cbind(tag = tag_all, video_id = tag_id_all) )
head( tag.df )

# Save out
saveRDS(tag.df, file = paste0(path,"tag_df.rds"))
