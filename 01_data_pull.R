
##############################################
# Name: 01_data_pull.R
# Author: Adam Walker
# Date: May 2019
# Description: Collect data on YouTube's trending videos
# Uses Wayback Machine urls to get YouTube video IDs, then pulls video attributes via tuber package and YouTube API.
##############################################

rm(list=ls())
gc()

# Disable scientific notation
options(scipen=999)

# Needed packages
library(RSelenium)
library(rvest)
library(tuber)

# Your path here!
path <- '/Users/walkerag/Documents/youtube_trending/data/'

# For tuber
# Need a YouTube API key and secret
yt_oauth(app_id = "YOUR_API_KEY"
         , app_secret = "YOUR_API_SECRET")


############################
# STEP 1: SCRAPE VIDEO URLS
############################

# Initialize scraper
driver <- rsDriver(browser=c("chrome"), chromever="74.0.3729.6")
remDr <- driver[["client"]]
remDr$open()

# Original page: "https://www.youtube.com/feed/trending"
# Pulled from Wayback Machine archive
trending_urls <- c(
  'https://web.archive.org/web/20190512000511/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190511000436/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190510000012/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190509001617/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190508000017/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190507000558/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190506000029/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190505000040/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190504000624/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190503001837/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190502000334/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190501000421/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190430000037/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190429000223/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190428001418/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190427000047/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190426000014/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190425000058/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190424000133/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190423000108/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190422000118/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190421000100/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190420003642/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190419003212/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190418000024/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190417000806/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190416005541/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190415001044/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190414000506/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190413003423/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190412001342/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190411002442/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190410000535/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190409002702/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190408000240/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190407001508/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190406000936/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190405001021/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190404000542/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190403000931/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190402000344/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190401003607/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190331003433/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190330001929/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190329001400/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190328000547/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190327003557/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190326000649/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190325001450/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190324000206/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190323000845/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190322001231/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190321000116/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190320001614/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190319001952/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190318000621/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190317002204/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190316000253/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190315001335/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190314000621/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190313000717/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190312000948/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190311000141/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190310004231/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190309011007/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190308001030/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190307001833/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190306000249/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190305001112/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190304002630/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190303000604/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190302001149/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190301001314/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190228000643/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190227000358/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190226000148/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190225000315/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190224000216/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190223001449/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190222002816/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190221001356/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190220000842/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190219000125/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190218002001/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190217001726/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190216000042/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190215002614/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190214001227/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190213000538/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190212001303/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190211000411/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190210000251/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190209000623/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190208001828/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190207000515/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190206001103/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190205001026/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190204002118/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190203000947/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190202004049/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190201001721/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190131001324/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190130002054/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190129001059/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190128000921/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190127000426/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190126002210/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190125001046/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190124000108/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190123000636/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190122001253/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190121000709/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190120000406/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190119000823/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190118000014/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190117002935/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190116000408/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190115000713/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190114001736/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190113003116/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190112000921/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190111000133/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190110000047/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190109000415/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190108001001/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190107002959/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190106002122/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190105001247/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190104000108/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190103011012/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190102005053/https://www.youtube.com/feed/trending'
  ,'https://web.archive.org/web/20190101002635/https://www.youtube.com/feed/trending'
)

#Loop through the URLs
good_video_links_all <- NULL
trending_dat_all <- {}
t <- 1
for( t in 1:length(trending_urls) ){
  
  #Be considerate
  Sys.sleep(0.2)
  
  trending_url <- trending_urls[t]
  print( trending_url )
  
  #SCRAPE
  remDr$navigate(trending_url)
  page_source<-remDr$getPageSource()
  trending_dat_all[[t]] <- read_html(page_source[[1]])
  rm(page_source)
  
  #Get YouTube links
  trending_dat_all[[t]] %>% html_nodes("a") %>% html_attr("href") -> video_links

  # Restrict to actual videos
  good_videos <- video_links[ grepl("\\/watch\\?v=",video_links) ]
  
  # Parse out IDs
  good_video_links <- substr(good_videos,sapply(good_videos, function(x) rev(gregexpr("v=", x)[[1]])[1]+2),nchar(good_videos))
  print( length(good_video_links) )
  
  # Add to vector
  good_video_links_all <- append( good_video_links_all, good_video_links)
  print(length(good_video_links_all))
  
  #Save output
  saveRDS(good_video_links_all, file = paste0(path,"good_video_links_all.rds"))
  
}

# Close selenium server
driver$server$stop()


####################################
# STEP 2: GET VIDEO STATS USING API
####################################

# Links from step 1
good_video_links_all <- readRDS(file = paste0(path,"good_video_links_all.rds"))

# Make sure links unique
good_video_links_all_unique <- unique( good_video_links_all )
print( length(good_video_links_all_unique) )

#Initialize empty objects
vid_stats_all <- {}
vid_details_all <- {}
vid_content_all <- {}

# Use tuber package to collect information on each video URL
i <- 1
for( i in 1:length(good_video_links_all_unique) ){
  
  print(i)
  
  # Select video ID
  id <- good_video_links_all_unique[i]

  #Get info on each video
  Sys.sleep(0.1)
  vid_stats_all[[i]] <- get_stats(video_id=id)
  Sys.sleep(0.1)
  vid_details_all[[i]] <- get_video_details(video_id=id)
  Sys.sleep(0.1)
  vid_content_all[[i]] <- get_video_details(video_id=id,part=c('contentDetails'))

  if( length( vid_details_all[[i]] ) == 0 ){ print('Missing video') }
  
  # Save every 10
  if((i %% 10)==0){
    
    #Save everything
    saveRDS(vid_stats_all, file = paste0(path,"vid_stats_all.rds"))
    saveRDS(vid_details_all, file = paste0(path,"vid_details_all.rds"))
    saveRDS(vid_content_all, file = paste0(path,"vid_content_all.rds"))

  }
  
}

# Save final lists
saveRDS(vid_stats_all, file = paste0(path,"vid_stats_all.rds"))
saveRDS(vid_details_all, file = paste0(path,"vid_details_all.rds"))
saveRDS(vid_content_all, file = paste0(path,"vid_content_all.rds"))

