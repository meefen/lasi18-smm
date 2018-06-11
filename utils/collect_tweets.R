# Methods of retrieving tweets

collect_tweets_from_gdrive <- function(url, tz="America/Toronto") {
  # Get tweets from Google Spreadsheet
  # For how to archive tweets in Google Spreadsheet, read:
  # http://mashe.hawksey.info/2013/02/twitter-archive-tagsv5/
  #
  # Args:
  #   key: file key
  #   gid: grid id of archive sheet
  #
  # Returns:
  #   Data frame containing tweets
  
  EnsurePackage("RCurl")
  
  conn <- textConnection(getURL(url))
  df <- read.csv(conn, stringsAsFactors = FALSE)
  close(conn)
  
  # formatting
  df$created_at <- strptime(df$time, "%d/%m/%Y %H:%M:%S")
  df$created_at <- format(as.POSIXct(df$created_at, tz="GMT"), tz=tz)
  df$geo_coordinates[df$geo_coordinates == ""] <- NA
  df$screen_name <- df$from_user
  
  return(tbl_df(df))
}