source("DataClean/DataInit.R")
CalArtistScore <- function(split_artist_plays_full){
  split_artist_plays_full <- split_artist_plays_full[order(split_artist_plays_full[,"Ds"]),]
  split_artist_plays_full[,"variance"] <- (split_artist_plays_full[,"Plays_predict"]+1)/(split_artist_plays_full[,"Plays"]+1) - 1
  split_artist_plays_full[,"variance"] <- split_artist_plays_full[,"variance"] * split_artist_plays_full[,"variance"]
  sum_plays <- sum(split_artist_plays_full[,"Plays"])
  sum_varience <- sum(split_artist_plays_full[,"variance"]) / (as.numeric((as.Date(as.character(split_artist_plays_full[nrow(split_artist_plays_full),"Ds"]),"%Y%m%d")-as.Date(as.character(split_artist_plays_full[1,"Ds"]),"%Y%m%d")))+1)
  score <- (1 - sqrt(sum_varience))* sqrt(sum_plays)
  return(data.frame(artist_id=split_artist_plays_full[1,"artist_id"],sum_plays=sum_plays,sum_varience=sum_varience,score=score))
}

CalScore <- function(submission_file_name, start_date, end_date){
  user_actions$is_play <- as.numeric(user_actions[,"action_type"] == 1)
  song_daily_play_num <- aggregate(user_actions$is_play, user_actions[,c("song_id","Ds")],sum)
  song_daily_play_num_with_song_info <- merge(song_daily_play_num, songs_data)
  artist_daily_play_num <- aggregate(song_daily_play_num_with_song_info$x, song_daily_play_num_with_song_info[,c("artist_id","Ds")],sum)
  colnames(artist_daily_play_num) <- c("artist_id","Ds","Plays")
  artist_daily_play_num_subset <- subset(artist_daily_play_num,Ds>=start_date&Ds<=end_date)
  artist_plays_predict <- read.csv(submission_file_name, header=FALSE)
  colnames(artist_plays_predict) <- c("artist_id","Ds","Plays_predict")
  artist_plays_full <- merge(artist_daily_play_num_subset,artist_plays_predict,all.x=TRUE)
  artist_plays_full[,"Plays_predict"][is.na(artist_plays_full[,"Plays_predict"])] <- 0
  split.artist_plays_full <- split(artist_plays_full, artist_plays_full[,"artist_id"])
  artist_plays_score_list <- lapply(split.artist_plays_full, CalArtistScore)
  artist_plays_score <- NULL
  for (i in 1:length(artist_plays_score_list)){
    artist_plays_score <- rbind(artist_plays_score, artist_plays_score_list[[i]])
  }
  return (artist_plays_score)
}

#Example:
#result <- CalScore("Data/sample_submission.csv", 20150301, 20150415)

