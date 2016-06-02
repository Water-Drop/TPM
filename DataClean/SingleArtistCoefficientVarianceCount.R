source("DataClean/DataInit.R")
Cal_Coefficient_Variance <- function(artist_play_data){
  print(artist_play_data[1,"artist_id"])
  artist_play_cv <- NULL
  for (i in as.Date("20150301","%Y%m%d"):as.Date("20150829","%Y%m%d")){
    start_date <- as.numeric(as.character(as.Date(i,origin="1970-01-01"),"%Y%m%d"))
    tmp.artist_play_data <- subset(artist_play_data,Ds>=start_date&Ds<=20150830)
    median <- median(tmp.artist_play_data$x)
    mean <- mean(tmp.artist_play_data$x)
    sd <- sd(tmp.artist_play_data$x)
    cv <- sd / mean
    artist_play_cv <- rbind(artist_play_cv, data.frame("artist_id"=artist_play_data[1,"artist_id"],"start_date"=start_date,"median"=median,"mean"=mean,"sd"=sd,"cv"=cv))
  }
  return(artist_play_cv)
}
user_actions$is_play <- as.numeric(user_actions[,"action_type"] == 1)
song_daily_play_num <- aggregate(user_actions$is_play, user_actions[,c("song_id","Ds")],sum)
song_daily_play_num_with_song_info <- merge(song_daily_play_num, songs_data)
artist_daily_play_num <- aggregate(song_daily_play_num_with_song_info$x, song_daily_play_num_with_song_info[,c("artist_id","Ds")],sum)
split.artist <- split(artist_daily_play_num, artist_daily_play_num$artist_id)
result.list <- lapply(split.artist, Cal_Coefficient_Variance)
coefficient_variance_result <- NULL
for (i in 1:length(result.list)){
  coefficient_variance_result <- rbind(coefficient_variance_result, result.list[[i]])
}