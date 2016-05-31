source("DataClean/DataInit.R")
Cal_Coefficient_Variance <- function(artist_play_data){
  mean <- mean(artist_play_data$x)
  mean_7 <- mean(artist_play_data[(nrow(artist_play_data)-6):nrow(artist_play_data),"x"])
  mean_30 <- mean(artist_play_data[(nrow(artist_play_data)-29):nrow(artist_play_data),"x"])
  sd <- sd(artist_play_data$x)
  sd_7 <- sd(artist_play_data[(nrow(artist_play_data)-6):nrow(artist_play_data),"x"])
  sd_30 <- sd(artist_play_data[(nrow(artist_play_data)-29):nrow(artist_play_data),"x"])
  cv <- sd / mean
  cv_7 <- sd_7 / mean_7
  cv_30 <- sd_30 / mean_30
  artist.result <- data.frame("artist_id"=artist_play_data[1,"artist_id"],"mean"=mean,"sd"=sd,"cv"=cv,
                              "mean_7"=mean_7,"sd_7"=sd_7,"cv_7"=cv_7,"mean_30"=mean_30,"sd_30"=sd_30,"cv_30"=cv_30)
  return(artist.result)
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
coefficient_variance_result <- coefficient_variance_result[order(coefficient_variance_result$cv_30),]