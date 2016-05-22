source("DataClean/DataInit.R")
library("forecast")
Song_Auto_Arima <- function(song_play_data){
  ts_data <- ts(song_play_data[,"x"])
  arimal <- auto.arima(ts_data, trace=T)
  forecast_result <- forecast(arimal,h=60,fan=T)
  rtn.result <- NULL
  for (i in 1:60){
    song.result <- data.frame("song_id"=song_play_data[1,"song_id"],"Plays"=forecast_result$mean[i],"Ds"=as.character(as.Date("20150831","%Y%m%d")+i,"%Y%m%d"))
    rtn.result <- rbind(rtn.result, song.result)
  }
  print(progress_index/length(split.song))
  progress_index <<- progress_index + 1
  return(rtn.result)
}
user_actions$is_play <- as.numeric(user_actions[,"action_type"] == 1)
song_daily_play_num <- aggregate(user_actions$is_play, user_actions[,c("song_id","Ds")],sum)
split.song <- split(song_daily_play_num, song_daily_play_num$song_id)
progress_index <- 1
result.list <- lapply(split.song, Song_Auto_Arima)
song_forecast_data <- NULL
for (i in 1:length(result.list)){
  print(i/length(result.list))
  song_forecast_data <- rbind(song_forecast_data, result.list[[i]])
}
song_forecast_data_with_song_info <- merge(song_forecast_data, songs_data)
artist_forecast_daily_play_num <- aggregate(song_forecast_data_with_song_info$Plays, song_forecast_data_with_song_info[,c("artist_id","Ds")],sum)
submission_data <- subset(artist_forecast_daily_play_num, select=c("artist_id","x","Ds"))
write.table(submission_data, "submission_sar_tj_20160520.csv", sep=",", col.names=F, row.names=F, quote=F)
