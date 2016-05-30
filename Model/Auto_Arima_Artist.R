source("DataClean/DataInit.R")
library("forecast")
Artist_Auto_Arima <- function(artist_play_data){
  ts_data <- ts(artist_play_data[,"x"])
  arimal <- auto.arima(ts_data, trace=T)
  forecast_result <- forecast(arimal,h=61,fan=T)
  rtn.result <- NULL
  for (i in 2:61){
    artist.result <- data.frame("artist_id"=artist_play_data[1,"artist_id"],"Plays"=forecast_result$mean[i],"Ds"=as.character(as.Date("20150830","%Y%m%d")+i,"%Y%m%d"))
    rtn.result <- rbind(rtn.result, artist.result)
  }
  return(rtn.result)
}
user_actions$is_play <- as.numeric(user_actions[,"action_type"] == 1)
song_daily_play_num <- aggregate(user_actions$is_play, user_actions[,c("song_id","Ds")],sum)
song_daily_play_num_with_song_info <- merge(song_daily_play_num, songs_data)
artist_daily_play_num <- aggregate(song_daily_play_num_with_song_info$x, song_daily_play_num_with_song_info[,c("artist_id","Ds")],sum)
split.artist <- split(artist_daily_play_num, artist_daily_play_num$artist_id)
result.list <- lapply(split.artist, Artist_Auto_Arima)
submission_data <- NULL
for (i in 1:length(result.list)){
  submission_data <- rbind(submission_data, result.list[[i]])
}
write.table(submission_data, "submission_aar_tj_20160529.csv", sep=",", col.names=F, row.names=F, quote=F)

