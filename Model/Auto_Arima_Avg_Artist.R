source("DataClean/UserNumCount.R")
library("forecast")
Artist_Auto_Arima <- function(artist_play_data){
  ts_data <- ts(artist_play_data[,"avg_plays"])
  arimal <- auto.arima(ts_data, trace=T)
  forecast_result <- forecast(arimal,h=61,fan=T)
  rtn.result <- NULL
  for (i in 2:61){
    artist.result <- data.frame("artist_id"=artist_play_data[1,"artist_id"],"avg_plays"=forecast_result$mean[i],"Ds"=as.character(as.Date("20150830","%Y%m%d")+i,"%Y%m%d"))
    rtn.result <- rbind(rtn.result, artist.result)
  }
  return(rtn.result)
}
user_actions$is_play <- as.numeric(user_actions[,"action_type"] == 1)
song_daily_play_num <- aggregate(user_actions$is_play, user_actions[,c("song_id","Ds")],sum)
song_daily_play_num_with_song_info <- merge(song_daily_play_num, songs_data)
artist_daily_play_num <- aggregate(song_daily_play_num_with_song_info$x, song_daily_play_num_with_song_info[,c("artist_id","Ds")],sum)
artist_daily_play_avg_num <- merge(artist_daily_play_num, user_num_data)
artist_daily_play_avg_num[,"avg_plays"] <- artist_daily_play_avg_num[,"x"] / artist_daily_play_avg_num[,"user_num_1"]
split.artist <- split(artist_daily_play_avg_num, artist_daily_play_avg_num$artist_id)
result.list <- lapply(split.artist, Artist_Auto_Arima)
plays_predict_result <- NULL
for (i in 1:length(result.list)){
  plays_predict_result <- rbind(plays_predict_result, result.list[[i]])
}
ts_data <- ts(user_num_data[,"user_num_1"])
arimal <- auto.arima(ts_data, trace=T)
forecast_result <- forecast(arimal,h=61,fan=T)
user_num_predict <- NULL
for (i in 2:61){
  user_num.result <- data.frame("user_num"=forecast_result$mean[i],"Ds"=as.character(as.Date("20150830","%Y%m%d")+i,"%Y%m%d"))
  user_num_predict <- rbind(user_num_predict, user_num.result)
}
submission_data <- merge(plays_predict_result, user_num_predict)
submission_data[,"Plays"] <- submission_data[,"avg_plays"] * submission_data[,"user_num"]
submission_data <- subset(submission_data, select=c("artist_id","Plays","Ds"))
submission_data[,"Plays"][submission_data[,"Plays"] < 0] <- 0
submission_data <- submission_data[order(submission_data$artist_id),]
write.table(submission_data, "submission_aar_tj_20160531.csv", sep=",", col.names=F, row.names=F, quote=F)