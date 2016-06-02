source("DataClean/DataInit.R")
j <- 1
Song_Aggregate <- function(song_daily_play_data){
  print(j/length(levels(song_daily_play_num$song_id)))
  song_daily_play_data <- song_daily_play_data[order(song_daily_play_data$Ds),]
  rtn.result <- data.frame("song_id"=song_daily_play_data[1,"song_id"])
  for (i in as.Date("20150301","%Y%m%d"):as.Date("20150830","%Y%m%d")){
    date <- as.numeric(as.character(as.Date(i,origin="1970-01-01"),"%Y%m%d"))
    tmp.song_daily_play_data <- subset(song_daily_play_data,Ds==date)
    if (nrow(tmp.song_daily_play_data) == 0){
      tmp.result <- data.frame(0)
      colnames(tmp.result) <- as.numeric(as.character(as.Date(i,origin="1970-01-01"),"%Y%m%d"))
      rtn.result <- cbind(rtn.result,tmp.result)
    } else {
      tmp.result <- data.frame(tmp.song_daily_play_data[1,"x"])
      colnames(tmp.result) <- as.numeric(as.character(as.Date(i,origin="1970-01-01"),"%Y%m%d"))
      rtn.result <- cbind(rtn.result,tmp.result)
    }
  }
  j <<- j+1
  return(rtn.result)
}
user_actions$is_play <- as.numeric(user_actions[,"action_type"] == 1)
song_daily_play_num <- aggregate(user_actions$is_play, user_actions[,c("song_id","Ds")],sum)
split.song <- split(song_daily_play_num, song_daily_play_num$song_id)
result.list <- lapply(split.song, Song_Aggregate)
song_play_num <- NULL
for (i in 1:length(result.list)){
  song_play_num <- rbind(song_play_num, result.list[[i]])
}
