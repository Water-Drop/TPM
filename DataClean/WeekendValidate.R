source("DataClean/DataInit.R")
user_actions$is_play <- as.numeric(user_actions[,"action_type"] == 1)
song_daily_play_num <- aggregate(user_actions$is_play, user_actions[,c("song_id","Ds")],sum)
song_daily_play_num_with_song_info <- merge(song_daily_play_num, songs_data)
artist_daily_play_num <- aggregate(song_daily_play_num_with_song_info$x, song_daily_play_num_with_song_info[,c("artist_id","Ds")],sum)
artist_daily_play_num$wday <- unclass(as.POSIXlt(as.character(artist_daily_play_num$Ds),format="%Y%m%d"))$wday
artist_daily_play_num_wday <- aggregate(artist_daily_play_num$x, artist_daily_play_num[,c("artist_id","wday")],sum)
artist_daily_play_num_wday <- artist_daily_play_num_wday[order(artist_daily_play_num_wday$artist_id),]
wday.split <- split(artist_daily_play_num, artist_daily_play_num$wday)
wday_play_num_sum <- data.frame(sapply(wday.split, FUN=function(y) sum(y$x)))