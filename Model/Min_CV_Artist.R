source("DataClean/SingleArtistCoefficientVarianceCount.R")
Artist_Min_CV <- function(artist_cv_data){
  print(artist_cv_data[1,"artist_id"])
  tmp.mean <- subset(artist_cv_data, cv==min(artist_cv_data$cv))[1,"mean"]
  rtn.result <- NULL
  for (i in 1:60){
    day.result <- data.frame("artist_id"=artist_cv_data[1,"artist_id"],"Plays"=tmp.mean,"Ds"=as.character(as.Date("20150831","%Y%m%d")+i,"%Y%m%d"))
    rtn.result <- rbind(rtn.result, day.result)
  }
  return(rtn.result)
}

split.artist <- split(coefficient_variance_result, coefficient_variance_result$artist_id)
result.list <- lapply(split.artist, Artist_Min_CV)
submission_data <- NULL
for (i in 1:length(result.list)){
  submission_data <- rbind(submission_data, result.list[[i]])
}
submission_data <- submission_data[order(submission_data$artist_id),]
write.table(submission_data, "submission_aar_tj_20160531_2.csv", sep=",", col.names=F, row.names=F, quote=F)