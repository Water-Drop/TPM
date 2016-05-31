source("DataClean/DataInit.R")
user_num_data <- NULL
for (i in as.Date("20150301","%Y%m%d"):as.Date("20150830","%Y%m%d")){
  end_date <- as.numeric(as.character(as.Date(i,origin="1970-01-01"),"%Y%m%d"))
  seven_day_before_end_date <- as.numeric(as.character(as.Date(i-6,origin="1970-01-01"),"%Y%m%d"))
  three_day_before_end_date <- as.numeric(as.character(as.Date(i-2,origin="1970-01-01"),"%Y%m%d"))
  tmp.user_actions_day <- subset(user_actions,Ds==end_date)
  tmp.user_actions_three_days <- subset(user_actions,Ds>=three_day_before_end_date&Ds<=end_date)
  tmp.user_actions_seven_days <- subset(user_actions,Ds>=seven_day_before_end_date&Ds<=end_date)
  tmp.user_actions_all_days <- subset(user_actions,Ds>=20150301&Ds<=end_date)
  
  user_num_data <- rbind(user_num_data, data.frame("Ds"=end_date,"user_num_1"=length(unique(tmp.user_actions_day[,"user_id"])),"user_num_3"=length(unique(tmp.user_actions_three_days[,"user_id"])),
                                                   "user_num_7"=length(unique(tmp.user_actions_seven_days[,"user_id"])),"user_num_all"=length(unique(tmp.user_actions_all_days[,"user_id"]))))
  print(as.numeric(as.Date(i,origin="1970-01-01") - as.Date("20150301","%Y%m%d"))/as.numeric(as.Date("20150830","%Y%m%d") - as.Date("20150301","%Y%m%d")))
}
#plot(row.names(user_num_data), user_num_data$user_num_1, type="l")
