library(randomForest)
songs_play_data <- read.csv("Data/song_play_num.csv")
songs_play_data2<-songs_play_data
train_start_num<-154
test_start_num<-155
for(i in 1:61){
test_data=as.character(as.Date("20150830","%Y%m%d")+i,"%Y%m%d")
songs_play_data[,paste("X",test_data,sep = "")]<-as.integer(0)
songs_play_data2[,paste("X",test_data,sep = "")]<-as.integer(0)
#对建立模型的列向后进行推移
train_start_num=train_start_num+1
test_start_num=test_start_num+1
temp1<-train_start_num
temp2<-test_start_num
for(j in 1:31){
  names(songs_play_data)[temp1]=paste("A",j,sep="")
  head(songs_play_data)
  temp1<-temp1+1
}
for(j in 1:31){
  names(songs_play_data2)[temp2]=paste("A",j,sep="")
  temp2<-temp2+1
}
#30天的歌曲播放量数据
traindata<-songs_play_data[,train_start_num:(temp1<-temp1-1)] 
#预测第31天的歌曲播放量数据
testdata<-songs_play_data2[,test_start_num:(temp2<-temp2-1)]
#取出预测列的列名
predict_colum<-names(songs_play_data[temp1])
rf=randomForest(A31~ .,ntree=95,mtry=3,data=traindata,importance=TRUE)
pred=predict(rf,testdata)
songs_play_data[temp2]=pred
}
write.table(songs_play_data, "test.csv", sep=",", col.names=T, row.names=F, quote=F)





