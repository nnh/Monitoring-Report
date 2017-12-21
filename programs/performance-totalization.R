# performance 集計 pgm
# 2017/1/12
# mamiko yonejima

# 設定部分
# 今日の日付
Today = "20170727"
setwd("../rawdata")

# Making List
list <- as.data.frame(list.files())
list$no  <- c(1:nrow(list))
list$df <- paste0("file",list$no)
colnames(list) <- c("file","no","name")

# ファイルの読み込み
for(i in 1:length(list$no)){
  eval(parse(text = paste0("file", list$no[i], " <- read.csv('", list$file[i], "', as.is = T)")))
}

#ファイルのバイント
filename <- paste(list$name, sep="", collapse=",") 

allData <- eval(
  parse(
    text=paste0("rbind(",filename,")")
  )
)
#集計
result1  <-capture.output(by(allData$送信シート数,allData$試験名,sum))
result2 <-capture.output(by(allData$督促中シート数,allData$試験名,sum))
#結果の書き出し
setwd("../output")
eval(
  parse(
    text=paste0("write.csv(result1,'送信シート数",Today,".csv',row.names = F)")
  )
)
eval(
  parse(
    text=paste0("write.csv(result2,'督促中シート数",Today,".csv',row.names = F)")
  )
)


