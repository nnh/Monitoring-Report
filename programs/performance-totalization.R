# performance 集計 pgm
# 作成日: 2017/1/12
# 作成者: mamiko yonejima
# 作成日: 2017/12/26
# 作成者: kazumi takeuchi, mamiko yonejima
# 定モニのスタイル変更に伴う変更


# 設定部分
prtpath <- "//192.168.200.222/Datacenter/学会事務/230_月１登録状況DM確認用/Ptosh/2017年/20171201/performance"

# rawdataのリストを作成
file_list <- list.files(paste0(prtpath, "/rawdata"))
setwd(paste0(prtpath, "/rawdata"))
allfiles <- Reduce(rbind, lapply(file_list,  read.csv)) # バインドしながらリストをすべて読み込み

# 試験名と施設名を繋ぐ変数の導出（CMTRT_HP）
allfiles$CMTRT_HP <- paste0(allfiles$試験名, "_", allfiles$施設科名)

# 試験施設名毎に症例登録数をsum
registration_number <- by(allfiles$症例登録数,allfiles$CMTRT_HP, sum)

# 試験施設名毎に送信シート数毎にsum
submit_sheet <- by(allfiles$送信シート数,allfiles$CMTRT_HP, sum)

# 試験施設名毎に督促中シート数毎にsum
demand_sheet <- by(allfiles$督促中シート数,allfiles$CMTRT_HP, sum)

# ｂｙででできた集計値をデータフレームに変換する関数
ConvertDataframe <- function(dataframe){  #Todoyonejima
  x <- as.vector(dataframe)
  試験施設名 <- names(dataframe)
  data.frame(試験施設名,x)
}

df_regi <- ConvertDataframe(registration_number)
df_submit <- ConvertDataframe(submit_sheet)
df_demand <- ConvertDataframe(demand_sheet)




# 症例登録数<- as.vector(registration_number)
# 試験施設名 <- names(registration_number)
#
# z <- data.frame(
#   試験施設名,症例登録数
# )
#
# 送信シート数<- as.vector(demand_sheet)
# 試験施設名 <- names(demand_sheet)
#
# z1 <- data.frame(
#   試験施設名,送信シート数
# )


# # 今日の日付
# Today 　<-  "20170727"
# setwd("../rawdata")
#
# # Making List
# list <- as.data.frame(list.files())
# list$no  <- c(1:nrow(list))
# list$df <- paste0("file",list$no)
# colnames(list) <- c("file","no","name")
#
# # ファイルの読み込み
# for(i in 1:length(list$no)){
#   eval(parse(text = paste0("file", list$no[i], " <- read.csv('", list$file[i], "', as.is = T)")))
# }
#
# #ファイルのバイント
# filename <- paste(list$name, sep="", collapse=",")
#
# allData <- eval(
#   parse(
#     text=paste0("rbind(",filename,")")
#   )
# )
# #集計
# result1  <-capture.output(by(allData$送信シート数,allData$試験名,sum))
# result2 <-capture.output(by(allData$督促中シート数,allData$試験名,sum))
# #結果の書き出し
# setwd("../output")
# eval(
#   parse(
#     text=paste0("write.csv(result1,'送信シート数",Today,".csv',row.names = F)")
#   )
# )
# eval(
#   parse(
#     text=paste0("write.csv(result2,'督促中シート数",Today,".csv',row.names = F)")
#   )
# )


