# performance 集計 pgm
# 作成日: 2017/1/12
# 作成者: mamiko yonejima
# 作成日: 2017/12/26
# 作成者: kazumi takeuchi, mamiko yonejima
# 変更内容：定モニのスタイル変更に伴う変更

# 関数の設定
# ｂｙででできた集計値をデータフレームに変換する関数
ConvertDataframe <- function(dataframe){
  x <- as.vector(dataframe)
  試験施設名 <- names(dataframe)
  data.frame(試験施設名,　x)
}

# config

# pathの設定
prtpath <- "//192.168.200.222/Datacenter/学会事務/230_月１登録状況DM確認用/Ptosh/2019年/20190603/performance"

# rawdataのリストを作成
file_list <- list.files(paste0(prtpath, "/rawdata"))
setwd(paste0(prtpath, "/rawdata"))
allfiles <- Reduce(rbind, lapply(file_list,  read.csv)) # バインドしながらリストをすべて読み込み
allfiles[is.na(allfiles)] <- 0
# 試験名と施設名を繋ぐ変数の導出（CMTRT_HP）
allfiles$CMTRT_HP <- paste0(allfiles$試験名, "_", allfiles$施設科名)

# 試験施設名毎に症例登録数をsum
registration_number <- by(allfiles$症例登録数, allfiles$CMTRT_HP, sum)

# 試験施設名毎に送信シート数毎にsum
submit_sheet <- by(allfiles$送信シート数, allfiles$CMTRT_HP, sum)

# 試験施設名毎に督促中シート数毎にsum
demand_sheet <- by(allfiles$督促中シート数, allfiles$CMTRT_HP, sum)

# 試験施設名毎に治療状況調査未回答症例数毎にsum
investigation <- by(allfiles$治療状況調査未回答症例数, allfiles$CMTRT_HP, sum)

# ｂｙででできた集計値をデータフレームに変換
df_regi <- ConvertDataframe(registration_number)
df_submit <- ConvertDataframe(submit_sheet)
df_demand <- ConvertDataframe(demand_sheet)
df_investigation <- ConvertDataframe(investigation)
# mergeと変数名変更
merge1 <- merge(df_regi, df_submit, by = "試験施設名", all = T)
merge2 <- merge(merge1, df_demand, by = "試験施設名", all = T)
names(merge2)[2:4] <- c("症例登録数","送信シート数","督促中シート数")
ads0 <- merge(merge2, df_investigation, by = "試験施設名", all = T)
names(ads0)[5] <- "治療状況調査未回答症例数"


# 試験名と施設名で分ける
ads0$試験名 <- sub("_.*", "", ads0$試験施設名)
ads0$施設名 <- sub("^.*._", "", ads0$試験施設名)

# 必要列のみ抽出
ads <- ads0[,c(6, 7, 2, 5, 3, 4)]

# 試験名をforで回すためにリストにする
list_trial <- levels(allfiles$試験名)

# データ出力
setwd(paste0(prtpath, "/output"))
for(i in 1:length(list_trial)){
  ds<- ads[ads$試験名==list_trial[i],]
  ds <- ds[order(ds$症例登録数, decreasing=T), ]
  write.csv(ds, paste0(list_trial[i], "_performance.csv" ), row.names = F)
}
