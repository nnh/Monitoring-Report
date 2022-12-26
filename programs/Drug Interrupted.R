# ECドメインから中止している薬剤および投与量間違いを抽出する
# 2022/12/23
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回-第2回の間(データクリーニング)/20221202"
#**************
kToday <- Sys.Date()
library(tidyverse)

# rawdataの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
ec_raw <- read_csv(paste0(rawdatapath, "EC.csv"))

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

# 症例登録番号と、薬剤名を繋げる(Key codeを作成)
ec_raw$key <- paste0(ec_raw$USUBJID, "_", ec_raw$ECTRT) # 元データ

# 投与中止となっている行のみを抽出する
ec_interrupted <- ec_raw[grep("Drug Interrupted", ec_raw$ECADJ) , ]

# 投与量間違いとなってる行のみを抽出する
ec_dosagemistake <- ec_raw[grep("Dosage Mistake", ec_raw$ECADJ), ]

# 投与中止と投与量間違いのデータセットを作成
ec_ds <- rbind(ec_interrupted, ec_dosagemistake)

# forで回すためにリストにする
list <- levels(as.factor(ec_ds$key))

setwd(paste0(prtpath, "/output"))
for(i in 1:length(list)){
  df <- ec_raw[ec_raw$key == list[i], ]
  df <- df[order(df$VISITNUM, decreasing=F), ]
  df <- df[,c(1:16)]
if((nrow(df)==1) && (df$ECADJ != "Dosage Mistake")){
  next
}
  if(df$ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE")
  {
    csv_name <- sub("METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE", "TIT", list[i])
    write.csv(df, paste0(csv_name, "_row_", nrow(df), ".csv" ), row.names = F, na = '')
  } else {
    write.csv(df, paste0(list[i], "_row_", nrow(df), ".csv" ), row.names = F, na = '')}}


