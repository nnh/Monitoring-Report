# CRF review 用にシート一括ダウンロードのデータからfieldを抜いたデータを成形する
# 2022/6/17
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回/データクリーニング/20220701"
kTrialTitle  <- "ALL-B19"
#**************
library(tidyverse)

rawdatapath <- paste0(prtpath, "/rawdata/")
outputpath <- paste0(prtpath, "/output/")

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

# rawdataフォルダ内のリストを作成
list <- list.files(rawdatapath)


for (i in 1:length(list)) {
  csv <- read_csv(paste0(rawdatapath,list[i]), col_names = FALSE)        # CSVの読み込み # 文字化けを防ぐため列名もデータとして読み込む
  outputcsv <- csv[, c(1:11, seq(11, length(colnames(csv)), by = 2))]    # fieldXの列を削除
  colnames(outputcsv) <- outputcsv[1,]                                   # データとして読み込んだ列名を見出しとして割り当て
  colnames(outputcsv) <- ifelse(is.na(colnames(outputcsv)), "",
                                ifelse(colnames(outputcsv) == "NA", "", colnames(outputcsv)))  # NAの処理
  outputcsv <- outputcsv[-1,]
  write.csv(outputcsv, paste(outputpath, list[i], sep="/"), na='""', row.names = F)
}


