# LBドメインから指定の検査項目について症例ごとの最終検査日を抽出
# 例として、複数のタイムポイントで実施される特定の検査項目（LBドメインで取得）の中から直近の（VISITNUMが一番大きい）検査の情報のみを抽出したい場合に使用
# 2022/5/20
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/51_ALL-T19/10.03.10 データレビュー書/第1回から第2回の間(データクリーニング)"
kTrialTitle  <- "ALL-T19"
kLBTESTCD <- "ASPAC" #抽出したい検査のTESTCDを指定
#**************
kToday <- Sys.Date()
library(tidyverse)

# rawdataの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
lb_raw <- read_csv(paste0(rawdatapath, "LB.csv"))

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

# 指定されたテストコードのデータを抽出する
# 未実施の場合、検査日は入力されないため対象から除く
lb <- lb_raw[lb_raw$LBTESTCD == kLBTESTCD & is.na(lb_raw$LBSTAT), ]

# 症例番号をリスト化
list <- levels(as.factor(lb$USUBJID))

result <- data.frame()

for(i in 1:length(list)){
  ds <- lb[lb$USUBJID==list[i], ]　# 症例番号をリスト化

  if(nrow(ds) == 1) {　　# データが１行の時はそれを採用
    ds_max <- ds
  } else {
    ds_max <- ds[which.max(ds$VISITNUM),]  # 各症例の提出済報告書から、VISITNUMが最大のものを抽出
  }

  if(i == 1){
    result <- ds_max
  } else {
    result <- rbind(result, ds_max) # VISITNUMが最大のものをバインド
  }}

setwd(outputpath)
write.csv(result, paste0(kTrialTitle," ", kLBTESTCD, " Max VISITNUM", kToday, ".csv" ), row.names = F, na = '')


