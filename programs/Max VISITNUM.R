# 症例ごとの最終コースの投与開始日を抽出
# 2022/5/20
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回/データクリーニング"
kTrialTitle  <- "ALL-B19"
#**************
kToday <- Sys.Date()
library(tidyverse)

# rawdataの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
sv <- read_csv(paste0(rawdatapath, "SV.csv"))

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

# 症例番号をリスト化
list <- levels(as.factor(sv$USUBJID))

result <- data.frame()

for(i in 1:length(list)){
ds <- sv[sv$USUBJID==list[i], ]　# 症例番号をリスト化

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
write.csv(result, paste0(kTrialTitle, "  Max VISITNUM", kToday, ".csv" ), row.names = F)



