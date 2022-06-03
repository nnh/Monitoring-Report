# LB／FA ドメインから未実施の検査を抽出
# 2022/6/1
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回/データクリーニング"
kTrialTitle  <- "ALL-B19"
#**************
kToday <- Sys.Date()
library(tidyverse)

# rawdataの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
lb <- read_csv(paste0(rawdatapath, "LB.csv"))
fa <- read_csv(paste0(rawdatapath, "FA.csv"))

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

# LBおよびFAドメインからNOT DONEの行を抽出する
lbnotdone <- lb[!is.na(lb$LBSTAT), ]
fanotdone <- fa[!is.na(fa$FASTAT), ]

# ファイルの書き出し
setwd(outputpath)
write.csv(lbnotdone, paste0(kTrialTitle, " LB NOT DONE", kToday, ".csv" ), row.names = F, na = '')
write.csv(fanotdone, paste0(kTrialTitle, " FA NOT DONE", kToday, ".csv"), row.names = F, na = '' )
