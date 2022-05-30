# 検査値などの外れ値を散布図を用いてチェック
# 2022/5/30
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

lb_dxt <-lb[lb$LBTESTCD =="WBC", ]


plot(lb_dxt$LBSEQ, lb_dxt$LBORRES)

