# 追加可能なシートで入力された薬剤投与状況をグラフで確認する
# 2023/2/21
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/11.03.03 中間解析用生データ/ALL-B19_cdisc_230221_1334"
kTrialTitle  <- "ALL-B19"
kVisitNum <- 2300
#**************
kToday <- Sys.Date()
library(tidyverse)
library(ggplot2)
library(ggsci)

# rawdataの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
ec <- read_csv(paste0(rawdatapath, "EC.csv"))

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

dxt_ec <- ec[ec$VISITNUM == kVisitNum, ] # kVisitNumで指定したコースのみを抽出する

list <- levels(as.factor(dxt_ec$USUBJID)) # 症例登録番号をリスト化する

for(i in 1:length(list)) {
dxt_usubjid <- dxt_ec[dxt_ec$USUBJID == list[i], ]
dxt_usubjid <- dxt_usubjid[order(dxt_usubjid$ECSTDTC, decreasing = F), ] # 投与日を昇順に並べ替える

setwd(outputpath)

png(paste(list[i], kToday, ".png"), width = 1000, height = 1000)

plot(x = as.factor(dxt_usubjid$ECSTDTC), y = dxt_usubjid$ECDOSE,   # X軸の表示が日付にならないのでfactorにしているが良くない気がするので詳しい人に確認したい
     xlab = "投与開始日",
     ylab = "投与量",
     ylim = c(0,100)
)
dev.off()
}


