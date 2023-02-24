# 追加可能なシートで入力された薬剤投与状況をグラフで確認する
# 2023/2/21
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回-第2回の間(データクリーニング)/20230221"
kTrialTitle  <- "ALL-B19"
kVisitNum <- 2300
#**************
kToday <- Sys.Date()
library(tidyverse)
library(ggplot2)

# rawdataの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
ec <- read_csv(paste0(rawdatapath, "EC.csv"))
sv <- read_csv(paste0(rawdatapath, "SV.csv"))


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

  p <- ggplot(data = dxt_usubjid, mapping = aes(x = as.factor(dxt_usubjid$ECSTDTC), y = dxt_usubjid$ECDOSE)) +
    geom_point() +
    scale_y_continuous(limits = c(0, NA)) +
    xlab ("投与開始日") +
    ylab ("維持療法 6-MP投与量 [mg/m2]") +
    theme(axis.text.x = element_text(angle = 90))

  ggsave(paste(list[i], "maintenance 6-MP", kToday, ".png"), plot = p, dpi = 100, width = 6.4, height = 4.8)

}

###### 以下、
# SVドメインから維持療法の開始日・終了日データを抽出し、「維持療法: 6-MP投与量報告」の投与開始日とマージする
dxt_sv <- sv[sv$VISITNUM == kVisitNum, ] # kVisitNumで指定したコースのみを抽出する

listsv <- levels(as.factor(dxt_sv$USUBJID)) # 症例登録番号をリスト化する

# ECドメインの中にリストの症例の維持療法のデータがあるか確認する
result <- NULL

for(i in 1:length(listsv)) {
if(nrow(dxt_ec[dxt_ec$USUBJID == listsv[i], ]) == 0){
  df <- data.frame(
  USUBJID = listsv[i],
  ECSTDTC_first = "",
  SVSTDTC = dxt_sv[dxt_sv$USUBJID == listsv[i], 7],
  ECSTDTC_last = "",
  SVENDTC = dxt_sv[dxt_sv$USUBJID == listsv[i], 8]
  )
} else {
  dxt <- dxt_ec[dxt_ec$USUBJID == listsv[i], ]
  df <- data.frame(
    USUBJID = listsv[i],
    ECSTDTC_first = dxt_ec[whitch.min(dxt$ECSTDTC) , 16],
    SVSTDTC = dxt_sv[dxt_sv$USUBJID == listsv[i], 7],
    ECSTDTC_last = dxt_ec[whitch.max(dxt$ECSTDTC) , 16],
    SVENDTC = dxt_sv[dxt_sv$USUBJID == listsv[i], 8]
  )
}
result <- rbind(result, df)
}

setwd(outputpath)
write.csv(result, paste0(kTrialTitle, "  maintenance", kToday, ".csv" ), row.names = F, na = '')
