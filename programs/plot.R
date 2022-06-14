# 検査値などの外れ値を散布図を用いてチェック
# 2022/5/30
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/51_ALL-T19/10.03.10 データレビュー書/第1回/データクリーニング"
kTrialTitle  <- "ALL-T19"
Specified <- 2 # 指定項目数を入力
# inputデータには散布図を書きたい検査のLBTESTCDを指定し、rawdataと同じ階層のフォルダに格納する
# LBドメインから、治療コースは問わず、特定の検査結果のみを抽出したい場合、detail1で[指定項目1]に抽出したい検査のTESTCDを指定する
# LBドメインから、治療コースを指定して、特定の検査結果のみを抽出したい場合、detail2で[指定項目1]に抽出したい検査のTESTCDを指定, [指定項目2]にコース名(例: baseline: SPIDに格納されている名であること)を準備する
#**************
kToday <- Sys.Date()
library(tidyverse)

# rawdataの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
lb <- read_csv(paste0(rawdatapath, "LB.csv"))

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

# inputdataの読み込み

inputdatapath <- paste0(prtpath, "/input/")
if(Specified == 1){
  detail <- read_csv(paste0(inputdatapath, "detail1.csv"))
} else {
  detail <- read_csv(paste0(inputdatapath, "detail2.csv"))
}

if(Specified == 1){
  for(i in 1:length(detail$指定項目1)){
    testcode <- detail$指定項目1[i]
    lb_dxt <-lb[lb$LBTESTCD == testcode, ]
    setwd(outputpath)
    write.csv(lb_dxt, paste0(testcode, kToday, ".csv" ), row.names = F, na = '')
    png(paste(testcode, kToday, ".png"), width = 1000, height = 1000)
    plot(lb_dxt$VISITNUM, lb_dxt$LBORRES,
         main = paste(testcode, "散布図"),
         xlab = "VISITNUM", ylab = testcode, col = "blue", pch = 20)
    dev.off()
  }} else {
    for(i in 1:length(detail$指定項目1)){
      testcode <- detail$指定項目1[i]
      spid <- detail$指定項目2[i]
      lb_dxt <-lb[lb$LBTESTCD == testcode & lb$LBSPID == spid, ]

      setwd(outputpath)
      write.csv(lb_dxt, paste0(spid, testcode, kToday, ".csv" ), row.names = F, na = '')
      png(paste(spid, testcode, kToday, ".png"), width = 1000, height = 1000)
      plot(lb_dxt$VISITNUM, lb_dxt$LBORRES,
           main = paste(testcode, "散布図"),
           xlab = "VISITNUM", ylab = testcode, col = "blue", pch = 20)
      dev.off()
  }}



