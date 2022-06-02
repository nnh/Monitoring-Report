# 検査値などの外れ値を散布図を用いてチェック
# 2022/5/30
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回/データクリーニング"
kTrialTitle  <- "ALL-B19"
Specified <- 2
# inputデータには散布図を書きたい検査のLBTESTCDを指定し、rawdataと同じ階層のフォルダに格納する
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
png(paste(testcode, kToday, ".png"), width = 1000, height = 1000)
plot(lb_dxt$VISITNUM, lb_dxt$LBORRES,
     main = paste(testcode, "散布図"),
    xlab = "VISITNUM", ylab = testcode, col = "blue", pch = 20)
dev.off()

}}else{
  for(i in 1:length(detail$指定項目1)){
    testcode <- detail$指定項目1[i]
    spid <- detail$指定項目2[i]
    lb_dxt <-lb[lb$LBTESTCD == testcode & lb$LBSPID == spid, ]

    setwd(outputpath)
    png(paste(spid, testcode, kToday, ".png"), width = 1000, height = 1000)
    plot(lb_dxt$VISITNUM, lb_dxt$LBORRES,
         main = paste(testcode, "散布図"),
         xlab = "VISITNUM", ylab = testcode, col = "blue", pch = 20)
    dev.off()

  } }




