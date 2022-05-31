# 検査値などの外れ値を散布図を用いてチェック
# 2022/5/30
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回/データクリーニング"
kTrialTitle  <- "ALL-B19"
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
detail <- read_csv(paste0(inputdatapath, "detail1.csv"))

for(i in 1:length(detail$指定項目1)){
testcode <- detail$指定項目1[i]
lb_dxt <-lb[lb$LBTESTCD == testcode, ]

setwd(outputpath)
png(paste(testcode, kToday, ".png"), width = 1000, height = 1000)
plot(lb_dxt$LBSEQ, lb_dxt$LBORRES,
     main = paste(testcode, "散布図"),
     xaxt = "n", xlab = "", ylab = testcode, col = "blue")
dev.off()

}
