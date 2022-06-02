# LB／FA ドメインから特定のコースのデータのみ抽出
# 2022/6/2
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回/データクリーニング"
kTrialTitle  <- "ALL-B19"
#**************
kToday <- Sys.Date()
library(tidyverse)


#*# inputdataの読み込み
inputdatapath <- paste0(prtpath, "/input/")
detail <- read_csv(paste0(inputdatapath, "detail4.csv"))

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

rawdatapath <- paste0(prtpath, "/rawdata/")
for(i in 1:length(detail$指定項目1)){
  data <- read_csv(paste0(rawdatapath, detail$指定項目1[i], ".csv"))
  valiable <- paste0("data$", colnames(data)[grep("SPID", colnames(data))])
  result <- data[eval(parse(text = paste0(valiable))) == detail$指定項目2[i], ]
  write.csv(result, paste0(outputpath,"/", kTrialTitle, " Extracted ", detail$指定項目1[i], detail$指定項目2[i], kToday, ".csv" ), row.names = F, na = '')
}

