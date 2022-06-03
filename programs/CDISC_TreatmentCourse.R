# LB／FA ドメインから特定のコースのデータのみ抽出
# 2022/6/2
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回/データクリーニング"
kTrialTitle  <- "ALL-B19"
#**************
kToday <- Sys.Date()
library(tidyverse)

# inputdataの読み込み
# input3では[指定項目1]にドメイン名(例: LB), [指定項目2]にコース名(例: baseline: SPIDに格納されている名であること)を準備する
inputdatapath <- paste0(prtpath, "/input/")
detail <- read_csv(paste0(inputdatapath, "detail3.csv"))

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

# rawdataの読み込み
# input3で指定した[指定項目1]のドメインを読み込み、[指定項目2]のコース名のデータを抽出し、CSVで書き出す
rawdatapath <- paste0(prtpath, "/rawdata/")
for(i in 1:length(detail$指定項目1)){
  data <- read_csv(paste0(rawdatapath, detail$指定項目1[i], ".csv"))
  valiable <- paste0("data$", colnames(data)[grep("SPID", colnames(data))])
  result <- data[eval(parse(text = paste0(valiable))) == detail$指定項目2[i], ]
  write.csv(result, 
            paste0(outputpath,"/", kTrialTitle, " Extracted ", detail$指定項目1[i], detail$指定項目2[i], kToday, ".csv" )
            , row.names = F, na = '')
}

