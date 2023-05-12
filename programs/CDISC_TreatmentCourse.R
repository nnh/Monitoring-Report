# 指定したドメインから特定のコースのデータのみ抽出
# 例として、LBドメインの中からベースラインのデータのみを抽出したい場合に使用
# 2022/6/2
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回-第2回の間(データクリーニング)/20230330"
kTrialTitle  <- "ALL-B19"
#**************
kToday <- Sys.Date()
library(tidyverse)

# inputdataの読み込み
# input3の作成方法
## UTF-8-BOMのCSV
## [指定項目1]にドメイン名(例: LB)
## [指定項目2]にコース名(例: baseline: SPIDに格納されている名であること)
## [指定項目3]に格納されている変数名(例:ECTRT)
## [指定項目4]に抽出したい項目(例:L-ASPARAGINASE)

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
  valiable1 <- paste0("data$", colnames(data)[grep("SPID", colnames(data))])
  valiable2 <- paste0("data$", detail$指定項目3[i])
  result <- subset(data, (eval(parse(text = paste0(valiable1))) == detail$指定項目2[i]
                 & eval(parse(text = paste0(valiable2))) == detail$指定項目4[i]))
  write.csv(result,
            paste0(outputpath,"/", kTrialTitle, " Extract ", detail$指定項目2[i], " ", kToday, ".csv" )
            , row.names = F, na = '')
}
