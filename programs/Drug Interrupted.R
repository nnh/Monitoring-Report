# ECドメインから中止している薬剤を抽出する
# 2022/12/23
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/51_ALL-T19/10.03.10 データレビュー書/第1回から第2回の間(データクリーニング)"
kTrialTitle  <- "ALL-T19"
#**************
kToday <- Sys.Date()
library(tidyverse)

# rawdataの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
ec_raw <- read_csv(paste0(rawdatapath, "EC.csv"))

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}
ec_raw$key <- paste0(ec_raw$USUBJID, "_", ec_raw$ECTRT)


dxt_ec <- ec_raw[grep("Drug Interrupted", ec_raw$ECADJ), ]
dxt_ec$key <- paste0(dxt_ec$USUBJID, "_", dxt_ec$ECTRT)


# forで回すためにリストにする
list <- levels(as.factor(dxt_ec$key))


setwd(paste0(prtpath, "/output"))
for(i in 1:length(list)){
df <- ec_raw[ec_raw$key == list[i], ]
df <- df[order(df$VISITNUM, decreasing=F), ]
df <- df[,c(1:16)]

if(df$ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE")
  {
  csv_name <- sub("METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE", "TIT", list[i])
  write.csv(df, paste0(csv_name, ".csv" ), row.names = F, na = '')
} else {
write.csv(df, paste0(list[i], ".csv" ), row.names = F, na = '')}}

