# ECドメインから中止している薬剤および投与量間違いを抽出する
# 薬剤中止理由と注目する有害事象のクリーニング
# 2022/12/23
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回-第2回の間(データクリーニング)/20221202"
#**************
kToday <- Sys.Date()
library(tidyverse)

# rawdataの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
ec_raw <- read_csv(paste0(rawdatapath, "EC.csv"))
fa_raw <- read_csv(paste0(rawdatapath, "FA.csv"))

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

# 症例登録番号と、薬剤名を繋げる(Key codeを作成)
ec_raw$key <- paste0(ec_raw$USUBJID, "_", ec_raw$ECTRT) # 元データ

# 投与中止となっている行のみを抽出する
ec_interrupted <- ec_raw[grep("Drug Interrupted", ec_raw$ECADJ) , ]

# 投与量間違いとなってる行のみを抽出する
ec_dosagemistake <- ec_raw[grep("Dosage Mistake", ec_raw$ECADJ), ]

# 投与中止と投与量間違いのデータセットを作成
ec_ds <- rbind(ec_interrupted, ec_dosagemistake)

# forで回すためにリストにする
list <- levels(as.factor(ec_ds$key))

setwd(paste0(prtpath, "/output"))
# 症例、薬剤ごとにCSVを出力する場合は、こちらを使用する
for(i in 1:length(list)){
  df <- ec_raw[ec_raw$key == list[i], ]
  df <- df[order(df$VISITNUM, decreasing=F), ]
  df <- df[,c(1:16)]
if((nrow(df)==1) && (df$ECADJ != "Dosage Mistake")){
  next
}
  if(df$ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE")
  {
    csv_name <- sub("METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE", "TIT", list[i])
    write.csv(df, paste0(csv_name, "_row_", nrow(df), ".csv" ), row.names = F, na = '')
  } else {
    write.csv(df, paste0(list[i], "_row_", nrow(df), ".csv" ), row.names = F, na = '')}}

# 投与中止、投与量間違い症例の一覧を出力する
df_matome <- NULL

for(i in 1:length(list)){
  df <- ec_raw[ec_raw$key == list[i], ]
  df <- df[order(df$VISITNUM, decreasing=F), ]
  df <- df[,c(1:16)]
  if((nrow(df)==1) && (df$ECADJ != "Dosage Mistake")){
    next
  } else {
    df_matome <- rbind(df_matome,df)}}
write.csv(df_matome, "Drug Interrupted matome.csv" , row.names = F, na = '')

#************#
# ALL-B19に特化した内容のため、通常は使用しない
# inputのデータフレームを作成する。薬剤中止理由と注目する有害事象の一覧。
input <- data.frame(
  reason = c("Drug Interrupted for Pancreatitis",
             "Drug Interrupted for Allergic reaction",
             "Drug Interrupted for Diabetes",
             "Drug Interrupted for Thromboembolic event",
             "Drug Interrupted for Liver disorder",
             "Drug Interrupted for Liver disorder",
             "Drug Interrupted for Hyperlipidaemia",
             "Drug Interrupted for Hyperlipidaemia"
             ),
  ae = c("Pancreatitis",
         "Allergic reaction",
         "Hyperglycemia",
         "Thromboembolic event",
         "Alanine aminotransferase increa",
         "Aspartate aminotransferase increased",
         "Cholesterol high",
         "Hypertriglyceridemia"
         )
)
# ECから中止理由毎に抽出
for(j in 1:length(input)){
  rsn <- ec_interrupted[ec_interrupted$ECADJ == input$reason[j],]
  rsn$key <- paste0(rsn$USUBJID, "_", rsn$VISITNUM)
  dxt_rsn <- rsn[,c("ECTRT","ECADJ","key")]
# 症例番号のリストを作成
  list <- levels(as.factor(rsn$USUBJID))
  for(i in 1:length(list)){
    # 中止理由毎に注目する有害事象名をFAより抽出する
    ae <- fa_raw[((fa_raw$USUBJID == list[i]) & (fa_raw$FAOBJ == input$ae[j])), ]
    # 有害事象のGradeと中止理由をマージする
    ae$key <- paste0(ae$USUBJID, "_", ae$VISITNUM)
    ds1 <- merge(ae, dxt_rsn, by = "key", all.x = T)
    ds2 <- ds1[order(ds1$VISITNUM, decreasing=F), ]
    result <- ds2[,-1]
    write.csv(result, paste0(list[i],"_", input$ae[j],".csv") , row.names = F, na = '')
  }}


