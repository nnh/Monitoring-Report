# ECドメインから中止している薬剤および投与量間違いを抽出する
# 薬剤中止理由と注目する有害事象のクリーニング
# 2022/12/23
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/51_ALL-T19/10.03.10 データレビュー書/第1回から第2回の間(データクリーニング)/20230310"
#**************
kToday <- Sys.Date()
library(tidyverse)

# rawdataの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
ec_raw <- read_csv(paste0(rawdatapath, "EC.csv"))
fa_raw <- read_csv(paste0(rawdatapath, "FA.csv"))
lb_raw <- read_csv(paste0(rawdatapath, "LB.csv"))
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
# for(i in 1:length(list)){
#   df <- ec_raw[ec_raw$key == list[i], ]
#   df <- df[order(df$VISITNUM, decreasing=F), ]
#   df <- df[,c(1:16)]
# if((nrow(df)==1) && (df$ECADJ != "Dosage Mistake")){
#   next
# }
#   if(df$ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE")
#   {
#     csv_name <- sub("METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE", "TIT", list[i])
#     write.csv(df, paste0(csv_name, "_row_", nrow(df), ".csv" ), row.names = F, na = '')
#   } else {
#     write.csv(df, paste0(list[i], "_row_", nrow(df), ".csv" ), row.names = F, na = '')}}

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
#*# ALL-B19または、ALL-T19に特化した内容のため、通常は使用しない
# inputのデータを作成する。
ae <- c(
  "Pancreatitis",
  "Allergic reaction",
  "Anaphylaxis",
  "Hyperglycemia",
  "Thromboembolic event",
  "Alanine aminotransferase increased",
  "Aspartate aminotransferase increased",
  "Cholesterol high",
  "Hypertriglyceridemia")

df_matome$key <- paste0(df_matome$USUBJID, "_", df_matome$VISITNUM)
fa_raw$key <- paste0(fa_raw$USUBJID, "_", fa_raw$VISITNUM)

# FAドメインから変数"ae"に入っている事象名の有害事象を抽出する
for(i in 1:(length(ae))){
  fa_ext_1st <- fa_raw[fa_raw$FAOBJ == ae[i], c("key", "FAORRES")]
  colnames(fa_ext_1st)[c(2,3)] <- c(ae[i], paste0(ae[i],"_Grade"))
  if(i == 1){
    result <- merge(df_matome, fa_ext_1st, by = "key", all.x = T)
  } else{
    result <- merge(result, fa_ext_1st, by = "key", all.x = T)
  }
}
result <- result[, -1]  #TODO 症例ごと、VISIT番号昇順でならべ変える
write.csv(result, "Drug Interrupted AE grade matome.csv" , row.names = F, na = '')


# #************#
# # ALL-T19に特化した内容のため、通常は使用しない
# # L-asp活性が2回連続して0.1U/ｍL未満であった場合、不活化(silent inactivation, SI)と定義
# lb_aspac <- lb_raw[lb_raw$LBTESTCD == "ASPAC", ]
# lb_aspac$SI <- ifelse(lb_aspac$LBORRES < 0.1, "<-", NA)
# lb_aspac_low <- subset(lb_aspac, lb_aspac$LBORRES < 0.1)
#
# # 症例番号のリストを作成
# list <- levels(as.factor(lb_aspac_low$USUBJID))
# setwd(paste0(prtpath, "/output"))
# for(i in 1:length(list)){
#   df <- lb_aspac[lb_aspac$USUBJID == list[i], ]
#   df <- df[order(df$VISITNUM, decreasing=F), ]
#   write.csv(df, paste0(list[i], "_ASPAC_", ".csv" ), row.names = F, na = '')
# } 　# 0.1未満のデータがある場合、その症例のL-asp活性測定値を抽出する
#
# if(nrow((subset(ec_raw, ec_raw$ECADJ == "Drug Interrupted for Silent Inactivation"))) == 0){
#   print("データは0行のため出力対象外です") # 出力データがない場合の処理
# } else {
#   si <- subset(ec_raw, ec_raw$ECADJ == "Drug Interrupted for Silent Inactivation")
#   write.csv(si, paste0("SI.csv" ), row.names = F, na = '')
# }　# 中止理由Drug Interrupted for Silent Inactivationのデータがある場合データ抽出する


