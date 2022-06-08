# 有害事象Gradeレビュー
# 2021/10/27
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JPLSG/49_ALL-B19/10.03.10 データレビュー書/第1回/データクリーニング"
kTrialTitle  <- "ALL-B19"
ctcae_version <- "v5.0"　# CTCAEのバージョンを入力する
#**************
# inputファイルの読み込み
inputpath <- paste0(getwd(), "/input/")
if(ctcae_version == "v4.0"){
  input <- read.csv(paste0(inputpath, "CTCAEv4 Life-threatening consequences.csv"), na.strings = c(""), as.is=T, fileEncoding="UTF-8-BOM") #生命を脅かすリストの読み込み
  ctcae <- read.csv(paste0(inputpath, "CTCAEv4 term.csv"), na.strings = c(""), as.is=T, fileEncoding="UTF-8-BOM") # CTCAEv4の読み込み
} else{
  input <- read.csv(paste0(inputpath, "CTCAEv5 Life-threatening consequences.csv"), na.strings = c(""), as.is=T, fileEncoding="UTF-8-BOM") #生命を脅かすリストの読み込み
  ctcae <- read.csv(paste0(inputpath, "CTCAEv5 term.csv"), na.strings = c(""), as.is=T, fileEncoding="UTF-8-BOM") # CTCAEv5の読み込み
}
ctcae$flag <- 1

kToday <- Sys.Date()


# rawdataの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
FA <- read.csv(paste0(rawdatapath, "FA.csv"), na.strings = c(""), as.is=T, fileEncoding="UTF-8-BOM")


# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

grade <- FA[FA$FATEST == "Grade", ] #FAからGradeのみを取り出す
dxt_grade4 <- grade[grade$FAORRES == 4, ] #Grade4のみを取り出す

dxt_grade4$FAOBJ <- ifelse(dxt_grade4$FAOBJ=="Febrile Neutropenia",
                           "Febrile neutropenia",dxt_grade4$FAOBJ) # R言語は小文字と大文字を「異なる文字」として区別して扱うので置き換える

# CTCAEで定義されている有害事象から生命を脅かす事象を抽出する
grade4_df1 <- merge(dxt_grade4, input, by.x = "FAOBJ", by.y = "Term", all.x = T)
grade4_df2 <- grade4_df1[!is.na(grade4_df1$definition), ]

# CTCAEで定義されていない有害事象からGrade4を抽出する
grade4_df3 <- merge(dxt_grade4, ctcae, by.x = "FAOBJ", by.y = "Term", all.x = T)

if(nrow(grade4_df3[is.na(grade4_df3$flag), ]) == 0) {
  print("データは0行のため出力対象外です") # 出力データがない場合の処理
  result_grade4 <- grade4_df2
} else {
  grade4_df4 <- grade4_df3[is.na(grade4_df3$flag), ]
  colnames(grade4_df4)[grep("flag", colnames(grade4_df4))] <- "definition"
  grade4_df4$definition <- "その他（具体的に記載）"
  result_grade4 <- rbind(grade4_df2, grade4_df4)
}

# Grade5の有害事象を抽出する
if(nrow(grade[grade$FAORRES == 5, ]) == 0) {
  print("データは0行のため出力対象外です") # 出力データがない場合の処理
  dxt_grade5 <- NULL
} else {
  dxt_grade5 <- grade[grade$FAORRES == 5, ]
  dxt_grade5$definition <- "Death"
}

grade4.5 <- rbind(result_grade4, dxt_grade5) # 結果をバインド
grade4.5[is.na(grade4.5)] <- ""


setwd(outputpath)
if(nrow(grade4.5) == 0) {
  print("データは0行のため出力対象外です") # 出力データがない場合の処理
} else {
  write.csv(grade4.5, paste0(kTrialTitle, " grade4.5 ", kToday, ".csv" ), row.names = F)　#出力データががある場合は該当の行を出力の処理
}
