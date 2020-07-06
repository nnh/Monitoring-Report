# AE Grade　集計　CDISC
# Mamiko Yonejima
# 2019/11/1

# 入力ファイル格納場所、リスク分類または割付けがあるかを指定
# *********************************
prtpath <- "//192.168.200.222/Datacenter/Trials/ASIA/DS-ALL-2016/11.03.03 Interim Analysis Raw Datasets/2020/20200601/DS-ALL-2016_cdisc_200601_1855"
kTrialTitle  <- "DS-ALL-2016"
ctcae_version <- "v4.0"　# CTCAEのバージョンを入力する　
# armで分けて集計するか あり: YES, なし: NO
arm <- "YES"　
## arm が "YES"の場合、DMドメインのCSVファイルはあるか。　# あり: YES, なし: NO
dm_domain <- "YES"
###　arm が "YES"の場合且つdm_domainが"NO"の場合、読み込むCSVダウンロードファイル名と、変数名を設定
kCsv <- "AML-SCT15_sct_191202_1315.csv"
kArm <- "移植前処置_種類"
# *********************************
kToday <- Sys.Date()
# Gradeごとにデータを抽出し、クロス集計を行い、成型するための関数を設定
DataShaping <- function(grade){
  dxt_grade <- subset(dxt, dxt$FAORRES == grade)
  if(nrow(dxt_grade) == 0) {
    NA
  } else {
    table_ae_grade <- xtabs( ~ FAOBJ  + FAORRES, data = dxt_grade)
    table_ae_grade2 <- merge(df_list_FAOBJ, table_ae_grade, by.x = "list_FAOBJ", by.y = "FAOBJ" , all.x = T)
    table_ae_grade2[,3]
  }
}
# データの読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
file_list <- list.files(rawdatapath)
dm_index <- grep("DM", file_list)  # DM.csvの存在を確認
if(length(dm_index > 0)) {
  DM <- read.csv(paste0(rawdatapath, "DM.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
} else {
  base_csv <- read.csv(paste0(rawdatapath, kCsv), na.strings = c(""), as.is=T, fileEncoding="CP932")
  dxt_csv <- base_csv[, c("症例登録番号", kArm)]
  dxt_csv$症例登録番号 <- ifelse(nchar(dxt_csv$症例登録番号) == 1, paste0("000",dxt_csv$症例登録番号),
                          ifelse(nchar(dxt_csv$症例登録番号) == 2, paste0("00",dxt_csv$症例登録番号),
                          ifelse(nchar(dxt_csv$症例登録番号) == 3, paste0("0",dxt_csv$症例登録番号),dxt_csv$症例登録番号)))
  dxt_csv$USUBJID <- paste0(kTrialTitle, "-", dxt_csv$症例登録番号)
  dxt_csv <- dxt_csv[,c("USUBJID", kArm)]
  colnames(dxt_csv)[2] <-  "ARM"
  DM <- dxt_csv
}
FA <- read.csv(paste0(rawdatapath, "FA.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
# outputのフォルダを作成
setwd(prtpath)
dir.create("output")

setwd("~/GitHub/Monitoring-Report/input")
if(ctcae_version == "v4.0") {
  CTCAE <- read.csv("CTCAE_4.03.csv", na.strings = c(""), as.is=T, fileEncoding="UTF-8") 
} else {
  CTCAE <- read.csv("CTCAE_5.0.csv", na.strings = c(""), as.is=T, fileEncoding="UTF-8") 
}
CTCAE$row_number <- rownames(CTCAE)

# 処理開始
FA <- FA[FA$FATEST == "Grade", ] #FAから、Gradeのみを取り出す

if(arm == "NO"){ 　# リスク分類なし、割付なしの場合の処理
  list_FASPID <- levels(as.factor(FA$FASPID))  #  治療コースのリスト
  
  for(i in 1:length(list_FASPID)){
    dxt <- FA[FA$FASPID == list_FASPID[i], ]  # 治療コース毎にデータを取得
    list_FAOBJ <- levels(as.factor(dxt$FAOBJ))  # 事象名のリスト
    df_list_FAOBJ <- as.data.frame(list_FAOBJ)
    
    emp_df <- data.frame(Toxicity = list_FAOBJ,
                         N = NA,
                         Grade3 = NA,
                         Grade3.percent = NA,
                         Grade4 = NA,
                         Grade4.percent = NA,
                         Grade5 = NA,
                         Grade5.percent = NA) # 事象名のリストのデータフレームを作る
    emp_df -> df
    df$N <- length(levels(as.factor(dxt$USUBJID))) # 分母
    df$Grade3 <- DataShaping(3)
    df$Grade3.percent <- ifelse(is.na(df$Grade3),
                                paste0(0, "%"),
                                paste0(floor(df$Grade3 / df$N* 100 + 0.5), "%"))
    df$Grade4 <- DataShaping(4)
    df$Grade4.percent <- ifelse(is.na(df$Grade4),
                                paste0(0, "%"),
                                paste0(floor(df$Grade4 / df$N* 100 + 0.5), "%"))
    df$Grade5 <- DataShaping(5)
    df$Grade5.percent <- ifelse(is.na(df$Grade5),
                                paste0(0, "%"),
                                paste0(floor(df$Grade5 / df$N* 100 + 0.5), "%"))
    
    df_merge <- merge(df, CTCAE, by.x = "Toxicity", by.y = "CTCAE.Term", all.x = T)
    df_merge$Grade3 <- ifelse(df_merge$Grade.3 == " -", " -", df_merge$Grade3)
    df_merge$Grade4 <- ifelse(df_merge$Grade.4 == " -", " -", df_merge$Grade4)
    df_merge$Grade5 <- ifelse(df_merge$Grade.5 == " -", " -", df_merge$Grade5)  # CTCAEで定義されていないものは"-"にする
    
    df_merge$Grade3.percent <- ifelse(df_merge$Grade.3 == " -", " -", df_merge$Grade3.percent)
    df_merge$Grade4.percent <- ifelse(df_merge$Grade.4 == " -", " -", df_merge$Grade4.percent)
    df_merge$Grade5.percent <- ifelse(df_merge$Grade.5 == " -", " -", df_merge$Grade5.percent) # CTCAEで定義されていないものは"-"にする
    
    df_merge$row_number <- ifelse(is.na(df_merge$row_number), 1000, as.integer(df_merge$row_number))
    df_merge <- df_merge[order(df_merge$row_number) , ]
    result <- df_merge[, c(1:8)]
    result[is.na(result)] <- 0
    colnames(result)[c(4, 6, 8)] <- "%"
    setwd(paste0(prtpath, "/output"))
    write.csv(result, paste0(kTrialTitle, "_", list_FASPID[i], "_AEgrade_", kToday, ".csv" ), row.names = F)
  }
  
} else { 　# risk分類,割付ありの場合の処理
  
  dxt_DM <- DM[, c("USUBJID", "ARM")]
  mFA <- merge(FA, dxt_DM, by = "USUBJID", all.x = T)
  list_FASPID <- levels(as.factor(mFA$FASPID))  #  治療コースのリスト
  
  for(i in 1:length(list_FASPID)){
    dxt0 <- mFA[mFA$FASPID == list_FASPID[i], ]  # 治療コース毎にデータを取得
    list_FAOBJ <- levels(as.factor(dxt0$FAOBJ))  # 事象名のリスト
    df_list_FAOBJ <- as.data.frame(list_FAOBJ)
    list_ARM <- levels(as.factor(mFA$ARM)) # ARMのリスト
    emp_df <- data.frame(Toxicity = list_FAOBJ,
                         N = NA,
                         Grade3 = NA,
                         Grade3.percent = NA,
                         Grade4 = NA,
                         Grade4.percent = NA,
                         Grade5 = NA,
                         Grade5.percent = NA) # 事象名のリストのデータフレームを作る
    for(j in 1:length(list_ARM)){
      dxt <- dxt0[dxt0$ARM == list_ARM[j], ]
      emp_df -> df
      df$N <- length(levels(as.factor(dxt$USUBJID))) # 分母
      df$Grade3 <- DataShaping(3)
      df$Grade3.percent <- ifelse(is.na(df$Grade3),
                                  paste0(0, "%"),
                                  paste0(floor(df$Grade3 / df$N* 100 + 0.5), "%"))
      df$Grade4 <- DataShaping(4)
      df$Grade4.percent <- ifelse(is.na(df$Grade4),
                                  paste0(0, "%"),
                                  paste0(floor(df$Grade4 / df$N* 100 + 0.5), "%"))
      df$Grade5 <- DataShaping(5)
      df$Grade5.percent <- ifelse(is.na(df$Grade5),
                                  paste0(0, "%"),
                                  paste0(floor(df$Grade5 / df$N* 100 + 0.5), "%"))
      df_merge <- merge(df, CTCAE, by.x = "Toxicity", by.y = "CTCAE.Term", all.x = T)
      df_merge$Grade3 <- ifelse(df_merge$Grade.3 == " -", " -", df_merge$Grade3)
      df_merge$Grade4 <- ifelse(df_merge$Grade.4 == " -", " -", df_merge$Grade4)
      df_merge$Grade5 <- ifelse(df_merge$Grade.5 == " -", " -", df_merge$Grade5)  # CTCAEで定義されていないものは"-"にする
      
      df_merge$Grade3.percent <- ifelse(df_merge$Grade.3 == " -", " -", df_merge$Grade3.percent)
      df_merge$Grade4.percent <- ifelse(df_merge$Grade.4 == " -", " -", df_merge$Grade4.percent)
      df_merge$Grade5.percent <- ifelse(df_merge$Grade.5 == " -", " -", df_merge$Grade5.percent) # CTCAEで定義されていないものは"-"にする
      df_merge$row_number <- ifelse(is.na(df_merge$row_number), 1000, as.integer(df_merge$row_number))
      df_merge <- df_merge[order(df_merge$row_number) , ]
      df_merge <- df_merge [order(df_merge$row_number) , ]
      result <- df_merge[, c(1:8)]
      result[is.na(result)] <- 0
      colnames(result)[c(4, 6, 8)] <- "%"
      setwd(paste0(prtpath, "/output"))
      write.csv(result, paste0(kTrialTitle, "_", list_FASPID[i], "_arm", list_ARM[j], "_AEgrade_", kToday, ".csv" ), row.names = F)
    }
  }
}




