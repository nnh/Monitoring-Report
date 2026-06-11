# 各シートの作成日とシート名症例番号の一覧を作成する
# 2026/6/11
# MAMIKO YONEJIMA

# config*******
prtpath <- "C:/Users/00281351/Box/Datacenter/Trials/JPLSG/51_ALL-T19/10.03.10 データレビュー書/第5回/作成日"
kTrialTitle  <- "ALL-T19"
#**************
library(tidyverse)

rawdatapath <- paste0(prtpath, "/rawdata/")
outputpath <- paste0(prtpath, "/output/")

# 出力フォルダが存在しなければ作成
outputpath <- paste0(prtpath, "/output")
if (!(file.exists(outputpath))){
  dir.create(outputpath)
}

# rawdataフォルダ内のリストを作成
list <- list.files(rawdatapath)

# --- 続きのコード（UTF-8 BOM対応版）---

library(readr)
library(purrr)
library(dplyr)

# 抽出したい列名のベクトル
target_cols <- c("シート名英数字別名", "作成日", "症例登録番号")

# 1. 各CSVファイルを読み込んで指定列を抽出する関数の定義
read_and_select <- function(filename) {
  file_full_path <- paste0(rawdatapath, filename)
  
  # UTF-8（BOM付き含む）で読み込み
  df <- read_csv(file_full_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
  
  # 指定した列が存在するものだけを抽出
  df_selected <- df %>% 
    select(any_of(target_cols))
  
  return(df_selected)
}

# 2. list（ファイル名一覧）に対して関数を繰り返し適用し、1つのデータフレームに結合
combined_df <- list %>% 
  map_dfr(~read_and_select(.x))

# 3. 結合したデータフレームをCSVとして出力
output_file_key <- paste0(outputpath, "/submit_date.csv")

write_excel_csv(combined_df, output_file_key, na = "")

# 完了メッセージ
cat("処理が完了しました。出力先:", output_file_key, "/n")