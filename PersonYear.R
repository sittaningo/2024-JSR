# パーソンイヤーデータ----

## 社会人口学的属性----

### 女性ダミー----
d <- d |> dplyr::mutate(female = q1_1 - 1, 
                        female = factor(female,
                                        levels = c(0, 1),
                                        labels = c("男性", "女性")))

### 大卒ダミー----
d <- d |> dplyr::mutate(univ = dplyr::case_when(edssm < 10 ~ 0, 
                                                edssm == 10 | edssm == 11 ~ 1, 
                                                TRUE ~ NA_real_), 
                        univ = factor(univ,
                                      levels = c(0, 1), 
                                      labels = c("非大卒", "大卒")))

### 年齢 ----
d <- d |> dplyr::mutate(age = q1_2_5)


## 職業経歴の操作化----

## 移動経験のあるものに限定
d <- d |> dplyr::filter(dansu > 1)

### 従業開始年齢----

Py <- d |> 
  dplyr::mutate(age1 = q8_h_1,
                age2 = q9_2_c_7,
                age3 = q9_3_c_7,
                age4 = q9_4_c_7,
                age5 = q9_5_c_7,
                age6 = q9_6_c_7,
                age7 = q9_7_c_7,
                age8 = q9_8_c_7,
                age9 = q9_9_c_7,
                age10 = q9_10_c_7,
                age11 = q9_11_c_7,
                age12 = q9_12_c_7,
                age13 = q9_13_c_7,
                age14 = q9_14_c_7,
                age15 = q9_15_c_7,
                age16 = q9_16_c_7,
                age17 = q9_17_c_7,
                age18 = q9_18_c_7,
                age22 = q9_22_c_7) |> 
  dplyr::select(id, age1:age22) |> 
  tidyr::pivot_longer(age1:age22, 
                      names_to = "dansu", 
                      names_prefix = "age", 
                      values_to = "num") |> 
  dplyr::mutate(num = dplyr::if_else(num > 80, NA_integer_, num)) |> 
  dplyr::group_by(id) |> 
  dplyr::select(id, num, dansu) |> 
  tidyr::complete(num = full_seq(10:81, 1)) |> 
  tidyr::fill(dansu, .direction="down") |> 
  tidyr::fill(num, .direction = "down") |> 
  dplyr::ungroup() |> 
  dplyr::group_by(id) |> 
  dplyr::distinct(num, .keep_all = TRUE) |> 
  dplyr::ungroup() |> 
  tidyr::drop_na()

### 無業経験----

Py <- d |> 
  dplyr::mutate(unemp1 = q8_h_2, 
                unemp2 = q9_2_b, 
                unemp3 = q9_3_b, 
                unemp4 = q9_4_b, 
                unemp5 = q9_5_b, 
                unemp6 = q9_6_b, 
                unemp7 = q9_7_b, 
                unemp8 = q9_8_b, 
                unemp9 = q9_9_b, 
                unemp10 = q9_10_b, 
                unemp11 = q9_11_b, 
                unemp12 = q9_12_b, 
                unemp13 = q9_13_b, 
                unemp14 = q9_14_b, 
                unemp15 = q9_15_b, 
                unemp16 = q9_16_b, 
                unemp17 = q9_17_b, 
                unemp18 = q9_18_b, 
                unemp22 = q9_22_b) |> 
  dplyr::select(id, unemp1:unemp22) |> 
  tidyr::pivot_longer(unemp1:unemp22, 
                      names_to = "dansu", 
                      names_prefix = "unemp", 
                      values_to = "unemp") |> 
  dplyr::mutate(unemp = dplyr::if_else(unemp == 2, 1, 0)) |> 
  (\(.) dplyr::inner_join(Py, ., by = c("id", "dansu")))()


### 職業小分類----
Py <- d |> 
  dplyr::mutate(occ1 = q8_f, 
                occ2 = q9_2_c_5, 
                occ3 = q9_3_c_5, 
                occ4 = q9_4_c_5, 
                occ5 = q9_5_c_5, 
                occ6 = q9_6_c_5, 
                occ7 = q9_7_c_5, 
                occ8 = q9_8_c_5, 
                occ9 = q9_9_c_5, 
                occ10 = q9_10_c_5, 
                occ11 = q9_11_c_5, 
                occ12 = q9_12_c_5, 
                occ13 = q9_13_c_5, 
                occ14 = q9_14_c_5, 
                occ15 = q9_15_c_5, 
                occ16 = q9_16_c_5, 
                occ17 = q9_17_c_5, 
                occ18 = q9_18_c_5, 
                occ22 = q9_22_c_5) |> 
  dplyr::select(id, occ1:occ22) |> 
  tidyr::pivot_longer(occ1:occ22, 
                      names_to = "dansu", 
                      names_prefix = "occ", 
                      values_to = "occ") |> 
  dplyr::mutate(occ = dplyr::if_else(occ > 8888, NA_integer_, occ)) |> 
  (\(.) dplyr::inner_join(Py, ., by = c("id", "dansu")))()

### 就業の有無----

Py <- d |> 
  dplyr::mutate(work1 = 1,
                work2 = q9_2_b,
                work3 = q9_3_b,
                work4 = q9_4_b,
                work5 = q9_5_b,
                work6 = q9_6_b,
                work7 = q9_7_b,
                work8 = q9_8_b,
                work9 = q9_9_b,
                work10 = q9_10_b,
                work11 = q9_11_b,
                work12 = q9_12_b,
                work13 = q9_13_b,
                work14 = q9_14_b,
                work15 = q9_15_b,
                work16 = q9_16_b,
                work17 = q9_17_b,
                work18 = q9_18_b,
                work22 = q9_22_b) |> 
  dplyr::select(id, work1:work22) |> 
  tidyr::pivot_longer(work1:work22, 
                      names_to = "dansu", 
                      names_prefix = "work", 
                      values_to = "work") |> 
  (\(.) dplyr::inner_join(Py, ., by = c("id", "dansu")))()

### その他の処理----

### dansuをnumeric型に
Py <- Py |> 
  dplyr::mutate(dansu = as.numeric(dansu))

### 性別と学歴を結合
Py <- d |> 
  dplyr::select(id, female, univ, age) |> 
  (\(.) dplyr::left_join(Py, ., by = "id"))()
