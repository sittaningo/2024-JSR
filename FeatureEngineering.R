# アウトカム----

## 65歳以降の就業----

old <- Py |> 
  dplyr::filter(age >= 65) |>
  dplyr::mutate(olderwork = dplyr::case_when(work == 1 ~ 1,
                                             work == 2 ~ 0, 
                                             TRUE ~ NA_integer_),
                work = "work") |> 
  dplyr::select(id, work, num, olderwork) |> 
  tidyr::pivot_wider(names_from = c(work, num), 
                     names_sep = "",
                     values_from = olderwork) |> 
  dplyr::distinct(id, .keep_all = TRUE) |>
  dplyr::select(id, work65, work66, work67, work68, work69, work70, work71, work72, work73, work74, work75, work76, work77, work78, work79) |> 
  dplyr::mutate(curwork = dplyr::if_else((work65 == 1 | work66 == 1 | work67 == 1 | work68 == 1 | work69 == 1 | work70 == 1 | work71 == 1 | work72 == 1 | work73 == 1 | work74 == 1 | work75 == 1 | work76 == 1 | work77 == 1 | work78 == 1 | work79 == 1), 1, 0)) |> 
  dplyr::select(id, curwork) |> 
  (\(.) dplyr::inner_join(old, ., by = "id"))()

# 処置 ----

## 初職開始年齢----
old <- old |> 
  dplyr::mutate(jobstart1 = if_else(q8_h_1 > 60, 0, q8_h_1))

## JSEI（mean）----

jsei <- readr::read_csv("https://raw.githubusercontent.com/ShoFujihara/OccupationalScales/master/SSM_sei_ssi_v1.0.csv") |> 
  dplyr::rename(occ = ssm) |> 
  dplyr::add_row(occ = 8888,
                        sei = 0,
                        ssi = 0)

# 無業0 | 上限59
old <- Py |> 
  dplyr::filter(age >= 65 & age <= 79 & num >= 15 & num < 60) |> 
  dplyr::left_join(jsei, by = "occ") |> 
  dplyr::group_by(id) |> 
  dplyr::summarise(jsei_sum_0_59 = sum(sei, na.rm = TRUE)) |> 
  dplyr::select(id, jsei_sum_0_59) |> 
  (\(.) dplyr::left_join(old, ., by = "id"))()


old <- old |> 
  dplyr::mutate(jsei_mean = jsei_sum_0_59 / (59 - jobstart1))

# 調整変数----

## 本人大卒ダミー----

old <- old |> 
  dplyr::mutate(univ = dplyr::recode(edssm, 
                                     `4` = 0L, 
                                     `5` = 0L, 
                                     `8` = 0L, 
                                     `10` = 1L, 
                                     `11` = 1L, 
                                     `88` = NA_integer_, 
                                     `99` = NA_integer_, 
                                     .default = NA_integer_), 
                univ = forcats::fct_recode(factor(univ), "Nonuniv" = "0", "Univ" = "1"))


## 父大卒ダミー----

old <- old |> 
  dplyr::mutate(father_univ = dplyr::recode(q22_a, 
                                             `1` = 0L, 
                                           `2` = 0L, 
                                           `3` = 0L, 
                                           `4` = 0L, 
                                           `5` = 0L, 
                                           `6` = 0L, 
                                           `7` = 1L, 
                                           `8` = 0L, 
                                           `9` = 0L, 
                                           `10` = 0L, 
                                           `11` = 0L, 
                                           `12` = 1L, 
                                           `13` = 1L, 
                                           `14` = NA_integer_, 
                                           `99` = NA_integer_, 
                                           `999` = NA_integer_), 
                father_univ = forcats::fct_recode(factor(father_univ), "Nonuniv" = "0", "Univ" = "1"))

## 無業期間----
old <- old |> 
  mutate(unempspan2 = dplyr::if_else(q9_2_b == 2 & (q9_2_c_7 < 60 & q9_2_c_8< 60), q9_2_c_8 - q9_2_c_7, 0), 
         unempspan3 = dplyr::if_else(q9_3_b == 2 & (q9_3_c_7 < 60 & q9_3_c_8< 60), q9_3_c_8 - q9_3_c_7, 0), 
         unempspan4 = dplyr::if_else(q9_4_b == 2 & (q9_4_c_7 < 60 & q9_4_c_8< 60), q9_4_c_8 - q9_4_c_7, 0), 
         unempspan5 = dplyr::if_else(q9_5_b == 2 & (q9_5_c_7 < 60 & q9_5_c_8< 60), q9_5_c_8 - q9_5_c_7, 0), 
         unempspan6 = dplyr::if_else(q9_6_b == 2 & (q9_6_c_7 < 60 & q9_6_c_8< 60), q9_6_c_8 - q9_6_c_7, 0), 
         unempspan7 = dplyr::if_else(q9_7_b == 2 & (q9_7_c_7 < 60 & q9_7_c_8< 60), q9_7_c_8 - q9_7_c_7, 0), 
         unempspan8 = dplyr::if_else(q9_8_b == 2 & (q9_8_c_7 < 60 & q9_8_c_8< 60), q9_8_c_8 - q9_8_c_7, 0), 
         unempspan9 = dplyr::if_else(q9_9_b == 2 & (q9_9_c_7 < 60 & q9_9_c_8< 60), q9_9_c_8 - q9_9_c_7, 0), 
         unempspan10 = dplyr::if_else(q9_10_b == 2 & (q9_10_c_7 < 60 & q9_10_c_8< 60), q9_10_c_8 - q9_10_c_7, 0), 
         unempspan11 = dplyr::if_else(q9_11_b == 2 & (q9_11_c_7 < 60 & q9_11_c_8< 60), q9_11_c_8 - q9_11_c_7, 0), 
         unempspan12 = dplyr::if_else(q9_12_b == 2 & (q9_12_c_7 < 60 & q9_12_c_8< 60), q9_12_c_8 - q9_12_c_7, 0), 
         unempspan13 = dplyr::if_else(q9_13_b == 2 & (q9_13_c_7 < 60 & q9_13_c_8< 60), q9_13_c_8 - q9_13_c_7, 0), 
         unempspan14 = dplyr::if_else(q9_14_b == 2 & (q9_14_c_7 < 60 & q9_14_c_8< 60), q9_14_c_8 - q9_14_c_7, 0), 
         unempspan15 = dplyr::if_else(q9_15_b == 2 & (q9_15_c_7 < 60 & q9_15_c_8< 60), q9_15_c_8 - q9_15_c_7, 0), 
         unempspan16 = dplyr::if_else(q9_16_b == 2 & (q9_16_c_7 < 60 & q9_16_c_8< 60), q9_16_c_8 - q9_16_c_7, 0), 
         unempspan17 = dplyr::if_else(q9_17_b == 2 & (q9_17_c_7 < 60 & q9_17_c_8< 60), q9_17_c_8 - q9_17_c_7, 0), 
         unempspan18 = dplyr::if_else(q9_18_b == 2 & (q9_18_c_7 < 60 & q9_18_c_8< 60), q9_18_c_8 - q9_18_c_7, 0), 
         unempspan22 = dplyr::if_else(q9_22_b == 2 & (q9_22_c_7 < 60 & q9_22_c_8< 60), q9_22_c_8 - q9_22_c_7, 0)) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(unempspan = sum(c(unempspan2, unempspan3, unempspan4, unempspan5, unempspan6, unempspan7, unempspan8, unempspan9, unempspan10, unempspan11, unempspan12, unempspan13, unempspan14, unempspan15, unempspan16, unempspan17, unempspan18, unempspan22), na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  dplyr::select(id, unempspan) |> 
  dplyr::inner_join(old, ., by = "id")

old <- old |> 
  dplyr::mutate(unemprate = (unempspan / (59 - jobstart1)) * 100)
