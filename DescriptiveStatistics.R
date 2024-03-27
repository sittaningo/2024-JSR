# 記述統計量の出力----
old_male |> 
  dplyr::select(curwork, jsei_mean, unemprate, age, univ, father_univ, ps) |> 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{p}"), 
              digits = list(all_continuous() ~ 2, 
                            all_categorical() ~ 2), 
              label = list(curwork ~ "就業ダミー（％）",
                           jsei_mean = "JSEI（平均）", 
                           ps = "傾向スコア",
                           unemprate = "無業期間割合", 
                           age ~ "年齢",
                           univ ~ "本人大卒ダミー（％）",
                           father_univ ~ "父大卒ダミー（％）")) |> 
  modify_header(label ~ "") |> 
  as_flex_table() |> 
  flextable::save_as_docx(path = "Results/desc_male.docx")

old_female |> 
  dplyr::select(curwork, jsei_mean, unemprate, age, univ, father_univ, ps) |> 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{p}"), 
              digits = list(all_continuous() ~ 2, 
                            all_categorical() ~ 2), 
              label = list(curwork ~ "就業ダミー（％）",
                           jsei_mean = "JSEI（平均）", 
                           ps = "傾向スコア",
                           unemprate = "無業期間割合", 
                           age ~ "年齢",
                           univ ~ "本人大卒ダミー（％）",
                           father_univ ~ "父大卒ダミー（％）")) |> 
  modify_header(label ~ "") |> 
  as_flex_table() |> 
  flextable::save_as_docx(path = "Results/desc_male.docx")

# ヒストグラム（性・就業状態別）----

old_overall |> 
  dplyr::select(jsei_mean, female, curwork) |> 
  tidyr::drop_na() |> 
  mutate(curwork = dplyr::recode(curwork, 
                                 `0` = "非就業", 
                                 `1` = "就業")) |> 
  ggplot(aes(x = jsei_mean, fill = female, alpha = 0.7)) + 
  geom_density() + 
  facet_wrap(~ curwork) + 
  theme_pubr(base_family = "SourceHanSans-Regular") + 
  scale_fill_grey()
