old_overall <- old |> 
  dplyr::select(curwork, jsei_mean, unemprate, female, univ, age, father_univ) |> 
  tidyr::drop_na()
old_male <- old |> 
  dplyr::filter(female == "男性") |> 
  dplyr::select(curwork, jsei_mean, unemprate, female, univ, age, father_univ) |> 
  tidyr::drop_na()
old_female <- old |> 
  dplyr::filter(female == "女性") |> 
  dplyr::select(curwork, jsei_mean, unemprate, female, univ, age, father_univ) |> 
  tidyr::drop_na()
