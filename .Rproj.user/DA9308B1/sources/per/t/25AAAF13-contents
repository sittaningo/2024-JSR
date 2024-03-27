# Viz----
# 推定でデータの作成
RobCheckPlot <- RobCheck |> 
  dplyr::arrange(-estimate) |>
  dplyr::mutate(row = dplyr::row_number())

# 可視化
Estimate <- RobCheckPlot |> 
  ggplot2::ggplot(aes(x = row, y = estimate)) + 
  geom_hline(yintercept = 0) + 
  geom_point(aes(shape = `gender`)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  gghighlight::gghighlight(p.value < 0.05) +
  theme_grey(base_family = "SourceHanSans-Regular") + 
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        legend.position = "top") + 
  scale_y_continuous(limits = c(-0.04, 0.04)) + 
  ggplot2::labs(y = "推定値")

# 決定点の同定
DecPoint <- RobCheckPlot |> 
  dplyr::mutate(`〜64歳` = dplyr::if_else(maxage == "64", TRUE, FALSE),
                `〜59歳` = dplyr::if_else(maxage == "59", TRUE, FALSE),
                `〜54歳` = dplyr::if_else(maxage == "54", TRUE, FALSE),
                `無業0` = dplyr::if_else(unemp_score == "0", TRUE, FALSE),
                `無業10` = dplyr::if_else(unemp_score == "10", TRUE, FALSE),
                `無業20` = dplyr::if_else(unemp_score == "20", TRUE, FALSE),
                `無業30` = dplyr::if_else(unemp_score == "30", TRUE, FALSE), 
                `GAM` = dplyr::if_else(func == "GAM", TRUE, FALSE),
                `LPM` = dplyr::if_else(func == "LPM", TRUE, FALSE),
                `Logit` = dplyr::if_else(func == "Logit", TRUE, FALSE),
                `Probit` = dplyr::if_else(func == "Probit", TRUE, FALSE),
                `無業含む` = dplyr::if_else(unemp == "無業含む", TRUE, FALSE),
                `無業除く` = dplyr::if_else(unemp == "無業除く", TRUE, FALSE),
  ) |>
  tidyr::pivot_longer(`〜64歳`:`無業除く`, 
                      names_to = "SpecificationItem", 
                      values_to = "Appli") |> 
  dplyr::mutate(ItemNum = dplyr::row_number())

DecP <- DecPoint |> 
  # dplyr::filter(gender == "男性") |> 
  ggplot2::ggplot(aes(x = row, y = SpecificationItem)) + 
  ggplot2::geom_point() + 
  gghighlight::gghighlight(Appli == TRUE, unhighlighted_colour = "white") +
  theme_grey(base_family = "SourceHanSans-Regular") + 
  theme(strip.background = element_rect(color="white", fill="white"), 
        strip.text = element_text(color = "white")) + 
  ggplot2::labs(y = "決定点", 
                x = "番号")

MultiVerse <- ggarrange(Estimate, DecP, nrow = 2, ncol = 1, align = "v")
MultiVerse

