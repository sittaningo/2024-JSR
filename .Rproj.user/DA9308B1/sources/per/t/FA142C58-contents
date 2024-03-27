# 平滑化パラメータの

knot <- 5

# スプライン回帰：推定----
gam_m <- gam(curwork ~ s(jsei_mean) + s(unemprate, age, ps, k = knot) + univ + father_univ, data = old_male)
gam_f <- gam(curwork ~ s(jsei_mean) + s(unemprate, age, ps, k = knot) + univ + father_univ, data = old_female)

# スプライン回帰：結果の確認----
summary(gam_m)
summary(gam_f)

# スプライン回帰：結果の記述----
par(family="SourceHanSans-Regular", mfrow = c(1, 2))

plot.gam(gam_m,
         residuals = TRUE,
         se = TRUE,
         select = 1,
         all.terms = TRUE,
         pch = "。", 
         main="男性",
         xlab="JSEI",
         ylab="",
         scheme = 1
)

plot_spline <- plot.gam(
  gam_f,
  residuals = TRUE,
  se = TRUE,
  select = 1,
  all.terms = TRUE,
  pch = "。", 
  main="女性",
  xlab="JSEI",
  ylab="",
  scheme = 1
  
)

# 平均部分効果----
margeff_m <- marginaleffects::marginaleffects(gam_m, variables = "jsei_mean")
margeff_f <- marginaleffects::marginaleffects(gam_f, variables = "jsei_mean")

Marg <- summary(margeff_m) |> dplyr::mutate(gender = "男性")
Marg <- summary(margeff_f) |> 
  dplyr::mutate(gender = "女性") |> 
  dplyr::bind_rows(Marg)


# 両方の結果を集約----

Marg |> 
  dplyr::mutate(gender = forcats::fct_relevel(gender, c("男性", "女性"))) |> 
  ggplot2::ggplot(aes(x = gender, y = estimate)) + 
  ggplot2::geom_hline(yintercept = 0, color = "grey") + 
  ggplot2::geom_point() + 
  ggplot2::geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  theme_pubclean(base_family = "SourceHanSans-Regular") + 
  labs(x = "性別", y = "平均限界効果")

