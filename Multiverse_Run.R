# Feature Engineering for Multiverse----

# 無業を0とカウント
old <- Py %>% 
  filter(age >= 65 & age <= 79 & num < 60) %>% 
  dplyr::left_join(jsei, by = "occ") %>%
  group_by(id) |> 
  summarise(jsei_sum_0 = sum(sei, na.rm = T), .groups = "keep") %>%
  ungroup() %>%
  select(id, jsei_sum_0) %>%
  left_join(old, ., by = "id")

# 無業を10とカウント
old <- Py %>% 
  filter(age >= 65 & age <= 79 & num < 60) %>% 
  dplyr::left_join(jsei, by = "occ") %>%
  dplyr::mutate(sei = dplyr::recode(sei, `0` = 10, .default = sei)) |> 
  group_by(id) |> 
  summarise(jsei_sum_10 = sum(sei, na.rm = T), .groups = "keep") %>%
  ungroup() %>%
  select(id, jsei_sum_10) %>%
  left_join(old, ., by = "id")

# 無業を20とカウント
old <- Py %>% 
  filter(age >= 65 & age <= 79 & num < 60) %>% 
  dplyr::left_join(jsei, by = "occ") %>%
  dplyr::mutate(sei = dplyr::recode(sei, `0` = 20, .default = sei)) |> 
  group_by(id) |> 
  summarise(jsei_sum_20 = sum(sei, na.rm = T), .groups = "keep") %>%
  ungroup() %>%
  select(id, jsei_sum_20) %>%
  left_join(old, ., by = "id")

# 無業を30とカウント
old <- Py %>% 
  filter(age >= 65 & age <= 79 & num < 60) %>% 
  dplyr::left_join(jsei, by = "occ") %>%
  dplyr::mutate(sei = dplyr::recode(sei, `0` = 30, .default = sei)) |> 
  group_by(id) |> 
  summarise(jsei_sum_30 = sum(sei, na.rm = T), .groups = "keep") %>%
  ungroup() %>%
  select(id, jsei_sum_30) %>%
  left_join(old, ., by = "id")

old <- old |> 
  mutate(jsei_mean = jsei_sum_0 / (59 - jobstart1), 
         jsei_mean64 = jsei_sum_0 / (64 - jobstart1), 
         jsei_mean54 = jsei_sum_0 / (54 - jobstart1),
         jsei_mean_10 = jsei_sum_10 / (59 - jobstart1), 
         jsei_mean64_10 = jsei_sum_10 / (64 - jobstart1), 
         jsei_mean54_10 = jsei_sum_10 / (54 - jobstart1),
         jsei_mean_20 = jsei_sum_20 / (59 - jobstart1), 
         jsei_mean64_20 = jsei_sum_20 / (64 - jobstart1), 
         jsei_mean54_20 = jsei_sum_20 / (54 - jobstart1),
         jsei_mean_30 = jsei_sum_30 / (59 - jobstart1), 
         jsei_mean64_30 = jsei_sum_30 / (64 - jobstart1), 
         jsei_mean54_30 = jsei_sum_30 / (54 - jobstart1))

# Sample----
old_male_mv <- old |> 
  dplyr::filter(female == "男性") |> 
  dplyr::select(curwork, jsei_mean, jsei_mean64, jsei_mean54, jsei_mean_10, jsei_mean_20, jsei_mean_30, jsei_mean64_10, jsei_mean64_20, jsei_mean64_30, jsei_mean54_10, jsei_mean54_20, jsei_mean54_30, unemprate, female, univ, age, father_univ) |> 
  tidyr::drop_na()
old_female_mv <- old |> 
  dplyr::filter(female == "女性") |> 
  dplyr::select(curwork, jsei_mean, jsei_mean64, jsei_mean54, jsei_mean_10, jsei_mean_20, jsei_mean_30, jsei_mean64_10, jsei_mean64_20, jsei_mean64_30, jsei_mean54_10, jsei_mean54_20, jsei_mean54_30, unemprate, female, univ, age, father_univ) |> 
  tidyr::drop_na()

# [Max=64]----

#___JSEI=0________----

#1: 0 x 64 x M x Gam x IncUnemp----
m <- lm(jsei_mean64 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean64) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む")

#2: 0 x 64 x F x Gam x IncUnemp----
m <- lm(jsei_mean64 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean64) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 0 x 64 x M x Lpm x IncUnemp----
m <- lm(jsei_mean64 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean64 + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |> 
  
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 0 x 64 x F x Lpm x IncUnemp----
m <- lm(jsei_mean64 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean64 + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 0 x 64 x M x Logit x IncUnemp----
m <- lm(jsei_mean64 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64 + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 0 x 64 x F x Logit x IncUnemp----
m <- lm(jsei_mean64 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64 + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 0 x 64 x M x Probit x IncUnemp----
m <- lm(jsei_mean64 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64 + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 0 x 64 x F x Probit x IncUnemp----
m <- lm(jsei_mean64 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64 + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 0 x 64 x M x Gam x ExclUnemp----
m <- lm(jsei_mean64 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean64) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 0 x 64 x F x Gam x ExclUnemp----
m <- lm(jsei_mean64 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean64) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 0 x 64 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean64 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean64 + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 0 x 64 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean64 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean64 + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 0 x 64 x M x Logit x ExclUnemp----
m <- lm(jsei_mean64 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64 + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 0 x 64 x F x Logit x ExclUnemp----
m <- lm(jsei_mean64 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64 + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 0 x 64 x M x Probit x ExclUnemp----
m <- lm(jsei_mean64 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64 + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 0 x 64 x F x Probit x ExclUnemp----
m <- lm(jsei_mean64 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64 + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "64", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#___JSEI=10_______----

#1: 10 x 64 x M x Gam x IncUnemp----
m <- lm(jsei_mean64_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean64_10) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#2: 10 x 64 x F x Gam x IncUnemp----
m <- lm(jsei_mean64_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean64_10) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 10 x 64 x M x Lpm x IncUnemp----
m <- lm(jsei_mean64_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean64_10 + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 10 x 64 x F x Lpm x IncUnemp----
m <- lm(jsei_mean64_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean64_10 + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 10 x 64 x M x Logit x IncUnemp----
m <- lm(jsei_mean64_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_10 + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 10 x 64 x F x Logit x IncUnemp----
m <- lm(jsei_mean64_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_10 + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 10 x 64 x M x Probit x IncUnemp----
m <- lm(jsei_mean64_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_10 + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 10 x 64 x F x Probit x IncUnemp----
m <- lm(jsei_mean64_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_10 + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 10 x 64 x M x Gam x ExclUnemp----
m <- lm(jsei_mean64_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean64_10) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 10 x 64 x F x Gam x ExclUnemp----
m <- lm(jsei_mean64_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean64_10) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 10 x 64 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean64_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean64_10 + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 10 x 64 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean64_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean64_10 + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 10 x 64 x M x Logit x ExclUnemp----
m <- lm(jsei_mean64_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_10 + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 10 x 64 x F x Logit x ExclUnemp----
m <- lm(jsei_mean64_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_10 + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 10 x 64 x M x Probit x ExclUnemp----
m <- lm(jsei_mean64_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_10 + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 10 x 64 x F x Probit x ExclUnemp----
m <- lm(jsei_mean64_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_10 + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "64", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#___JSEI=20_______----

#1: 20 x 64 x M x Gam x IncUnemp----
m <- lm(jsei_mean64_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean64_20) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#2: 20 x 64 x F x Gam x IncUnemp----
m <- lm(jsei_mean64_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean64_20) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 20 x 64 x M x Lpm x IncUnemp----
m <- lm(jsei_mean64_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean64_20 + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 20 x 64 x F x Lpm x IncUnemp----
m <- lm(jsei_mean64_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean64_20 + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 20 x 64 x M x Logit x IncUnemp----
m <- lm(jsei_mean64_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_20 + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 20 x 64 x F x Logit x IncUnemp----
m <- lm(jsei_mean64_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_20 + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 20 x 64 x M x Probit x IncUnemp----
m <- lm(jsei_mean64_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_20 + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 20 x 64 x F x Probit x IncUnemp----
m <- lm(jsei_mean64_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_20 + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 20 x 64 x M x Gam x ExclUnemp----
m <- lm(jsei_mean64_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean64_20) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 20 x 64 x F x Gam x ExclUnemp----
m <- lm(jsei_mean64_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean64_20) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 20 x 64 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean64_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean64_20 + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 20 x 64 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean64_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean64_20 + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 20 x 64 x M x Logit x ExclUnemp----
m <- lm(jsei_mean64_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_20 + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 20 x 64 x F x Logit x ExclUnemp----
m <- lm(jsei_mean64_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_20 + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 20 x 64 x M x Probit x ExclUnemp----
m <- lm(jsei_mean64_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_20 + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 20 x 64 x F x Probit x ExclUnemp----
m <- lm(jsei_mean64_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_20 + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "64", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)


#___JSEI=30_______----

#1: 30 x 64 x M x Gam x IncUnemp----
m <- lm(jsei_mean64_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean64_30) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#2: 30 x 64 x F x Gam x IncUnemp----
m <- lm(jsei_mean64_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean64_30) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 30 x 64 x M x Lpm x IncUnemp----
m <- lm(jsei_mean64_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean64_30 + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 30 x 64 x F x Lpm x IncUnemp----
m <- lm(jsei_mean64_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean64_30 + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 30 x 64 x M x Logit x IncUnemp----
m <- lm(jsei_mean64_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_30 + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 30 x 64 x F x Logit x IncUnemp----
m <- lm(jsei_mean64_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_30 + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 30 x 64 x M x Probit x IncUnemp----
m <- lm(jsei_mean64_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_30 + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 30 x 64 x F x Probit x IncUnemp----
m <- lm(jsei_mean64_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_30 + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 30 x 64 x M x Gam x ExclUnemp----
m <- lm(jsei_mean64_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean64_30) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 30 x 64 x F x Gam x ExclUnemp----
m <- lm(jsei_mean64_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean64_30) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 30 x 64 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean64_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean64_30 + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 30 x 64 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean64_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean64_30 + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean64_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 30 x 64 x M x Logit x ExclUnemp----
m <- lm(jsei_mean64_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_30 + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 30 x 64 x F x Logit x ExclUnemp----
m <- lm(jsei_mean64_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_30 + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 30 x 64 x M x Probit x ExclUnemp----
m <- lm(jsei_mean64_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean64_30 + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean64_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 30 x 64 x F x Probit x ExclUnemp----
m <- lm(jsei_mean64_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean64_30 + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean64_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "64", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

# [Max=54]----

#___JSEI=0________----

#1: 0 x 54 x M x Gam x IncUnemp----
m <- lm(jsei_mean54 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean54) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#2: 0 x 54 x F x Gam x IncUnemp----
m <- lm(jsei_mean54 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean54) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 0 x 54 x M x Lpm x IncUnemp----
m <- lm(jsei_mean54 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean54 + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |> 
  
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 0 x 54 x F x Lpm x IncUnemp----
m <- lm(jsei_mean54 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean54 + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 0 x 54 x M x Logit x IncUnemp----
m <- lm(jsei_mean54 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54 + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 0 x 54 x F x Logit x IncUnemp----
m <- lm(jsei_mean54 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54 + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 0 x 54 x M x Probit x IncUnemp----
m <- lm(jsei_mean54 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54 + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 0 x 54 x F x Probit x IncUnemp----
m <- lm(jsei_mean54 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54 + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 0 x 54 x M x Gam x ExclUnemp----
m <- lm(jsei_mean54 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean54) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 0 x 54 x F x Gam x ExclUnemp----
m <- lm(jsei_mean54 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean54) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 0 x 54 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean54 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean54 + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 0 x 54 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean54 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean54 + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 0 x 54 x M x Logit x ExclUnemp----
m <- lm(jsei_mean54 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54 + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 0 x 54 x F x Logit x ExclUnemp----
m <- lm(jsei_mean54 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54 + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 0 x 54 x M x Probit x ExclUnemp----
m <- lm(jsei_mean54 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54 + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 0 x 54 x F x Probit x ExclUnemp----
m <- lm(jsei_mean54 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54 + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "54", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#___JSEI=10_______----

#1: 10 x 54 x M x Gam x IncUnemp----
m <- lm(jsei_mean54_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean54_10) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#2: 10 x 54 x F x Gam x IncUnemp----
m <- lm(jsei_mean54_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean54_10) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 10 x 54 x M x Lpm x IncUnemp----
m <- lm(jsei_mean54_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean54_10 + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 10 x 54 x F x Lpm x IncUnemp----
m <- lm(jsei_mean54_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean54_10 + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 10 x 54 x M x Logit x IncUnemp----
m <- lm(jsei_mean54_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_10 + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 10 x 54 x F x Logit x IncUnemp----
m <- lm(jsei_mean54_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_10 + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 10 x 54 x M x Probit x IncUnemp----
m <- lm(jsei_mean54_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_10 + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 10 x 54 x F x Probit x IncUnemp----
m <- lm(jsei_mean54_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_10 + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 10 x 54 x M x Gam x ExclUnemp----
m <- lm(jsei_mean54_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean54_10) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 10 x 54 x F x Gam x ExclUnemp----
m <- lm(jsei_mean54_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean54_10) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 10 x 54 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean54_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean54_10 + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 10 x 54 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean54_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean54_10 + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 10 x 54 x M x Logit x ExclUnemp----
m <- lm(jsei_mean54_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_10 + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 10 x 54 x F x Logit x ExclUnemp----
m <- lm(jsei_mean54_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_10 + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 10 x 54 x M x Probit x ExclUnemp----
m <- lm(jsei_mean54_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_10 + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 10 x 54 x F x Probit x ExclUnemp----
m <- lm(jsei_mean54_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_10 + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "54", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#___JSEI=20_______----

#1: 20 x 54 x M x Gam x IncUnemp----
m <- lm(jsei_mean54_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean54_20) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#2: 20 x 54 x F x Gam x IncUnemp----
m <- lm(jsei_mean54_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean54_20) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 20 x 54 x M x Lpm x IncUnemp----
m <- lm(jsei_mean54_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean54_20 + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 20 x 54 x F x Lpm x IncUnemp----
m <- lm(jsei_mean54_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean54_20 + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 20 x 54 x M x Logit x IncUnemp----
m <- lm(jsei_mean54_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_20 + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 20 x 54 x F x Logit x IncUnemp----
m <- lm(jsei_mean54_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_20 + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 20 x 54 x M x Probit x IncUnemp----
m <- lm(jsei_mean54_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_20 + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 20 x 54 x F x Probit x IncUnemp----
m <- lm(jsei_mean54_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_20 + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 20 x 54 x M x Gam x ExclUnemp----
m <- lm(jsei_mean54_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean54_20) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 20 x 54 x F x Gam x ExclUnemp----
m <- lm(jsei_mean54_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean54_20) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 20 x 54 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean54_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean54_20 + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 20 x 54 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean54_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean54_20 + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 20 x 54 x M x Logit x ExclUnemp----
m <- lm(jsei_mean54_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_20 + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 20 x 54 x F x Logit x ExclUnemp----
m <- lm(jsei_mean54_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_20 + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 20 x 54 x M x Probit x ExclUnemp----
m <- lm(jsei_mean54_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_20 + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 20 x 54 x F x Probit x ExclUnemp----
m <- lm(jsei_mean54_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_20 + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "54", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)


#___JSEI=30_______----

#1: 30 x 54 x M x Gam x IncUnemp----
m <- lm(jsei_mean54_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean54_30) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#2: 30 x 54 x F x Gam x IncUnemp----
m <- lm(jsei_mean54_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean54_30) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 30 x 54 x M x Lpm x IncUnemp----
m <- lm(jsei_mean54_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean54_30 + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 30 x 54 x F x Lpm x IncUnemp----
m <- lm(jsei_mean54_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean54_30 + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 30 x 54 x M x Logit x IncUnemp----
m <- lm(jsei_mean54_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_30 + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 30 x 54 x F x Logit x IncUnemp----
m <- lm(jsei_mean54_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_30 + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 30 x 54 x M x Probit x IncUnemp----
m <- lm(jsei_mean54_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_30 + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 30 x 54 x F x Probit x IncUnemp----
m <- lm(jsei_mean54_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_30 + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 30 x 54 x M x Gam x ExclUnemp----
m <- lm(jsei_mean54_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean54_30) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 30 x 54 x F x Gam x ExclUnemp----
m <- lm(jsei_mean54_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean54_30) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 30 x 54 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean54_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean54_30 + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 30 x 54 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean54_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean54_30 + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean54_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 30 x 54 x M x Logit x ExclUnemp----
m <- lm(jsei_mean54_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_30 + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 30 x 54 x F x Logit x ExclUnemp----
m <- lm(jsei_mean54_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_30 + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 30 x 54 x M x Probit x ExclUnemp----
m <- lm(jsei_mean54_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean54_30 + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean54_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 30 x 54 x F x Probit x ExclUnemp----
m <- lm(jsei_mean54_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean54_30 + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean54_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "54", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

# [Max=59]----

#___JSEI=0________----

#1: 0 x 59 x M x Gam x IncUnemp----
m <- lm(jsei_mean ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#2: 0 x 59 x F x Gam x IncUnemp----
m <- lm(jsei_mean ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 0 x 59 x M x Lpm x IncUnemp----
m <- lm(jsei_mean ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |> 
  
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 0 x 59 x F x Lpm x IncUnemp----
m <- lm(jsei_mean ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 0 x 59 x M x Logit x IncUnemp----
m <- lm(jsei_mean ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 0 x 59 x F x Logit x IncUnemp----
m <- lm(jsei_mean ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 0 x 59 x M x Probit x IncUnemp----
m <- lm(jsei_mean ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 0 x 59 x F x Probit x IncUnemp----
m <- lm(jsei_mean ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 0 x 59 x M x Gam x ExclUnemp----
m <- lm(jsei_mean ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 0 x 59 x F x Gam x ExclUnemp----
m <- lm(jsei_mean ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 0 x 59 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 0 x 59 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 0 x 59 x M x Logit x ExclUnemp----
m <- lm(jsei_mean ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 0 x 59 x F x Logit x ExclUnemp----
m <- lm(jsei_mean ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 0 x 59 x M x Probit x ExclUnemp----
m <- lm(jsei_mean ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 0 x 59 x F x Probit x ExclUnemp----
m <- lm(jsei_mean ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "0",
                maxage = "59", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#___JSEI=10_______----

#1: 10 x 59 x M x Gam x IncUnemp----
m <- lm(jsei_mean_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean_10) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#2: 10 x 59 x F x Gam x IncUnemp----
m <- lm(jsei_mean_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean_10) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 10 x 59 x M x Lpm x IncUnemp----
m <- lm(jsei_mean_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean_10 + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 10 x 59 x F x Lpm x IncUnemp----
m <- lm(jsei_mean_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean_10 + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 10 x 59 x M x Logit x IncUnemp----
m <- lm(jsei_mean_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_10 + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 10 x 59 x F x Logit x IncUnemp----
m <- lm(jsei_mean_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_10 + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 10 x 59 x M x Probit x IncUnemp----
m <- lm(jsei_mean_10 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_10 + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 10 x 59 x F x Probit x IncUnemp----
m <- lm(jsei_mean_10 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_10 + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 10 x 59 x M x Gam x ExclUnemp----
m <- lm(jsei_mean_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean_10) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 10 x 59 x F x Gam x ExclUnemp----
m <- lm(jsei_mean_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean_10) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 10 x 59 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean_10 + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 10 x 59 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean_10 + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_10") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 10 x 59 x M x Logit x ExclUnemp----
m <- lm(jsei_mean_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_10 + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 10 x 59 x F x Logit x ExclUnemp----
m <- lm(jsei_mean_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_10 + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 10 x 59 x M x Probit x ExclUnemp----
m <- lm(jsei_mean_10 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_10 + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_10")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 10 x 59 x F x Probit x ExclUnemp----
m <- lm(jsei_mean_10 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_10 + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_10")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "10",
                maxage = "59", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#___JSEI=20_______----

#1: 20 x 59 x M x Gam x IncUnemp----
m <- lm(jsei_mean_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean_20) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#2: 20 x 59 x F x Gam x IncUnemp----
m <- lm(jsei_mean_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean_20) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 20 x 59 x M x Lpm x IncUnemp----
m <- lm(jsei_mean_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean_20 + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 20 x 59 x F x Lpm x IncUnemp----
m <- lm(jsei_mean_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean_20 + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 20 x 59 x M x Logit x IncUnemp----
m <- lm(jsei_mean_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_20 + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 20 x 59 x F x Logit x IncUnemp----
m <- lm(jsei_mean_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_20 + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 20 x 59 x M x Probit x IncUnemp----
m <- lm(jsei_mean_20 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_20 + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 20 x 59 x F x Probit x IncUnemp----
m <- lm(jsei_mean_20 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_20 + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 20 x 59 x M x Gam x ExclUnemp----
m <- lm(jsei_mean_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean_20) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 20 x 59 x F x Gam x ExclUnemp----
m <- lm(jsei_mean_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean_20) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 20 x 59 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean_20 + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 20 x 59 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean_20 + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_20") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 20 x 59 x M x Logit x ExclUnemp----
m <- lm(jsei_mean_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_20 + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 20 x 59 x F x Logit x ExclUnemp----
m <- lm(jsei_mean_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_20 + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 20 x 59 x M x Probit x ExclUnemp----
m <- lm(jsei_mean_20 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_20 + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_20")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 20 x 59 x F x Probit x ExclUnemp----
m <- lm(jsei_mean_20 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_20 + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_20")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "20",
                maxage = "59", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)


#___JSEI=30_______----

#1: 30 x 59 x M x Gam x IncUnemp----
m <- lm(jsei_mean_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean_30) + s(unemprate, age, ps, k = knot), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#2: 30 x 59 x F x Gam x IncUnemp----
m <- lm(jsei_mean_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean_30) + s(unemprate, age, ps, k = knot), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#3: 30 x 59 x M x Lpm x IncUnemp----
m <- lm(jsei_mean_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean_30 + unemprate + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#4: 30 x 59 x F x Lpm x IncUnemp----
m <- lm(jsei_mean_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean_30 + unemprate + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#5: 30 x 59 x M x Logit x IncUnemp----
m <- lm(jsei_mean_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_30 + unemprate + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#6: 30 x 59 x F x Logit x IncUnemp----
m <- lm(jsei_mean_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_30 + unemprate + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#7: 30 x 59 x M x Probit x IncUnemp----
m <- lm(jsei_mean_30 ~ unemprate + age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_30 + unemprate + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#8: 30 x 59 x F x Probit x IncUnemp----
m <- lm(jsei_mean_30 ~ unemprate + age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_30 + unemprate + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業含む") |> 
  dplyr::bind_rows(RobCheck)

#9: 30 x 59 x M x Gam x ExclUnemp----
m <- lm(jsei_mean_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- gam(curwork ~ s(jsei_mean_30) + s(age, ps), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "男性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#10: 30 x 59 x F x Gam x ExclUnemp----
m <- lm(jsei_mean_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- gam(curwork ~ s(jsei_mean_30) + s(age, ps), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "女性", 
                func = "GAM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#11: 30 x 59 x M x Lpm x ExclUnemp----
m <- lm(jsei_mean_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- estimatr::lm_robust(curwork ~ jsei_mean_30 + age + ps, data = mv1)

RobCheck <- mv1 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "男性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#12: 30 x 59 x F x Lpm x ExclUnemp----
m <- lm(jsei_mean_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- estimatr::lm_robust(curwork ~ jsei_mean_30 + age + ps, data = mv2)

RobCheck <- mv2 |> broom::tidy() |> tidyr::as_tibble() |> 
  dplyr::filter(term == "jsei_mean_30") |> 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) |>    
  
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "女性", 
                func = "LPM", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#13: 30 x 59 x M x Logit x ExclUnemp----
m <- lm(jsei_mean_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_30 + age + ps, family = binomial(link = "logit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "男性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#14: 30 x 59 x F x Logit x ExclUnemp----
m <- lm(jsei_mean_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_30 + age + ps, family = binomial(link = "logit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "女性", 
                func = "Logit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#15: 30 x 59 x M x Probit x ExclUnemp----
m <- lm(jsei_mean_30 ~ age + univ + father_univ, data = old_male_mv)
mv1 <- old_male_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv1 <- glm(curwork ~ jsei_mean_30 + age + ps, family = binomial(link = "probit"), data = mv1)
mv1 <- marginaleffects::marginaleffects(mv1, variables = "jsei_mean_30")

RobCheck <- mv1 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "男性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

#16: 30 x 59 x F x Probit x ExclUnemp----
m <- lm(jsei_mean_30 ~ age + univ + father_univ, data = old_female_mv)
mv2 <- old_female_mv |> 
  dplyr::mutate(ps = predict(m, type = "response"))
mv2 <- glm(curwork ~ jsei_mean_30 + age + ps, family = binomial(link = "probit"), data = mv2)
mv2 <- marginaleffects::marginaleffects(mv2, variables = "jsei_mean_30")

RobCheck <- mv2 |> summary() |> 
  dplyr::mutate(unemp_score = "30",
                maxage = "59", 
                gender = "女性", 
                func = "Probit", 
                unemp = "無業除く") |> 
  dplyr::bind_rows(RobCheck)

