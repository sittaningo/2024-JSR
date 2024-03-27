m <- lm(jsei_mean ~ unemprate + age + univ + father_univ, data = old_male)
old_male <- old_male |> 
  mutate(ps = predict(m, type = "response"))
m <- lm(jsei_mean ~ unemprate + age + univ + father_univ, data = old_female)
old_female <- old_female |> 
  mutate(ps = predict(m, type = "response"))

