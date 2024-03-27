# tidyverse
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

# renv
if (!require("renv")) {
  install.packages("renv")
  library(renv)
}

# SPSS, Stata, SASのデータを読み込む
if (!require("haven")) {
  install.packages("haven")
  library(haven)
}
if (!require("foreign")) {
  install.packages("foreign")
  library(foreign)
}


# 可視化
if (!require("ggpubr")) {
  install.packages("ggpubr")
  library(ggpubr)
}

# 可視化
if (!require("gghighlight")) {
  install.packages("gghighlight")
  library(gghighlight)
}

# 記述統計量
if (!require("gtsummary")) {
  install.packages("gtsummary")
  library(gtsummary)
}

# 記述統計量
if (!require("flextable")) {
  install.packages("flextable")
  library(flextable)
}

# DAG
if (!require("dagitty")) {
  install.packages("dagitty")
  library("dagitty")
}

# DAG
if (!require("ggdag")) {
  install.packages("ggdag", 
                   dependencies = TRUE)
  library("ggdag")
}

# Gam
if (!require("mgcv")) {
  install.packages("mgcv")
  library("mgcv")
}

# AME
if (!require("marginaleffects")) {
  install.packages('https://cran.r-project.org/src/contrib/Archive/marginaleffects/marginaleffects_0.8.1.tar.gz', repos=NULL, type='source')
  library("marginaleffects")
}

