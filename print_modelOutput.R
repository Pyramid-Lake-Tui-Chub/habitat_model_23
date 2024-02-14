#### INSTALL PACKAGES ----
library(rempsyc)
library(gt)
library(furrr)
library(broom.mixed)
library(flextable)
library(report)

### This really is way easier with sjplot/tab_model
### will keep here for historical value

#### PRINT TABLES ----
setwd("C:\\RCreations\\ROutput")

# conditional effects
conditional <- broom::tidy(m2, component = "cond", effects = "fixed", exponentiate = TRUE) %>% gt() %>% opt_stylize(style = 1, color = "gray") %>% fmt_number(decimals = 2)
gtsave(conditional, "conditional_exp23_2.docx")

# hurdle
hurdle <- broom::tidy(m2, component = "zi", effects = "fixed", conf.int = TRUE) %>% gt() %>% opt_stylize(style = 1, color = "gray")%>% fmt_number(decimals = 2)
gtsave(hurdle, "hurdle_exp23_2.docx")

# likelihood ratio output
lrtest <- gt(lr) %>% fmt_number(decimals = 2)
gtsave(lrtest, "lrtest_exp23.docx")

