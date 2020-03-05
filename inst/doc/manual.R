## ---- include = FALSE---------------------------------------------------------

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "# >"
)

mtcars <- datasets::mtcars



## ----setup--------------------------------------------------------------------
library(CRUF)

mtcars <- datasets::mtcars

desctable <- tabkris_2(mtcars)

knitr::kable(desctable)

## ----auto_detect--------------------------------------------------------------
# In mtcars, "cyl", "vs", "am", "gear" and "carb" are encoded as numeric but they are factors in reality.
# tabkris_2 changes each variable and display a message for each transformation.
desctable <- tabkris_2(mtcars, auto_detect = T, lev_co = 8)


knitr::kable(desctable)

## ----return_table-------------------------------------------------------------
# desc_prep <- tabkris_2(mtcars, return_table = F, auto_detect = T)
# 
# # Change the method for variable "vs" from a binomial to a categorical method
# desc_prep$method["vs"] <- "cate"
# 
# desctable <- tabkris_2(desc_prep)
# 
# # Variable of interest set to "am", also using the previous changed arguments
# desc_prep$varint <- "am"
# 
# desctable_2 <- tabkris_2(desc_prep)


## ----return_method------------------------------------------------------------
desc_prep <- tabkris_2(mtcars, return_table = F)

# Change the method for all binomial variable to categorical
desc_prep$default_method[2] <- "cate"

desctable <- tabkris_2(desc_prep)

# Changing only the method for "vs" to categorical
desc_prep$method["vs"] <- "cate"

desctable_2 <- tabkris_2(desc_prep)


## ----presentations------------------------------------------------------------

# Changing the names
lab <- c("Miles/US gallon", "Number of cylinders", "Displacement", "Horsepower", "Rear axle ratio", "Weight", "1/4 mile time", "Engine", "Transmission", "N Forward gears", "N carburetors")

desctable <- tabkris_2(mtcars, names = lab,
                       pres_quant = c("mean", "range"),
                       pres_quali = c("n", "total", "per"),
                       explicit_na = T,
                       digits = 1,
                       lang = "fr",
                       auto_detect = T)

knitr::kable(desctable)


