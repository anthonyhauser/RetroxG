library("dplyr")
library("tidyr")
library("lubridate")
library("stringr")
library("ggplot2")
library("data.table") #rbindlist
library("flextable")
library("stringi")
library("cmdstanr")

library("worldfootballR")
library(StatsBombR) #devtools::install_github("statsbomb/StatsBombR")
#devtools::install_github("FCrSTATS/SBpitch")


# source functions ----
fili <- dir(paste0(code_root_path,"R/"), pattern = "^[0-9]")
print(fili)
lapply(X = fili, FUN = function(x) {source(paste0(code_root_path,"R/", x), encoding="UTF-8",echo=FALSE)} )


#functions
logit <- qlogis
inv_logit <- plogis
