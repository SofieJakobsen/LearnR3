## code to prepare `nurses-stress` dataset goes here


library(here)



untar(
  here("data-raw/nurses-stress.tar"),
  exdir = here("data-raw/nurses-stress/")
)





usethis::use_data(nurses-stress, overwrite = TRUE)
