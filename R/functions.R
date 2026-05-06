#' Read in one nurses' stress data file.
#'
#' @param file_path Path to the data file.
#' @param max_rows Maximum number of rows to read.
#'
#' @returns Outputs a data frame/tibble.
#'
read <- function(file_path, max_rows = 100) {
  data <- file_path |>
    readr::read_csv(show_col_types = FALSE,
                    name_repair = snakecase::to_snake_case,
                    n_max = max_rows
    )
  return(data)
}




#' Read in all files in the nurse-stress folder
#'
#' @param file the name or type of data that needs to be loaded.
#'
#' @returns Dataframe with a Id, collectioin time and the data point ef hr.

read_all <- function(file, max_rows = 100) {
  # Code that does something
  hr_files <- here::here("data-raw/nurses-stress/") |> #pipe the first argument
    fs::dir_ls(regexp = file,
               recurse = TRUE)

  hr_data <- hr_files |>
    purrr::map(\(file) read(file, max_rows = max_rows)) |>
    purrr::list_rbind(names_to = "file_path_id")

  return(hr_data)
}



# Global variables -----
#.DATASET_DIR <- here::here("data-raw/nurses-stress/") # 1

##' Read in all files in the nurse-stress folder
##'
##' @param file the name or type of data that needs to be loaded.
##'
##' @returns Dataframe with a Id, collectioin time and the data point ef hr.

#read_all <- function(file, max_rows = 100) {
  # Code that does something
#  files <- .DATASET_DIR |> #pipe the first argument
#    fs::dir_ls(regexp = file,
#               recurse = TRUE)
#
#  data <- files |>
#    purrr::map(\(file) read(file, max_rows = max_rows)) |> # er ikke sikker på jeg forstår denne del
#    purrr::list_rbind(names_to = "files_path_id")

 # return(data)
#}

# this function was moved to functions.R




