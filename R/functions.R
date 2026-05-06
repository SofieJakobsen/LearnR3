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



#' Extract Id form the file_path_id
#'
#' @param data the coloum that ocntains the ID we would like to extract
#'
#' @returns the data frame with the new id coloum containing the ids with two alnum, where the file_path_ide is removed

get_participant_id <- function(data) {
  data_with_id <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id,
        "(?<=/stress/)[:alnum:]{2}(?=/)" #?<= this means that it should look for this /stress/ to stand before the id [:alnum:]{2} that we arw looking for and from the following ?=/ is to remove or look for the id/ but not include the /.
      ),
      .before = file_path_id
    ) |>
    dplyr::select(-file_path_id)
  return(data_with_id)
}




#' Summraise data by daytime, mean, sd, median. across all numeric values?
#'
#' @param data the data fram eg HR
#'
#' @returns a data frame with ids, collection daytime and each daytime mean, sd and median

summarise_by_datetime <- function(data) {
  summarised_data <- data |>
    # Fill in below with the code we just wrote.
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute"
      )
    ) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric),
        list(mean = mean, sd = sd, median = median)
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}


#' Read file and process it, read all files, get ID and summarize by date.
#'
#' @param filename the files the function should find eg HR.csv.gz
#' @param max_rows The number of rows it should load
#'
#' @returns a colelcted file with IDs groups ect

read_sensor_data <- function(filename, max_rows = 100) {
  data <- read_all(filename, max_rows = max_rows) |>
    get_participant_id() |>
    summarise_by_datetime()
  return(data)
}

read_sensor_data("HR.csv.gz")
