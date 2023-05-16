#' Read data input
#'
#' This function reads different type of data input (xlsx, csv, rds, sav, txt, sas) into a dataframe.
#' It also allows users to choose only the required variables for data processing.
#'
#' @param file the name of the file which the data are to be read from.
#' @param header a logical value indicating whether the file contains the names of the variables as its first line.
#' @param sep the field separator character.
#' @param fileEncoding character string declares the encoding used on a file.
#' @param ... Additional arguments may apply.
#' @return A dataframe of data input
#' @examples
#' data_test <- epinion_read_data(file = epinionSampleCleaning_example("sample_test.xlsx"))
#' data_test_sav <- epinion_read_data(file = epinionSampleCleaning_example("sample_test.sav"), cols = c("gruppe", "runde"))

epinion_read_data <- function(file, header = TRUE, sep = ";", fileEncoding = "UTF-16LE" , cols = everything(), ...){

  if (!file.exists(file)) {
    message("The file does not exist")
  } else {

    extension<-tools::file_ext(file)

    if (extension == "xlsx") {
      df = openxlsx::read.xlsx(xlsxFile = file, colNames = TRUE)
    }

    if (extension == "csv") {
      df = read.csv(file = file, header = header, sep = sep)
    }

    if (extension == "rds") {
      df = readRDS(file = file)
    }

    if (extension == "sav") {
      df = haven::read_sav(file = file)
    }

    if (extension == "sas7bdat") {
      df = haven::read_sas(data_file = file)
    }

    if (extension == "txt") {
      df = read.table(file = file, header = header,
                      quote = "", sep = sep,
                      fileEncoding = fileEncoding,
                      skipNul = TRUE)
    }

    require(dplyr)

    df <- df %>%
      select(all_of(cols))

    return(df)

  }
}


validate_email <- function(email) {
  # Regular expression to match email addresses
  regex <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
  # Check if email matches regular expression
  if (grepl(regex, email)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Validate format of email field in the data
#'
#' This function evaluates format of email field in the data.
#' Select all cases with invalid email and save a log file for further review
#' to the working directory named "Log - invalid_email.txt".
#' Replace detected invalid emails with blank in the data.
#'
#' @param data the name of the dataframe.
#' @param email_var string vector of email.
#' @return A dataframe with proper formatted email
#' @examples
#' data_email_validated <- epinion_email_validation(data_test, "mail")

epinion_email_validation <- function(data, email_var){

  # Validate email
  data$email_validation <- lapply(trimws(data[[email_var]]), validate_email)

  # Select data case with detected invalid email and Save log file
  invalid_email <- data %>%
    filter(email_validation == FALSE) %>%
    select(-email_validation)

  write.table(x = invalid_email,
              file = paste0(getwd(), "/Log - invalid_email.txt"),
              col.names = TRUE,
              row.names = FALSE,
              quote = FALSE,
              sep = ";",
              fileEncoding = "UTF-16LE")

  # Replace detected invalid emails with blank
  data <- data %>%
    mutate(!!as.symbol(email_var) := ifelse(email_validation == TRUE, trimws(!!as.symbol(email_var)), "")) %>%
    select(-email_validation)

  return(data)
}


#' Clean data header before uploading sample to SPSSD
#'
#' This function removes semi-colon, double-quote, tab and line break if detected anywhere in the data.
#' Remove special and non-ASCII character in data header.
#' Replace some Danish characters that may cause the error when uploading the sample to SPSSD with the Latin characters.
#' Save the cleaned data to a .txt file under working directory
#'
#' @param data the name of the dataframe.
#' @param output name of the text file
#' @return A cleaned dataframe
#' @examples
#' data_final <- epinion_data_cleaning(data_email_validated, "sample.txt")

epinion_data_cleaning <- function(data, output) {

  # Remove semi-colon, double-quote, tab and line break if detected anywhere in the data
  for (var in names(data)) {
    data[[var]] <- lapply(data[[var]], function (i) stringr::str_replace_all(i, '[;\"\t\r\n]' , ""))
  }

  # Remove special and non-ASCII character in data header
  names(data) <- stringr::str_remove_all(names(data), "[ !?$^&*()+-/\\\\@#%\\[\\]{}|<>’',.`~]")
  names(data) <- stringr::str_replace_all(names(data), c("Æ" = "Ae",
                                                         "æ" = "ae",
                                                         "Ø" = "O",
                                                         "ø" = "o",
                                                         "Å" = "A",
                                                         "å" = "a"))
  data <- as.matrix(data)

  # Write table
  write.table(x = data,
              file = paste0(getwd(), "/", output),
              col.names = TRUE,
              row.names = FALSE,
              quote = FALSE,
              sep = ";",
              fileEncoding = "UTF-16LE")

  return(data)
}



