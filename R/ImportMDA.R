#' Import file from MDA
#'
#' Imports a file from a file stored in \href{https://mda.vliz.be/}{Marine Data Archive} (MDA).
#' Previous registration to the MDA is needed, as the funtion uses MDA credential to access data that
#' is either publically available or that belongs to the MDA user.
#'
#' @param username Character. Username (MDA account) to use for the connection.
#' @param password Character. Password (MDA account) to use for the connection.
#' @param file_id Character. MDA id of the file to download. See 'Details'.
#' @param sep Character. If known. Single character use to separate fields within a record.
#' Options are: `","`, `";"`, `"."`, `" "`, `""`, `"\t"`.
#'
#' @details
#' You will find the MDA file ID by checking the file 'Metadata'. in 'Direc link', evereything starting from
#' `VLIZ_` is the file id.
#' For example for \href{https://mda.vliz.be/print.php?file=VLIZ_00000308_6422b9034a3fe367642157}{this file},
#' the file id is: `VLIZ_00000308_6422b9034a3fe367642157`.
#'
#' @return A data object.
#' @export
#'
#' @examples
#' \dontshow{
#' username <- Sys.getenv("mda_userid_gmail")
#' password <- Sys.getenv("mda_password_gmail")
#' }
#' \dontrun{
#' username <- "myemail@email.com"
#' password <- "mysafepasswd"
#' }
#'
#' # file id (See 'Details'.)
#' MDA_file_id <- "VLIZ_00000308_6422b9034a3fe367642157"
#'
#' # get file
#' traits <- importMDAFile(username = username, password = password, file_id = MDA_file_id)
importMDAFile <- function(username, password, file_id, sep = ";"){
  # check arguments
  checkmate::assert_character(username)
  checkmate::assert_character(password)
  checkmate::assert_character(file_id)
  checkmate::assert_choice(sep, c(",", ";", ".", " ", "", "\t"))

  # prepare login/request info
  download_url <- "https://mda.vliz.be/download.php"
  loginurl <- "https://mda.vliz.be/mda_login.php"
  # credentials
  login_data <- list('mda_username' = username,
                    'mda_password' = password,
                    'submit' = "login")
  # define the request
  aw <- httr::POST(loginurl, body = login_data, encode = "form")

  # perform the request (get response)
  url <- paste0(download_url, "?file=", file_id)
  print("Getting file from MDA...")
  r <- httr::GET(url)

  if (is.null(r$headers$`content-disposition`)) {
    stop("You do not have access to this file. Please select a file from your folder or from a public folder")
  } else {
    namec = strsplit(r$headers$`content-disposition`, "=")
  }

  # get file information
  file_name <- stringr::str_split(gsub('"', '', namec[[1]][[2]]), "[.]")[[1]][1]
  file_ext <- stringr::str_split(gsub('"','', namec[[1]][[2]]), "[.]")[[1]][2]

  # if (file_ext == "7z") {
  #   f <- tempfile()
  #   writeBin(httr::content(r), f)
  #   y <- archive(f)$path
  #   print(paste0(file_name, ".", file_ext, " archive found"))
  #   c <- y[str_detect(y, "csv")]
  #   if (length(c) == 0) {
  #     stop("No csv files found in 7z archive")
  #     }
  #   print(paste0(c[1], " of ", y, " read from the archive"))
  #   res_csv <- readr::read.csv2(archive_read(f, c[1]), sep = sep)
  # }

  # if (file_ext == "zip") {
  #   f <- tempfile()
  #   writeBin(content(r), f)
  #   y <- archive(f)$path
  #   print(paste0(file_name, ".", file_ext, " archive found"))
  #   c <- y[str_detect(y, "csv")]
  #   if(length(c) == 0) stop("No csv files found in 7z archive")
  #   print(paste0(c[1], " of ", y, " read from the archive"))
  #   res_csv <- readr::read.csv2(archive_read(f, c[1]), sep = sep)
  # }

  if(file_ext == "csv"){
    f <- tempfile()
    writeLines(httr::content(r, 'text'), f)
    print(paste0(file_name, ".", file_ext, " read from the archive"))
    res_csv <- utils::read.csv2(f, sep = sep)
  }

  if(file_ext == "xlsx"){
    f <- tempfile()
    writeBin(httr::content(r), f)
    print(paste0(file_name, ".", file_ext, " read from the archive"))
    res_csv <- readxl::read_xlsx(f)
  }

  if(file_ext == "xls"){
    f <- tempfile()
    writeBin(httr::content(r), f)
    print(paste0(file_name, ".", file_ext, " read from the archive"))
    res_csv <- readxl::read_xls(f)
  }

  if(!file_ext %in% c("csv", "xlsx", "xls")){
    stop(message(paste0("The file extension '", file_ext, "' is not recognized")))
  }
  return(res_csv)
}
