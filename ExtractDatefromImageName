#' @title Extract Date from the File name of a Trail Camera Photograph
#' @description A simple function for extracting the date information from the file name of a trail camera photograph,
#'   using the file path string and a specified 'date.code' pattern. This function was optimized for Reconyx *.jpg photos,
#'   but is generalized to work on any file path.
#'
#' @param filename A character string of the image file path
#' @param data.code A character string of the date information pattern in the file path (see examples)
#'
#' @note
#' This function is an enhancement on 'phenopix::extractDate' which is not currently compatible with Reconyx file strings.
#' Potentially useful any analyses dealing with remote cameras for either wildlife or vegetation studies.
#'
#' @author Robert Ritson <robert.ritson@idfg.idaho.gov>
#'
#' @references
#' Gianluca Filippa, Edoardo Cremonese, Mirco Migliavacca, Marta Galvagno, Matthias Folker, Andrew D. Richardson and Enrico Tomelleri
#'   (2020). phenopix: Process Digital Images of a Vegetation Cover. R package version 2.4.2. https://CRAN.R-project.org/package=phenopix
#'
#' @examples
#' ###NOT RUN:
#' ##Examples 'date.code' patterns
#' #Example 1 (Reconyx style)
#' file_path <- 'C:/Photos/Reconyx/STUDYAREA003_20190801_202115_MD_1.jpg' #Example date info is '20190801_202115' (Aug 1, 2019 8:21:15 PM)
#' file_date <- extractDate(filename = file_path, date.code = 'yyyymmdd_HHMMSS")
#' 
#' #Example 2
#' file_path <- 'C:/Photos/Other/STUDYAREA004_2019_08_01_20_21.jpg' #Example date info is '2019_08_01_20_21' (Aug 1, 2019 8:21 PM)
#' file_date <- extractDate(filename = file_path, date.code = 'yyyy_mm_dd_HH_MM")
#' ###END NOT RUN
#'
#' @export extractDate
extractDate <- function(filename, date.code){
  code <- stringr::str_replace_all(date.code,"[[:alpha:]]","\\\\d")
  file <- basename(filename)
  filename.cleaned <- stringr::str_extract_all(file, pattern = code)[[1]]
  separated <- stringr::str_split(filename.cleaned, '')[[1]]
  year.char <- ifelse(length(gregexpr('y', date.code)[[1]]) == 2, '%y', '%Y')
  year.pos <- unlist(gregexpr('y', date.code)[[1]])
  year.val <- paste(separated[year.pos], collapse='')
  month.pos <- unlist(gregexpr('m', date.code)[[1]])
  month.val <- paste(separated[month.pos], collapse='')
  day.pos <- unlist(gregexpr('d', date.code)[[1]])
  day.val <- paste(separated[day.pos], collapse='')
  hour.pos <- unlist(gregexpr('H', date.code)[[1]])
  hour.val <- ifelse(hour.pos[1] > 0, paste(separated[hour.pos], collapse=''), '12')
  min.pos <- unlist(gregexpr('M', date.code)[[1]])
  min.val <- ifelse(min.pos[1] > 0, paste(separated[min.pos], collapse=''), '00')
  sec.char <- ifelse(length(gregexpr('S', date.code)[[1]]) == 2, ':%S', NA)
  sec.pos <- unlist(gregexpr('S', date.code)[[1]])
  sec.val <- ifelse(sec.pos[1] > 0, paste(separated[sec.pos], collapse=''), '00')
  final.date <- paste(year.val, month.val, day.val, sep='-')
  final.time <- paste(hour.val, min.val, sec.val,sep=':')
  final.datetime <- paste(final.date, final.time)
  final.format <- ifelse(!is.na(sec.char),paste0(year.char,'-%m-%d %H:%M',sec.char),paste0(year.char,'-%m-%d %H:%M'))
  date <- as.POSIXct(strptime(final.datetime, format=final.format))
  if (is.na(date)) stop(paste('Date extraction from',filename,'failed'))
  return(date)	
}
