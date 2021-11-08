#' Loads a CSV file
#'
#' @description
#' The function loads NHTSA data about fatal injuries from a CSV file specified 
#' by the  \code{filename} argument and returns a tibble. If the argument does 
#' not point to a usable file, an error is thrown.
#'
#' @param filename Path to FARS CSV file (character)
#'
#' @return The function returns a tibble (data.frame) based on the CSV file.
#'
#' @examples
#' \dontrun{
#' accident_2014 <- fars_read("./data/accident_2014.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make a new filename
#'
#' @description
#' The function creates a filename for a file in .csv.bz2 format based on the 
#' \code{year} argument in the format "accident_<year>.csv.bz2". It ends in an 
#' error unless provided a numerical or integer input.
#'
#' @param year Numerical or integer input indicating the year to be observed.
#'
#' @return Returns a file name string of format "accident_<year>.csv.bz2".
#'
#' @examples
#' \dontrun{
#' makefilename(2015)
#' }
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read in month and year from files
#'
#' @description
#' The function accepts a vector or list of years and returns a list of data
#' frames with MONTH and year columns based on data in "accident_<year>.csv.bz2
#' files. The files need to be located in the working directory.
#'
#' @param years A vector or list of years in numeric or integer format.
#'
#' @return Returns a list of tibbles (data frames) with the same number of rows
#' as the data in "accident_<year>.csv.bz2" files and two columns - MONTH and
#' year. Returns NULL and a warning if the file does not exist.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2012:2015)
#' fars_read_years(list(2012, 2013))
#'
#' # Results in a warning
#' fars_read_years(2016)
#' }
#'
#' @importFrom dplyr %>% mutate select
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Reports number of accidents per month and year
#'
#' Based on the list of years, the function calculates the number of accidents
#' in the US for the given months. The accident files need to be in the working
#' directory; the years can be passed as a list or a vector.
#'
#' @param years A vector or list of years (numeric or integer) used to identify
#' correct parts of data files.
#'
#' @return Returns a pivot tibble (data frame) with months in rows and selected
#' years in columns containing the number of accidents. Returns a warning for
#' every input year not contained in the datasets. The function returns an error 
#'if a different than numeric or integer input is presented.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2014:2016)
#' }
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots accidents on a US map
#'
#' The function accepts a state number and year and plots the accidents in a
#' simple map. The state number must be integer or numerical and must exist
#' in the FARS data, otherwise the function terminates with an error. Also
#' returns an error if the data file for the year input does not exist.
#'
#' @param state.num The number code corresponding to a US state.
#' 
#' @param year The year corresponding to required data (numeric or integer)
#'
#' @return Returns a plot of the accidents based on the \code{state.num} and
#' \code{year} inputs. Returns an error if the state or year do not exist in the
#' data.
#'
#' @examples
#' \dontrun{
#' fars_map_state(45, 2015)
#'
#' # Results in an error
#' fars_map_state(42, 2015)
#' fars_map_state(22, 2016)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}