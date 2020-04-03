#' shaper_data
#'
#' @description This function allows you to retrive the wanted shapefile.
#'
#' @param country_code The country's ISO code.
#' @param level The shapefile's level of granularity.
#'
#' @import curl
#' @import utils
#' @import rgdal
#'
#' @return Shapefile for the country and level requested.
#' @export
#'
#' @seealso \code{\link{shaper_country}} for the country's ISO code.
#'
#' @examples
#' map <- shaper_data(country_code = "ITA")
#' map <- shaper_data(country_code = "ITA", level=1)
#' map <- shaper_data("ITA", 1)
#' map <- shaper_data("USA", 2)
#'

# Function 1: Data collection

shaper_data <- function(country_code, level=0){

  if (country_code == "WWW") {
    # downloading the shapefile
    url <- paste0("https://sites.socialdatasciencelab.org/data/shapefiles/world.zip")
    download_dir <- file.path(tempdir(), "temp.zip")
    curl::curl_download(url, download_dir)

    # unziping the downloaded shapefile
    unzip_dir <- file.path(paste0(tempdir(), "/"))
    utils::unzip(download_dir, exdir = unzip_dir)
    unlink(download_dir)

    # reading the shapefile
    csv_file <- file.path(paste0(tempdir(), "/world.shp"))
    map <- rgdal::readOGR(csv_file, verbose = FALSE)
  }

  else {
    # downloading the shapefile
    url <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_", country_code , "_shp.zip")
    download_dir <- file.path(tempdir(), "temp.zip")
    curl::curl_download(url, download_dir)

    # unziping the downloaded shapefile
    unzip_dir <- file.path(paste0(tempdir(), "/"))
    utils::unzip(download_dir, exdir = unzip_dir)
    unlink(download_dir)

    # reading the shapefile
    csv_file <- file.path(paste0(tempdir(), "/gadm36_", country_code, "_", level, ".shp"))
    map <- rgdal::readOGR(csv_file, verbose = FALSE)
  }
  unlink(tempdir())
  return(map)
}



# Function 2: Countries' code reconciliation
# If the user does not know the ISO code of a country, s.he has access to the answer in natural language through this query

#' shaper_country
#' @description This function allows you to find and search for the right country's ISO code.
#' If no argument is filed, each ISO code of each country will be displayed.
#'
#' @param country The name of the country.
#'
#' @return Country's ISO code.
#' @export
#' @seealso \code{\link{shaper_data}} to collect shapefiles when you have the country code.
#'
#' @examples
#' mycountry <- shaper_country()
#' mycountry <- shaper_country(country = "Italy")
#' mycountry <- shaper_country("Italy")
#'

shaper_country <- function(country) {
  countryRef <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1sPNet_GHXm14YEAWqM0zSTR9c4bpa0z4CaJ_ajeWKq8/edit#gid=0")
  shaper_countries_natural_language <- stats::na.omit(unique(countryRef[, c(1,10)]))
  if (missing(country)) {
    shaper_countries_natural_language
  } else {
    shaper_countries_natural_language[grep(country, shaper_countries_natural_language$nameEng, ignore.case = TRUE), ]
  }
}
