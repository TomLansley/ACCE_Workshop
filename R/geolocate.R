#' Calculate the location of a stem based on azimuth and distance.
#'
#' @param decimalLongitude numeric vector of decimal longitudes
#' @param decimalLatitude numeric vector of decimal latitudes
#' @param stemAzimuth numeric vector of stem azimuths
#' @param stemDistance numeric vector of stem distances
#'
#' @return A tibble of pairs of coordinates
get_stem_location <- function(decimalLongitude, decimalLatitude, #require four variables
                              stemAzimuth, stemDistance){
  #add a validation check
  checkmate::assert_numeric(decimalLongitude)
  checkmate::assert_numeric(decimalLatitude)
  checkmate::assert_numeric(stemAzimuth)
  checkmate::assert_numeric(stemDistance)
  
  #assign output to 'out' as a tibble
  out <- geosphere::destPoint(
    p = cbind(decimalLongitude, decimalLatitude),
    b = stemAzimuth, d = stemDistance
  )  |> #base pipe so that no packages need to be loaded to use this function
    tibble::as_tibble()
  
  #add a test for NAs in the output
  checkmate::assert_false(any(is.na(out)))
  
  return(out) #produce the output
}