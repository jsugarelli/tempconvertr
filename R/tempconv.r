#'@title Package 'tempconvertr'
#'
#'@description Comfortable conversion of temperatures
#'
#'  This package provides functionality to convert temperatures between degrees
#'  Celsius, degrees Fahrenheit and Kelvin in any direction. Its main function
#'  is \code{\link{convert.temp}()}.
#'
#'@name tempconvertr
NULL



#' @title Converting Temperatures
#' @description Converts temperatures temperatures between degrees Celsius,
#'   degrees Fahrenheit and Kelvin.
#'
#' @param from.temp Temperature value to be converted.
#' @param from.unit Scale of the temperature value to be converted. Either
#'   \code{"Celsius"} (default), \code{"Fahrenheit"} or \code{"Kelvin"}.
#' @param to.unit Target temperature scale. Either \code{"Celsius"} (default),
#'   \code{"Fahrenheit"} or \code{"Kelvin"}.
#' @param as.json Indicates if function returns JSON object describing the
#'   conversion instead of just the converted temperature value.
#'
#' @return Either the converted temperature value or a JSON object (see
#'   details).
#'
#' @details If \code{as.json==TRUE} then \code{convert.temp()}  returns a JSON
#'   object containing two sub-object \code{from} and \code{to} each of which
#'   has a \code{temp} and a \code{unit} field to describe the temperature. The
#'   \code{from.unit} and \code{to.unit} arguments are case-insensitive.
#'
#' @examples
#' result.fahrh.json <- convert.temp(24.5, from.unit="celsius", to.unit="fahrenheit", as.json=TRUE)
#' result.kelv <- convert.temp(24.5)
#'
#' @import jsonlite
#' @export
convert.temp <- function(from.temp, from.unit = "Celsius", to.unit = "Kelvin", as.json = FALSE) {
  if(tolower(from.unit) %in% c("celsius", "kelvin", "fahrenheit") &
     tolower(to.unit) %in% c("celsius", "kelvin", "fahrenheit")) {
    conv.add1 <- data.frame(list(celsius = c(0,0,-32),
                                 kelvin = c(0,0,-32),
                                 fahrenheit = c(0,-273.15,0)))
    conv.mult <- data.frame(list(celsius = c(1,1,5/9),
                                 kelvin = c(1,1,5/9),
                                 fahrenheit = c(9/5,9/5,1)))
    conv.add2 <- data.frame(list(celsius = c(0,-273.15,0), kelvin = c(273.15,0,273.15),
                                 fahrenheit = c(32,32,0)))
    from.index <- which(c("celsius", "kelvin", "fahrenheit")
                        == tolower(from.unit))[1]
    to.index <- which(c("celsius", "kelvin", "fahrenheit")
                      == tolower(to.unit))[1]
    to.temp <- (from.temp + conv.add1[from.index, to.index]) *
     conv.mult[from.index, to.index] +
     conv.add2[from.index, to.index]

    if(as.json) res <- jsonlite::toJSON(list(from=list(temp=from.temp, unit=from.unit)),
                                          to=list(temp=to.temp, unit=to.unit))
    else res <- to.temp
    return(res)
  }
  else {
    stop("Arguments from.unit and to.unit must be \"Celsius\",
		\"Kelvin\" or \"Fahrenheit\".")
  }
}
