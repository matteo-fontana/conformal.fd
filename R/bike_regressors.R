#' Regressors to model the log of all bike rentals in Milan in 2016.
#'
#' A dataset containing temperature and humidity data to model the bike flows from
#' Milano's Duomo district to itself.
#'
#' @format A list of 41 observed days, each containing a list of 4 components:
#' a flag indicating whether the day is part of the weekend or not, the amount of
#' rain at a given time t of the day (in mm), the difference between the mean temperature
#' in the last few days and the actual temperature at time t and an interaction
#' term between weekend and rain.
#' \describe{
#'   \item{weekend}{flag for weekend}
#'   \item{rain}{amount of rain (in mm)}
#'   \item{dtemp}{different in temperature w.r.t. the last days}
#'   \item{weekend_rain}{interaction term among rain and weekend}
#' }
#' @source \url{https://www.mate.polimi.it/biblioteca/add/qmox/19-2019.pdf}

"bike_regressors"
