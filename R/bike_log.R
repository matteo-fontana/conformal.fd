#' Log of all bike rentals in Milan in 2016 form January to March.
#'
#' A dataset containing the log of all the bike trips in Milan (using the BikeMi
#' service), in the period from 25th of January to the 6th of March from Duomo to Duomo.
#'
#' @format A list of 41 observed days, each containing a list of 2 components:
#' one which indicates the number of bike trips starting from Duomo at hour t and
#' the other about the number of trips ending in Duomo at time t. Therefore each
#' component is made up by 90 time steps, ranging from 7.00 A.M. to 1.00 A.M.
#' Therefore each
#' component is made up by 90 time steps, ranging from 7.00 A.M. to 1.00 A.M.
#' \describe{
#'   \item{start}{number of departing trips from Duomo}
#'   \item{end}{number of ending trips in Duomo}
#' }
#' @source \url{https://www.mate.polimi.it/biblioteca/add/qmox/19-2019.pdf}

"bike_log"
