#' Earth Distance
#'
#' Second order estimation of the distance between two points parametrized by latitude and longitude
#' @param lon1 Longitude of the first point
#' @param lat1 Latitude of the second point
#' @param lon2 Longitude of the first point
#' @param lat2 Latitude of the second point
#' @return Second order approximation of the distance between the two points in kilometers. 
#' @export
earth.dist <- function(lon1, lat1, lon2, lat2)
{
    rad <- pi / 180
    a1 <- lat1 * rad
    a2 <- lon1 * rad
    b1 <- lat2 * rad
    b2 <- lon2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat / 2)) ^ 2 + cos(a1) * cos(b1) * (sin(dlon/2)) ^ 2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- R * c
    return(d)
}
