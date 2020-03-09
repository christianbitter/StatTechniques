#'@title Random Processes
#'@name poisson_process
#'@param t0 the targeted time frame
#'@param lambda the expected arrival rate
#'@param upper technical parameter to ensure number arrivals is generated
#'@description
#'@return the number of arrivals in time frame t0
#'@author christian bitter
poisson_process <- function(t0, lambda, upper = 100) {
  if (is.null(t0)) stop("poisson_process - t0 missing");
  if (is.null(lambda)) stop("poisson_process - lambda missing");

  NoEvents <- rpois(n = 1, lambda * upper);
  UnorderedArrivalTimes <- runif(n = NoEvents, min = 0, max = upper);
  Sn <- sort(UnorderedArrivalTimes);
  
  # the smallest when we did not exceed - arrivals in 0 - t1
  n <- min(which(Sn > t0));
  
  return(n - 1);
}

#'@title Random Processes
#'@name geometric_renewal
#'@param t0 the targeted time frame
#'@param p first success probability
#'@param M number of simulated values
#'@description
#'@return the number of arrivals in time frame t0
#'@author christian bitter
geometric_renewal <- function(t0 = 5, M = 1e2, p = .2) {
  # cat("Simulating with t0=", t0, "\r\n");
  # t0 <- 5; # time frame
  Tn <- rgeom(M, prob = p); # interarrival times
  Sn <- cumsum(Tn); #arrivals
  n  <- min(which(Sn > t0)); # which are in t0
  
  return(n - 1);
}