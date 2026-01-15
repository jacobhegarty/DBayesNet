#'Takes a DBN structure learned using bn learn (bn object) and outputs a dbn object
#'
#'
#' @param bn The bn object representing the DBN. Node names must be in the format \{VARIABLENAME\}_t-\{LAG\} for lagged cols and \{VARIABLENAME\}_t for time slice 0.
#' @param minMarkovLag This parameter is included for the edge case where within slice connections were allowed (minMarkovLag =0) but none were learned.
#' If not supplied, it will default to lowest lag a connection came frome, which will be one for this case, for all other cases this will give the correct value.
#' @return dbn object were first element contains information on rolled dbn structure (learning process, connections in rolled network, nodes),
#' and the second element contains the unrolled bn representation.
#' @export
roll_dbn <- function(bn,minMarkovLag=NULL){
  #check input
  if (!inherits(bn, "bn")) {
    stop("x must be an object of class 'bn'.", call. = FALSE)
  }

  #get arcs
  arcs <- as.data.frame(bnlearn::arcs(bn))

  #get arcs lags
  lags <- data.frame(from = extract_lag(arcs$from),
                     to = extract_lag(arcs$to))
  #get nodes
  nodes <- data.frame(from = sub("_\\(t(?:-\\d+)?\\)$", "",arcs$from),
                      to = sub("_\\(t(?:-\\d+)?\\)$", "",arcs$to))
  #check that all connections are to lag 0
  if (sum(lags$to != 0)){
    stop("Blacklistt error, links to nodes at time point other than 0.")
  }

  #make rolled arcs df
  rolledArcs <- data.frame(nodes, lag = lags$from )
  #get min and max markov lag
  maxMarkovLag <- max(lags$from)

  #NOTE if minMarkovLag not supplied, assumed to be lowest lag a connection came from, this is okay for all cases other than when minMarkovLag was 0 but no within slice connections were learned.
  if (is.null(minMarkovLag)){
  minMarkovLag <- min(lags$from)}
  #make dbn object and return
  return(dbn(bn,rolledArcs,maxMarkovLag,minMarkovLag))
}


