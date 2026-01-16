#'Get adjacency matrix of rolled dbn structure
#'@param x dbn object. Usually output of `dbn_learn()`.
#'@returns Adjacency matrix showing connectivity of dbn object with from nodes in columns and to nodes in rows.
#' Note that by definition self-connections are present.
#'@export
as.adjacency.dbn <- function(x) {
  #check input
  if (!inherits(x, "dbn")) {
    stop("x must be an object of class 'dbn'.", call. = FALSE)
  }

  #initalise adj matrix
  adjMat <- matrix(0,nrow = x$dbn$learning$nVars,ncol =  x$dbn$learning$nVars)
  colnames(adjMat) <- names(x$dbn$nodes)
  rownames(adjMat) <- names(x$dbn$nodes)

  #fill adjMat using rolled arcs
  for (i in seq(nrow(x$dbn$rolledArcs))){
    adjMat[x$dbn$rolledArcs$to[i],x$dbn$rolledArcs$from[i]] <- 1
  }
  #add one to diagonal - self links always present
  adjMat <- adjMat + diag(x$dbn$learning$nVars)

  return(adjMat) }


#'Calculate arc strengths from dbn object.
#'@param x dbn onbject.  Usually output of `dbn_learn()`.
#'@param data Unlagged data used to learn structure.
#'@returns Dataframe with four columns:
#'   \itemize{
#'      \item from: Parent node
#'       \item to: Child node
#'       \item lag: Markov lag of connection
#'       \item strength: Strength of this connection
#'    }
#'@export
arc.strength.dbn <- function(x,data){
  #check input
  if (!inherits(x, "dbn")) {
    stop("x must be an object of class 'dbn'.", call. = FALSE)
  }
  if (!identical(names(x$dbn$nodes),colnames(data))) {
    stop("colname mismatch between data and network. Have you supplied same unlagged data used to learn network structure?", call. = FALSE)
  }
  #make dynamic data
  dData <- make_dynamic_data(data,x$dbn$learning$minMarkovLag,x$dbn$learning$maxMarkovLag)
  #calculate arc strength using bnlearn
  arcs <- bnlearn::arc.strength(x$bn,dData)

  #extract from lag
  lags <- data.frame(from = extract_lag(arcs$from))
  #extract node name
  nodes <- data.frame(from = sub("_\\(t(?:-\\d+)?\\)$", "",arcs$from),
                      to =  sub("_\\(t(?:-\\d+)?\\)$", "",arcs$to))

  return(data.frame(nodes,lag = lags$from,strength = arcs$strength))
}
