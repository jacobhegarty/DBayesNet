#' Makes Whitelist and Blacklist for enforcing basic DBN structure
#'
#' This function creates a list containing 2 vectors. The first contains links which should be whitelisted
#' and the second contains links which should be blacklisted when learning dynamic BN structures.
#' Note that although the maximum markov lag is taken as an input, minimum markov lag will always be 1.
#' Uses same node naming convention (NODE_t) as `make_dynamic_data()`.
#
#' @param varNames Names of variables in the network. These should match column names of data frame passed to make_data.
#' @param nNodes Number of nodes in static network (ie number of variables in analysis). It is only recommended to set this parameter if not supplying variable names, in this case nNodes variable names will be generated in the form Ni..
#' @param minMarkovLag The minimum time lag in the DBN structure.
#' @param maxMarkovLag The maximum time lag in the DBN structure.
#' @return list  with whitelist in first element and blacklist in second.
#' @export
make_wb_lists<-  function(varNames = NULL,nNodes = NULL,minMarkovLag =1, maxMarkovLag = 1){

  #check inputs
  if(!is.numeric(maxMarkovLag)|| length(maxMarkovLag)!=1|| maxMarkovLag <1 || maxMarkovLag %% 1 != 0){
    stop("maxMarkovLag must be a positive integer")
  }

  if(!is.numeric(minMarkovLag)|| length(minMarkovLag)!=1|| minMarkovLag <0 || minMarkovLag %% 1 != 0)
    stop("minMarkovLag must be a positive integer or 0")

  if(minMarkovLag> maxMarkovLag)
    stop("minMarkovLag must not be greater than maxMarkovLag")

  #Sort out nNodes/varNames
  if(is.null(nNodes)& is.null(varNames)){
    stop("Must specify nNodes or varNames")
  }

  if(!is.null(varNames)&!is.null(nNodes)){
    if(nNodes != length(varNames)){
      stop("If both specified, nNodes must equal length(varNames)")
    }
  }

  if(is.null(varNames)){
      if(!is.numeric(nNodes)|| length(nNodes)!=1|| nNodes <1 || nNodes %% 1 != 0){
        stop("nNodes must be a positive integer")
      }else{
        varNames <- paste0("N",seq(1,nNodes))
      }
  }
  if(is.null(nNodes)){
    if (anyDuplicated(varNames) > 0){
      stop("Variable names must be unique")
    } else{
    nNodes <- length(varNames)
    }
  }

  #whitelist----
  whitelist <- data.frame()#initialize whitelist

  for (n in 1:nNodes){  #for each node
    for (i in max(1,minMarkovLag):maxMarkovLag){ #for each lag other than base (t)
      whitelist <- rbind(whitelist,data.frame( from = sprintf("%s_(t-%d)", varNames[n], i),
                                               to = sprintf("%s_(t)",varNames[n]))) #whitelist that lag to base lag t
    }
  }

  #blacklist----
  lags = c(0,minMarkovLag:maxMarkovLag)
  #start with all possible connections between lags, filter for those backwards in time, bellow minimum markov lag or to node not in base lag
  blacklistLags <- expand.grid(from = lags, to = lags)|>
    dplyr::filter((from-to)>maxMarkovLag|(from-to)<minMarkovLag|to!=0)

  #dynamic noide labelling function
  make_label <- function(v, L) {
    ifelse( L == 0, sprintf("%s_(t)", v), sprintf("%s_(t-%d)", v, L))
  }

  #make blacklist based on banned connections between lags with every combination of variable names
  blacklistList <- lapply(seq(nNodes), function(node1) {

    blacklistListInner <- lapply(seq(nNodes), function(node2) {
      data.frame(
          from = make_label(varNames[node1], blacklistLags$from),
          to   = make_label(varNames[node2], blacklistLags$to)
        )
     })
    do.call(rbind,blacklistListInner)
  })

  blacklist <- do.call(rbind,blacklistList)

  return(list(whitelist,blacklist))
}
