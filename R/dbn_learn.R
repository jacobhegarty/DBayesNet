#'Learn dynamic Bayesian network structure.
#'@param data Data frame with variables in column and observations in rows.
#'@param score Score to be used implemented using \pkg{bnlearn}.
#'@param minMarkovLag Minimum markov lag to be used in analysis. Must be less than maximum Markov lag and ≥ 0. If 0, output is not a true DBN as it may contain connections within time slice.
#' @param maxMarkovLag Maximum Markov lag to be used in analysis. Minimum value of 1.
#'@param search Search type implemented using \pkg{bnlearn}.
#'    Must be one of:
#'    \itemize{
#'      \item \code{"hc"}: hill-climbing search
#'       \item \code{"tabu"}: tabu search
#'    }
#'@param ... Other arguments to be supplied to \code{\link[bnlearn]{hc}} or \code{\link[bnlearn]{tabu}}.
#'@export
dbn_learn <- function(data,score,minMarkovLag = 1, maxMarkovLag =1,search = 'hc' , ...){

  if(!search %in% c('hc','tabu'))
     stop("search must be 'hc' or 'tabu'")
  #check inputs
  if(!is.numeric(maxMarkovLag)|| length(maxMarkovLag)!=1|| maxMarkovLag <1 || maxMarkovLag %% 1 != 0){
    stop("maxMarkovLag must be a positive integer")
  }
  if(!is.numeric(minMarkovLag)|| length(minMarkovLag)!=1|| minMarkovLag <0 || minMarkovLag %% 1 != 0)
    stop("minMarkovLag must be a positive integer or 0")
  if(minMarkovLag> maxMarkovLag)
    stop("minMarkovLag must not be greater than maxMarkovLag")

  #make whitelist and blacklist
  WB <- make_wb_lists(varNames = colnames(data),minMarkovLag = minMarkovLag,maxMarkovLag=maxMarkovLag)
  whitelist <- WB[[1]]
  blacklist <- WB[[2]]

  #make dynaiic data
  dData <- make_dynamic_data(data,minMarkovLag = minMarkovLag,maxMarkovLag = maxMarkovLag)

  #pass all to bn learn for stucture learning
  if(search =='hc'){
    bn <- bnlearn::hc(dData,whitelist = whitelist, blacklist = blacklist,score = score, ...)
  } else if (search == 'tabu'){
    bn <- bnlearn::tabu(dData,whitelist = whitelist, blacklist = blacklist,score = score, ...)
  }

  #produce dbn class and return
  return(roll_dbn(bn,minMarkovLag))

}
