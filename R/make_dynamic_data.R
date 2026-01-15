
#' Takes dataframe containing time-series observations of variables and reformats as data for DBN structure learning,
#'  where each column corresponds to a node in the dynamic network.
#'
#' @param data Data frame containing observations in rows and variables in columns.
#' @param maxMarkovLag Maximum Markov lag to be used in analysis. Minimum value of 1.
#' @param minMarkovLag Minimum Markov lag to be used in analysis. Must be less than maximum Markov lag and ≥ 0.
#' Value of 0 is allowed experimentally and output may include  within time-slice connections.
#' This is not recommended and should only be considered where information lag in system is less than the temporal resolution of data.
#' @return Dataframe containing lagged columns of all variables up to maxMarkovLag.
#'
#' @export
make_dynamic_data <- function(data,minMarkovLag=1,maxMarkovLag=1){
  #check inputs
  if(!is.numeric(maxMarkovLag)|| length(maxMarkovLag)!=1|| maxMarkovLag <1 || maxMarkovLag %% 1 != 0)
    stop("maxMarkovLag must be a positive integer")

  if(!is.numeric(minMarkovLag)|| length(minMarkovLag)!=1|| minMarkovLag <0 || minMarkovLag %% 1 != 0)
    stop("minMarkovLag must be a positive integer or 0")

  if(minMarkovLag> maxMarkovLag)
    stop("minMarkovLag must not be greater than maxMarkovLag")

  if(!is.data.frame(data))
    stop("data must be a dataframe")

  if(ncol(data) == 0)
    stop("data must contain at least one column")

  if(nrow(data) <maxMarkovLag)
    stop("data must contain at least maxMarkovLag rows")
  if(anyDuplicated(names(data)))
    stop("column names of data must be unique")


  lagsList = list() #initialise list

  #make vector containing value of lags
  if (minMarkovLag != 0) {
    l <- c(0, minMarkovLag:maxMarkovLag)
  } else {
    l <- minMarkovLag:maxMarkovLag}


  for (i in seq_along(l)){#loop to max ML, storing lagged cols in list element and naming cols based on ML
    lagI <- l[i]
    lagsList[[i]] <- data |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::lag(.x, n = lagI), .names =if (lagI == 0) "{.col}_(t)" else paste0("{.col}_(", "t-", lagI, ")")))|>
     dplyr::select(-seq(ncol(data)))

  }

  dataLagged <- do.call(cbind,lagsList) #combine lags into single DF

  dataLaggedTrim <- dplyr::slice(dataLagged,-seq(maxMarkovLag)) #remove rows with NAs (corresponding to when ML0 has no observations due to lags)


  return(dataLaggedTrim)
}
