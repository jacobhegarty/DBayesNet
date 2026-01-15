######Internal funct to create dbn object
dbn <- function( bn,rolledArcs,maxMarkovLag,minMarkovLag ){
  dynamic <- list()

  #store basic dbn info
  dynamic$rolledArcs <- rolledArcs
  dynamic$learning$maxMarkovLag <- maxMarkovLag
  dynamic$learning$minMarkovLag <- minMarkovLag

  #calculate number of ltime slices
  nLags <- if (minMarkovLag>0) (2+maxMarkovLag-minMarkovLag) else (1+maxMarkovLag-minMarkovLag)

  #variables in learning process
  dynamic$learning$nVars <-length(bn$nodes) / nLags

  #loop throguh nodes, calculate parents and children of that node, and add to nodes list
  nodes <- list()

  #i want nodes to be in order of df cols from data input - this will be same order at they appear in whitelist
  nodesOrdered <- c(unique( sub("_\\(t(?:-\\d+)?\\)$", "",bn$learning$whitelist[,1])))
  for (node in nodesOrdered){
    parents <- rolledArcs |> dplyr::filter(to == node) |>
      dplyr::pull(from)|> unique()
    children <- rolledArcs |> dplyr::filter(from == node) |>
      dplyr::pull(to) |> unique()

    add <- list(parents = parents,children = children)
    nodes[[node]] <-add
  }
  dynamic$nodes <- nodes

  #make dbn object
  DBN <- list(dbn = dynamic,bn = bn)
  class(DBN) <- c('dbn','bn')

  return(DBN)

}

#' Prints Information on rolled structure, followed by standard unlearn information on unrolled structure
#' @details This is the S3 `print()` method for objects of class `dbn`. Prints basic information on the dbn.
#' Note to access bnlearns print output for information on the unrolled model use `print(x$bn)`.
#' @param x Object of dbn class
#' @export
#' @method print dbn
print.dbn <- function(x){

  rolledModel <- "\t"
  nCharLine=0
  #make string representing reduce model
  for (node in unique(x$dbn$rolledArcs$to)){
    if (nCharLine>80){
      rolledModel <- paste(rolledModel,"\n\t")
      nCharLine=0
    }
    focus <- dplyr::filter(x$dbn$rolledArcs,to == node) |>
      dplyr::select(to,from)|>
      dplyr::distinct()


    if (nrow(focus)==0){
      new <- paste0("[",node,"]")
    }else{
      new <-paste0("[",node,"|",paste(focus$from,collapse = ':'),"]")

    }
    nCharLine <- nCharLine+ nchar(new)
    rolledModel <- paste0(rolledModel,new)

  }

  cat("\n Dynamic Bayesian Network (dbn) \n \n")
  cat("    Max Markov lag:", x$dbn$learning$maxMarkovLag, "\n")
  cat("    Min Markov lag:", x$dbn$learning$maxMarkovLag, "\n")
  cat("    Number of variables:",x$dbn$learning$nVars,"\n")
  cat("    Number of arcs in rolled model:",nrow(unique(as.matrix(x$dbn$rolledArcs[,c("to","from")]))),"\n")


  cat("    Rolled model: \n")
  cat(rolledModel)
  cat("\n")
  invisible(x)

}


#'Plots dbn structure.
#'
#' @param x dbn project to be plotted. Will usually be output of `dbn_learn()`.
#' @param roll Plot rolled form of dbn?
#' @param whitelist If plotting unrolled dbn, should whitelist links be shown. If true these links will be shown in grey.
#' @param ylim Y coordinate limits for plotting window. Can be used to resize plotting pane. In form `c(lowerLimit,upperLimit)`.
#' @param xlim X coordinate limits for plotting window. Can be used to resize plotting pane. In form `c(lowerLimit,upperLimit)`.
#' @param radius When plotting the rolled network, the radius of circle around which nodes will be arranged.
#' @param arrow Offset from centre of node to arrow-head. May have to be fine tuned depending on number of nodes in network.
#' @param cex Controls node size.
#' @export
#' @method plot dbn
plot.dbn <- function( x,roll = T,whitelist = T, ylim = c(0, 1200), xlim = ylim, radius = 300, arrow = 80,cex=8){

  if (!roll){##IF UNROLLED

    #make list of lags and convert to string for labels
    lags <- x$dbn$learning$minMarkovLag:x$dbn$learning$maxMarkovLag

    if (x$dbn$learning$minMarkovLag!= 0){lags <- rev(c('t',paste0('t-',lags)))
    }else{lags <- rev(c('t',paste0('t-',lags[2:length(lags)])))}

    #count number of time slices
    nLags = length(lags)


    #calculate coodinates of nodes
    xGap = diff(xlim) / (nLags + 1)
    xCoords = xGap * (seq(nLags) -0.5)
    xCoords[length(xCoords)] <- xCoords[length(xCoords)] + (xGap*0.5)#last timepoint (t) offset
    yGap = diff(ylim) / (x$dbn$learning$nVars + 1)
    yCoords = yGap *seq(x$dbn$learning$nVars)

    #initalise plot
    par(mar = c(0, 6, 0, 0),oma=c(0,0,0,0),mgp=c(0,0,0)) # large left margin to allow for node name labels

    plot(x = 0, y = 0, xlim = xlim, ylim = ylim, xlab = "",ylab = "", axes = F, type = "n")

    axis(side = 2, at = yCoords, labels = names(x$dbn$nodes), tck = -0.015, lwd = 0, lwd.ticks = 0, cex.axis = 0.9, las =1)

    #plot whitelisted links if required

    if (whitelist) {#IF WHITELIST
      for (n in seq_len(x$dbn$learning$nVars)) {#loop variables
        for (i in seq_along(1:(length(xCoords)-1))){#loops lags (ie x coords)

          #get coords
          fromX <- xCoords[i]
          toX   <- xCoords[length(xCoords)] - arrow
          y     <- yCoords[n]

          #make curve
          ctrlX <- mean(c(fromX, toX))
          ctrlY <- y + 0.4 * diff(ylim) / x$dbn$learning$nVars
          sp <- xspline(
            c(fromX, ctrlX, toX),
            c(y,     ctrlY, y),
            shape = 1,
            draw = FALSE
          )
          #draw curve
          lines(sp$x, sp$y, col = "grey")

          #add arrow
          arrows(
            sp$x[length(sp$x) -10],
            sp$y[length(sp$y)-10],
            sp$x[length(sp$x)],
            sp$y[length(sp$y)],
            angle = 15,
            length = 0.2,
            col = "grey"
          )
        }
      }
    }#IF WHITELIST
    rolledArcs <- x$dbn$rolledArcs |> dplyr::filter(to!=from)
    #separate in lag arcs as these will be curved
    crossLagArcs <-  rolledArcs[rolledArcs$lag != 0,]
    inLagArcs <-  rolledArcs[rolledArcs$lag == 0,]

    #plot cross lag arcs
    if (nrow(crossLagArcs)>0){
      for (a in seq(nrow(crossLagArcs))){#LOOP CROSS LAG ARCS
        arc<- crossLagArcs[a,]
        arrows(xCoords[x$dbn$learning$maxMarkovLag +1 - arc$lag], yCoords[names(x$dbn$nodes) == arc$from],
               xCoords[length(xCoords)]-arrow,yCoords[names(x$dbn$nodes) == arc$to],
               angle = 15, length = 0.2,
               col = 'black')
      }
    }#IF

    #plot in lag arcs

    if (nrow(inLagArcs)>0){
      for (a in seq(nrow(inLagArcs))){
        arc<- inLagArcs[a,]
        #offset for arrow should be negative if from node is below to node, else negative
        offset <- if (match(arc$from, names(x$dbn$nodes)) < match(arc$to, names(x$dbn$nodes))) (arrow*-1) else arrow

        #get coords
        X <- xCoords[length(xCoords)]
        fromY     <- yCoords[match(arc$from, names(x$dbn$nodes))]
        toY <- yCoords[match(arc$to, names(x$dbn$nodes))] + offset

        #make curve
        ctrlY <- mean(c(fromY, toY))
        ctrlX <- X + 0.3 * diff(xlim) / (nLags)
        sp <- xspline(
          c(X, ctrlX, X),
          c(fromY,     ctrlY, toY),
          shape = 1,
          draw = FALSE
        )

        #plot curve
        lines(sp$x, sp$y)

        #add arrow
        arrows(
          sp$x[length(sp$x) -5],
          sp$y[length(sp$y)-5],
          sp$x[length(sp$x)],
          sp$y[length(sp$y)],
          angle = 15,
          length = 0.2
        )

      }
    }#IF

    #add nodes and labels
    for (i in 1:nLags){
      for (n in 1:x$dbn$learning$nVars){
        points(xCoords[i],yCoords[n], pch = 21, cex = cex, bg = "white")
      }
      text(xCoords[i],yCoords[length(yCoords)]+(0.5*yGap),lags[i])
    }

    }else{ #IF ROLLED
    #initialise plot
    plot(x = 0, y = 0, xlim = xlim, ylim = ylim, xlab = "",ylab = "", axes = F, type = "n")

    #calculate coords in circle - adapted from bnlearn
    unit = 2 * pi / length(x$dbn$nodes)
    xc = mean(xlim)
    yc = mean(ylim)
    coords = matrix(c(xc + radius * cos(1:length(x$dbn$nodes) * unit + pi/2),
                      yc + radius * sin(1:length(x$dbn$nodes) * unit + pi/2)),
                    dimnames = list(names(x$dbn$nodes), c("x" , "y")),
                    ncol = 2, byrow = FALSE)

    arcs <- as.matrix(x$dbn$rolledArcs[c(1,2)])
    #for each arc, calculate y coordinate of arrow  and draw

    if (nrow(arcs) > 0) apply (arcs, 1, function(a) {
      y = (coords[a[2],] - coords[a[1],]) *
        (1 - arrow / sqrt(sum((coords[a[2],] - coords[a[1],])^2))) +
        coords[a[1],] # calculate y coord
      #draw arrow
      arrows(signif(coords[a[1], "x"]), signif(coords[a[1], "y"]),
             signif(y[1]), signif(y[2]), angle = 15, length = 0.2,
             col = 'black')
        })

      #draw node
      points(coords, pch = 21, cex = cex, bg = "white")

      for (i in 1:length(x$dbn$nodes)) { # add label to node

          text(coords[i, 1], coords[i, 2], names(x$dbn$nodes)[i], col = "black")
      }


    }#CLOSE IF ELSE
  return(invisible(x))
}
