# DBayesNet

## Overview

This package provides helper functions which allow dynamic Bayesian network structure learning in `bnlearn`.

## Installation

``` r
remotes::install_github("jacobhegarty/DBayesNet", build_vignettes = TRUE)
```

## Usage

``` r
library(DBayesNet)
```

For most uses `dbn_learn()` will suffice:

``` r
dbn_learn(df,score = 'aic-g',minMarkovLag=1,maxMarkovLag = 1)
```

However the internals steps of this function have also been provided to allow more flexibility in the structure learning process. In this case it is important to follow the following pipeline to ensure correct column name formatting.

``` r
#make whitelist and blacklist to force dbn structure.
wb <- make_wb_lists(varNames = colnames(df),minMarkovLag = 1,maxMarkovLag = 1)

#coherce data into dynamic format
dData <- make_data(df,minMarkovLag = 1,maxMarkovLag = 1)

#learn structure
net <- bnlearn::hc(dData,whitelist=wb[[1]],blacklist = wb[[2]],score = 'aic-g')

#reduce bn object to dbn
dnet <- reduce_dbn(net)
```

Note that it is important that the maxMarkovLag and minMarkovLag arguments remain constant within this pipeline.

A few helper functions are provided to summarise the dbn structure.

``` r
#print some useful information on the rolled up dbn structure.
print(dnet)

#plots the rolled up version of the dbn.
plot(dnet,roll=T)

#plots the unrolled version of the dbn, with nodes in rows and time-slices in columns.
#When whitelist = T links whitlisted to conform with dbn structure will be shown in grey.
plot(dnet,roll=F,whitelist=F)


#outputs rolled netwrok structure as adjacency matrix, with from nodes in columns and to nodes in rows.
as.adjacency.dbn(dnet)
```

## Documentation

-   Access help pages: `?functionName`
-   See vignette for basic tutorial:
``` r
vignette("DBayesNet", package = "DBayesNet")
```

## Dependencies

-   R (\>= 4.4.2)

-   bnlearn (\>= 5.0.2)

-   dplyr (\>= 1.1.4)
