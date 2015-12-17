##################################################
# Miscellaneous useful R functions
# Jonathan A. Whiteley	2012-04-10
# R v2.12
##################################################
## INITIALISE
##################################################

clmem <- rmall <- function() {  rm(list=ls()) }


##################################################
## DATA
##################################################
peek <- function( data="" ) {
	if ( class(data)== "data.frame" )
		invisible( edit(data) )
	else
		data.entry(data=data)
}



##################################################
## MULTIVARIATE ORDINATION ANALYSES
##################################################

## SIMPER
# Similarity Percentages: contribution of individual variables to pariwise dissimilarity between all pairs of sites between to groups defined a priori.
## see: Clarke, K.R.  1993.  Non-parametric multivarite analysis of changes in community structure.  Australian journal of ecology, 18, 117-143.

simper <- function( data, groups, method="bray") {
	require(vegan)	# uses the vegan package for ecologically-relevant distance metrics.
	# data is the data matrix: n samples x p variables / species
	# groups is a vector with length = # of samples, specifying a priori groups for each observation / sample
	# method is the distance method used.
	if (length(groups)!=dim(data)[1] || is.null(dim(groups)) != TRUE)
		stop("invalid dimensions of groups: must be a vector with length equal to the number of rows in data")
	if (method != "bray" )
      stop("simper() currently only works with bray-curtis dissimilarity (method= \"bray\")")

	# initialize some variables
	groups.levels <- unique(groups)
	groups.pairs  <- t( combn(groups.levels, 2)	) # all combinations of n elements, taken 2 at a time (all pairs); transposed to 2 columns, 1 with each grouping level in the pair.
    vars <- colnames(data)
    Result <- list(within = list(), between = list())

    ## WITHIN GROUP SIMILARITIES
    for (group in groups.levels)
    {
      gdata <- data[which(groups == group), ]
      group.dist <- mean( vegdist(gdata, method = method), na.rm = TRUE)
      g.pairs <- t( combn( 1:nrow(gdata), 2 ) )
      g.pairs <- as.data.frame(g.pairs)
      colnames(g.pairs) <- c("j", "k")
      bray.j <- gdata[g.pairs$j, ]
      bray.k <- gdata[g.pairs$k, ]
      bray.jk <- g.pairs
      bray.jk$Yjk <- apply( bray.j + bray.k, 1, sum )
      bray.jk  <- cbind(bray.jk[, 1:3], abs(bray.j - bray.k) / bray.jk$Yjk )
      bray.sim <- pmin(bray.j, bray.k)
      colnames(bray.sim) <- colnames(bray.jk[, -(1:3)]) 
      bray.sim <- 2 * bray.sim / bray.jk$Yjk 
      bray.sim <- cbind(bray.jk[, 1:3], bray.sim)
      group.sim <- mean( 1 - apply(bray.jk[, -(1:3)], 1, sum) )
      group.sim <- mean(    apply(bray.sim[, -(1:3)], 1, sum) )    # should be the same

      Result.group <- data.frame(Variable = "", Mean.Value = 0, 
                                 Avg.Sim = 0, SD.Sim = 0, Avg_SD = 0,
                                 Perc.Sim = 0.0, Cum.Perc = 0.0
      )
      Result.group <- Result.group[0, ] # structure only
      for (vari in vars)
      {
        gvdata <- gdata[vari]       # stays as a data.frame
        ##         var.sim <- mean( vegdist(gvdata, method = method), na.rm = TRUE)
        var.sim <- mean( bray.sim[[vari]] )
        SD.sim  <-   sd( bray.sim[[vari]] )
        Result.group <- rbind(Result.group,
                              data.frame(Variable = vari, Mean.Value = mean(gvdata[1]), 
                                         Avg.Sim = var.sim, SD.sim = SD.sim,
                                         Avg_SD = var.sim / SD.sim,
                                         Perc.Sim = var.sim / group.sim, 
                                         Cum.Perc = NA
                                         )
        )
      }
      attr(Result.group, "Average Bray-Curtis Similarity") <- group.sim
      colnames(Result.pair)[2] <- paste("Mean", group, sep = "_")
      ## sort result
      Result.group <- Result.group[ order(Result.group$Avg.Sim, decreasing = TRUE), ]
      Result.group$Cum.Perc  <- cumsum(Result.group$Perc.Sim)
      Result$within[[group]] <- Result.group
    }
	
    ## BETWEEN GROUP DISTANCES
    for (g in 1:nrow(groups.pairs))
    {
      pair <- groups.pairs[g, ]
      pair.lab <- paste(pair, collapse = ":")
      gdata <- data[which(groups %in% pair), ]
      group.dist <- mean( vegdist(gdata, method = method), na.rm = TRUE)
      ## the hardest part is probably getting the list of sample pairs; the rest is the same as above
      g.pairs <- t( combn( 1:nrow(gdata), 2 ) )
      g.pairs <- as.data.frame(g.pairs)
      colnames(g.pairs) <- c("j", "k")
      g.pairs$group.j <- groups[g.pairs$j]
      g.pairs$group.k <- groups[g.pairs$k]
      g.pairs <- g.pairs[g.pairs$group.j != g.pairs$group.k, 1:2]
      ## Compute Distance between all pairs of samples, between all group pairs, using one variable/species at a time.
      bray.j <- gdata[g.pairs$j, ]
      bray.k <- gdata[g.pairs$k, ]
      bray.jk <- g.pairs
      bray.jk$Yjk <- apply( bray.j + bray.k, 1, sum )
      bray.jk  <- cbind(bray.jk[, 1:3], abs(bray.j - bray.k) / bray.jk$Yjk )
      pair.dist <- mean( apply(bray.jk[, -(1:3)], 1, sum) )

      Result.pair <- data.frame(Variable = "", Mean.Value.1 = 0, Mean.Value.2 = 0,  
                                 Avg.Dist = 0, SD.Dist = 0, Avg_SD = 0,
                                 Perc.Dist = 0.0, Cum.Perc = 0.0
      )
      Result.pair <- Result.pair[0, ] # structure only
      for (vari in vars)
      {
        gvdata <- gdata[vari]       # stays as a data.frame
        ##         var.sim <- mean( vegdist(gvdata, method = method), na.rm = TRUE)
        var.dist <- mean( bray.jk[[vari]] )
        SD.dist  <-   sd( bray.jk[[vari]] )
        Result.pair <- rbind(Result.pair,
                              data.frame(Variable = vari, 
                                         Mean.Value.1 = mean(gdata[which(groups == pair[1]), vari]), 
                                         Mean.Value.2 = mean(gdata[which(groups == pair[2]), vari]), 
                                         Avg.Dist = var.dist, SD.dist = SD.dist,
                                         Avg_SD = var.dist / SD.dist,
                                         Perc.Dist = var.dist / pair.dist, 
                                         Cum.Perc = NA
                                         )
        )
      }
      attr(Result.pair, "Average Bray-Curtis Distance") <- pair.dist
      colnames(Result.pair)[2:3] <- paste("Mean", pair, sep = "_")
      ## sort result
      Result.pair <- Result.pair[ order(Result.pair$Avg.Dist, decreasing = TRUE), ]
      Result.pair$Cum.Perc  <- cumsum(Result.pair$Perc.Dist)
      Result$between[[pair.lab]] <- Result.pair
    }
	
	## Similarity percent = mean distance along variable i / mean distance using all variables = % of mean distance contributed by variable i.
	
	# Result:
	# for each group pair:
	# a table (data frame) with each species / variable as a row with the following columns:
	# - mean value of variable in group 1
	# - mean value of variable in group 2
	# - mean distance using only this variable
	# - SD(of distances between all pairs of samples between groups)
	# - mean distance of variable / SD(of distances between all pairs of samples)
	# - mean distance of variable / mean overall distance (%)
	# - cumulative percent
	#
	# Same columns for each single group: to account for variable contributing to similarity within a particular group.
	
    return(Result)

    if (FALSE)
    {                                  # Testing code
      library(vegan)
      data(mite)
      data(mite.env)
      simper(mite, groups = mite.env$Topo)
    }
}
