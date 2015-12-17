\name{plotMeans}
\alias{plotMeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotMeans(response, factor1, factor2, error.bars = c("se", "sd", "conf.int", "none", "custom"), level = 0.95, xlab = deparse(substitute(factor1)), ylab = paste("mean of", deparse(substitute(response))), legend.lab = deparse(substitute(factor2)), main = "Plot of Means", pch = 1:n.levs.2, lty = 1:n.levs.2, col = palette(), bg = NULL, cex = 2, lwd = NULL, ylim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{response}{
%%     ~~Describe \code{response} here~~
}
  \item{factor1}{
%%     ~~Describe \code{factor1} here~~
}
  \item{factor2}{
%%     ~~Describe \code{factor2} here~~
}
  \item{error.bars}{
%%     ~~Describe \code{error.bars} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{legend.lab}{
%%     ~~Describe \code{legend.lab} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{bg}{
%%     ~~Describe \code{bg} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (response, factor1, factor2, 
  error.bars = c("se", "sd", "conf.int", "none", "custom"), 
  level = 0.95, xlab = deparse(substitute(factor1)), 
  ylab = paste("mean of", deparse(substitute(response))), 
  legend.lab = deparse(substitute(factor2)),
  main = "Plot of Means", pch = 1:n.levs.2, lty = 1:n.levs.2,
  col = palette(), bg=NULL, cex=2, lwd=NULL, ylim=NULL, ... ) 
{
    if (!is.numeric(response)) 
        stop(gettextRcmdr("Argument response must be numeric."))
    xlab
    ylab
    legend.lab
    error.bars <- match.arg(error.bars)
    if (missing(factor2)) {
        if (!is.factor(factor1)) 
            stop(gettextRcmdr("Argument factor1 must be a factor."))
        valid <- complete.cases(factor1, response)
        factor1 <- factor1[valid]
        response <- response[valid]
        means <- tapply(response, factor1, mean)
        sds <- tapply(response, factor1, sd)
        ns <- tapply(response, factor1, length)
        if (error.bars == "se") 
            sds <- sds/sqrt(ns)
        if (error.bars == "conf.int") 
            sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) * 
                sds/sqrt(ns)
        if (error.bars == "custom") 
            sds <- level ## custom error bar widths.  Added by JAW.  
            # custom widths passed as an appropriate matrix, or a single value replicated: matrix( level, nrow = dim(sds)[1], ncol = dim(sds)[2] )
        sds[is.na(sds)] <- 0
        yrange <- if (error.bars != "none") 
            c(min(means - sds, na.rm = TRUE), max(means + sds, 
                na.rm = TRUE))
        else range(means, na.rm = TRUE)
        yrange <- if ( is.null(ylim)==FALSE ) ylim else yrange # added by JAW
        levs <- levels(factor1)
        n.levs <- length(levs)
        plot(c(1, n.levs), yrange, type = "n", xlab = xlab, ylab = ylab, 
            axes = FALSE, main = main, ... )	# `...` argument added by JAW
        points(1:n.levs, means, type = "b", pch = pch, cex = cex, bg = bg, lty=lty, lwd=lwd)	# originally: pch=16, cex=2.  Modified by JAW.
        box()
        axis(2)
        axis(1, at = 1:n.levs, labels = levs)
        if (error.bars != "none") 
            arrows(1:n.levs, means - sds, 1:n.levs, means + sds, 
                angle = 90, lty = 2, code = 3, length = 0.125)
    }
    else {
        if (!(is.factor(factor1) | is.factor(factor2))) 
            stop(gettextRcmdr("Arguments factor1 and factor2 must be factors."))
        valid <- complete.cases(factor1, factor2, response)
        factor1 <- factor1[valid]
        factor2 <- factor2[valid]
        response <- response[valid]
        means <- tapply(response, list(factor1, factor2), mean)
        sds <- tapply(response, list(factor1, factor2), sd)
        ns <- tapply(response, list(factor1, factor2), length)
        if (error.bars == "se") 
            sds <- sds/sqrt(ns)
        if (error.bars == "conf.int") 
            sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) * 
                sds/sqrt(ns)
        if (error.bars == "custom") 
            sds <- level ## custom error bar widths.  Added by JAW.
        sds[is.na(sds)] <- 0
        yrange <- if (error.bars != "none") 
            c(min(means - sds, na.rm = TRUE), max(means + sds, 
                na.rm = TRUE))
        else range(means, na.rm = TRUE)
        yrange <- if ( is.null(ylim)==FALSE ) ylim else yrange # added by JAW
        levs.1 <- levels(factor1)
        levs.2 <- levels(factor2)
        n.levs.1 <- length(levs.1)
        n.levs.2 <- length(levs.2)
        if (length(pch) == 1) 
            pch <- rep(pch, n.levs.2)
        if (length(col) == 1) 
            col <- rep(col, n.levs.2)
        if (length(lty) == 1) 
            lty <- rep(lty, n.levs.2)
        ## More graphical parameters added by JAW.
        if (length(bg) == 1) 
            bg <- rep(bg, n.levs.2)
        if (length(lwd) == 1) 
            lwd <- rep(lwd, n.levs.2)
        if (length(cex) == 1) 
            cex <- rep(cex, n.levs.2)
        ## ##
        if (n.levs.2 > length(col)) 
            stop(sprintf(gettextRcmdr("Number of groups for factor2, \%d, exceeds number of distinct colours, \%d."), 
                n.levs.2, length(col)))
        plot(c(1, n.levs.1 * 1.4), yrange, type = "n", xlab = xlab, 
            ylab = ylab, axes = FALSE, main = main, ... )	# `...` argument added by JAW
        box()
        axis(2)
        axis(1, at = 1:n.levs.1, labels = levs.1)
        for (i in 1:n.levs.2) {
            points(1:n.levs.1, means[, i], type = "b", pch = pch[i], 
                cex = cex[i], col = col[i], lty = lty[i], lwd = lwd[i], bg = bg[i] )
            if (error.bars != "none") 
                arrows(1:n.levs.1, means[, i] - sds[, i], 1:n.levs.1, 
                  means[, i] + sds[, i], angle = 90, code = 3, 
                  col = col[i], lty = lty[i], length = 0.125)
        }
        x.posn <- n.levs.1 * 1.1
        y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3, 4)])
        text(x.posn, y.posn, legend.lab, adj = c(0, -0.5))
        legend(x.posn, y.posn, levs.2, pch = pch, col = col, 
            lty = lty, pt.bg=bg, pt.lwd=lwd, lwd=lwd, pt.cex=cex*0.75 )
    }
    invisible(NULL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
