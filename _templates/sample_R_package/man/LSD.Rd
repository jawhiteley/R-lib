\name{LSD}
\alias{LSD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LSD(model, formula, data = NULL, alpha = 0.05, mode = c("pairwise", "MSE", "manual"), n = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{mode}{
%%     ~~Describe \code{mode} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function( model, formula, data=NULL, alpha=0.05, mode=c("pairwise", "MSE", "manual"), n=NULL ){
	## Planned Multiple Comparisons using Least Significant Differences (LSD) -> comparison intervals for graphical display.
	# aovlist: pass in appropriate level of nesting (not just Within for MSE: MSE from the appropriate aov object).
	mode <- match.arg(mode)
	if(mode=="manual"){
		lsd.n <- as.numeric(n)	# relevant group ? : 
	}
	if(mode=="pairwise"){
		lsd.n <- replications( formula, data )	# sample sizes, according to model structure (formula).  I can't find an easy to derive this directly from an aov object passed in, so this is the only reason that formula & data are required as arguments.  replications() returns a nasty list if the data are unbalanced :(
		# if the item name is a problem, use as.numeric() to convert to a pure number.  If no group specified, a vector is produced with values for all treatment combinations.
	}
	if(mode=="MSE") {
		lsd.df <- model$df	# for MSE from fitted model?
	} else {
		lsd.df <- (2*lsd.n)-2	# for group differences. or for MSE from fitted model?
	}
	lsd.mse <- sum(resid(model)^2)/model$df	# MSE from model ( SS / df )
	lsd.se <- sqrt(2*lsd.mse/lsd.n)	# se of a difference
	# unbalanced: sqrt( (var[1]/n[1]) + (var[2]/n[2]) )
	pvalue <- 1 - (alpha/2)	# for a 2-tailed test
	lsd.width <- qt(pvalue, lsd.df)*lsd.se	# LSD based on error rate (alpha).
	return(lsd.width)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
