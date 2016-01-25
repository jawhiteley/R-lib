################################################################
### Useful plot and graphing functions
### Jonathan Whiteley     R v3.2.2    2016-01-25
################################################################

##==============================================================
## Pairplots
##==============================================================
pairplot <- function(x, panel=points, upper.plots=TRUE, mirror.panels=TRUE,
                     correlations=TRUE, histograms=TRUE, add.smooth=TRUE, ...) 
{
  ## pairplot: wrapper for pairs().
  ## bivariate plots of variable pairs with a few extra useful bits of info
  ## * Mixed Effects Models with Extensions in Ecology with R Figure A.2 p.534 (Appendix)
  ## * Analyzing Ecological Data Figure 4.9 p.33 (CH4)
  ## Requires:
  ## + panel.cor      copied here from books' R code
  ## + panel.hist     copied here from books' R code
  ## + panel.smooth   the books defined a crippled version of the same built-in function
  ##   - see graphics::panel.smooth()
  ## + panel.ablines  There is a version in the lattice package (panel.lines).  
  ##   - I copied the one from the books' code (panel.lines2) and renamed it.
  ## ARGUMENTS ##
  ## x          object with data for pairplots (usually a data frame).  
  ##            Passed to pairs()
  ## panel      panel function, same default as pairs().
  ## upper.plots    TRUE:  correlation values in lower panels, plots in upper panels
  ##                FALSE: vice versa.
  ## mirror.panels  TRUE:  same panel function in upper & lower panels
  ##                FALSE: allow smoothers on one side and default panel on the other
  ##                Only relevant if correlations=FALSE
  ## correlations   TRUE:  Show Correlation values in font size proportional 
  ##                       to value in panels of one side.  
  ##                FALSE: do not show correlation values 
  ##                       (panel function will be used instead).
  ## histograms     TRUE:  Add histograms in diagonal panels.  FALSE: do not ...
  ## add.smooth     TRUE:  Scatterplots with smoother curves in panels.
  ##                FALSE: Use default (or specified) panel function instead.
  panel.plot <- if (add.smooth && mirror.panels) panel.smooth else panel
  panel1     <- if (add.smooth)   panel.smooth else panel.plot
  panel2     <- if (correlations) panel.cor    else panel.plot
  up.panel   <- if (upper.plots)  panel1       else panel2
  low.panel  <- if (upper.plots)  panel2       else panel1
  d.panel    <- if (histograms)   panel.hist   else NULL
  pairs(x, panel=panel,
        upper.panel =  up.panel,
         diag.panel =   d.panel,
        lower.panel = low.panel,
        ...
        )

  if (FALSE) {  ## test code
    pairplot(CO2)
    pairplot(CO2, add.smooth=FALSE)
    pairplot(CO2, hist=FALSE)
    pairplot(CO2, cor=FALSE)
    pairplot(CO2, upper=FALSE)
    pairplot(CO2, panel=panel.smooth, cor=FALSE)
    pairplot(CO2, cor=FALSE, mirror=FALSE)
  }
}

##______________________________________________________________
## PANEL FUNCTIONS

panel.cor <- function(x, y, digits=1, prefix="", cex.cor)
{
  ## "panel.cor" in "MyLibrary.R" from:
  ##   Analysing Ecological Data. (2007). Zuur, Ieno and Smith. Springer, 680p.
  ##   This function was produced by Alain Zuur (highstat@highstat.com)
  ##   www.highstat.com
  ## put correlations on the panels,
  ## with size proportional to the correlations.
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))

  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}

panel.hist <- function(x, ...)
{
  ## "panel.hist" in "MyLibrary.R" from:
  ##   Analysing Ecological Data. (2007). Zuur, Ieno and Smith. Springer, 680p.
  ##   This function was produced by Alain Zuur (highstat@highstat.com)
  ##   www.highstat.com
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

panel.ablines <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                           cex = 1, ...)
{
  ## "panel.lines2" in "MyLibrary.R" from:
  ##   Analysing Ecological Data. (2007). Zuur, Ieno and Smith. Springer, 680p.
  ##   This function was produced by Alain Zuur (highstat@highstat.com)
  ##   www.highstat.com
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)){
    tmp=lm(y[ok]~x[ok])
    abline(tmp)}

}

## Coplots with linear fits (from Zuur et al. 2007 Chapter 22 R code)
## I usually use faceted ggplots instead of Coplots
panel.lines2 <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, ...)
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)){
        tmp=lm(y[ok]~x[ok])
        abline(tmp)}

}


##================================================
## ggplot2 custom options
##================================================
library(grid)       # needed for unit() - why does ggplot not load `grid` automatically?

theme_jaw <- function(x) 
{
  ## add common custom options to ggplot2 objects.
  ## useage: my.ggplot + theme_jaw()
  require(ggplot2)
  if (FALSE) {
    if (class(x)!="ggplot") return(x)
    x <- x + theme_bw()                  # change to white background colour scheme.
    x <- x + opts(legend.key = theme_rect(colour = NA)) # remove rectangle from legend keys.
    return(x)
  } else {
    ## add common custom options to ggplot2 objects as a template.
    ## I got tired of copying the same code over & over again.
    plot.template <- list(theme_bw(base_size = 18),   # change to white background colour scheme & increase base font size.
                         theme(legend.key = element_rect(colour = NA), # remove rectangle from legend keys.
                              ## move axis tick labels closer to ticks
                              axis.ticks.length = unit(0.3, "lines"), 
                              axis.text = element_text(margin=unit(0.3, "lines")),
                              ## move axis labels away from axis text :/
                              axis.title.x = element_text(vjust=0, hjust=0.55),
                              axis.title.y = element_text(vjust=0.3, hjust=0.5, angle = 90)
                              )
    )
    return(plot.template)
  }
}

## custom function for grImport "Pictures" as x axis tick labels
## http://stackoverflow.com/questions/8905101/how-can-i-use-a-graphic-imported-with-grimport-as-axis-tick-labels-in-ggplot2-u
picture_axis <- function (pics, icon.size = unit(1, "lines"), ...) {
  structure(
      function(label, x = 0.5, y = 0.5, ...) {
         absoluteGrob(
           do.call("gList", mapply(symbolsGrob, pics[label], x, y, SIMPLIFY = FALSE)),
           height = icon.size)
    }
)}


##==============================================================
## Saving & Output functions (with defaults)

save_ggplot <- function (plot=NULL, file.name="plot", dir="./", 
                         data.plot=TRUE, format.ext=c(".pdf", ".png"), 
                         width=16, height=12, units="cm", scale=1, ...)
{ ## Save ggplots to specified file types
  for (type in format.ext)
  {
    ggsave(filename=paste(dir, file.name, type, sep=""), 
           plot=plot, width=width, height=height, units=units, scale=scale, ... )
  }
  warning.message <- ""
  if (is.null(data.plot)) data.plot <- FALSE
  if ( !( "logical" %in% class(data.plot) || "data.frame" %in% class(data.plot) ) ) {
    warning.message <- sprintf("Specified plot data is not a data frame (class: '%s'). ", class(data.plot))
    data.plot <- FALSE
  }
  if ( "data.frame" %in% class(data.plot) ) {
    ## Save associated data, manually specified.
    write.csv(data.plot, paste(dir, file.name, ".csv", sep=""), row.names=FALSE)
  } else if (data.plot==TRUE) { 
    ## Save associated data, extracted from plot object.
    write.csv(plot$data, paste(dir, file.name, ".csv", sep=""), row.names=FALSE)
  } else if (data.plot==FALSE) { 
    ## Do not save plot data, with a warning
    warning(sprintf("%sNo plot data saved for %s.", warning.message, file.name))
  }
}

export_plots <- function(plot.data=FALSE, ...)
{   ## 
  plot.objects <- grep("\\.plot$", ls(), value=TRUE)
  for (plot.obj in plot.objects)
  {
    plot.file <- gsub("\\.", "_", sub("\\.plot", "", plot.obj)) 
    if (plot.data)
      save_ggplot(get(plot.obj), file=plot.file, ... )
    else
      save_ggplot(get(plot.obj), data=get( sub("\\.plot", ".pdata", plot.obj) ),
                 file=plot.file, ... )
  }
  cat("`.plot` objects saved!\n")
}

