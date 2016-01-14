################################################################
### Province toolkit
### Goodies
### Tools for working with Canadian Provinces in data and graphs
### Jonathan Whiteley		R v2.15.3		2015-01-14
################################################################
## http://www.nrcan.gc.ca/earth-sciences/geography/place-names/useful-material-translators/9237

## Full names of provinces, with sorting options
prov_names <- function(lang="en", country.first=TRUE, sort.alpha=FALSE, label=c(CA="Canada"), ...)
{
  ## Default order: West-to-East, based on location of *capitals*, territories last.
  provinces <- 
    switch(lang,
           en=c( # country
                BC="British Columbia",
                AB="Alberta",
                SK="Saskatchewan",
                MB="Manitoba",
                ON="Ontario",
                QC="Quebec",
                NB="New Brunswick",
                PE="Prince Edward Island",
                NS="Nova Scotia",
                NL="Newfoundland and Labrador",
                YT="Yukon",  # "Yukon Territories" prior to 2003-04-01
                NT="Northwest Territories",
                NU="Nunavut"
                ),
           fr=c(
                BC="Colombie-Britannique",
                AB="Alberta",
                SK="Saskatchewan",
                MB="Manitoba",
                ON="Ontario",
                QC="Québec",
                NB="Nouveau-Brunswick",
                PE="Île-du-Prince-Édouard",
                NS="Nouvelle-Écosse",
                NL="Terre-Neuve-et-Labrador",
                YT="Yukon",
                NT="Territoires du Nord-Ouest",
                NU="Nunavut"
                )
           )
  if (sort.alpha) provinces <- sort(provinces, ...)
  if (!is.na(country.first)) {
    if (country.first) {
      provinces <- c(label, provinces)
    } else {
      provinces <- c(provinces, label)
    }
  }
  return(provinces)
}

## Generating, or converting province codes
##  This is fairly aggressive, so it should only be used on data that is expected to be some form of province labels.
prov_codes <- function(provs=NA, lang="en", fuzzy=TRUE, ignore.case=TRUE, ...)
{
  if (all( is.na(provs) )) { ## standard codes, in (more-or-less) alphabetical order.
    prov.codes <- c("CA", "AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT")
  } else { ## regex to match appropriate codes.
    prov.codes <- provs
    prov.codes <- gsub("Le |The |Province| of | de ", "", prov.codes, ignore.case=ignore.case, ...)
    prov.codes <- gsub("^\\s*|\\s*$", "", prov.codes) # trim leading and trailing spaces
    if (fuzzy) { ## try fuzzy matching to full names first?
      prov.matches <- lapply(prov.codes, agrep, prov_names(lang=lang), ignore.case=ignore.case, value=TRUE, ...)
      prov.matches <- sapply(prov.matches, function (s) if (length(s)==1) s else NA)
      if (!all(is.na(prov.matches)))
        prov.codes[!is.na(prov.matches)] <- prov.matches[!is.na(prov.matches)]
    }
    ## regex to catch anything left
    ##  these catch the most common variations I've seen, including things that agrep() won't match.
    prov.codes <- sub(".*Canad.*|.*TOTAL.*"                               , "CA" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub("^Al(b|ta).*"                                       , "AB" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub(".*Brit.*|^B[.-]*C.*|^C[.-]*B.*"                    , "BC" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub("^Man.*"                                            , "MB" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub("^N(ew|ouv.*).?Bruns.*|^N[.-]*B.*"                  , "NB" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub(".*N(ewf.*|euv.*)|^N[.-]*L.*|^T[.-]*N[.-]*L.*"      , "NL" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub("^Nov.*|Nouv.*coss.*|^N[.-]*S.*|^N[.-]*[EÉ].*"      , "NS" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub("^North.*|^N[.-]*W[.-]*T.*|^T[.-]*N[.-]*O.*"        , "NT" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub("^Ont.*"                                            , "ON" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub(".*Prince.*|^P[.-]*E[.-]*I.*|[IÎ][.-]*P[.-]*[EÉ].*" , "PE" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub("^Q(u|c).*"                                         , "QC" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub("^Sask.*"                                           , "SK" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub("^Y(uk|n).*|^Y[.-]*T.*"                             , "YT" , prov.codes , ignore.case=ignore.case , ...)
    prov.codes <- sub("^Nun.*"                                            , "NU" , prov.codes , ignore.case=ignore.case , ...)
  }
  return(prov.codes)
}

prov_factor <- function(provs=NULL, ...)
{
  fac <- as.factor(provs)
  fac <- factor(fac, levels=levels(fac),
                   labels=prov_codes(levels(fac), ...) ) # convert to standard codes
  ## ensure consistent order, of whatever is present
  fac <- factor(fac, levels=c(intersect(prov_codes(), levels(fac)), 
                                   setdiff(levels(fac), prov_codes()) ) )
  fac
}

##______________________________________________________________
## Plot map/legend for provinces
prov_make_legend <- function(Canada.ref=FALSE, replace.rows=NULL)
{
  prov.legend <- 
    data.frame(
               prov.code = prov_codes(),
               Province  = c(
                             "Canada",
                             "Alberta",
                             "British Columbia",
                             "Manitoba",
                             "New Brunswick",
                             "Newfoundland & Labrador",
                             "Nova Scotia",
                             "Northwest Territories",
                             "Nunavut",
                             "Ontario",
                             "Prince Edward Island",
                             "Quebec",
                             "Saskatchewan",
                             "Yukon Territory"
                             ),
               colour = c(
                          "#660000", 
                          "#006600", 
                          "#000066", 
                          "#99CC33", 
                          "#993399", 
                          "#6666CC", 
                          "#CC6633", 
                          "#9999FF", 
                          "#CCCCCC",
                          "#990000", 
                          "#FF9900", 
                          "#3333CC", 
                          "#33FF33", 
                          "#FF66FF"
                          ),
               lty  = c(
                        "solid", 
                        "solid", 
                        "11", 
                        "31", 
                        "31", 
                        "solid", 
                        "11", 
                        "solid", 
                        "11",
                        "solid", 
                        "11", 
                        "solid", 
                        "31",
                        "31"
                        ),
               ## Population in 2013 (CANSIM 051-0001)
               popn = c(35.0, 4.0, 4.6, 1.3, 0.8, 0.5, 0.94, 0.04, 0.04, 13.5, 0.15, 8.1, 1.1, 0.04), 
               stringsAsFactors = FALSE
               )
  row.names(prov.legend) <- prov.legend$prov.code
  if (Canada.ref) 
  {                         # use Canada as a reference value
    ca.pop <- prov.legend["CA", "popn"] # save for later
    prov.legend["CA", "popn"]   <- 0.5  # temporary: to calculate size
    prov.legend["CA", "colour"] <- "#333333"
    prov.legend["CA", "lty"]    <- "21"
  }
  prov.legend <- 
    within(prov.legend,
           {
             if (!is.null(replace.rows))
             {                         # custom replacement rows, identified by row name (2-letter code)
               ## Not developed yet.
             }
             size = sapply(popn, function (x) 
                           {    # calculate a size range proportional to population
                             ## x = max(20, x)
                             ## x = min( 1, x)
                             x = log10(x +1) +0.5
                             x = round(x, 1)
                           })
           })
  if (Canada.ref) prov.legend["CA", "popn"] <- ca.pop
  prov.legend
}

prov.legend <- prov_make_legend() # also (re-)calculates size column
