nums <- seq(1:100)
a <- data.frame("letters"=sample(letters, 100, replace=TRUE), "numbers"=sample(nums))
plot(a)
str(a)	# everything random
a$factor <- factor( a$letters, levels=sample(letters) )	# this makes a factor of letters in random order?
# 'levels=c(...)' to specify the same levels in the desired order.
str(a)	# factor column, data in same order, factor in desired (random) order.
plot( a$factor, a$numbers, type="p" )
a$factor2 <- factor( a$factor, levels=sort(levels(a$factor)) )	# NEW factor: the levels are the same as the old factor, but sorted (alphabetically).  Note that R treats factor levels as text.
levels(a$factor2) <- seq(1:26)	# REPLACE letters with numbers (numbers are in the same order as the letters)
head(a)
str(a)	# factor column, data in same order, factor in desired order.
plot( a$factor2, a$numbers, type="p" )
# create a numeric factor - levels are in expected order of numeric sorting (1, 2, 3...)
a$factor.num <- factor( a$numbers )
plot( a$factor.num, a$numbers, type="p" )
sort(levels(a$factor.num))	# shows what happens when you sort numbers as text (1, 10, 100, 11, ...)


Factor <- factor( rep( c(2, 1, 3, 4), 5) , levels = c(4, 3, 2, 1) )
levels(Factor) <- c( "level 1", "level 2" )	# rename factor levels: ORDER MATTERS (levels reassigned, not reordered)!  Unspecified factor levels are NOT dropped, and values not specified are not changed.
# rename and set order of factor levels in a single command, using a named list:  
levels(Factor) <- list( "level 1"=1, "level 2"=2 , 3, "level 9"=9)	# rename (and set order of) factor levels using a named list('New'='old') of value pairs.  
	# Non-existent levels are ignored; Unspecified levels are replaced with NA
	# Levels with no 'new' value specified are replaced with empty strings.

Factor <- factor( Factor, levels=c( 'level 4', '', 'level 2', 'level 1' ) )	# safely reorder factor levels (doesn't change data values).  
Factor <- factor( Factor, levels=c( 'level 1', 'level 2' ), labels = c('level A', 'level B') )	# safely reorder (existing) factor levels AND re-name labels.  
# See reorder() for fancy sorting of factor levels by summary stats or other values.


# Converting a factor to a "dummy" variable for sorting,
# based on raw numeric codes (i.e. the level number, not the text label):  see ?factor
Fac.numeric <- as.numeric(Factor)  # I want to use the raw Numeric Codes for sorting.
Fac.relvl <- factor(Factor, levels=c( 'level 1', 'level 2' ) )
pos.factor == pos.relvl                  # TRUE
levels(pos.factor) == levels(pos.relvl)  # FALSE
