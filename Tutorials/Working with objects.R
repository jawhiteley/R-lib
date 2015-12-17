##############################
# Working with objects in R
# Jonathan Whiteley
# 2010-09-25	R v2.11
##############################

R.version	# R is case-sensitive!
version		# same as above (shortcut for S compatibility)
R.Version()	# same information, but less pretty
pi			# the number pi
print(pi, digits=16)	# the number pi, to 16 digits (limits of "double" data type?)
letters		# lower-case letters
LETTERS		# upper-case letters: R is case=sensitive!
data(CO2)	# CO2 data (built-in)

##############################
## Exploring objects
# numbers
str(pi)			# NULL
attributes(pi)	# NULL
typeof(pi)		# "double"
class(pi)		# "numeric"
summary(pi)		# not very useful
is.list(pi)		# FALSE
is.vector(pi)	# TRUE : this is a numeric vector (with only one value)!
is.vector(1)	# TRUE

numbers <- c(1, 2, 3)	# c = combine values into a VECTOR
str(numbers)			# numbers
attributes(numbers)		# NULL
typeof(numbers)			# "double"
class(numbers)			# "numeric"
summary(numbers)		# summary statistics
class( summary(numbers) )	# "table"	
	# values returned by other functions have these properties, too!
is.list(numbers)		# FALSE
is.vector(numbers)		# TRUE
class(numbers) <- "list"	# These functions can be used to change object attributes, too
is.list(numbers)		# TRUE
is.vector(numbers)		# TRUE	; lists are also vectors
numbers_list <- list(1, 2, 3)	# create a list
is.list(numbers_list)	# TRUE
is.vector(numbers_list)	# TRUE
numbers_matrix <- as.matrix(numbers_list)	# convert (coerce) to a matrix
is.list(numbers_matrix)		# TRUE
is.vector(numbers_matrix)	# FALSE	***** no longer a vector
is.matrix(numbers_matrix)	# TRUE
dim(numbers_matrix)			# dimensions of the matrix: 3 rows, 1 column

# character strings
str(letters)		# structure
attributes(letters)	# NULL
typeof(letters)		# "character"
class(letters)		# "character"
summary(letters)	# not very useful
is.list(letters)	# FALSE
is.vector(letters)	# TRUE : this is a character vector!
typeof(		 "1")	# "character"
is.numeric(	  1	) 	# TRUE	; this is a number
is.numeric(	 "1")	# FALSE	; this is not a number, but rather text (character)
is.character("1")	# TRUE	; see?

# Data Frames
str(CO2)		# structure: column data types & values
attributes(CO2)	# attributes: these can be referred to directly
typeof(CO2)		# "list"
class(CO2)		# a character vector of multiple classes, ending in "data.frame"
class( 		class(CO2) )	# character
is.vector( 	class(CO2) )	# TRUE
summary(CO2)		# some summary statistics
is.list(CO2)		# TRUE
is.vector(CO2)		# FALSE
is.data.frame(CO2)	# TRUE

# lists
str(version)		# structure
attributes(version)	# attributes: these can be referred to directly
typeof(version)		# "list"
class(version)		# "simple.list" ; see "class" from attributes() output
names(version)		# see "names" from attributes() output
summary(version)	# not particularly relevant

##############################
## Comparing objects
R.Version() == version		# ERROR
## Use special functions to compare entire objects directly (of any type or class)
all.equal(R.Version(), version)	# reports differences
identical(R.Version(), version)	# FALSE

# If R.Version() and R.version give the same imformation, why do they look different on output?
v <- R.Version()	# save output of function for reference
R.version			# R is case-sensitive!
version				# same as above (shortcut for S compatibility)
v					# same information, but less pretty
typeof(v)			# "list"
typeof(version)		# "list"
class(v)			# "list"
class(version)		# "simple.list" *****
typeof(v) == typeof(version)	# TRUE	: they are both of the "list" type
class(v) == class(version)		# FALSE	: they have slightly different classes
class(v) <- class(version)		# change the classes to match
class(v) == class(version)		# TRUE
v								# looks just like version now
all.equal( v , version )		# TRUE
identical( v , version )		# TRUE

# Why does a "simple.list" look different from a "list" when output to console?
version			# prints the value to the console.
print(version)	# same thing: this is what is actually called when you only send the name to the console.
methods(class="list")	# things you can do with lists: 
	# most functions will do something different, 	# depending on the "class" of the object passed to it 
	# (class-specific methods in object-oriented programming).
	# e.g. the print function.  There is no "print.list" function, 
	# so it is using the default method when printing lists
	# ("print.default").
methods("print")	# All the variants of the print function.  
	# Note there is no "print.list", 
	# BUT there IS a "print.simple.list" method.
methods( class="simple.list" )	# methods for "simple.list".  
	# It has its own print command, 
	# which is why objects of class "simple.list" 
	# come out looking differently than regular lists.

print.default(version)		# now it looks like if it were a "list".
print.simple.list( R.Version() )	# even a "list" can be made to look like if it were a "simple.list".


## Methods for Data Frames
methods( class="data.frame" )	# all the functions specific to data frames.