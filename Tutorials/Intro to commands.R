##############################
# Getting Started with R, and some elementary commands
# Jonathan Whiteley
# 2010-09-26	R v2.11
##############################

# This is a comment.  
# Anything on a line following a pound (number) sign # 
# is a Comment, and will be ignored (not executed) by R.
# This is a useful way to include comments, notes and 
# explanations for commands and code in a script file.
# These are useful to help you remember 
# why you did things a certain way,
# especially when you come back to a project after a while.
# They are also useful to others who read your code
# to explain to them what you are trying to accomplish
# and why it is written a certain way.

## GETTING STARTED: try typing these into the console.
# If an empty window appears, look at the console again: 
# it may be asking you to hit the <Return> key.

demo()			# gives a list of available demos to show off the capabilities of R
demo(graphics)	# some example graphs you can make in R
demo(image)		# visual images (mostly raster maps)
demo(lm.glm)	# linear & generalized linear modelling examples

# Try other demos you might be interested in.
# Remember that the demos not only show you the results, 
# you can also see the code that produces 
# those results in the console.


## HELP!!
help.start()	# a good place to start: 
	# HTML documentation, with many tutorials, examples, and concepts.
?help		# get help with a specific function
?c			# learn about the often-used combine function
help("c")	# same as above

# What if you don't know the name of the function, but you know what you want to do?
?help.search	# the help.search function (or 2 question marks ??)
	# will search the help files for the term you ask for.
??"t-test"
help.search("t-test")

#__________________________________________________
# READING a Help page
## DESCRIPTION
#	Brief description of what the function is and what it does.
#	
## USAGE
#	This shows how a function should be used:
#	what arguments are expected, 
#	and the way commands should be spelled and arranged (syntax)
#	The arguments are anything inside the brackets, 
#	separated by commas.
#	Arguments are the information passed INTO a function 
#	that tells it what to work with.  
#	This includes things like data or objects to be processed,
#	and many options that affect how the function works.
#	Arguments that have an equals sign = after them 
#	have a default value:
#	They do not need to be specified when calling a function,
#	and this also shows you what those default values are.
#	Arguments without an equal sign = are REQUIRED.
#	Arguments can be included in a cal l to a function in 2 ways:
#	- by Order: arguments don't have to be named,
#		but must be listed in the same order as shown here.
#	- by Name: arguments can be in any order, as long as
#		they are correctly named, as shown in this section: 
#		( name=value , name=value , ... )
#
## ARGUMENTS
#	More details about specific arguments
#
## DETAILS
#	Explanation of how a function works, and considerations
#	Sometimes very technical and programming-oriented.
#	At least it is available, should you need it.
#
## VALUE
#	What the function returns as a value,
#	and the characteristics of that value (class, type, etc.)
#	Usually, you will want to assign this value to an object:
#	e.g. a <- c(1,2,3)
#	The above code creates a vector with the numbers 1, 2, 3, (using the 'c' function) and then assigns that resulting value to the object named 'a'.
#	
## AUTHORS
#	Who wrote the function.  
#	For credit, and in case you want to try to 
#	contact them directly with questions or comments.
#	
## SEE ALSO
#	Related topics you might also be interested in.
#	
## EXAMPLES ***
#	These are examples that show the function in action.  
#	They can be copied & pasted to the console to run as-is.  
#	I find them extremely useful for learning how a function 
#	works, especially by editing the example code to make sure 
#	I understand how to get it to do what I want.
#	Examples can also be run from the console using the
#	example() function.  Look it up!
#__________________________________________________

## COMMANDS

# You already entered functions with brackets after them, for the demos & help:
demo()	# no arguments, but you still need the empty brackets 
		# to tell R that you want to run the function
R.Version()	# the version of R.  
			# There is a nicer way to get this information, 
			# which you will see later in this file.
# Note that when you type the name of a function without the brackets, R spits out the code for that function instead:
	# usually, this is pretty meaningless, 
	# but can be useful if you want to see 
	# exactly how a function works, in terms of R code.
	# Useful for functions in external packages,
	# or if you want to see how the pros write code ...
help
c	# the combine function

# You can use R as a fancy calculator:
2+2
2*22
(2^2)*pi

# R can also do matrix algebra, but you must use special operators
c(1,2,3)  *  c(3,2,1)	# multiplication, element-by-element
c(1,2,3) %*% c(3,2,1)	# Matrix multiplication of 2 vectors.

# You can compare values in R, using *comparison operators*:
1 == 2	# FALSE
2 <  pi	# TRUE
?Comparison	# For more information about comparison operators

# If you want to save values for later, assign them to named objects
# Names can contain alphanumeric characters, and '.' and '_',
# but they must start with a non-numeric character or '.' followed by a non-numeric characters
a <- 1				# the preffered assignment operator
b <- c(1,2,3)		# making a vector of multiple values.
1:6 -> dice			# different direction, same effect
num_throws = 100	# this works, but is generally not recommended, because it can lead to confusion.
# You will notice that R is pretty silent after executing these commands.
# Usually, no feedback is a good thing: it means nothing went wrong.

# To see the value of a named object, just type the name in the console:
a
b
dice
num_throws
# Objects can be used in other commands: 
# they are like placeholder variables for the values 
# they currently contain
game <- sample( dice , size=num_throws , replace=T )
	# this command is like
	# throwing a die with 6 sides 
	#	('dice' is a vector of integers from 1 to 6)
	# 100 times
	#	('num_throws' was set to value '100' earlier)
	# unless you have changed the code above ;-)
# Note that the result is now stored in an object called 'game'.
# If we ask for 'game' again, R will not re-roll the dice,
# it will simply tell us the same result every time
# (the result of the first game, stored as a value)
game
game


# R includes a few things that have already been named: 
pi
letters
LETTERS		# R is case-sensitive!
R.version	# 'version' also works
# You can overwrite these, but only locally. 
# If you assign a value to these names, a local copy is created.
# When the local copy is removed, the default value is re-instated
pi <- 1		# oops
pi			# That shouldn't be...
rm(pi)		# the remove function (rm) deletes a named object
pi			# Yay!  back to normal.

# Some names to avoid: these are already names of functions,
# or have special meanings!
c	# the combine function
C	# a function for making contrasts.  
	# Remember, R is case-sensitive!
t	# the transpose function
T	# short for "TRUE" (logical values)
F	# short for "FALSE" (logical values)

# Look carefully at the R console after entering a command 
# (pressing the <Return> key):
# What you see at the end of the console tells you what R is doing:
# The usual command prompt looks like a "greater-than" symbol:
# >
# This means R is ready for a new command.  If it changes to a 'plus sign':
# +
# It means R is still waiting for the end of the last command.  
# For example:
c(1,2,3)	# produces a vector with the three numbers in it,
	# followed by a new > prompt.  The command finished.
# But:
c( 1 , 2	# this line is incomplete: 
			# R is still waiting for the closing brackets.  
			# The prompts is now '+'
	, 3		# More arguments and commands, but still no ')'
)			# R is now satisfied and executes the entire command.

# Notice that this means that R commands and functions 
# can be spread out with spaces, and across multiple line.  
# This is extremely useful when writing scripts, to help 
# make your commands and code easier to read and less overwhelming.
# Complex expressions can be broken apart visually, 
# to make them easier to read and understand for humans:
# it makes no difference to R, however.

# If you get stuck in the console with a + prompt,
# and don't know how to get out of it, you can always press the
# <Esc> (Escape) key to tell R to stop waiting for input.
# R will switch back to the usual > prompt.


## ERROR Messages
## R sometimes produces Error messages that are not very helpful.
## Here are some common ones:
c(foo)
# Error in ...: object '...' not found
	# This usually means the object (name inside the quotes)
	# has not been defined.
	# Usually, this means you forgot to 
	# assign a value to that object,
	# or the text should be in quotations in your code
c("foo"))
# Error: unexpected '...' in "..."
	# R encountered a character 
	# that doesn't belong where it is.
	# The problem is usually at the end of the text
	# in the second pair of double-quotes.
	# This is often unpaired parentheses ())
2a <- c(""foo")"
# Error: unexpected symbol in "..."
	# R encountered a character it does not recognize.
		# The second pair of double quotes shows you where the problem is:
		# if it only contains part of an expression, 
		# the problem is somewhere near the end of this section, 
		# not after it.
	# This could mean a character that is out of place,
	# such as trying to name something starting with a number,
	# or unpaired quotation marks,
	# or something that is not an ASCII character like fancy quotes.
	# ** 
	# You will get a lot of these errors if you write code
	# in a program like Microsoft Word 
	# and try to paste it in R.  :(
	# ALWAYS write your R code in R's script editor,
	# or a TEXT editor,
	# or an Integrated Development Environment (IDE),
	# which includes a proper text editor for R.





# A little housekeeping: 
rm(list-ls())	
	# This removes all objects in R's memory 
	# (like wiping the slate clean), 
	# and removes everything the above scripts has done.  
	# Make sure you are not going to lose anything else 
	# that might be important before doing this!