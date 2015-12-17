# Some Other Great Style Guides for Reference

* [Hadley Wickam's R Style Guide][R-style-had]
  [R-style-had]: http://adv-r.had.co.nz/Style.html
* [Google's R Style Guide][R-style-had]
  [R-style-google]: https://google.github.io/styleguide/Rguide.xml
* [The conventions I used for my PhD analyses][SECC-conventions]
  [SECC-conventions]: https://github.com/jawhiteley/SECC.R.JAW/blob/master/doc/SECC-Naming-Conventions.txt
  
# NAMING CONVENTIONS

Files:          words separated by `\s` (`_` only if \s not allowed) or `-` ; Sentence case.
Objects (variables):        
                lower case, words separated by `.`
                camelCase (max. 2 words)
Constants:      UPPER_CASE, words separated by `_`
Data objects:   plural / collective,
?               camelCase (max. 2 words), suffixes separated by `.`
?               CamelCase (max. 2 words), suffixes separated by `.`
Data columns:   singular, Sentence case or Title Case, words separated by `.`
Factor levels:  lower case (re-coded for presentation: plots and labels)
functions:      lower case; words separated by `_` or UpperCamelCase
                ONLY suffix/prefix (methods, namespace) separated by `.`
  function arguments: (same as variables)
                lower case; words separated by `.` or camelCase (max. 2 words)


# Naming conventions are not consistent

- alllowercase [X] *(I really dislike this one)*
* period.style          Period.style        PERIOD.STYLE
* underscore_style      Underscore_style    UNDERSCORE_STYLE
* lowerCamelCase        UpperCamelCase      ALLCAPS [X]
* Underscore_Caps ?

I am of two minds about case: on the one hand, Capitals stand out more and can help avoid conflicts in case-sensitive languages; on the other mixed case is harder to read, and can lead to confusion (human memory is less case-sentive than R). 

I find it easier to distinguish objects I created from built-in ones if they are Capitalized, however, so this may take some getting used to.
It is also true that Caps are more often used in literal text, but I find that decent syntax highlighting makes it easy to tell these apart from variable names.

I do like the thought of starting function names with a Capital letter (or using CamelCase): it somehow feels more important, more *active* --- like the function is *Doing* something.
It's also consistent with Google's conventions (although those seem not terribly consistent, either).

Capital letters can be distracting: I ascribe more importance to the capitalized word, rather than paying attention to the meaning of the words.
Keeping case consistent means being able to focus on the meanings of the words, and less need to use the Shift key.
The more I think about it, the less I like Capital letters in code. They should be used sparingly.

lowerCamelCase is apparently [quite popular for variable names](http://stackoverflow.com/questions/1944910/what-is-your-preferred-style-for-naming-variables-in-r), especially in other languages.


Some style guides recommend [separating words with underscores][R-consistent-naming], because it's generally easier to read, and more familiar to those who write in other languages, where periods have meaning.
Frankly, periods are easier to type, and I've gotten really used to using them in object names --- bot **not** in function names.

[R-consistent-naming]: http://www.r-bloggers.com/consistent-naming-conventions-in-r/


I seem to have become habituated to using `.` to separate *terms*, like a file extension, not just words, so it has taken on a different semantic meaning to me (but not R itself). For example, I use it to group related objects together with suffixes following a `.`.
This means that objects with names longer than one "word" often end up using a different convention, like CamelCase, or underscore_separation, reserving the period for suffixes.
This can lead to some mixed conventions, but is it really so bad or ugly?


A leading `.` is often used to mark internal functions and constants. It might be foolish to use this for my variables.
Some package developers also name internal variables things like `..data..`.
R object names cannot start with a `_` (letter or `.`).

I like to use a trailing `.` or `_` as some kind of marker: calculated columns in a processed data frame, for example.

Ultimately, it feels like a trade-off: use a convention that's easier to type, or one that's easier to read? 
Both feel like good reasons to me, although I seem to default to whatever's easier to type.

The most important thing is to be consistent.
There is also an elegance to having as few rules as possible.
Fewer rules, however, can also make it harder to avoid naming conflicts.

http://journal.r-project.org/archive/2012-2/RJournal_2012-2_Baaaath.pdf


# File Names

File names should be meaningful and end in .R.
Separate words in file names with, in order of preference: spaces, hyphens, or underscores.

If files need to be run in sequence, prefix them with numbers:

    0-download.R
    1-parse.R
    2-explore.R

# Object names

>   “There are only two hard things in Computer Science: cache invalidation and naming things.”
>   
>   -- Phil Karlton

... "and off-by-one errors." (Who added this part?)

Variable names should use the same case, in general: all lower case or ALL CAPS. 
Basically, name variables like in regular text: capitalize only where it would makes sense to do so.
The first letter of a variable may be capitalized if:
  * it is necessary to avoid conflict with a built-in object (e.g., "Data" vs "data", or "Plot" vs "plot"), and a better lowercase name cannot be found.
  * it is a Proper Name or an acronym
  * it has to match another object name that has caps in it.

I reserve ALLCAPS for constants, global variables and settings in init scripts for an entire project.

Use periods to separate words in variable names. 
Do not use underscores in variable names. 
I prefer to use periods when adding descriptors as suffixes to related objects.

    Data        # Generic name for main data object in a script (not the data() function).
    my.data
    dat         # temporary data.frame
    df          # temporary data.frame
    Data.raw
    Data.processed
    plot.data   # data for a plot (aka plotting data)
    Data.plot   # a plotting object based on Data.

    #
    my_data.raw

The only time to use an underscore in a variable name is when combined with ALL_CAPS: i.e., project settings and constants.
I think of a constant much like a function: it's storing something I want to refer to, not a value I want to modify.

    DATA_DIR

Underscores make more sense to me when words are in grammatical order, rather than describing some relationship (as with periods). 
For example, where descriptors are place *before* the modified term.
This works well with function names, which should be verbs (see below).

## Columns (names inside objects)

Column names in data frames often start with a Capital letter, with words separated by periods (instead of spaces).
Most data from other sources (e.g., Excel) will use Capital words in column names.
The default for make.names() is to replace spaces and other illegals characters with `.`.
Use singular forms of nouns for data columns: use plural forms for names of data frame objects.

    my.data$Variable
    Data$Variable
    Data$Var
    cars$Model


## Functions

Function names should be lower-case and words separated by an underscore (`_`).
One reason for this is that periods are used to identify methods of a generic function: use it for that in function names. I also reserve periods for variables,so this makes it easier to distinguish functions from other object names.

    is_positive()
    clean_data()

I sometimes use CamelCase for functions, per Google's style guide.

    IsPos()
    CleanData()


> "Generally, variable names should be nouns and function names should be verbs. Strive for names that are concise and meaningful (this is not easy!)."
>  -- Hadley Wickham


# Assignment

Use `<-` for assignment rather than `=` (except in function calls)

# Spacing

Indent code using two spaces per indent. 
Never use tabs.

Put spaces around operators (`=`, `+`, `-`, `<-`, etc.).
Exceptions to this rule: 
* `:`, `::` and `:::` don’t need spaces around them.
* Spaces around `=` are optional when passing parameters to a function call. 

Put a space after a comma, never before.

Put a space before left parentheses, except in a function call.
Do not place spaces inside parentheses or square brackets (unless there’s a comma, in which case see above).
I add an exception to this rule: when nesting multiple functions, it is sometimes helpful to put spaces inside *some* parentheses, usually the outer ones. On a related note: if I am nesting cmore than 3 functions, I should ask myself if there is a simpler way to do it, or whether I should be defining my own function.

Extra spacing (i.e., more than one space in a row) is ok if it improves alignment between lines; things like assignment, or closing parentheses/brackets/braces, for example.

# Curly Braces

Many style guides recommend *never* putting a curly brace on it's own line. This can make a properly-indented code block a little easier to read.

I've actually gotten used to having them on their own line: it takes up an extra line, yes, but I find it easier to find `for` statements and function definitions. 
Many auto-indentation schemes in text editors work better with opening braces on their own line.

Put curly braces on their own line.
Unless it is followed by `else`:
`else` should always be surrounded by curly braces.

    if (y == 0) 
    {
      log(x)
    } else {
      y ^ x
    }

# Line length

Strive to limit your code to 80 characters per line. This fits comfortably on a printed page with a reasonably sized font. If you find yourself running out of room, this is a good indication that you should encapsulate some of the work in a separate function.

# Comments

Comments are a Good Thing(TM). I use full-line separators to create sections:

    ################################################################
    ### HEADING 1 (High-level) [64 #]

    ##==============================================================
    ## Heading 2 (sub-section) [##, 62 =]

    ##______________________________________________________________
    ## Heading 3 (sub-sub-section)

    ##--------------------------------------------------------------
    ## heading 4 (I rarely use this, except maybe within a function)

My comments follow the ESS style for auto-indenting:

    ### No indentation (always align to extreme left)
    ## Auto-indent with code
    # A form of 'right-align': used on the same line after code,
    # or to 'comment-out' code


