# Internal values for use in package
# 
# This file contains a number of internal (global) values that
# are used throughout the functions in this package. It allows
# for easy redefinition of eg what is considered a special 
# function, which classes are accepted as valid for a pim-related
# object and so forth.
# 
.valids.pim <- c("character","factor","numeric","integer","logical")

.specials.pim <- c("P","PO","L","R")