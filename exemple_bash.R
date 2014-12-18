#!/usr/bin/env Rscript
#### Passing Command-Line Arguments to R Programs

message ("Imprimim els paràmetres passats al script")
######################
args <- commandArgs( trailingOnly = TRUE)
print (paste("Argument1:", args[1]))
print (paste("Argument2:", args[2]))

#rnorm(n=as.numeric(args[1]), mean=as.numeric(args[2])



#myarg <- commandArgs()
#print(iris[1:myarg, ])
######################


cat("Hello\n")

x <- c(2,45,6,8,3,4,24,2,4,21,5,6,7,8,9)

mean(x)
