# Introduction 
This is the source code for rpart.translator. This package will translate rpart models into table formatted rules with value volumes for each node in a tree.

# Installation
Installing local package in R:
setwd("C:/pathtopackage")
install.packages('rpart.translator_1.0.0.tar.gz'), repos=NULL, type='source')

Installing local package in Docker:
RUN R -e "install.packages('rpart.translator_1.0.0.tar.gz'), repos=NULL, type='source')"

# Latest File
The most recent build of the latest file can be found in the top level of this directory and labled rpart.translator_buildversion.tar.gz.

# Example
library(rpart)
library(rpart.translator)

fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)

dataTable <- rpart.translator(fit, kyphosis)