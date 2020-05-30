
install.packages("swirl")
library(swirl)
install_from_swirl("R Programming")
swirl()
Paul

3

library(tidyverse)
library(dplyr)
library(datasets)
data(iris)
?iris
str(iris)
vir <- iris[,c("Species", "Sepal.Length")]
vir
colMeans(vir, 101:length(vir))

str(vir)
class(vir)
