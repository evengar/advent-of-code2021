library(tidyverse)
bin <- read.table("data/03-input.txt",colClasses = "character")

d <- separate(bin, V1, sep = "", into = letters[1:(nchar(d[1,1]) + 1)], convert = TRUE)
d <- d[2:ncol(d)]

gamma <- as.numeric(colSums(d) > nrow(d)/2)
epsilon <- 1 - gamma

gamma_str <- paste(gamma, collapse = "")
epsilon_str <- paste(epsilon, collapse = "")

g <- strtoi(gamma_str, base = 2)
e <- strtoi(epsilon_str, base = 2)
g * e

# wrong, try with example
ex <- read_csv(
  "00100
  11110
  10110
  10111
  10101
  01111
  00111
  11100
  10000
  11001
  00010
  01010", 
  col_names = "V1")

d <- separate(ex, V1, sep = "", into = letters[1:(nchar(d[1,1]) + 1)], convert = TRUE)
d <- d[2:ncol(d)]

gamma <- as.numeric(colSums(d) > nrow(d)/2)
epsilon <- 1 - gamma


g * e

# Now solution is correct, problem was with separate :(

## Part 2

binvec <- bin$V1

a <- sum(as.numeric(str_sub(binvec, 1, 1))) > length(binvec) / 2
a <- paste(as.numeric(a))
nchar(binvec)
bin2 <- binvec[str_sub(binvec, 1, 1) == a]


generator <- function(vec, gas = "O2"){
  x <- character(0)
  for(index in 1:nchar(vec[1])){
    common <- as.numeric(sum(as.numeric(str_sub(vec, index, index))) >= length(vec) / 2)
    if(gas == "CO2") common <- 1 - common
    #print(common)
    vec <- vec[str_sub(vec, index, index) == paste(common)]
    if(length(vec) == 1) break
  }
  return(vec)
}

O2 <- generator(binvec)
CO2 <- generator(binvec, gas = "CO2")

strtoi(O2, 2) * strtoi(CO2, 2)
