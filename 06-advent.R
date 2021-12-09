# example
lantern <- c(3,4,3,1,2)

for (i in 1:18){
  reprod <- lantern == 0
  lantern <- lantern - 1
  if(any(reprod)){
    lantern[reprod] <- 6
    lantern <- c(lantern, rep(8, sum(reprod)))
  }

}
lantern

# function

lanternfish <- function(lantern, ngen){
  for (i in 1:ngen){
    reprod <- lantern == 0
    lantern <- lantern - 1
    if(any(reprod)){
      lantern[reprod] <- 6
      lantern <- c(lantern, rep(8, sum(reprod)))
    }
  }
  return(lantern)
}

lanternfish(c(3,4,3,1,2), 18)


# data
d <- scan("data/06-input.txt", sep = ",")

length(lanternfish(d, 80))

# part 2


# length(lanternfish(d, 256))

# well, that won't run, let's re-implement the algorithm
# we only need to keep track of how many lanternfish are at each stage

library(dplyr)

lantern <- c(3,4,3,1,2)
init <- data.frame(table(lantern))
init$lantern <- as.numeric(init$lantern)

lantern_data <- data.frame(lantern = 0:8) %>%
  left_join(init) %>%
  mutate(Freq = ifelse(is.na(Freq), 0, Freq))
lantern_data

for(i in 1:18){
  reprod <- lantern_data$Freq[1]
  lantern_data$Freq <- lead(lantern_data$Freq)
  lantern_data$Freq[9] <- reprod
  lantern_data$Freq[7] <- lantern_data$Freq[7] + reprod
}

sum(lantern_data$Freq)

table(lantern)
x <- rep(0, 9)
names(x) <- 0:8

x[names(table(lantern))] <- table(lantern)

# function

lanternfish_faster <- function(lantern, ngen){
  lantern_data <- rep(0, 9)
  names(lantern_data) <- 0:8
  lantern_table <- table(lantern)
  lantern_data[names(lantern_table)] <- lantern_table
  
  for(i in 1:ngen){
    reprod <- lantern_data[1]
    lantern_data <- lead(lantern_data)
    lantern_data[9] <- reprod
    lantern_data[7] <- lantern_data[7] + reprod
  }
  return(lantern_data)
}

# test with example
sum(lanternfish_faster(lantern, 18))

sum(lanternfish_faster(lantern, 256))

# with data

d <- scan("data/06-input.txt", sep = ",")

paste(sum(lanternfish_faster(d, 256)))

# wohoo!