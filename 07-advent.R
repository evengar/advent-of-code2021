# example

crabs <- c(16,1,2,0,4,2,7,1,2,14)


fuelUsage <- function(x, crabs){
  sum(abs(x - crabs))
}

?optim

optim(par = 15, fuelUsage, crabs = crabs)$par

# part 1

d <- scan("data/07-input.txt", sep = ",")

minFuel <- optim(par = mean(d), fuelUsage, crabs = d)
round(minFuel$value)

## part 2
## Example

sum(sapply(abs(5 - crabs), function(x) sum(1:x)))

16:5
length(1:(5-16))

fuelUsageScaling <- function(x, crabs){
  sum(sapply(abs(x - crabs), function(x) if (x > 1) sum(1:x) else x))
}

fuelUsageScaling(5, crabs)

16:5 - 5

5-16


minFuel <- optim(par = mean(d), fuelUsageScaling, crabs = d)

minFuel$par

fuelUsageScaling(486, d)
fuelUsageScaling(485, d)
fuelUsageScaling(484, d)

optimize(fuelUsageScaling, interval = range(d), crabs = d)



minFuel$value
