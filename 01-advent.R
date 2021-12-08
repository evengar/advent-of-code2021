# part 1:
d <- scan("data/01-input.txt")
diff(d)
sum(diff(d) > 0)
# correct answer!

result <- rep(NA, length(d) - 2)
# part 2:
for(i in 1:(length(d) - 2)){
  result[i] <- sum(d[i:(i+2)])
}

sum(diff(result) > 0)
# correct!