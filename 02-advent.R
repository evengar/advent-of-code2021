d <- read.table("data/02-input.txt")
pos <- tapply(d$V2, d$V1, sum)

vertical <- pos["down"] - pos["up"]
vertical * pos["forward"]
# correct!

# part 2:
d$aim_change <- ifelse(d$V1 == "down", d$V2, 
                       ifelse(d$V1 == "up", -d$V2, 0))
d$aim <- cumsum(d$aim_change)

d_move <- d[d$V1 == "forward", c("V2", "aim")]

d_move$horizontal <- cumsum(d_move$V2)
d_move$vertical <- cumsum(d_move$V2 * d_move$aim)

with(d_move[nrow(d_move),], horizontal * vertical)

# wohoo!