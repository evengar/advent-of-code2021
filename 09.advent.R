library(stringr)
teststr <- "2199943210
3987894921
9856789892
8767896789
9899965678"
test <- read.table(text = teststr, sep = "")
testmat <- apply(str_split(test$V1, "", simplify = TRUE), 2, as.numeric)

row_decrease <- t(apply(testmat, 1, diff)) < 0
col_decrease <- apply(testmat, 2, diff) < 0
invrow_decrease <- t(apply(testmat, 1, diff)) < 0






testmat[1,1] < testmat[c(1,2),c(1,2)]
result <- testmat[1:2,1:2] == min(testmat[list(c(1,2), c(2,1))])

testmat[1,1] < c(testmat[1,2], testmat[2,1])
testmat[1, 2] < c(testmat[1,1], testmat[1,3], testmat[2,2])


topo[1:2, 1:2] <- result
result <- testmat[1:2,2:3] == min(testmat[1:2, 2:3])

a <- c(2,2)


getAdjacent <- function(mat, row, col){
  
  indices <- list(c(row - 1, col), c(row + 1, col), c(row, col - 1), c(row, col + 1))
  oob <- sapply(indices, function(x) any(x < 1) | any(x > dim(mat)))
  indices <- indices[!oob]
  vals <- rep(NA, length(indices))
  for (i in seq_along(indices)){
    vals[i] <- mat[indices[[i]][1], indices[[i]][2]]
  }
  return(vals)
}

testmat[1,1] < getAdjacent(testmat, 1, 1)

testmat[1,2] < getAdjacent(testmat, 1, 2)

topo <- matrix(FALSE, nrow = nrow(testmat), ncol = ncol(testmat))

for (i in 1:nrow(testmat)){
  for (j in 1:ncol(testmat)){
    smallest <- all(testmat[i,j] < getAdjacent(testmat, i, j))
    if (smallest){
      topo[i, j] <- TRUE
    }
  }
}

sum(1 + testmat[topo])

findLowest <- function(mat){
  topo <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))
  
  for (i in 1:nrow(mat)){
    for (j in 1:ncol(mat)){
      smallest <- all(mat[i,j] < getAdjacent(mat, i, j))
      if (smallest){
        topo[i, j] <- TRUE
      }
    }
  }
  return(topo)
}

# with data
d <- readLines("data/09-input.txt")
input_mat <- apply(str_split(d, "", simplify = TRUE), 2, as.numeric)

sum(1 + input_mat[findLowest(input_mat)])
