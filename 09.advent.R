library(stringr)
teststr <- "2199943210
3987894921
9856789892
8767896789
9899965678"
test <- read.table(text = teststr, sep = "")
testmat <- apply(str_split(test$V1, "", simplify = TRUE), 2, as.numeric)


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

# part 2
getAdjacentPos <- function(mat, row, col){
  
  indices <- list(c(row - 1, col), c(row + 1, col), c(row, col - 1), c(row, col + 1))
  oob <- sapply(indices, function(x) any(x < 1) | any(x > dim(mat)))
  indices <- indices[!oob]
  return(indices)
}


low_points <- which(findLowest(input_mat), arr.ind = TRUE)
basins <- input_mat < 9

bottom <- basins[low_points[1, 1], low_points[1, 2]]

x <- 0
for(pos in getAdjacentPos(basins, low_points[1, 1], low_points[1, 2])){
  if(basins[pos[1], pos[2]]){
    x <- x + 1
    basins[pos[1], pos[2]] <- FALSE
  }
  new_pos <- getAdjacentPos(basins, pos[1], pos[2])
  for (pos2 in new_pos){
    if(basins[pos2[1], pos2[2]]){
      x <- x + 1
      basins[pos2[1], pos2[2]] <- FALSE
    }
  }
}
x

bottom <- basins[low_points[1, 1], low_points[1, 2]]
ind1 <- basins


dim(bottom)

which(low_points, arr.ind = TRUE)
trackingBasin <- matrix(FALSE, nrow = nrow(basins), ncol = ncol(basins))
getBasin <- function(mat, start_row, start_col){
  # logical matrix as input
  # start at bottom of basin
  print(paste("new iteration, start_row is", start_row, "start_col is", start_col))
  position <- mat[start_row, start_col]
  print(position)
  print(trackingBasin[start_row, start_col])
  x <- 0
  if(position & !trackingBasin[start_row, start_col]){
    print("inside if-statement")
    x <- x + 1
    trackingBasin[start_row, start_col] <<- TRUE
  }else{
    return(x)
  }
  print(trackingBasin)
  for (pos in getAdjacentPos(mat, start_row, start_col)){
    #if(!trackingBasin[start_row, start_col])
    x <- x + getBasin(mat, pos[1], pos[2])
  }
  return(x)
}

ex_lowest <- which(findLowest(testmat), arr.ind = TRUE)

trackingBasin <- matrix(FALSE, nrow = nrow(testmat), ncol = ncol(testmat))
#getBasin(testmat < 9, ex_lowest[1,1], ex_lowest[1,2])
result <- rep(NA, nrow(ex_lowest))
for(i in 1:nrow(ex_lowest)){
  result[i] <- getBasin(testmat < 9, ex_lowest[i, 1], ex_lowest[i, 2])
}
result

prod(sort(result, decreasing = TRUE)[1:3])

# function for getting all basins

with_env <- function(f, e=parent.frame()) {
  stopifnot(is.function(f))
  environment(f) <- e
  f
}

allBasins <- function(mat){

  getBasin <- function(mat, start_row, start_col){
    # logical matrix as input
    # start at bottom of basin
    position <- mat[start_row, start_col]

    x <- 0
    if(position & !trackingBasin[start_row, start_col]){
      x <- x + 1
      trackingBasin[start_row, start_col] <<- TRUE
    }else{
      return(x)
    }
    for (pos in getAdjacentPos(mat, start_row, start_col)){
      x <- x + getBasin(mat, pos[1], pos[2])
    }
    return(x)
  }
  
  bottoms <- which(findLowest(mat), arr.ind = TRUE)
  
  trackingBasin <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))
  
  result <- rep(NA, nrow(bottoms))
  for(i in 1:nrow(bottoms)){
    result[i] <- getBasin(mat < 9, bottoms[i, 1], bottoms[i, 2])
  }
  return(result)
}


allBasins(testmat)

a <- allBasins(input_mat)

prod(a[order(a, decreasing = TRUE)][1:3])

max(a)

testmat[ex_lowest[1,1], ex_lowest[1,2]]
