library(stringr)

padMatrix <- function(mat){
  mat <- rbind(NA, mat, NA)
  mat <- cbind(NA, mat, NA)
  return(mat)
}

exstr <- "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"

ex <- apply(str_split(read.table(text = exstr)$V1, "", simplify = TRUE), 2, as.numeric)

mat <- padMatrix(ex)

mat + 1

mat <- mat + 2

flash <- which(mat > 9, arr.ind = TRUE)

adjacentSquare <- function(coord){
  rw <- (coord[1] - 1):(coord[1] + 1)
  cl <- (coord[2] - 1):(coord[2] + 1)
  return(list(rw,cl))
}

sq <- adjacentSquare(flash[1,])
mat[flash[1,1], flash[1,2]] <- -9999
mat[sq[[1]], sq[[2]]] <- mat[sq[[1]], sq[[2]]] + 1
mat


mat <- padMatrix(ex)
nflashes <- 0

for(i in 1:100){
  mat <- mat + 1
  flash <- mat > 9
  #print(paste("round", i, "there are", sum(flash, na.rm = TRUE), "values above 9"))
  while(any(flash, na.rm = TRUE)){
    nflashes <- nflashes + 1
    #print(paste("new iteration, nflashes is", nflashes))
    #print(paste("round", i, "inside while-loop, there are", sum(flash, na.rm = TRUE), "values above 9"))
    flash_ind <- which(flash, arr.ind = TRUE)[1,]
    sq <- adjacentSquare(flash_ind)
    mat[flash_ind[1], flash_ind[2]] <- -9999
    mat[sq[[1]], sq[[2]]] <- mat[sq[[1]], sq[[2]]] + 1
    flash <- mat > 9
  }
  mat[mat < 0] <- 0
}


octoFlash <- function(mat, n_iter){
  mat <- padMatrix(mat)
  nflashes <- 0
  
  for(i in 1:n_iter){
    mat <- mat + 1
    flash <- mat > 9
    #print(paste("round", i, "there are", sum(flash, na.rm = TRUE), "values above 9"))
    while(any(flash, na.rm = TRUE)){
      nflashes <- nflashes + 1
      #print(paste("new iteration, nflashes is", nflashes))
      #print(paste("round", i, "inside while-loop, there are", sum(flash, na.rm = TRUE), "values above 9"))
      flash_ind <- which(flash, arr.ind = TRUE)[1,]
      sq <- adjacentSquare(flash_ind)
      mat[flash_ind[1], flash_ind[2]] <- -9999
      mat[sq[[1]], sq[[2]]] <- mat[sq[[1]], sq[[2]]] + 1
      flash <- mat > 9
    }
    mat[mat < 0] <- 0
  }
  return(nflashes)
}

octoFlash(ex, 100)

# with data
d <- apply(str_split(read.table("data/11-input.txt")$V1, "", simplify = TRUE), 2, as.numeric)
octoFlash(d, 100)

# part 2
allFlash <- function(mat, n_iter){
  mat <- padMatrix(mat)
  
  for(i in 1:n_iter){
    mat <- mat + 1
    flash <- mat > 9
    #print(paste("round", i, "there are", sum(flash, na.rm = TRUE), "values above 9"))
    while(any(flash, na.rm = TRUE)){
      #print(paste("new iteration, nflashes is", nflashes))
      #print(paste("round", i, "inside while-loop, there are", sum(flash, na.rm = TRUE), "values above 9"))
      flash_ind <- which(flash, arr.ind = TRUE)[1,]
      sq <- adjacentSquare(flash_ind)
      mat[flash_ind[1], flash_ind[2]] <- -9999
      mat[sq[[1]], sq[[2]]] <- mat[sq[[1]], sq[[2]]] + 1
      flash <- mat > 9
    }
    if(all(mat < 0, na.rm = TRUE)) return(i)
    mat[mat < 0] <- 0
  }
  return(NA)
}

allFlash(ex, 200)
allFlash(d, 1000)
