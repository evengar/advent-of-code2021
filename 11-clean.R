library(stringr)

# utility for padding matrix with NA
# makes indexing easier
padMatrix <- function(mat){
  mat <- rbind(NA, mat, NA)
  mat <- cbind(NA, mat, NA)
  return(mat)
}

# find the square of fields adjacent to a coordinate
# Input is vector c(row, col)
adjacentSquare <- function(coord){
  rw <- (coord[1] - 1):(coord[1] + 1)
  cl <- (coord[2] - 1):(coord[2] + 1)
  return(list(rw,cl))
}

# Loop over all iterations  and find all flashes
octoFlash <- function(mat, n_iter){
  mat <- padMatrix(mat)
  nflashes <- 0
  for(i in 1:n_iter){
    mat <- mat + 1
    flash <- mat > 9
    while(any(flash, na.rm = TRUE)){
      nflashes <- nflashes + 1
      # look only at first match
      # because flash will change for next iteration anyways
      flash_ind <- which(flash, arr.ind = TRUE)[1,]
      sq <- adjacentSquare(flash_ind)
      # set the current field to an arbitrarily large negative value
      # to avoid counting it twice
      mat[flash_ind[1], flash_ind[2]] <- -9999
      mat[sq[[1]], sq[[2]]] <- mat[sq[[1]], sq[[2]]] + 1
      flash <- mat > 9 # update flashes
    }
    # reset the large negative values before next iteration
    mat[mat < 0] <- 0
  }
  return(nflashes)
}


# very similar to above function, but rather than counting
# it returns whenever all fields are flashing simultaneously
allFlash <- function(mat, n_iter){
  mat <- padMatrix(mat)
  
  for(i in 1:n_iter){
    mat <- mat + 1
    flash <- mat > 9
    while(any(flash, na.rm = TRUE)){
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

# import example data
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

# import real data

d <- apply(str_split(read.table("data/11-input.txt")$V1, "", simplify = TRUE), 2, as.numeric)


# test with example
octoFlash(ex, 100)

# apply to data
octoFlash(d, 100)

# part 2

allFlash(ex, 200)
allFlash(d, 1000)

# implementation depends on doing enough iterations
# as long as this is high enough it shouldn't matter what you set it to,
# as the function will return upon all fields flashing anyway
