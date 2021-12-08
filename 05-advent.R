d <- read.table(text = gsub("->", ",", readLines("data/05-input.txt")), sep = ",")
names(d) <- c("x1", "y1", "x2", "y2")
dims <- max(d)

coord <- matrix(rep(0, dims*dims), ncol = dims)

straight <- with(d, d[x1 == x2 | y1 == y2,])

for (i in 1:nrow(straight)){
  with(straight[i, ], {
    if(x1 == x2){
      vec <- y1:y2
      for(j in vec){
        coord[x1, j] <<- coord[x1, j] + 1
      }
    }else{
      vec <- x1:x2
      for(j in vec){
        coord[j, y1] <<- coord[j, y1] + 1
      }
    }
    
  })
}

sum(coord > 1)

straight[1,]

coord[536, 470:863]

with(straight[1,], if(x1 == x2) y2:y1) 


# part 2: diagonal lines

coord <- matrix(rep(0, dims*dims), ncol = dims)

straight <- with(d, d[x1 == x2 | y1 == y2,])

for (i in 1:nrow(d)){
  with(d[i, ], {
    if (x1 == x2){
      vec <- y1:y2
      for(j in vec){
        coord[x1, j] <<- coord[x1, j] + 1
      }
    }else if (y1 == y2){
      vec <- x1:x2
      for(j in vec){
        coord[j, y1] <<- coord[j, y1] + 1
      }
    }else{
      vec <- data.frame(x = x1:x2, y = y1:y2)
      for(j in 1:nrow(vec)){
        coord[vec$x, vec$y] <<- coord[vec$x, vec$y] + 1
      }
    }
    
  })
}

sum(coord > 1)


diag(with(d[3,], coord[x1:x2, y1:y2]))
