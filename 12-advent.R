library(stringr)
library(dplyr)

is.lwr <- function(string){
  string == str_to_lower(string)
}


exstr <- "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"

ex <- str_split(str_split(exstr, "\n")[[1]], "-", simplify = TRUE)


ex[1,1] == str_to_lower(ex[1,1])

exmat <- rbind(ex, ex[,c(2,1)])
exmat <- exmat[!(exmat[,2] == "start" | exmat[,1] == "end"),]


ex_list <- split(exmat[,2], exmat[,1])

# proof of concept:
for(path in ex_list[["start"]]){
  small_cave <- FALSE
  if(is.lwr(path)) small_cave <- TRUE
  print(paste("first room is", path))
  for(path2 in ex_list[[path]]){
    if(!is.lwr(path2) | !small_cave){
      print(paste("second room is", path2))
    }
  }
}

navigateCaves <- function(caves, room = "start", small_cave = FALSE){
  print(paste("new room, room is:", room))
  if(room == "end") return(room)
  
  results <- room

  
  for(path in caves[[room]]){
    print("new iteration")
    if(!is.lwr(path) | !small_cave){
      if(is.lwr(path)){
        print(paste("small cave", path, "visited, no more can be visited"))
        small_cave <- TRUE
      }
      results <- c(results, navigateCaves(caves, room = path, small_cave = small_cave))
    }

  }
  return(results)
}

navigateCaves(ex_list)

