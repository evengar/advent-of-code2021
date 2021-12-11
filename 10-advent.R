library(stringr)

exstr <- "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"

ex <- str_split(exstr, "\n")[[1]]

braces <- data.frame(
  type = c("round",  "square", "curly", "angle"),
  opening = 0,
  closing = 0,
  score = c(3, 57, 1197, 25137)
)

length(str_split(ex, "")[[1]])

for(string in str_split(ex, "")){
  for(char in string){
    current <- switch(
      char,
      "(" = c("round", "opening"),
      ")" = c("round", "closing"),
      "[" = c("square", "opening"),
      "]" = c()
      
    )
  }
}

braces <- rep(0, 8)
names(braces) <- c("(", ")", "[", "]", "{", "}", "<", ">")

braces[str_split(ex, "")[[1]][1]] <- 1

braces

braces[c(2, 4, 6, 8)] > braces[c(1,3,5,7)]

bracketError <- function(charvec){
  braces <- rep(0, 8)
  names(braces) <- c("(", ")", "[", "]", "{", "}", "<", ">")
  
  for(char in charvec){
    
    braces[char] <- braces[char] + 1
    
    if(any(braces[c(2, 4, 6, 8)] > braces[c(1,3,5,7)])){
      return(char)
    }
  }
  return("")
}

bracketError(str_split(ex, "")[[5]])

# above is wrong logic! Trying again
# add to a vector when bracket is opened, remove when it's closed


# using vectors as "dictionary"
opening_braces <- c("(", "[", "{", "<")
closing_braces <- c(")", "]", "}", ">")

opening_braces[closing_braces == ")"]

findError <- function(charvec){
  # using vectors as "dictionary"
  opening_braces <- c("(", "[", "{", "<")
  closing_braces <- c(")", "]", "}", ">")
  # vector for keeping track
  open <- character(0)
  
  for(char in charvec){
    #print(paste("start of loop, open is", paste0(open, collapse = " ")))
    #print(char)
    opening <- char %in% opening_braces
    
    if(any(opening)){
      #print(paste(char, "is an opening bracket, adding to open"))
      open <- c(open, char)
    }else{
      opposite <- opening_braces[closing_braces == char]
      last <- length(open)
      if(opposite == open[last]){
        #print(paste(char, "matches", open[last], "shortening open by 1"))
        open <- open[1:(last - 1)]
      }else{
        #print(paste("incompatible char", char, "does not match", opposite))
        return(char)
      }
    }
  }
  return("ok")
}

findError(str_split(ex, "")[[3]])

errors <- sapply(str_split(ex, ""), findError)

scores <- c(3, 57, 1197, 25137, 0)
names(scores) <- c(")", "]", "}", ">", "ok")

sum(scores[errors])

# with data
d <- readLines("data/10-input.txt")
errors <- sapply(str_split(d, ""), findError)
sum(scores[errors])

# part 2

d_incomplete <- d[errors == "ok"]

reverseBracket <- function(char){
  opening_braces <- c("(", "[", "{", "<")
  closing_braces <- c(")", "]", "}", ">")
  
  if (any(char %in% opening_braces)){
    return(closing_braces[char == opening_braces])
  }else{
    return(opening_braces[char == closing_braces])
  }
  
}

findIncomplete <- function(charvec){
  
  opening_braces <- c("(", "[", "{", "<")
  closing_braces <- c(")", "]", "}", ">")
  # vector for keeping track
  open <- character(0)
  
  for(char in charvec){
    #print(paste("start of loop, open is", paste0(open, collapse = " ")))
    #print(char)
    opening <- char %in% opening_braces
    
    if(any(opening)){
      #print(paste(char, "is an opening bracket, adding to open"))
      open <- c(open, char)
    }else{
      opposite <- reverseBracket(char)
      last <- length(open)
      if(opposite == open[last]){
        #print(paste(char, "matches", open[last], "shortening open by 1"))
        open <- open[1:(last - 1)]
      }else{
        #print(paste("incompatible char", char, "does not match", opposite))
        return(char)
      }
    }
  }
  return(open)
}

remaining <- lapply(str_split(d_incomplete, ""), findIncomplete)

closings <- lapply(remaining, function(x){
  xvec <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xvec[i] <- reverseBracket(x[i])
  }
  return(rev(xvec))
})

closings

scoring <- function(charvec){
  scores <- c(1, 2, 3, 4)
  names(scores) <- c(")", "]", "}", ">")
  result <- 0
  for(char in charvec){
    result <- result * 5 + scores[char]
  }
  return(result)
}

median(sapply(closings, scoring))
