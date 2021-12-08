bseq <- readLines("data/04-input.txt", 1)
bseq <- as.numeric(unlist(strsplit(bseq, ",")))

d <- read.table("data/04-input.txt", skip = 2)

boards <- lapply(split(d, rep(1:(nrow(d)/5), each = 5)), as.matrix)
boards_df <- split(d, rep(1:(nrow(d)/5), each = 5))

which(rowSums(matrix(boards[[1]] %in% bseq[1:50], ncol = 5)) == 5)

boards_df[[1]] %in% bseq



playBingo <- function(board, sequence){
  bingo_board <- matrix(board %in% sequence, ncol = 5)
  colBingo <- colSums(bingo_board) == 5
  rowBingo <- rowSums(bingo_board) == 5
  if(any(rowBingo)){
    row <- which(rowBingo)
    print(paste("Bingo! In row", row))
    return(TRUE)
  }
  if(any(colBingo)){
    col <- which(colBingo)
    print(paste("Bingo! In column", col))
    return(TRUE)
  }
  return(FALSE)
}


for(i in 5:length(bseq)){
  bingos <- sapply(boards, playBingo, sequence = bseq[1:i])
  if(any(bingos)){
    board <- which(bingos)
    winning_number <- bseq[i]
    print(paste("on board", board, "with the number", winning_number, "in round", i))
    break
  }
}

sum(boards[[board]][!boards[[board]] %in% bseq[1:i]]) * winning_number

boards2 <- boards

for(i in 5:length(bseq)){
  bingos <- sapply(boards2, playBingo, sequence = bseq[1:i])
  if(any(bingos)) print(paste("bingo in boards", paste(which(bingos), collapse = ", ")))
  if(length(boards2) == 1){
    if(bingos){
      print("Bingo on last board!")
      losing_board <- boards2[[1]]
      winning_number <- bseq[i]
      break
    }
  }else{
    boards2 <- boards2[!bingos]
    print(paste("boards left:", length(boards2)))
  }
}

losing_board

sum(losing_board[!losing_board %in% bseq[1:i]]) * bseq[i]

matrix(losing_board %in% bseq[1:(i-35)], ncol = 5)


matrix(losing_board[!losing_board %in% bseq[1:i-1]], ncol = 5)

# wow, that was unnecessarily tough
# lesson learned: check the return statements!