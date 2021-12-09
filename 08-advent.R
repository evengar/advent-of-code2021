library(stringr)
library(tidyverse)
d <- read.table("data/08-input.txt", sep = "|", col.names = c("input", "output"))



sum(sapply(lapply(str_split(str_trim(d$output), " "), nchar), function(x) sum(x %in% c(2, 3, 4, 7))))

# part 2
## Example
common_char <- function(strings, ref){
  sapply(str_split(strings, ""), function(x) sum(x %in% str_split(ref, "")[[1]]))
}

ex <- "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

exin <- str_split(str_trim(str_split(ex, "[|]")[[1]][1]), " ")[[1]]
exnchar <- nchar(exin)
exnum <- 0:9



commons <- sapply(exin, common_char, strings = exin)
rownames(commons) <- colnames(commons)

actual_numbers <- c(8, 5, 2, 3, 7, 9, 6, 4, 0, 1)
names(actual_numbers) <- exin
fingerprint <- rowSums(commons)
# universal reference for the problem
reference <- data.frame(fingerprint, actual_numbers) %>%
  remove_rownames()


sort_chars <- function(str_vec){
  sapply((lapply(str_split(str_vec, ""), sort)), paste0, collapse = "")
}

ex2 <- "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"

ex2in <- str_split(str_trim(str_split(ex2, "[|]")[[1]][1]), " ")[[1]] %>%
  sort_chars()
ex2out <- str_split(str_trim(str_split(ex2, "[|]")[[1]][2]), " ")[[1]] %>%
  sort_chars()
commons2 <- sapply(ex2in, common_char, strings = ex2in)
fingerprint2 <- rowSums(commons2)
ref_frame <- dplyr::left_join(reference, data.frame(ex2in, fingerprint = fingerprint2)) %>%
  column_to_rownames("ex2in")

as.numeric(paste0(ref_frame[ex2out,]$actual_numbers, collapse = ""))


## function

translate_numbers <- function(string, reference){
  input <- str_split(str_trim(str_split(string, "[|]")[[1]][1]), " ")[[1]] %>%
    sort_chars()
  output <- str_split(str_trim(str_split(string, "[|]")[[1]][2]), " ")[[1]] %>%
    sort_chars()
  fingerprint <- rowSums(sapply(input, common_char, strings = input))
  #print(fingerprint)
  #print(input)
  ref_frame <- dplyr::left_join(reference, data.frame(input, fingerprint), by = "fingerprint") %>%
    column_to_rownames("input")
  as.numeric(paste0(ref_frame[output,]$actual_numbers, collapse = ""))
}

translate_numbers(ex, reference)
translate_numbers(ex2, reference)

translate_numbers("fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg", reference)

# test with data
all_strings <- readLines("data/08-input.txt")
sum(sapply(all_strings, translate_numbers, reference = reference))




#### Failed attempts below!

names(exnum)[2] <- exin[exnchar == 2]
names(exnum)[8] <- exin[exnchar == 3]
names(exnum)[5] <- exin[exnchar == 4]
names(exnum)[9] <- exin[exnchar == 7]
#names(exnum)[4] <- exin[nchar(exin) == 5]
common_w1 <- common_char(exin, names(exnum)[2])
names(exnum)[7] <- exin[exnchar == 6 & common_w1 == 1]
names(exnum)[4] <- exin[exnchar == 5 & common_w1 == 2]
common_w3 <- common_char(exin, names(exnum)[4])
#names(exnum)[10] <- exin[exnchar == 6 & common_w3 == ]


sapply((lapply(str_split(ex2in, ""), sort)), paste0, collapse = "")



mapply(function(x, y) x %in% str_split(ref_frame$ex2in, ""), str_split(ex2out, ""), str_split(ex2in, ""))

common_w1
exin
exnchar
sapply(str_split(exin, ""), function(x) sum(x %in% str_split(names(exnum)[[2]], "")[[1]]))


chardiff(exin, names(exnum)[2])

exnum

known_chars <- str_split(names(exnum), "")
pos <- rep(NA, 10)

pos[1] <- known_chars[[8]][!known_chars[[8]] %in% known_chars[[2]]]


lengths <- data.frame(
  num = 0:9,
  length = c(6, 2, 5, 5, 4, 5, 6, 3, 7, 6)
)

lengths

nchar(exin)
nchar_tab <- table(nchar(exin))
nchar_tab == 1

library(dplyr)
chars <- data.frame(chars = exin, length = nchar(exin))
char_pos <- dplyr::left_join(lengths, chars)

with(char_pos, {
  number6 <- chars[num == 6]
  number1 <- chars[num == 1]
  diffs <- mapply(function(x,y) sum(x!=y),strsplit(number1,""),strsplit(number6,""))
  results <- list()
  results["6"] <- number6[which.max(diffs)]
  results
  })

# onto something here, definitely some unnecessary steps though ...

exin_split <- str_split(exin, "")
split(exin, nchar(exin)) ## THIS IS GOOD

exin_split[[10]] %in% exin_split[[9]]


chars