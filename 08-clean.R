library(stringr)

# part 1
d <- read.table("data/08-input.txt", sep = "|", col.names = c("input", "output"))
sum(sapply(lapply(str_split(str_trim(d$output), " "), nchar), function(x) sum(x %in% c(2, 3, 4, 7))))

# part 2

## function that returns matrix of pairwise common characters in
## a vector of strings
common_char <- function(strings, ref){
  sapply(str_split(strings, ""), function(x) sum(x %in% str_split(ref, "")[[1]]))
}

# utility for sorting the characters in a string
sort_chars <- function(str_vec){
  sapply((lapply(str_split(str_vec, ""), sort)), paste0, collapse = "")
}

# use example string with known result
ex <- "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
exin <- str_split(str_trim(str_split(ex, "[|]")[[1]][1]), " ")[[1]]

# sum of pairwise differences between strings is a unique
# "fingerprint" for each number
commons <- sapply(exin, common_char, strings = exin)
fingerprint <- rowSums(commons)
actual_numbers <- c(8, 5, 2, 3, 7, 9, 6, 4, 0, 1)

# make a reference table from the example data
reference <- data.frame(fingerprint, actual_numbers)

## function using this reference table to infer the number

translate_numbers <- function(string, reference){
  input <- str_split(str_trim(str_split(string, "[|]")[[1]][1]), " ")[[1]] %>%
    sort_chars()
  output <- str_split(str_trim(str_split(string, "[|]")[[1]][2]), " ")[[1]] %>%
    sort_chars()
  fingerprint <- rowSums(sapply(input, common_char, strings = input))
  ref_frame <- dplyr::left_join(reference, data.frame(input, fingerprint), by = "fingerprint") %>%
    tibble::column_to_rownames("input")
  as.numeric(paste0(ref_frame[output,]$actual_numbers, collapse = ""))
}

# get data and apply function to each row
all_strings <- readLines("data/08-input.txt")
sum(sapply(all_strings, translate_numbers, reference = reference))

