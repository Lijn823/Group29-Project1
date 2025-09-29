
setwd('/Users/jianingli/Documents/留学/学业-爱丁堡大学/Extended Statistical Programming/GitHub repo/Group29-Project1')
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,fileEncoding="UTF-8")
# Read file into R, skipping the start and the end
head(a, 10)
tail(a, 10) # Print the first and last 10 words to see what is in a

stage_direction_left <- grep("[", a, fixed = TRUE) # Find absolute positions of the left brackets
to_delete <- integer(0)
for (i in stage_direction_left) {
  search_end <- i + 100 # Search within the next 100 words
  stage_direction_right_relative <- grep("]", a[i:search_end], fixed = TRUE)[1] 
  # Use the first found bracket as the right endpoint of the deletion interval
  # When the same word contains bilateral brackets, the return value is 1
  if (!is.na(stage_direction_right_relative)) {
    stage_direction_right <- i + stage_direction_right_relative - 1 # Convert relative position to absolute position
    stage_direction <- i:stage_direction_right # Absolute position of stage direction
    to_delete <- c(to_delete, stage_direction) # Add it to the to_delete vector
  }
}
a_a <- a[-to_delete] #I'm not sure whether unique(to_delete) is necessary

upper_arabic <- rep(FALSE, length(a_a))
for (i in 1:length(a_a)) {
  test_word <- a_a[i]
  if(test_word == toupper(test_word) && test_word != "I" && test_word != "A") {
    upper_arabic[i] <- TRUE
  } # Fully upper case (except A and I): turn FALSE into TRUE
  else if (grepl("^[0-9]+$", test_word)) {
    upper_arabic[i] <- TRUE
  } # Numbers: turn FALSE into TRUE
}
a_b <- a_a[!upper_arabic] # Keep those that are not fully upper case or numbers (FALSE)
# Can this task be solved without using a loop?

a_c <- gsub("-|_", "", a_b) # Replace "-" and "_" with ""

split_punct <- function(x) {
  with_punc <- grep("([,.;!:?])", x) # Should I make punctuation marks parameters of the function?
  to_split <- gsub("([,.;!:?])", " \\1", x)
  split_result <- strsplit(to_split," ")
  y <- unlist(split_result)
  y
}

a_e <- split_punct(a_c)

a <- tolower(a_e) # Convert cleaned word vector to lower case
