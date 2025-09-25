
setwd('/Users/jianingli/Documents/留学/学业-爱丁堡大学/Extended Statistical Programming/GitHub repo/Group29-Project1')
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,fileEncoding="UTF-8")
# read file into R, skipping the start and the end
head(a, 10)
tail(a, 10) # print the first and last 10 words to see what is in a

#4(a)
stage_direction_left <- grep("[", a, fixed = TRUE) # find absolute positions of the left brackets
to_delete <- integer(0)
for (i in stage_direction_left) {
  search_end <- i + 100 # search within the next 100 words
  stage_direction_right_relative <- grep("]", a[i:search_end], fixed = TRUE)[1] 
  # use the first found bracket as the right endpoint of the deletion interval
  # when the same word contains bilateral brackets, the return value is 1
  if (!is.na(stage_direction_right_relative)) {
    stage_direction_right <- i + stage_direction_right_relative - 1 # convert relative position to absolute position
    next_left_bracket <- stage_direction_left[stage_direction_left > i][1] # find the next left bracket
    if (!is.na(next_left_bracket) && stage_direction_right > next_left_bracket) {
      next 
    } # if the right bracket is after the next left bracket, do not delete anything
    stage_direction <- i:stage_direction_right # absolute position of stage direction
    to_delete <- c(to_delete, stage_direction) # add it to the to_delete vector
  }
}
a_a <- a[-to_delete]

#4(b)
upper_arabic <- (a_a == toupper(a_a)) & (a_a != "I") & (a_a != "A")
a_b <- a_a[!upper_arabic]

#4(c)
a_c <- gsub("-|_", "", a_b) # replace "-" and "_" with ""

#4(d)
split_punct <- function(words, punct_marks) {
  words_cor <- words   # initialize the iteration words inputted
  for (i in punct_marks) {
    
    
    pun_reg <- paste("\\", i, sep="")  # change the special characters into the literal meanings, we call it regulariztion.
    
    idex_pun <- grep(pun_reg, words_cor) # find the indices of the word with punctuation
    
    
    full_words <- rep(0,length(idex_pun)+length(words_cor)) ## vector to store the new entry in the vector of words after spliting the punctions
    words_pun_idx <-  idex_pun + 1:length(idex_pun ) - 1             # which should the puncution in the words?
    pun_idx <- words_pun_idx + 1    
    
    
    full_words[words_pun_idx] <- words_cor[idex_pun]    # insert the words with punc
    full_words <- gsub( pun_reg ,"",full_words)        # insert the words removed the punctuation
    full_words[pun_idx] <- rep(i,length(idex_pun))    # insert the punctuation to the next place besides the word
    full_words[-c(pun_idx,words_pun_idx)] <- words_cor[-idex_pun]  # insert the words without the punctuation : pure other words
    
    
    words_cor <- full_words # create the new "words" to prepare iteration.
    
  }
  
  return(full_words)
}

#4(e)
a_e <- split_punct(a_c, c(",", ".", ";", "!", ":" , "?")) # use the function
a_e # test if it works

#4(f)
a <- tolower(a_e) # convert cleaned word vector to lower case
