

# Group Members: Jianing Li (s2819183); Ruotong Yang (s2808509); Hao Jiang (s2694843)
# Contributions: 
  # Jianing Li: Data cleaning (except splitting punctuation marks), a version of the main model function, code integration and consolidation
  # Hao Jiang: The punctuation marks splitting function, a version of the main model function, function testing
  # Ruotong Yang: The common words vector, the matrix M, a version of the main model function


# In this file, we are coding up a 'small language model' to simulate sentences resembling those written by Shakespeare.

# Given a sequence of p words, 
# our model will continuously randomly select the next word depending on the probabilities 
# that each possible word follows the most recent m words in our existing sequence,
# until a full stop is drawn.

# Specifically, we use the complete works of Shakespeare to compute the probability,
# using a mixture model that combines predictions from different context lengths (from m words down to 1 word) 
# with equal probability allocated to each matching round.

# To simplify the problem, 
# our model will only cover the 1000 most common words,
# and work with numerical 'tokens' representing them.



# File Reading


# read file into R, skipping the start and the end
setwd('/Users/jianingli/Documents/留学/学业-爱丁堡大学/Extended Statistical Programming/个人代码/小组作业/practical-1')
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,fileEncoding="UTF-8")
# print the first and last 10 words to see what is in a
head(a, 10)
tail(a, 10) 



# Data Cleaning


# Remove stage directions in brackets "[]"
stage_direction_left <- grep("[", a, fixed = TRUE) # find absolute positions of the left brackets
to_delete <- integer(0) # initiate a vector to store those indices of words to delete
# use a loop to find the matching right bracket for each left bracket
for (i in stage_direction_left) {
  search_end <- i + 100 # search within the next 100 words after the left brackets
  stage_direction_right_relative <- grep("]", a[i:search_end], fixed = TRUE)[1] 
  # use the first found bracket as the right endpoint of the deletion interval
  # when the same word contains bilateral brackets, the return value is 1
  
  # when we find a right bracket within the next 100 words
  if (!is.na(stage_direction_right_relative)) {
    stage_direction_right <- i + stage_direction_right_relative - 1 
    # convert the right bracket's relative position to absolute position
    
    # in case some left brackets are unmatched
    # we need to identify those left brackets and we do not delete anything
    next_left_bracket <- stage_direction_left[stage_direction_left > i][1] # find the next left bracket
    if (!is.na(next_left_bracket) && stage_direction_right > next_left_bracket) {
      next 
    } # if the right bracket is after the next left bracket, do not delete anything
    
    stage_direction <- i:stage_direction_right # absolute position of stage direction
    to_delete <- c(to_delete, stage_direction) # add it to the to_delete vector
  }
  
}
a_a <- a[-to_delete] # now a_a does not contain any stage directions


# Remove names, headings and numbers that are fully upper case or expressed by arabic numerals
# these words are equal to their upper case version
upper_arabic <- (a_a == toupper(a_a)) & (a_a != "I") & (a_a != "A") # but we do not remove "I" or "A"
a_b <- a_a[!upper_arabic]


# Remove "_" and "-" from words
a_c <- gsub("-|_", "", a_b) # replace "-" and "_" with ""


# Write a function to split specified punctuation marks from words
split_punct <- function(words, punct_marks) {
  current_words <- words   # store input words vector for iterative processing
  
  for (i in punct_marks) { # for each punctuation marks given in the argument
    
    pun_reg <- paste("\\", i, sep="") # add backslash to treat punctuation marks as literal characters
    
    index_punct <- grep(pun_reg, current_words) # find the indices of the words with punctuation marks
    
    # then we create a new vector long enough to store the words and punctuation marks after splitting them
    full_words <- rep("", length(index_punct)+length(current_words))
    words_pun_idx <-  index_punct + 1:length(index_punct) - 1 # find the indices of words in the new vector
    pun_idx <- words_pun_idx + 1 # find the indices of punctuation marks in the new vector
    
    full_words[words_pun_idx] <- current_words[index_punct] 
    # assign words with punctuation marks to the elements of full_words indexed by words_pun_idx 
    full_words <- gsub(pun_reg,"",full_words) # remove punctuation marks from all words
    full_words[pun_idx] <- rep(i,length(index_punct)) # place punctuation marks right after their corresponding words
    full_words[-c(pun_idx,words_pun_idx)] <- current_words[-index_punct]  
    # store other words in the elements of full_words not reserved for split words or punctuation marks
    
    current_words <- full_words # update the current_words vector in preparation for the next iteration
  }
  
  return(full_words) # return the full_words vector after removing all punctuation marks
}


# Use that punctuation marks splitting function to remove ",", ".", ";", "!", ":" , "?"
a_e <- split_punct(a_c, c(",", ".", ";", "!", ":" , "?"))
a_e # test if it works


# Convert cleaned word vector to lower case
a <- tolower(a_e)



# Model Preparation


# Create a vector of common words
unique_words <- unique(a) # extract all unique words from the text
index_vec <- match(a, unique_words) # find the index of each element in vector a within the unique_words vector
unique_words_count <- tabulate(index_vec) # count the number of occurrences of each unique word in a
# this returns a vector where the i'th element is the frequency of the i'th unique word in a
count_ranks <- rank(-unique_words_count) # rank unique words by frequency in descending order
# this returns a vector where the i'th element is the rank of the i'th unique word
top_n <- 1000 
b <- unique_words[count_ranks <= top_n] # select the 1000 most common words


# Construct a matrix M with its rows containing all the sequences of m+1 word tokens occurring in the text
tokens <- match(a,b) # generate the token vector for the entire text
mlag <- 4 # set the maximum lag to 4, which is the maximum order of model considered
n <- length(tokens)
M <- matrix(NA, nrow = n-mlag, ncol = mlag+1) # initialize matrix M
# fill each column of the matrix
for (i in 0:mlag){
  M_vec <- tokens[(1+i):(n-mlag+i)]
  M[, i+1] <- M_vec
}



# Main Function


# Write the main function that can generate the next word
next.word <- function(key, M, M1=match(a,b), w=rep(1,ncol(M)-1)){ 
  # M1 is the vector of word tokens for the whole text
  # w is a vector of mixture weights
  # by default, the total probability for each matching round is 1
  
  key_tokens <- match(key, b) # transform the key vector into its indices in the common word vector
  key_length <- length(key_tokens) # compute length
  
  mlag <- ncol(M) - 1 # compute mlag
  
  if (key_length > mlag) {
    key_tokens <- key_tokens[key_length-mlag+1:key_length]
    key_length <- mlag
  } # when the given key is too long, use only the last 'mlag' elements of the key_token
  # which means that we only use the most recent mlag words to generate the next word
  
  results <- integer(0) # initiate an integer vector to store potential valid results
  results_probs <- numeric(0) # initiate an numeric vector to store probabilities of potential valid results
  
  for (m in key_length:1) { # start matching from the entire key_token, matching one less word in each round, key_length rounds in total
    mc <- 1 # start matching from the first column of the matrix
    me <- m # to the m'th column of the matrix
    ii <- colSums(!(t(M[,mc:me,drop=FALSE])==key_tokens[(key_length-m+1):key_length])) 
    # use the last m elements in the key_token to match the first m columns of the matrix M
    # return a vector, with the i'th element as the number of unmatched elements on the i'th row of the matrix M
    match_rows <- which(ii == 0 & is.finite(ii)) 
    # find perfect matched rows with0 unmatched elements, excluding rows with NA comparisons
    # the row index will be stored in another vector called match_rows
    
    if (length(match_rows) > 0) { # if there are perfectly matched rows
      u <- M[match_rows, m+1] # use the (m+1)'th column of the match_rows
      u <- u[is.finite(u)] # delete those NAs
      # then we get the valid result for this round
      
      if (length(u) > 0) { # if there are elements in the valid result vector for this round
        result <- u
        n_u <- length(u) # the number of words in our result
        result_prob <- rep(w[m]/n_u, n_u) # compute the probabilities for this round
        # in each round, the matched words share the total probability(defaults to 1) of that round
        # probability is distributed proportionally to match frequencies
        
        results <- c(results, result) # put it in the results vector
        results_probs <- c(results_probs, result_prob) # as well as its probability
      } 
    }
  }
  
  if (length(results) > 0) { # if there are elements in the valid results vector after all rounds
    return(sample(results, 1, prob = results_probs)) # sample from these words
  } else { # if there is no valid result after all rounds
    cleaned_M1 <- M1[!is.na(M1)]
    return(sample(cleaned_M1, 1)) # sample from word tokens for the whole text from which NAs have been removed
  }
  
}



# Function Testing


# Select a single word to test the function
is_punct <- a %in% c(",", ".", "!", "?", ";", ":")
# this returns a logic vector in which the i'th element indicates whether the i'th element of a is a punctuation mark
a_no_punct <- a[!is_punct] 
# remove all punctuation marks from a by only keeping those elements that are not puctuation marks
start_point <- sample(a_no_punct,size=1) 
# then select a single word token at random from the text without punctuation marks as the start point


# Use the single word selected randomly to start simulate from the model until a full stop is reached
key <- start_point

max_length <- 100 # set a ending point in case it never reaches a full stop
start_token <- match(key, b) # find the start point's token
gen_tokens <- c(start_token) # initiate a vector to store the start point's token the the tokens generated from our model

for (m in 1:max_length) { 
  
  next_token <- next.word(key, M=M) # using the model to predict the next token
  gen_tokens <- c(gen_tokens,next_token) # update the token sequence by adding the token generated to the end
  key <- c(key,b[next_token])  # update the key as the first argument for producing the next token
  
  if(b[next_token] == ".") {
    break # stop when a full stop is generated
  }
}
# completing this loop, the gen_tokens vector now contains the tokens of both our key and the generated words

gen_words <- b[gen_tokens] # change the tokens into real common words
# however, if our key is not a common word, then the first element of gen_words will be NA
gen_words_without_NA <- c(start_point, gen_words[2:length(gen_words)]) 
# so we change the first word into the original key when it is not a common word
model_words <- paste(gen_words,collapse=" ") # paste the words together using " " between each words
# then remove " " before each punctuation mark
model_words_chars <- strsplit(model_words, "")[[1]]
punct_indices <- which(model_words_chars %in% c(",", ".", "!", "?", ";", ":")) # find the indices of the punctuation marks
if(length(punct_indices) > 0) { # in case next line does not return anything
  model_words_nicely <- paste(model_words_chars[-(punct_indices - 1)], collapse = "")
  # remove the " " before punctuation marks and paste them together again without anything between each character
} else {
  model_words_nicely <- model_words
} # otherwise, when there is no punctuation mark generated, we just use the first pasted version


# Compare our model with randomly selected words from the common words vector
next_words <- c() # initiate a vector to store words selected randomly from the common words vector
new_word <- "" # initiate a new_word to start the loop

for (i in 1:max_length) {
  if(new_word == ".") {
    break # run until the new sampled word is a full stop
  }
  new_word <- sample(b, size = 1) # sample a word randomly from common words vector as new_word
  next_words <- c(next_words, new_word) # add it to the next_word vector
}
# completing this loop, randomly selected words are all stored in the next_words vector

radom_words <- c(start_point, next_words) # concatenate the next words with the start word
random_model_words <- paste(radom_words, collapse=" ") # paste the words together using " " between each words
# then remove " " before each punctuation mark
random_model_words_chars <- strsplit(random_model_words, "")[[1]]
punct_indices <- which(random_model_words_chars %in% c(",", ".", "!", "?", ";", ":")) # find the indices of the punctuation marks
if(length(punct_indices) > 0) { # in case next line does not return anything
  random_model_words_nicely <- paste(random_model_words_chars[-(punct_indices - 1)], collapse = "")
  # remove the " " before punctuation marks and paste them together again without anything between each character
} else {
  random_model_words_nicely <- random_model_words
} # otherwise, when there is no punctuation mark generated, we just use the first pasted version

# compare the text generated
cat("Our model Generation:", model_words_nicely, "\n")
cat("The random common words Generation:", random_model_words_nicely)

