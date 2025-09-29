#7 Write the "next.word" function
next.word <- function(key, M, M1, w = rep(1,ncol(M)-1)){
  mlag = ncol(M)-1 #Determine the value of mlag
  
  # Handling the issue of the length of the key vector
  key_length = length(key)
  if (key_length <= mlag){
    current_key = key
  } else {
    current_key = key[(length(key) - mlag + 1) : length(key)] #Extract the last mlag elements of the key vector
  }
  
  #Find the row in M that matches the current_key
  mc = 1 #Match columns 1 to mlag of column M
  ii = colSums(!(t(M[, mc:mlag, drop = FALSE]) == current_key)) #Calculate the cases where each column does not match the current_key
  match_rows = which(ii == 0 & is.finite(ii)) #Find the rows that are exactly matched and have no missing values
  
  #Predict the next word based on different situations
  if (length(match_rows) == 0) {
    # No fully matching row
    full_counts = tabulate(M1) # Count the occurrence frequency of all tokens in the entire text
    max_count = max(full_counts) # Find the maximum occurrence count
    next_token = which(full_counts == max_count)[1] # Select the token that appears the most frequently for the first time
  } else {
    # At least one line matches the key
    next_tokens = M[match_rows, mlag+1] # Find the (m + 1)th token of all matching lines in M
    max_valid_token = length(b) 
    clean_tokens = next_tokens[!is.na(next_tokens) & next_tokens <= max_valid_token] #Remove NA and tokens that exceed the valid range
    
    if (length(clean_tokens) == 0){ #If there is still no valid token after cleaning, use the default value of M1
      full_counts = tabulate(M1)
      max_count = max(full_counts)
      next_token = which(full_counts == max_count)[1]
    } else {
      counts = tabulate(clean_tokens) # Count the occurrence frequency of each token
      valid_tokens = which(counts > 0) # Count the valid tokens that are greater than 0
      if (length(valid_tokens) == 1){
        next_token <- valid_tokens[1]
      } else {
        probs = counts[valid_tokens] / sum(counts[valid_tokens]) # Calculate the probability of each occurrence in the next word
        next_token = sample(valid_tokens, size = 1, prob = probs) #  Sample from the valid tokens according to the probability
      }
    }
  }
  return(next_token)
}
