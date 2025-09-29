
# Task 7
next.word <- function(key, M, M1=match(a,b), w=rep(1,ncol(M)-1)){ 
  
  key_tokens <- match(key, b) #transform the key vector into indices
  key_length <- length(key_tokens) #compute length
  
  if (key_length > mlag) {
    key_tokens <- key_tokens[key_length-mlag+1:key_length]
    key_length <- mlag
  } #when the given key is too long, use only the last 'mlag' elements of the key_token
  
  results <- integer(0) #initiate an integer vector to store potential valid results
  results_weights <- numeric(0) #initiate an numeric vector to store weights of potential valid results
  
  for (m in key_length:1) { #start matching from the entire key_token, matching one less word in each round, matching key_length rounds in total
    mc <- 1 #Start matching from the first column of the matrix
    me <- m #to the mth column of the matrix
    ii <- colSums(!(t(M[,mc:me,drop=FALSE])==key_tokens[(key_length-m+1):key_length])) 
    #professor's code, use the last m elements in the key_token to match the first m columns of the matrix
    #return a vector, with the ith element as the number of unmatched elements on the ith row of the matrix
    match_rows <- which(ii == 0 & is.finite(ii)) 
    #when the numbers of unmatched elements is 0，the row index will be stored in another vector called match_rows
    
    if (length(match_rows) > 0) { #if there are successfully matched rows
      u <- M[match_rows, m+1] #use the (m+1)th column of the match_rows
      u <- u[is.finite(u)] #delete those NAs
      #then we get the valid results for this round
      
      if (length(u) > 0) { #if there are elements in the valid results vector for this round
        n_u <- length(u) #record the number of elements（including repeated ones）to calculate weight
        u_unique <- unique(u) #create a non-repeated valid results vector
        u_word_count <- tabulate(match(u,u_unique)) 
        #Record how many times each result in the non-repeated valid results vector is repeated
        #in the repeated valid results vector
        
        for (i in 1:length(u_unique)) { #for each unique valid result
          result <- u_unique[i] #record that valid result
          result_count <- u_word_count[i] #how many time it is repeated
          result_weight <- w[m] * (result_count/n_u) #compute its weight
          
          if (result %in% results) { #if that valid result is already in the valid results vector
            result_loc <- which(results == result) #find its location
            results_weights[result_loc] <- results_weights[result_loc] + result_weight #sum its weight
          } else { #if it is a new result
            results <- c(results, result) #put it in the results vector
            results_weights <- c(results_weights, result_weight) #as well as its weight
          }
        }
      }
    }
  }
  if (length(results) > 0) {
    return(sample(results, 1, prob = results_weights)) #sample from the distribution
  } else {
    return("NA")
  }
}