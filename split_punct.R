



####（4）

#the function to search for split the words and punctuations.

split_punct <- function(words, punct_marks) {
  words_cor<- words   # initialize the iteration words inputted
  for (i in punct_marks) {
    
    
    pun_reg <- paste("\\", i, sep="")  # change the special characters into the literal meanings, we call it regulariztion.
    
    idex_pun <- grep(pun_reg, words_cor) # find the indices of the word with punctuation
    
    
    full_words <- rep(0,length(idex_pun)+length(words_cor)) ## vector to store the new entry in the vector of words after spliting the punctions
    words_pun_idx<-  idex_pun +1:length(idex_pun )-1             # which should the puncution in the words?
    pun_idx<-  words_pun_idx+1    
    
    
    full_words[  words_pun_idx]<-words_cor[idex_pun]    # insert the words with punc
    full_words<- gsub( pun_reg ,"",full_words)        # insert the words removed the punctuation
    full_words[pun_idx]<-rep(i,length(idex_pun))    # insert the punctuation to the next place besides the word
    full_words[-c(pun_idx,words_pun_idx)]<-words_cor[-idex_pun]  # insert the words without the punctuation : pure other words
    
    
    words_cor<- full_words # create the new "words" to prepare iteration.
    
  }
  
  return(full_words)
}



