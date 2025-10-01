



######## task 8 #########




cleaned_Text<-aa
M1<-match(aa,b)

where_puns<-which(cleaned_Text %in% c(",", ".", ";", "!", ":","?"))  # search the position of the puns

cleaned_Text_no_puns<-cleaned_Text[-where_puns]                     # remove the puns

start_point<-sample(cleaned_Text_no_puns,size=1,replace=TRUE,prob=rep(1/length(cleaned_Text_no_puns),length(cleaned_Text_no_puns)))





######### task 9 #############

# initialize key


key<-start_point
key<-match(key,b)

start_token<-match(start_point,b)
gen_tokens<-c(start_token)
next_token<-b[start_token]

while ((b[next_token] != ".") | (!is.finite(b[next_token]) )){
  
 # max_order<-max_order+1  # set innitail value max_order
  
  
  next_token<-next.word(key,M=M,M1=M1,w=rep(1,ncol(M)-1)) # using the model to simulate (predict) the next token
    
  gen_tokens<-c(gen_tokens,next_token) # update the token sequence one by one.
    
  key<-c(key,next_token)  # update the key to prepare producing the next token.
    
    
  }
  
}




#gen_tokens
#convert tokens to words.
words<-b[gen_tokens]
nicely_model_words<-paste(words,collapse=" ")
# comparing
start<-c(start_point)
probb<-rep(1/length(b),length(b))
next_tokenss<-sample(b,size=max_order-1,replace=TRUE,prob=probb) # following wods.
radom_next_tokens<-c(start,next_tokenss)
nicely_random_next_words<-paste(radom_next_tokens,collapse=" ")

print('The model Generation:') ;nicely_model_words

print("The common words text Generation:") ; nicely_random_next_words










