

#simplicity

a<-awords_rm_toupper

############# 5 #############

b<-unique(a)                                # remove the repeated items
vector_corresponding_idx<-match(a,b)        # the length is the same as a, which the indices about b
# check length(a)==length(vector_corresponding)
count_times<-tabulate(vector_corresponding)         # calculate the occuring times for the index vector b


# using the Rank function, we need to change the ordering direction to catch the first 1000 the most common words
count_inv_rank<-rank(-count_times)       # change the order about ranking：The smallest ranking corresponds to the largest number. change the odering dirction.
selct_idx<-which(count_inv_rank<=1000)  # <=1000 or <1000 I think <1000 is enough the final result ~~1000
# And we finally get the indices of the unique vector as new vector b
new<-b[selct_idx]                                  # using index to renew the b vector
# check the length of the words if too small <<1000 we use rescale the range.

### check if is approximated to 1000
if (abs(length(new)-1000)>=2){
  scale<-abs(length(new)-1000)
  # change the order about ranking：The smallest ranking corresponds to the largest number. change the odering dirction.
  selct_idx<-which(count_inv_rank<=1000+scale/2)
  
  b<-b[selct_idx]                 # renew the b vector
}else{
  b<-new     # keep the original ones.
}
############



####### 6 #####################


###(a)###
text_match<-match(a,b) # to find each word correspending to which places in the vector token.
###(b)###
n<-length(a)

mlag<-4# initialize.
M<-matrix(NA,(n - mlag),(mlag + 1)) # initialize M
token<-text_match # initialize the first coloumn of M
M[,1]<-token[1:(n-mlag)] # set the token
for(i in 2:(mlag+ 1)){
  #new_token1<-token[-c(1:i-1)]#remove the start
  # new_token2<-token[-c((i+n-mlag-1):length(token))]# remove the end of the shifted vectores in token
  
  if(i<(mlag+ 1)){
    new_token<-token[-c(1:(i-1),(i+n-mlag):length(token))]
    M[,i]<-new_token # row of M gives the indices in b (tokens) of a sequence of mlag+1 adjacent words in the text
    # update the text_match by shift
    
  }else{
    
    new_token<-token[-c(1:(i-1))]
    M[,i]<-new_token # row of M gives the indices in b (tokens) of a sequence of mlag+1 adjacent words in the text
    # update the text_match 
  }
  # new_token<-token[-c(1:(i-1),((i+n-mlag):(n-mlag)))]
  # M[,i]<-new_token # row of M gives the indices in b (tokens) of a sequence of mlag+1 adjacent words in the text
  # update the text_match by shift
}




####### 7 final ##############



next.word<-function(key,M,M1,w=rep(1,ncol(M)-1)){
  
  
  # if the length of the key is too large
  # do a turncation of key from the end of end, which the length is mlag
  
  if(length(key)>mlag) {
    key<-key[(length(key)-mlag+1):length(key)] # length(key)-mlag+1 is the start postion used to turncate
    
    u<-c() # initialize for combining.
    
    for(mc in 2:mlag){
      u_where_row<-c() # set initialize and update
      key<-key[mc:mlag]  # the reduced version of key
      # matching process
      ii<-colSums(!(t(M[,mc:mlag,drop=FALSE])==key)) 
      for(j in 1:length(ii))
        if (ii[j]==0 & is.finite(ii[j])){
          u_where_row<-c(u_where_row,j)
        }else{
          u_where_row<-u_where_row # keep the original ones.
          
        }
      if(length(u_where_row)==0){ # dealing with the event when u is NULL length(u)=0
        
        u<-u # keep the same.
        #n_text<-length(M1)
        # probablities<-rep(1/n_text, n_text)
        # sample_next<-sample(M1,size=1,replace = FALSE,prob=probablities)
        # return(sample_next )
        
      }else{
        
        u <- c(u,c(M[u_where_row, mlag + 1])) # do the storage for u
        # remove NA values
        u <- u[!is.na(u)]
        
      }
      
      
    } # for loop ending.
    
    if(length(u)==0){ # dealing with the event when u is NULL length(u)=0
      n_text<-length(M1)
      probablities<-rep(1/n_text, n_text)
      sample_next<-sample(M1,size=1,replace = FALSE,prob=probablities)
      return(sample_next )
    }else{
      
      w_m<-1 # no need normalize
      u_n<-length(u)
      
      probablities<-rep(w_m/n_u, nu)
      
      sample_next_word_token<-sample(u,size=1,replace=FALSE,prob=probablities)
      return(sample_next_word_token)
    }
    
    ################
    ##########we consider length(key) isn't large.#########
    ################
  }else{ 
    
    
    u_where_row<-c() # initialize an empty vector of the next words
    
    ## 1>##to find rows of M where the first m columns match key
    ii<-colSums(!(t(M[,mlag,drop=FALSE])==key))               # using is.finite to remove the NA 
    for(j in 1:length(ii))
      if (ii[j]==0 & is.finite(ii[j])){
        u_where_row<-c( u_where_row,j)
      }else{
        u_where_row<- u_where_row # keep the original ones.
        
      }
    
    ## 2>##  extract the (m+1)th column for each rows called u
    
    
    if(length(u_where_row)==0){ # dealing with the event when u is NULL length(u)=0
      n_text<-length(M1)
      probablities<-rep(1/n_text, n_text)
      sample_next<-sample(M1,size=1,replace = FALSE,prob=probablities)
      return(sample_next )
      
    }else{
      
      u <- M[u_where_row, mlag + 1]
      # remove NA values
      u <- u[!is.na(u)]
      # associate with each element of u a probability wm/nu
      
      # initialize
      w_m<-w[mlag]
      u_n<-length(u)
      
      probablities<-rep(w_m/n_u, nu)
      
      sample_next_word_token<-sample(u,size=1,replace=FALSE,prob=probablities)
      return(sample_next_word_token)
    }
    
    
  }
  
  
  
  
}
