
####### 7  ##############



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
