#5(a)Build a list of common words
unique_word <- unique(a) # Extract all the unique words from the text
#5(b)Find the index of the corresponding element of the unique word vector b in vector a
index_vec <- match(a, unique_word)
#5(c)Count the number of occurrences of each unique word
a_counts <- tabulate(index_vec)
#5(d)Select the 1000 most common words
top_n <- 1000
count_ranks <- rank(-a_counts)# Rank the occurrences of a_counts in descending order
b <- unique_word[count_ranks <= top_n]


#6(a)Generate the token vector for the entire text
tokens <- match(a,b)
#6(b)Construct matrix M
mlag <- 4 # Initialize
n <- length(tokens) # Initialize
M <- matrix(NA, nrow = n-mlag, ncol = mlag+1) # Initialize matrix M
#Fill each column of the matrix
for (i in 0:mlag){
  M_vec <- tokens[(1+i):(n-mlag+i)]
  M[, i+1] <- M_vec
}