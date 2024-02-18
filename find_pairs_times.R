find_pairs <- function(vecs, window_size) {
  pairs <- list()
  n <- length(vecs)
  
  # Iterate over all combinations of pairs of vectors
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      vec1 <- vecs[[i]]
      vec2 <- vecs[[j]]
      
      # Iterate over all combinations of values from different vectors
      for (k in 1:length(vec1)) {
        for (l in 1:length(vec2)) {
          if (abs(vec1[k] - vec2[l]) <= window_size) {
            pairs[[paste0("pair_", length(pairs) + 1)]] <- list(
              value1 = vec1[k],
              value2 = vec2[l],
              vector1 = paste0("vec", i),
              vector2 = paste0("vec", j)
            )
          }
        }
      }
    }
  }
  
  return(pairs)
}

# Example usage:
vec1 <- c(1, 5, 9, 13,17,22,25,26,41)
vec2 <- c(2, 6, 10,12,26,42)
vec3 <- c(3, 7, 11, 15, 19,43)


vecs <- list(vec1, vec2, vec3)
window_size <- 2
pairs <- find_pairs(vecs, window_size)
pairs

# Convert list of pairs into dataframe
pairs_df <- do.call(rbind, pairs)

pairs_df <- as.data.frame.matrix(pairs_df)
pairs_df <- as.data.frame(lapply(pairs_df, function(x) unlist(x)))

pairs_df <- data.frame(
          value1 = pmin(pairs_df$value1, pairs_df$value2), 
          value2 = pmax(pairs_df$value1, pairs_df$value2),
          vector1 = ifelse(pairs_df$value1 <= pairs_df$value2, pairs_df$vector1, pairs_df$vector2),
          vector2 = ifelse(pairs_df$value1 <= pairs_df$value2, pairs_df$vector2, pairs_df$vector1))


pairs_df


### Put all into one function

find_pairs1 <- function(x, w){
  pairs <- find_pairs(x,w)
  pairs_df <- do.call(rbind, pairs)
  pairs_df <- as.data.frame.matrix(pairs_df)
  pairs_df <- as.data.frame(lapply(pairs_df, function(x) unlist(x)))
  
  out <- data.frame(
    value1 = pmin(pairs_df$value1, pairs_df$value2), 
    value2 = pmax(pairs_df$value1, pairs_df$value2),
    vector1 = ifelse(pairs_df$value1 <= pairs_df$value2, pairs_df$vector1, pairs_df$vector2),
    vector2 = ifelse(pairs_df$value1 <= pairs_df$value2, pairs_df$vector2, pairs_df$vector1))
  
  return(out)
}

vecs
find_pairs1(vecs, w=2)



