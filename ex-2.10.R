
getRow <- function(df, r) {
  return(unlist(df[r, , drop = TRUE])) 
}

meanCenter <- function(df) {
  row_names <- row.names(df)
  col_names <- names(df)
  m <- matrix(nrow = nrow(df), ncol = ncol(df))
  for (r in 1:nrow(df)) {
    vec <- getRow(df, r)
    mean_vec <- vec - mean(vec, na.rm = TRUE)
    m[r,] <- mean_vec
  }
  out_df <- data.frame(m, row.names = row_names)
  names(out_df) <- col_names
  return(out_df)
}




generateDF <- function(){
  user_ids <- 1:5
  item_ids <- 1:6
  df <- data.frame(c(7,6,NA,1,1), 
                 c(6,7,3,2,NA),
                 c(7,NA,3,2,1),
                 c(4,4,1,3,2),
                 c(5,3,1,3,3),
                 c(4,4,NA,4,3), 
                 row.names = user_ids)
  names(df) <- item_ids
  return(df)
}

u_row <- 2
v_row <- 3

pearson_ <- function(df, u_row, v_row) {
  (u <- getRow(df, u_row))
  (v <- getRow(df, v_row))
  
  common <- u & v
  common[is.na(common)] <- FALSE
  u <- u[common]
  v <- v[common]
  return(cor(u, v, method = "pearson"))
}


##
## 2.10.1
##
## Predict absolute rating of item-3 for user-2
## via UBCF w/ pearson correlation & mean centering

(df <- generateDF())
(mc_df <- meanCenter(df))


