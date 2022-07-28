# Get Integer Factors
# a function for integer factorization of a number
get_integer_factors <- function(n){
  out <- NULL
  for(i in 1:n){
    if((n %% i)==0){
      out <- if(is.null(out)){i}else{c(out,i)}
    }
  }
  return(out)
}
