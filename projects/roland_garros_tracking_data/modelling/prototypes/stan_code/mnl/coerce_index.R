coerce_index <- function( ... ) {
  L <- list(...)
  if ( is.list(L[[1]]) && length(L)==1 ) L <- L[[1]]
  if ( length(L)==1 ) {
    # first try to coerce straight to integer as test for any NAs
    x <- as.integer(L[[1]])
    if ( any(is.na(x)) ) 
      # brute method
      x <- as.integer(as.factor(as.character(L[[1]])))
    return( x )
  } else {
    # multiple inputs
    vnames <- match.call()
    vnames <- as.character(vnames)[2:(length(L)+1)]
    # generate levels that include all labels from all inputs
    M <- L
    for ( i in 1:length(L) ) M[[i]] <- as.character(L[[i]])
    Mall <- M[[1]]
    for ( i in 2:length(L) ) Mall <- c( Mall , M[[i]] )
    Mall <- unique(Mall)
    new_levels <- levels(as.factor(Mall))
    for ( i in 1:length(L) ) {
      M[[i]] <- factor(M[[i]],levels=new_levels)
      M[[i]] <- as.integer(M[[i]])
    }
    names(M) <- paste( vnames , "_idx" , sep="" )
    return(M)
  } 
}
