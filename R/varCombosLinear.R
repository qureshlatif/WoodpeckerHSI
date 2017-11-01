varCombosLinear <- function(V, K.max = length(V), exclude = NULL) {
  mods <- list()
  if(K.max > length(V)) K.max <- length(V)
  for(i in 1:K.max) {
    index <- combinations(length(V),i)
    for (a in 1:nrow(index)) {
      v <- V[index[a, ]]
      m <- paste(v, collapse="+")
      check <- 0
      if(!is.null(exclude))
        for(x in 1:length(exclude)) if(sum(exclude[[x]] %in% v)>1) check <- 1
      if(check == 0) mods[[(length(mods)+1)]] <- m
    }
  }
  return(mods)
}
