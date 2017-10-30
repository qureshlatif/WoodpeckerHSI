varCombosLinear <- function(vars, K.max = length(vars), exclude = NULL) {
  mods <- list()
  for(i in 1:K.max) {
    index <- combinations(length(vars),i)
    for (a in 1:nrow(index)) {
      v <- vars[index[a, ]]
      m <- paste(v, collapse="+")
      if(!is.null(exclude)) {
        check <- 0
        for(x in 1:length(exclude)) if(sum(exclude[[x]]) %in% v) check <- 1
        if(check == 0) mods[[(length(mods)+1)]] <- m
      }
    }
  }
  return(mods)
}
