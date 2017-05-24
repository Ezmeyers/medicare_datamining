
# checks which packages to install
installPackages = function() {
  # get a list of installed packages
  p = installed.packages()
  packs = p[,1]
  # our packages to install
  my.packs = data.frame(c("ggplot2", "Matrix",
                        "stringr","dplyr","tidyr","gridExtra", "broom","ggcorrplot"),
                        stringsAsFactors = FALSE)
  names(my.packs) = "packs"
  # is our package already installed?
  pack.idx = ifelse(is.element(noquote(my.packs$packs), packs), FALSE, TRUE)
  # if not, extract the list of packages to install
  my.packs.inst = my.packs[pack.idx, ]
  # if there's packages to install, install them
  if (length(my.packs.inst) > 0) {
    install.packages(my.packs.inst)
  }
  # load all the packages
  loadLibs(my.packs)
}

# library includes
loadLibs = function(p) {
  lapply(p$packs, library, character.only = T)
}

# removes dollar signs from our data we don't want
removeDollars = function(x) {
  stringr::str_replace_all(x, fixed("$"), "")
}

# converts factor columns to numeric
factors2Numeric = function(x){
  factorX <- sapply(x, FUN=is.factor)
  x = x[,factorX]
  apply(x,2, FUN  = function(x) as.numeric(gsub(",","",x,fixed=TRUE)))
}

# change character cols to numeric
chars2Numeric = function(x){
  apply(x,2,FUN=function(x) as.numeric(gsub(",","",x,fixed=TRUE)))
}


# for use with reduce, takes in a data frame of vectors
# and makes a bunch of lists from those vectors
toList = function(x){
  my.list = list()
  for(i in 1:length(x[,2])){
    my.list[[i]] = unlist(x[i,2])
  }
  return(my.list)
}

pTable = function(x){
  
}
