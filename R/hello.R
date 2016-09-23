
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

get_class <- function(obj) {lapply(obj,class) %>% unlist}
get_na <- function(obj) apply(obj, 2, function(x) x %>% is.na %>% sum)

get_dist_num <- function(obj) {
  cc <- get_class(obj)
  out <- data.frame(matrix(NA, ncol(obj), 6))
  for(i in grep("numeric|integer", cc)) {
    v <- unlist(obj[i])
    out[i,] <- c(quantile(v, c(0,.5,1), na.rm = T), mean(v, na.rm=T) %>% round(2), IQR(v, na.rm=T), sum(v > median(v, na.rm = T) + 3*mad(v, na.rm = T) | v < median(v, na.rm = T) - 3*mad(v, na.rm = T), na.rm = T))
  }
  colnames(out) <- c('min', 'median', 'max', 'mean', 'IQR', 'n.outlier.3mad')
  #out$variable <- factor(colnames(obj))
  rownames(out) <- colnames(obj)
  return(out)
}

get_entropy <- function(tv){
  p <- tv/sum(tv)
  ent <- -1/log(length(p))*sum(p*log(p))
  return(ent %>% round(2))
}

get_dist_char <- function(obj) {
  cc <- get_class(obj)
  out <- data.frame(matrix(NA, ncol(obj), 3))
  for(i in grep("character|factor", cc))  {
    v <- unlist(obj[i])
    tv <- table(v)
    mltv <- which.max(tv) %>% names
    mltv2 <- paste0(mltv, ':', tv[which.max(tv)])
    ent <- get_entropy(tv)
    out[i,] <- c(length(unique(v)), ent, mltv2)
  }
  colnames(out) <- c('n.unique', 'entropy', 'max.level')
  #out$variable <- factor(colnames(obj))
  rownames(out) <- colnames(obj)
  return(out)
}

get_file <- function(x){
  require(tools)
  f <- file.choose()
  filetype <- file_ext(f)

  if(filetype=='xlsx'){
    obj <- read_excel(f)
  } else if (filetype=='txt' | filetype==''){
    obj <- read.table(f, header=T)
  } else if (filetype=='csv'){
    obj <- read.csv(f)
  }
  return(obj)
}

all_na <- function(obj){
  o <- apply(obj,2,function(x)all(is.na(x)))
  if(sum(o) > 0){
    return(which(o))
  } else { return(NA) }
}

var_scope <- function(obj){
  cc <- get_class(obj)
  allna <- all_na(obj)
  nacols <- NULL
  if(!is.na(sum(allna))) {cc[allna] <- NA; nacols=names(allna)}
  nac <- data.frame(variable=colnames(obj), type=get_class(obj), na.count=get_na(obj))
  nac$na.frac <- nac$na.count/100
  numsum <- get_dist_num(obj[grep("numeric|integer", cc)])
  charsum <- get_dist_char(obj[grep("character|factor", cc)])
  out <- list(na.summary=nac, numeric=numsum, character=charsum, na.columns=nacols)
  return(out)
}
