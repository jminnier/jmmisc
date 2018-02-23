# Functions by Tianxi Cai, or others that I picked up in Phd program somewhere

VTM<-function(vc, dm){
  matrix(vc, ncol=length(vc), nrow=dm, byrow=T)
}

sum.I <-function(yy,FUN,Yi,Vi=NULL,ties.method = "first") ## ties either 'f' or 'a'
{
  if (FUN=="<"|FUN==">=") { yy <- -yy; Yi <- -Yi}
  pos <- rank(c(yy,Yi),ties.method=ties.method)[1:length(yy)]-rank(yy,ties.method=ties.method)
  if (substring(FUN,2,2)=="=") pos <- length(Yi)-pos
  if (!is.null(Vi)) {
    if(substring(FUN,2,2)=="=") tmpind <- order(-Yi) else  tmpind <- order(Yi)
    ##Vi <- cumsum2(as.matrix(Vi)[tmpind,])
    Vi <- apply(as.matrix(Vi)[tmpind,,drop=F],2,cumsum)
    return(rbind(0,Vi)[pos+1,])
  } else return(pos)
}

## Most from http://gettinggeneticsdone.blogspot.com/2013/06/customize-rprofile.html

## List objects and classes (from @_inundata)
lsa <- function() {
  obj_type <- function(x) { class(get(x)) }
  foo=data.frame(sapply(ls(envir=.GlobalEnv),obj_type))
  foo$object_name=rownames(foo)
  names(foo)[1]="class"
  names(foo)[2]="object"
  return(unrowname(foo))
}

## List all functions in a package (also from @_inundata)
lsp <-function(package, all.names = FALSE, pattern) {
  package <- deparse(substitute(package))
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}


#List objects by memory size
ls.objects <- function (pos = 1, pattern, order.by,
                             decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# Shorthand
lsos <- function(..., n=10) {
  ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

mysetwd <- function(wd) {
  test = try(setwd(wd))
  if(class(test)=="try-error") dir.create(wd,recursive = TRUE)
  setwd(file.path(wd))
}

## Show the first 5 rows and first 5 columns of a data frame or matrix
hh <- function(d) if(class(d)=="matrix"|class(d)=="data.frame") d[1:5,1:5]

gl <- dplyr::glimpse
