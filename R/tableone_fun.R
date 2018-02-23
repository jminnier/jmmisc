
#' CreateTableOne_Strata
#'
#' @param vars
#' @param strata
#' @param data
#' @param keep_test
#' @param nonnormal
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' mtcars2 = mtcars%>%mutate(gear=factor(gear),vs=factor(vs))
#' CreateTableOne_Strata(vars=c("mpg","vs","gear"),strata="cyl",data=mtcars2)
CreateTableOne_Strata <- function(vars,strata,data,keep_test=FALSE,nonnormal=NULL,...) {
  data_tbl1 = data[,c(strata,vars),drop=F]
  data_tbl1 = dplyr::as_data_frame(data_tbl1)

  strata_levels = levels(factor(data %>% dplyr::pull(!!strata)))

  t0 = tableone::CreateTableOne(colnames(data_tbl1)[-1],data=data_tbl1,includeNA=FALSE)
  t1 = tableone::CreateTableOne(colnames(data_tbl1)[-1],data=data_tbl1,
                      strata=strata,
                      includeNA=FALSE)
  nummiss = apply(!is.na(data_tbl1[,t0$MetaData$vars]),2,sum)
  nummiss = data.frame("rownames1"=t0$MetaData$vars,
                       "num_not_missing"=nummiss,stringsAsFactors = FALSE)


  x0 <- print(t0,printToggle = FALSE,nonnormal=nonnormal)
  x1 <- print(t1,printToggle = FALSE,nonnormal=nonnormal)

  tmp0 = unlist(lapply(rownames(x0),function(k) strsplit(k,split=" ",fixed=TRUE)[[1]][1]))
  tmp = data.frame("rownames"=rownames(x0),"rownames1"=tmp0,stringsAsFactors = FALSE)
  tmp = dplyr::left_join(tmp,nummiss)
  tmp$num_not_missing[is.na(tmp$num_not_missing)] = ""
  tmp = tmp[,-2]

  x0 = data.frame(x0)
  x1 = data.frame(x1)

  tmpcolnames = c("Vars","Num Not Missing", "Overall",
                  paste(strata,"=",strata_levels),
                  "P-value","Test")

  if(!keep_test) {
    x1 <- x1%>%dplyr::select(-test)
    tmpcolnames <- tmpcolnames[-7]
  }
  table1 <- cbind(tmp,x0,x1)

  colnames(table1) = tmpcolnames
  rownames(table1) = NULL
  table1$Vars = gsub("   ","___",table1$Vars)
  dplyr::as_data_frame(table1)
}


