
#' CreateTableOne_Strata
#'
#' An extension to tableone::CreateTableOne that makes a table of overall information,
#' cbinded to a strata table with p-values, adding a column of number of non-missing
#' values for each variable, returning the printed table (from print.TableOne) as a data frame
#' with no rownames. Column names include strata name.
#'
#' @param vars Variables to be summarized given as a character vector. Factors are handled as categorical variables, whereas numeric variables are handled as continuous variables. If empty, all variables in the data frame specified in the data argument are used.
#' @param strata Stratifying (grouping) variable name(s) given as a character vector. default NULL when no grouping variable
#' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
#' @param keep_test logical: keep the column "test"
#' @param smd logical: show standardized mean difference column
#' @param pval logical: show p-value column
#' @param nonnormal A character vector to specify the variables for which the p-values should be those of nonparametric tests. By default all p-values are from normal assumption-based tests (oneway.test).
#' @param exact A character vector to specify the variables for which the p-values should be those of exact tests. By default all p-values are from large sample approximation tests (chisq.test).
#' @param ... need to add this: other arguments to CreateTableOne
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' mtcars2 <- mtcars %>% mutate(gear=factor(gear),vs=factor(vs))
#' CreateTableOne_Strata(vars=c("mpg","vs","gear"),strata="cyl",data=mtcars2)
#' CreateTableOne_Strata(vars=c("mpg","vs","gear"),strata="cyl",data=mtcars2, pval=FALSE, smd=TRUE)
#'
#' # Paired t-test
#' small_data <- mtcars2 %>% group_by(vs)%>%slice(1:5L)
#' CreateTableOne_Strata(vars=c("mpg"),strata="vs",data=small_data,
#'     testNormal = t.test, argsNormal = list(paired=FALSE))
#' CreateTableOne_Strata(vars=c("mpg"),strata="vs",data=small_data,
#'     testNormal = t.test, argsNormal = list(paired=TRUE))
#'
#' CreateTableOne_Strata(vars=c("mpg","vs","gear"),data=mtcars2)
#'
CreateTableOne_Strata <- function(vars,
                                  strata=NULL,
                                  data,
                                  keep_test=FALSE,
                                  smd=FALSE,
                                  pval=TRUE,
                                  nonnormal=NULL,
                                  exact=NULL,...) {
  data_tbl1 = data[,c(strata,vars),drop=F]
  data_tbl1 = dplyr::as_data_frame(data_tbl1)

  if(!is.null(strata)) strata_levels = levels(factor(data %>% dplyr::pull(!!strata)))

  t0 = tableone::CreateTableOne(vars,data=data_tbl1,includeNA=FALSE)
  nummiss = apply(!is.na(data_tbl1[,t0$MetaData$vars]),2,sum)
  nummiss = data.frame("rownames1"=t0$MetaData$vars,
                       "num_not_missing"=nummiss,stringsAsFactors = FALSE)


  x0 <- print(t0,printToggle = FALSE,nonnormal=nonnormal,exact=exact)

  tmp0 = unlist(lapply(rownames(x0),function(k) strsplit(k,split=" ",fixed=TRUE)[[1]][1]))
  tmp = data.frame("rownames"=rownames(x0),"rownames1"=tmp0,stringsAsFactors = FALSE)
  tmp = dplyr::left_join(tmp,nummiss)
  tmp$num_not_missing[is.na(tmp$num_not_missing)] = ""
  tmp = tmp[,-2,drop=FALSE]
  x0 = data.frame(x0)


  if(!is.null(strata)) {
    t1 = tableone::CreateTableOne(vars,
                                  data=data_tbl1,
                                  strata=strata,
                                  includeNA=FALSE,
                                  ...)
    x1 <- print(t1,printToggle = FALSE,nonnormal=nonnormal,exact=exact, smd=smd)
    x1 <- data.frame(x1)
    tmpstrata <- c(paste(strata,"=",strata_levels),"P-value","Test", "SMD")

    if(!keep_test) {
      x1 <- x1%>%dplyr::select(-test)
      tmpstrata <- tmpstrata[-which(tmpstrata=="Test")]
    }
    if(!smd) {
      tmpstrata <- tmpstrata[-which(tmpstrata=="SMD")]
    }
    if(!pval) {
      x1 <- x1%>%dplyr::select(-p)
      tmpstrata <- tmpstrata[-which(tmpstrata=="P-value")]
    }

  }else{
    tmpstrata = c()
  }


  tmpcolnames = c("Vars","Num Not Missing", "Overall",
                  tmpstrata)


  if(!is.null(strata)) {
    table1 <- cbind(tmp,x0,x1)
  }else{
    table1 <- cbind(tmp,x0)
  }

  colnames(table1) = tmpcolnames
  rownames(table1) = NULL
  table1$Vars = gsub("   ","___",table1$Vars)
  dplyr::as_data_frame(table1) %>% mutate_if(is.factor,as.character)
}


