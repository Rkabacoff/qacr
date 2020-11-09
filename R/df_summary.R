#' @title df_summary
#' @description A comprehensive description of a data frame
#' @param df a data frame
#' @param digits number of decimal digits for statistics.
#' @param maxcat maximum number of levels of a character/factor variable to print.
#' @param label_length maximum length of factor level label to print. Longer labels
#' will be truncated.
#' @return the data frame invisibly (so that it can be used in a pipeline)
#' @details Prints a comprehensive description of a data frame via several tables,
#' a general summary table and tables that provide a breakdown of quantitative and categorical variables
#' @examples
#' testdata <- data.frame(height=c(4, 5, 3, 2, 100),
#'                        weight=c(39, 88, NA, 15, -2),
#'                        names=c("Bill","Dean", "Sam", NA, "Jane"),
#'                        race=c('b', 'w', 'w', 'o', 'b'))
#'
#' df_summary(testdata)
#'
#' @rdname df_summary
#' @import dplyr
#' @import readr
#' @import e1071
#' @export

df_summary <- function(df, digits = 2, maxcat=10, label_length=40){
  if(!(is.data.frame(df))){
    stop("You need to input a data frame")
  }

  dfname <- deparse(substitute(df))

  class(df) <- "data.frame"  # converts tibbles to data frame

  cat("\nThe data frame", dfname, "has", format(nrow(df), big.mark=","),
      "observations and", format(ncol(df), big.mark=","), "variables.\n\n")

  general_summary <- function(df, digits=digits){
    varnames <- colnames(df)
    tbl <- matrix( nrow = length(varnames), ncol = 6, byrow = TRUE)
    colnames(tbl) <- c("pos", "variable", "type", "n_unique", "n_missing", "pct_missing")
    rownames(tbl) <- varnames
    tbl[ ,1 ] <- c(1 : length(varnames))
    tbl[ , 2] <- varnames
    for(i in 1:length(varnames)){
      tbl[i,3] <- class(df[,i])[1]
    }
    for(i in 1:length(varnames)){
      tbl[i,4] <- length(unique(df[,i]))
    }
    for(i in 1:length(varnames)){
      tbl[i,5] <- sum(is.na(df[,i]))
    }
    n <- nrow(df)
    for(i in 1:length(varnames)){
      tbl[i,6] <- paste0(round(sum(is.na(df[,i]))*100/n, digits=digits) , "%")
    }


    cat("\nOverall\n",
          "=======\n", sep="")
    prmatrix(tbl, quote=FALSE, rowlab=rep("", nrow(tbl)), right=TRUE)
  }
  variable_summary<-function(data, digits=digits){
    cdf<-data.frame()
    ndf<-data.frame()
    for (k in names(data)) {
      numeric<-c()
      categorical<-c()
      if(is.numeric(data[[k]])|is.integer(data[[k]])){
        numeric<-append(numeric, k)
        table1<-data%>%
          select(numeric)
        for (i in 1:length(table1)){
          x<-unlist(table1[,i])
          x<-na.omit(x)
          table_n<-table1%>%
            select(numeric[i])%>%
            na.omit()%>%
            summarise(name=numeric[i], n=sum(!is.na((x))),
                      mean=round(mean(x), digits=digits),
                      sd=round(sd(x), digits=digits),
                      skew=round(e1071::skewness(x), digits=digits),
                      min=round(min(x), digits=digits),
                      p25=round(quantile(x, 0.25), digits=digits),
                      median=round(median(x), digits=digits),
                      p75=round(quantile(x, 0.75), digits=digits),
                      max=round(max(x), digits=digits))%>%
            mutate(outliers=sum((x>p75+(1.5*(p75-p25)))|
                                   x<p25-(1.5*(p75-p25))))
          ndf<-rbind(ndf, table_n)
        }
      }

      if(is.character(data[[k]])|is.factor(data[[k]])){
        categorical<-append(categorical, k)
        table2<-data%>%
          select(categorical)
        for (i in 1:length(table2)){
          x<-unlist(table2[,i])
          table_c<-table2%>%
            na.omit()%>%
            group_by(.dots=categorical[i])%>%
            summarise(variable=categorical[i], n=n())%>%
            mutate(pct=round(n/sum(n), digits=digits))

          colnames(table_c)<-c("level","variable",'n', 'pct')
          table_c<- table_c[c("variable", "level", "n", 'pct')]

          # long labels
          table_c$level <- substr(table_c$level, 1, label_length)

          # many levels
          if (nrow(table_c)>maxcat){
            table_c$level<-as.character(table_c$level)
            row_final<-c(variable=categorical[i], level="<...>",
                      n=sum(table_c$n[(maxcat+1):nrow(table_c)]),
                      pct=sum(table_c$pct[(maxcat+1):nrow(table_c)]))
            table_c = table_c[1:maxcat,]
            table_c= rbind(table_c, row_final)
            cdf<-rbind(cdf, table_c)}
          else{
            cdf<-rbind(cdf, table_c)
          }

          cdf$variable[duplicated(cdf$variable)] <- " "
        }
      }
    }
    if (nrow(ndf)>0){
      cat("\nQuantitative Variables\n",
            "======================\n", sep="")
      print (data.frame(ndf), row.names=FALSE)
    }
    if (nrow(cdf)>0){
      cat("\nCategorical Variables\n",
            "=====================\n", sep="")
      print (data.frame(cdf), row.names=FALSE, right=FALSE)
    }
  }

  general_summary(df, digits)
  variable_summary(df, digits)
  invisible(df)
}

