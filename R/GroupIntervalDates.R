#' Group overlapping time intervals and provide a sequential index
#'
#' \code{GroupIntervalDates} returns a sequential vector of the same length as input x. Overlapping time intervals are groupped, sequenced, and indexed starting with 1.
#' If grouping implemented, sequence begins will begin with 1 for each group. Each obversation requires a start and end date to form the time interval. 
#' Two modalities of grouping are available: consecutive and overlapping. See details. 
#'
#' @usage GroupIntervalDates(dat, start, end, by, ...)
#' 
#' @param dat an data.frame or data.table containing input data (see details)
#' @param start date variable in the dataset representing the start of an observation
#' @param end date variable in the dataset representing the end of an observation
#' @param by person or group id
#' @param ... further arguments (see values)
#' 
#' @return Returns a sequential vector of the same length as input x 
#' 
#' \item{...}{further arguments:
#'            \itemize{
#'              \item \code{lag} specify the lag days; default is 1.
#'              \item \code{zero.index} specify whether the indexing sequence should start at 0 instead of 1; default is FALSE
#'              }}
#' 
#' @importFrom lubridate interval %within%
#' @importFrom data.table setDT
#' @importFrom parallel mcMap
#' 
#' @examples
#' DATA$lag1 <- GroupIntervalDates(dat=DATA, start=start_date, end=end_date, by=UID)
#' DATA$lag0 <- GroupIntervalDates(dat=DATA, start=start_date, end=end_date, by=UID, lag = 0, zero.index = T)
#'                                                          
# --- FUN ----
GroupIntervalDates <- function(dat, start, end, by, ...){
   
   # calls
   call      <- gsub("()", "",  match.call()[1])
   is.table  <- deparse(substitute(dat))!=""
   input     <- as.list(match.call()[-1])
   
   if(length(list(...)) == 0){
      
      lag         <- 1
      zero.index  <- FALSE
      
   } else {
      
      default <- setdiff(c("lag", "zero.index"), names(list(...)))
      
      if(length(default) == 1){
         
         lapply(default,
                function(x){
                   
                   if(x == "lag"){
                      val <- 1
                   } else {
                      val <- FALSE
                   }
                   assign(x, val, envir = parent.frame(2))
                })
      }
      
      lapply(names(list(...)),
             function(x)
                assign(x, unlist(list(...)[x]),
                       envir = parent.frame(2)))
      
   }
   
   # ParamCheck
   vars  <- c("start", "end", "by")
   
   ParamCheck(input, vars, call, is.table)
   
   # Row order matters! Before sorting, creating a row index.
   index <- sort(as.numeric(eval(substitute(dat$start))), 
                 index.return = T)$ix
   
   dat <- setDT(dat[index, ])
   
   dat[, sequence := DateVectoriser(dat = .SD, 
                                    start = input$start, 
                                    end = input$end, 
                                    lag = lag, 
                                    zero.index = zero.index), 
       by = eval(deparse(substitute(by)))]
   
   dat <- dat[order(index)]
   
   return(dat$sequence)
}


