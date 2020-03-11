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
#' 
#' @examples 
#' # Example dataset
#' DATA[, by = UID
#'      , flag := GroupIntervalDates(start = start_date, 
#'                                   end = end_date, 
#'                                   mode = "c")]
#'                        
#' DATA %>%
#'  group_by(UID) %>%
#'  mutate(flag = GroupIntervalDates(start = start_date, 
#'                                   end = end_date, 
#'                                   mode = "c"))       
#'                                   
#' DATA$flag <- unlist(by(DATA, DATA$UID, function(x) {
#'   GroupIntervalDates(start = x$start_date,
#'                      end = x$end_date, 
#'                      mode = "c") }))   
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
   
   dat <- dat[eval(substitute(order(dat$by, dat$start))), ]
   
   # LDAT <- as.list(dat[,paste(c(input$by, input$start, input$end))])
   
   LDAT <- Map(list,
                ST = as.list(eval(substitute(dat$start))),
                EN = as.list(eval(substitute(dat$end))))

   by <- factor(dat[,paste(input$by)])
   
   output <- tapply(LDAT, by, function(x){
      # browser()
      interval <- Map("interval", 
                      lapply(x, function(x) x$ST - lag),
                      lapply(x, function(x) x$EN + lag))
      
      vec <- Map(function(start, end){
         
         return(sapply(interval, function(x){

            +(start %within% x | end %within% x)
            
         }))
      }, 
      start = lapply(x, `[[`, "ST"), 
      end = lapply(x, `[[`, "EN"))
      
      browser()
      
      encoding <- sapply(1:length(vec), function(x) {
         
         starting <- if(zero.index){
            0L
         } else {
            1L
         }
         seq <- vec[[x]]
         
         return(
            if(x == 1L){ # Set first element
               starting
            } else { 
               if(seq[x - 1] == 0){ # If prior position is 0, then encode as 1L [results in new index during cumsum]
                  1L
               } else { 
                  0L
               }
            })
      })
      
      return(cumsum(encoding))
   })
   
   # Recombine groups by factor order
   return(unsplit(output, f = by)) 
}


