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
      
      lag <- 1
      
   } else {
      
      lapply(names(list(...)),
             function(x)
                assign(x, unlist(list(...)[x]),
                       envir = parent.frame(2)))
      
   }
   
   # ParamCheck
   vars  <- c("start", "end", "by")
   
   ParamCheck(input, vars, call, is.table)
   
   # Vectorise
   input <- Map(list, 
                as.list(as.Date(dat$start)),
                as.list(as.Date(dat$end)),
                Map("interval",
                    as.list(as.Date(dat$start) - lag),
                    as.list(as.Date(dat$end) + lag)))
   
   by <- factor(eval(substitute(dat$by)))
   
   output <- tapply(input, by, function(x){
      
      st.date  <- lapply(x, `[[`, 1)
      en.date  <- lapply(x, `[[`, 2)
      interval <- lapply(x, `[[`, 3)
      
      mapply(function(start, end){
         
         return(+(unlist(Map('%within%', start, interval)) | 
                            unlist(Map('%within%', end, interval))))
         
      }, 
      start = st.date, 
      end = en.date)
      
   })
   
   
   
   
   # browser()
   # output <- lapply(dat$start, function(x) {
   #    
   #    int  <- Map("interval", 
   #                as.list(as.Date(dat$start) - lag),
   #                as.list(as.Date(dat$end) + lag))
   #    
   #    return(+(unlist(Map('%within%', x, int))))
   # })
   # 
 browser()
 
 red <- Reduce('+', output)
 
 # Output interpreter
 # n.int <- unlist(Map('sum', output))
 # index <- cumsum(replace(n.int, which(n.int!=1), 0))
 c.index <- sapply(output, "[[", 1)
 
 # lengths <- rle(unlist(output))$lengths
 
 lengths <- rle(c.index)$lengths
 index   <- rep(seq(lengths), lengths)
 # browser()
 # index <- browser()
 
 
 return(as.numeric(c.index))
 
}

# # Implement with base R
# DATA$flag <- unlist(by(DATA, DATA$UID, function(x) {
#   GroupIntervalDates(start = x$start_date, 
#                      end = x$end_date, 
#                      mode = "c") }))


