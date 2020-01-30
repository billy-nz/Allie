#' Group overlapping time intervals and provide a sequential index
#'
#' \code{GroupIntervalDates} returns a sequential vector of the same length as input x. Overlapping time intervals are groupped, sequenced, and indexed starting with 1.
#' If grouping implemented, sequence begins will begin with 1 for each group. Each obversation requires a start and end date to form the time interval. 
#' Two modalities of grouping are available: consecutive and overlapping. See details. 
#'
#' @usage GroupIntervalDates(dat, start, end, mode)
#' 
#' @param start date variable in the dataset representing the start of an observation
#' @param end date variable in the dataset representing the end of an observation
#' @param mode specify interval type - use string "c" for "consecutive" or "o" for "overlapping". See details.
#' 
#' @details 
#' \itemize{
#' \item
#' }
#' 
#' @return Returns a sequential vector of the same length as input x 
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
#                    
# --- FUN ----
GroupIntervalDates <- function(start, end, mode,...){
 
 vars   <- as.list(match.call()[-1])
 
 # Param Check
 param.dat <- deparse(substitute(dat))!=""
 
 params  <- c("start", "end", "mode")
 
 for(i in params){
  if(eval(substitute(missing(i)))) {
   stop(paste("Missing parameter(s):", sQuote(i)), call. = F)
  }
 }

 offset <- +(tolower(mode) %in% c("c", "consecutive"))
 output <- lapply(start, function(x) {
   
   int  <- Map("interval", 
               as.list(as.Date(start) - offset),
               as.list(as.Date(end) + offset))
   
   # browser()
   
   return(+(unlist(Map('%within%', x, int))))
   # return(which(unlist(Map('%within%', x, int))))
 })
 
 common <- lapply(output, function(x) browser())
 
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

# Implement with base R
DATA$flag <- unlist(by(DATA, DATA$UID, function(x) {
  GroupIntervalDates(start = x$start_date, 
                     end = x$end_date, 
                     mode = "c") }))


