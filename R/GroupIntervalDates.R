#' Group overlapping time intervals and provide a sequential index
#'
#' \code{GroupIntervalDates} returns a sequential vector of the same length as input x. Overlapping time intervals are groupped, sequenced, and indexed starting with 1.
#' If grouping implemented, sequence begins will begin with 1 for each group. Each obversation requires a start and end date to form the time interval. 
#' Two modalities of grouping are available: consecutive and overlapping. See details. 
#'
#' @usage GroupIntervalDates(dat, start, end, mode)
#' 
#' @param dat a data.frame or data.table
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

# --- FUN ----
GroupIntervalDates <- function(dat, start, end, mode,...){
 
 vars   <- as.list(match.call()[-1])
 
 # Param Check
 param.dat <- deparse(substitute(dat))!=""
 
 params  <- c("start", "end", "mode")
 
 for(i in params){
  if(eval(substitute(missing(i)))) {
   stop(paste("Missing parameter(s):", sQuote(i)), call. = F)
  }
 }

 # Dataset provided
 if(param.dat){
  dat       <- data.table::as.data.table(dat, row.names = NULL)
  vars$dat  <- NULL
  vars$mode <- NULL
  input   <- as.vector(sapply(vars, as.character))

  # Missing Check
  is.missing <- any(!input %in% names(dat))
  
  if(is.missing){
   to.check <- input[!input %in% names(dat)]
   stop(paste("Check input(s) names:", paste(sQuote(to.check), collapse = ", ")), call. = F)
  }
  
  data.table::setnames(dat, 
                       as.character(c(vars$start, vars$end)), 
                       c("start", "end"))
 }
 
 # Mode Check
 offset <- +(mode %in% c("c", "consecutive"))
 
 # Sort
 
 browser()
 
}

GroupIntervalDates(DATA, start = visit_date, end = result_date, mode = "c")

