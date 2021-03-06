#' Date Vectoriser for GroupIntervalDates
#'
#' \code{DateVectoriser} returns a sequential vector of binary values in the same length as input x. 
#' Set first element always the starting position. In subsequent elements; a new index is formed where index position equates 1 and the prior index is 0.
#'
#' @usage DateVectoriser(dat, start, end, lag, zero.index)
#' 
#' @param dat a data.table containing input data (see details)
#' @param start date variable in the dataset representing the start of an observation
#' @param end date variable in the dataset representing the end of an observation
#' @param lag specify the lag days; default is 1
#' @param zero.index specify whether the indexing sequence should start at 0 instead of 1; default is FALSE
#' 
#' @return Returns a sequential vector of the same length as input x 
#' 
#' @export DateVectoriser

DateVectoriser <- function(dat, start, end, lag, zero.index){

 start <- dat[, eval(start)]
 end <- dat[, eval(end)]
 
 interval <- interval(start - lag, end + lag)
  
 vec <- Map(function(start, end){
  return(+(start %within% interval | end %within% interval))
 }, 
 start = start, 
 end = end)
 
 starting <- if(zero.index){
   0L
 } else {
   1L
 }

 encoding <- sapply(1:length(vec), function(x) {
  
  seq <- vec[[x]]
  
  return(
   if(x == 1L){ 
    starting
   } else { 
    if(any(seq[1:x-1] == 1)){ 
     0L
    } else { 
     1L
    }
   })
  
 })
 
 return(cumsum(encoding))
 
}