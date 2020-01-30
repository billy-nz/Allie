#' Identify Nearest Date in a Timeseries Relative to an Index Timepoint
#' 
#' \code{FindNearestDate} returns a binary vector of the same length as input data. The value 1 indicates the vector or row position of the comparison
#'                        where the nearest data to the index date is true. Applies to each group if grouping implemented.
#'               
#' @usage FindNearestDate(index, comparison, mode, from, to)
#' 
#' @param index a index date representing a single timepoint
#' @param comparison a vector of timeseries dates to identify as the nearest one
#' @param mode specify search priority - use string "d" for "directional" or "n" for "nearest". See details.
#' @param from interval starting point - specify a negative value presenting days prior to index. See details.
#' @param to interval end point - specify a positive value presenting days after index. See details.
#' 
#' @details If dataset contains multiple individuals or groups, then grouping should be implemented. Further, the index timepoint must be the same for each 
#' individual or group. See examples.
#' \itemize{
#' \item Where \code{mode} is set to "d" or "directional", the function will prioritise earlier records by first looking back prior to index date and if no nearest comparison dates exist, then look forward beyond index date.
#' Where \code{mode} is set to "n" or "nearest", the function will look for the nearest comparison date within the entire interval. 
#' If there is a tie (e.g. two comparison dates of equal distance from index),then the first record is flagged.
#' \item The parameters \code{from} and \code{to} determine the upper and lower limits of the comparisons dates. 
#' The parameter \code{from} is supplied as a negative value to represents the number of days prior to index date that a comparison date is eligible; while 
#' the parameter \code{to} is supplied as a positive value to represent the number of days after the index date tgat a comparison date is eligible for flagging.
#' }
#' 
#' @return a binary vector of the same length as input. Missing values written as NA.
#' 
#' @examples 
#' # Example dataset
#' DATA <- data.frame(UID = c(rep("AAA", 5), rep("BBB", 5)),
#'                    visit_date = c(rep(as.Date("2016-01-31"), 5), rep(as.Date("2017-06-15"), 5)),
#'                    result_date = c(sort(sample(seq(as.Date('2015-06-01'), as.Date('2016-04-30'), by="day"), 5)),
#'                                    sort(sample(seq(as.Date('2016-10-01'), as.Date('2017-09-30'), by="day"), 5))),
#'                    stringsAsFactors = F)
#'                    
#' # Implement with data.table
#' setDT(DATA)
#' DATA[, by = UID
#'      , flag := FindNearestDate(index = visit_date, 
#'                                comparison = result_date, 
#'                                mode = "d",
#'                                from = -730
#'                                to = 548)]
#'                                
#' # Implement with dplyr
#' DATA %>% 
#'   group_by(UID) %>% 
#'   mutate(flag = FindNearestDate(index = visit_date, 
#'                                 comparison = result_date, 
#'                                 mode = "n",
#'                                 from = -730,
#'                                 to = 548))
#' 
#' # Implement with base R 
#' DATA$flag <- unlist(by(DATA, DATA$UID, function(x) {
#'   FindNearestDate(index = x$visit_date,
#'                   comparison = x$result_date,
#'                   mode = "n",
#'                   from = -730,
#'                   to = 548) }))

# --- FUN ----FindNearestDate (v2)
FindNearestDate <- function(index, comparison, mode, from, to){
 
  # Param Check
  param.dat <- deparse(substitute(dat))!=""
  
  params  <- c("index", "comparison", "mode", "from", "to")
  
  for(i in params){
    if(eval(substitute(missing(i)))) {
      stop(paste("Missing parameter(s):", sQuote(i)), call. = F)
    }
  }
  
 x <- as.Date(unique(index))
 y <- comparison
 
 if(tolower(mode) %in% c("n", "nearest")){
  
  diff      <- as.numeric(as.Date(y) - as.Date(x))
  diff.ran  <- diff[which(diff >= from & diff <= to)]
  
  if(length(diff.ran) == 0) {     # There are no results in range
   return(rep(0, length(diff)))
  }
  
  index <- +(abs(diff) %in% min(abs(diff.ran)))
  flag  <- +(diff == min(diff[index==1]))
  
 }
 
 if(tolower(mode) %in% c("d", "directional")){
  
  diff  <- as.numeric(as.Date(y) - as.Date(x))
  prior <- diff[which(diff <= 0 & diff >= from)]
  post  <- diff[which(diff > 0 & diff <= to)]
  
  flag <- if(length(prior) >= 1){ # There is at least 1 result prior to index
   +(diff == max(prior))
   
  } else if(length(post) >= 1) {  # There is at least 1 result post index
   +(diff == min(post))
   
  } else {                        # There are no results
   rep(0, length(diff))
   
  }
  
  if(sum(flag) > 1){      # In a tie, select first record and overwrite other(s) to 0.
   flag <- replace(flag, which(flag==1)[-1], 0)
  }
  
 }
 
 return(as.numeric(flag))
 
}

