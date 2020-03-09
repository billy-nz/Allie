#' General Parameter Checking for PredictRiskScores
#'
#' \code{ParamCheck} evaluates input parameters inherited from the parent.frame; to check for names, values, and validity and ensure that a risk calculation can be carried out.
#' The call is stopped completely when required arguments are not stated or when compulsary inputs are invalid.
#' Invalid values may still be calculated an \code{NA} returned with an error message indicating one or more problematic variables.
#'
#' @usage ParamCheck(input, vars, call, is.table)
#'
#' @param input list of all input values inherited from the function call
#' @param vars character vector of required parameters
#' @param call name of the function being called
#' @param is.table logical; whether a dataset is provided in the parent.frame
#' @details
#' \code{ParamCheck} identifies the following errors and invalid inputs:
#' \enumerate{
#'  \item missing arguments - operations stops
#'  \item missing input data - operation stops
#'  }
#' @return
#' \code{ParamCheck} returns warning messages if any invalid values are detected. In cases where input data is invalid, \code{ParamCheck} will
#' stop the function's operation and return an error message.
#' @author
#' Billy Wu (R Developer)
#'
ParamCheck <- function(input, vars, call, is.table){
 
 # 1.  Missing argument check
 if(is.table){
  input <- get("input", parent.frame())
 }
 
 if(!all(vars %in% names(input))) {
  stop(call. = F,
       paste("Missing parameter(s):",
             paste(setdiff(vars, names(input)),
                   collapse = ", ")))
 }
  
 # 2. Missing input check
 if(is.table){  # Dataset provided (but missing correct input columns)
  browser()
  dat       <- as.data.frame(get("dat", parent.frame()), row.names = NULL)
  input     <- input[!names(input) %in% c("dat", "lag")]
  colnames  <- as.vector(sapply(input, as.character))
  
  is.missing <- any(!colnames %in% names(dat))
  
  if(is.missing){
   to.check <- colnames[!input %in% names(dat)]
   stop(call. = F,
        paste("Check input(s) names:",
              paste(sQuote(to.check),
                    collapse = ", ")))
  }
  
  input[]  <- dat[, colnames]
  
 } else { # One-off calculator (but missing correct input values)
  browser()
  to.check <- names(input)[!sapply(input, class) %in% c("character", "numeric", "name", "logical", "Date")]
  
  if(length(to.check) > 0){
   stop(call. = F,
        paste("multiple input values detected for",
              paste(sQuote(to.check),
                    collapse = ", ")))
  }
  
 }
 
 # 3. Compulsary input check
 has.na <- sapply(vars,
                  function(x){
                   if(class(input[[x]]) == "name"){
                     browser()
                    input[[x]] <- as.character(input[[x]])
                   }
                   any(is.na(input[[x]]) | input[[x]] == "NA")
                  }, USE.NAMES = TRUE)
 
 if(any(has.na)){
  stop(call. = F,
       paste(paste(sQuote(names(which(has.na))),
                   collapse = ", "),
             "cannot contain NA! For acceptable values,", paste0("see ?", call)))
 }
}