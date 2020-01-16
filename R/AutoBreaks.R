#' Automatic Breaks for Continuous Data
#'
#' \code{AutoBreaks} returns a character vector of the same length as the input numeric or integer vector x.
#'                   Each value of x is replaced by a label matching the break parameters. Break parameters can either be fixed, defined, or split.
#'
#' @param x input numeric or integer vector
#' @param by numeric breaks to be applied. Can take three types of breaks: i) Fixed, ii) Defined, or iii) Split. See 'Details'.
#' @param ... Set decimal place for integers. Default is 2. Can provide value only. Optional.
#'
#' @details The values for paremeter \code{by} can be supplied in 1 of 3 ways:
#' \itemize{
#' \item Fixed Breaks: supply a single value. This feature automatically sets break points by value suppled starting from the minimum value.
#' \item Defined Breaks: supply a sequnce of values in ascending order. User defines break points and the number of categories.
#' \item Split Breaks: supply two of same values. This feature will automatically split all values into one of the two categories.
#' }
#'
#' @return returns a character vector of the same length as input numeric or integer vector. Missing values written as NA.
#'
#' @seealso \code{\link{FreqTable}} utilised this function when users attempt to create a frequency table using continous variables.
#' @examples
#' # Example Data
#' norm.int <- runif(1000, 0, 1)
#' cont.int <- runif(1000, 1, 15)
#' rand.num <- sample(20:84, 100, replace = T)
#'
#' # Fixed breaks (takes a single value):
#' table(AutoBreaks(rand.num, by = 10))
#' table(AutoBreaks(cont.int, 3))
#' table(AutoBreaks(norm.int, 0.2))
#'
#' # Defined breaks (user-defined breaks):
#' # NB: Starting value can be 0 if required
#' table(AutoBreaks(rand.num, by = c(30,45,65)))
#' table(AutoBreaks(cont.int, c(2,3,4,5)))
#' table(AutoBreaks(norm.int, c(0,0.8)))
#'
#' # Split break
#' table(AutoBreaks(rand.num, c(55, 55)))
#' table(AutoBreaks(norm.int, by = c(0.8, 0.8), 6)) # can set decimal place
#'
#' # Can set decimal place
#' table(AutoBreaks(cont.int, c(3,4,5), dp=4))
#' table(AutoBreaks(norm.int, c(0.4,0.6,0.8), 5))

# --- FUN ----
AutoBreaks <- function(x, by, ...){

   dat <- as.character(x)

   # Parameters
   dp <- if(length(list(...))==0){2
   }else{
      list(...)[[1]]}

   x <- round(x, dp)

   min.x <- min(x, na.rm = T)
   max.x <- max(x, na.rm = T)

   interv.value <- unique(by)
   interv.fixed <- length(by)==1

   unit <- if(all(x%%1==0)){
      1
   } else {
      1/(10 ^ dp)
   }

   band.start <- if(interv.fixed){
      seq(from = min.x, to = max.x, by = interv.value)
   } else {
      c(min.x, interv.value)
   }

   band.finish <- if(interv.fixed){
      c(seq(from = min.x + (interv.value - unit), to = max.x, by = interv.value), max.x)
   } else {
      c(interv.value - unit, max.x)
   }

   remaind.head <- if(interv.fixed){
      FALSE
   } else {
      min.x < head(interv.value, 1)
   }

   remaind.tail <- if(interv.fixed){
      (max.x / interv.value) %% 1 != 0
   } else {
      max.x > tail(interv.value, 1)
   }

   # Function
   WithinInterval <- Vectorize(function(x, y, z){
      any(isTRUE(x >= y & x <= z),
          isTRUE(all.equal(x, y)),
          isTRUE(all.equal(x, z)))
   })

   # Evaluate
   for(i in seq_along(band.start)){

      index <- which(WithinInterval(x, band.start[i], band.finish[i]))

      label <- if(i==1 & remaind.head){
         paste0("<=", band.finish[1])
      } else if(i==length(band.finish) & remaind.tail){
         paste0(">=", band.start[i])
      } else {
         paste(band.start[i], band.finish[i], sep="-")
      }

      dat <- replace(dat, index, label)
   }

   return(dat)
}
