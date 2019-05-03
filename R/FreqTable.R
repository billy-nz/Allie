#' Frequency Table for Multiple Variables
#'
#' \code{FreqTable} compiles a contingency table that allows for multiple comparison variables, against a reference variable. 
#'  Each variable supplied can contain multiple categories. If one or more continuous variables are supplied, users are prompted to label values using \code{AutoBreaks}.
#'  Users can relabel variable names, add percentages, add a totals margin, and change the order of reference variable categories. Users can easily copy the output contingency table to a spreadsheet.
#'
#' @param data input data as data.frame or data table
#' @param xvar single or multiple variables containing comparison categories. See 'Details'.
#' @param yvar a single variable containing reference categories
#' @param xlab User defined label(s) in the order of the comparison variables used in xvar. Optional. See 'Details'.
#' @param ylab User defined label for the reference variable used in yvar. Optional.
#' @param yord User defined ordering for cateogries in the reference variable. Optional. See 'Details'.
#' @param margin If TRUE, totals for each group are shown in the table's margin.
#' @param pcn  Show two types of percentages: "v" or "h". Optional. See 'Details'.
#' @param copy If TRUE, copies output to clipboard - enabling pasting to excel (using 'Ctrl + v'). Optional.
#' 
#' @details 
#' \itemize{
#' \item \code{xvar} can supple one or more variables. If multiple are used, then use format \code{c("var1", "var2", "var3")}. 
#' \item \code{xlab} number of labels supplied must match length and order of xvar. If multiple are used, then use format \code{c("name1", "name2", "name3")}.
#' \item \code{yord} number of labels supplied must number of levels in yvar. Missing can be ignored.
#' \item \code{pcn}  use "v" for vertical proportion which shows the \% of total across comparison group(s); else "h" for horizontal proportions which shows the \% of total reference groups. Default is NULL.
#' \item If numeric or integer variables are supplied, then user will be prompt with \code{AutoBreaks} which automatically labels a numeric data as character strings.
#' }
#' 
#' @return Returns a data.frame with a contingency table structure. Missing values written as NA. Can be copied to excel using the argument \code{copy=TRUE}
#'
#' @export
#' @examples
#' FreqTable(data = SAMPLE, 
#'           xvar = c("en_nzdep_q", "en_eth","hx_broad_cvd"), 
#'           yvar = "gender",
#'           xlab = c("DEPRIVATION", "ETHNICITY", "HISTORY CVD"),
#'           ylab = "SEX",
#'           yord = c("M", "F"),
#'           margin = T,
#'           pcn = "v",
#'           copy = T)
#'
#' FreqTable(data = SAMPLE,
#'           xvar = c("index_age", "cvd_policyrisk5y_score"),
#'           yvar = "gender",
#'           pcn ="h")
#'           
   
# ---FUN---
FreqTable <- function(data, xvar, yvar, xlab, ylab, margin, pcn, yord, copy){
   
   # -- 1.   Parameter Check ----
   args <- as.list(match.call()[-1])
   
   if(!"data" %in% names(args) ){
      stop(paste("Data required: Accepts data.frame or data.table!"))
   } else {
      data <- as.data.frame(data)
   }
   
   if(!"xvar" %in% names(args)){
      stop(paste("Variable required: At least one subgroup required!"))
   } else {
      xvar <- xvar
   }
   
   if(length(args$yvar)>1){
      stop(paste("Only 1 variable can be used in yvar!"))
   }
   
   if(!"yvar" %in% names(args)){
      yvar <- FALSE
   }
   
   if(!"margin" %in% names(args)){
      margin <- FALSE
   }
   
   if(!"pcn" %in% names(args)){
      pcn <- FALSE
   } 
   
   if(!pcn %in% c("v", "h", FALSE)){
      stop(paste("Please use 'v' (vertical %), 'h' (horizontal %), FALSE, or omit argument (hide %)"))
   }
   
   if(!"yord" %in% names(args)){
      yord <- FALSE
   } 
   
   if(!"copy" %in% names(args)){
      copy <- FALSE
   }
   
   if(!"xlab" %in% names(args)){
      xlab <- FALSE
   } 
   
   if(!"ylab" %in% names(args)){
      ylab <- FALSE
   } 
   
   # --- 1b.   Continuous Variable Management ----
   for(var in xvar){
       
      class.type <- eval(substitute(class(data[,var])))
      
      if(class.type %in% c("numeric", "integer")){
         level.count <- eval(substitute(length(unique(data[,var]))))
         
         if(level.count>20){
            ans <- readline(prompt = paste(var, "is a continuous variable!! Enter interval value (eg. 5), specify starting values (eg. c(20, 45, 65)), or Q to quit:  "))
            
            if(ans=="Q"){
               stop(paste("Exiting process - classification of continous variable(s) required!!"))
            } else {
               by <- as.numeric(eval(parse(text=ans)))
               # browser()
               data[,var] <- eval(substitute(AutoBreaks(data[,var], by)))
            }
         }
      } 
      
      if(class.type=="Date"){
         data[,var] <- eval(substitute(format(data[,var], "%Y")))
      }
      
   }
   
   # -- 2.  Data for all xvars ----
   for(row.var in xvar){
      
      t <- if(yvar==FALSE){
         as.matrix(eval(substitute(table(data[,row.var], exclude=NULL))))
      } else {
         eval(substitute(table(data[,row.var], data[,yvar], exclude=NULL)))
      }
      
      freq.i <- cbind(data.frame(Subgroup = row.names(t)), 
                      as.data.frame.matrix(t))
      
      # Capture total margin
      # nb: if yvar is empty, then there is only one column :. omit this step!
      if(yvar!=FALSE){
         freq.i$Total <- apply(freq.i[,-1],1,sum)
      }
      
      # Label / Spacing
      label.i <- as.data.frame(matrix(ncol=length(freq.i), nrow=1))
      label.i[1,] <- c(paste0(row.var), rep("", length(freq.i)-1))
      names(label.i) <- names(freq.i)
      
      freq.i <- rbind(label.i, freq.i)
      
      names(freq.i)[1] <- "Subgroup"
      
      # Merge to other xvars in loop
      freq.tbl <- if(row.var==xvar[1]){
         freq.i
      } else {
         rbind(freq.tbl, freq.i)
      }
      
      row.names(freq.tbl) <- NULL
   }
   
   # -- 3.  Column Names, labels, & top row ----
   
   # Set Col names
   if(yvar==FALSE){
      names(freq.tbl) <- c("Subgroup", "Total")
   } else {
      names(freq.tbl) <- c("Subgroup", names(freq.tbl)[-1])
   }
   
   # Add top row total
   if(yvar==FALSE){
      
      margin.0 <- data.frame(Subgroup = paste0("n="), 
                             Total = nrow(data))
      
   } else {
      yvar.vals <- as.data.frame.matrix(t(eval(substitute(table(data[,yvar], exclude=NULL)))))
      margin.0  <- cbind(data.frame("Subgroup"= paste0(yvar)), yvar.vals)
      
      margin.0$Total  <- apply(margin.0[,-1],1,sum)
      names(margin.0) <- names(freq.tbl)
      
   }
   
   # Reorder yvar if required
   freq.tbl <- rbind(margin.0, freq.tbl)
   
   if(any(yord!=FALSE)){
      
      if(!all(yord %in% names(freq.tbl))){
         stop(paste("The value", paste(yord[!yord %in% names(freq.tbl)], collapse=" & "), "not found in", yvar, "!"))
      }
      
      if("NA" %in% names(freq.tbl)) {
         new.order <- setdiff(yord, "NA")
         freq.tbl <- freq.tbl[, c(1, match(new.order, names(freq.tbl)), c(length(freq.tbl)-1, length(freq.tbl)))]
      } else {
         
         new.order <- setdiff(yord, "NA")
         freq.tbl <- freq.tbl[, c(1, match(new.order, names(freq.tbl)), length(freq.tbl))]
      }
      
   } 
   
   # xlabels / ylabel
   if(any(xlab!=FALSE)){
      freq.tbl$Subgroup <- as.character(freq.tbl$Subgroup)
  
      if(length(xvar)!=length(xlab)){
         stop(paste("Error: Number of x-labels and x-variables do not match!!"))
         suppressWarnings()
      } else {
         freq.tbl$Subgroup[which(freq.tbl$Subgroup %in% xvar)] <- xlab
      }
      
   }
   
   if(ylab!=FALSE){
      freq.tbl$Subgroup <- as.character(freq.tbl$Subgroup)
      freq.tbl$Subgroup[which(freq.tbl$Subgroup %in% yvar)] <- ylab
   }

   
   # -- 4.  Basic Output ----
   if(margin==TRUE & pcn==FALSE){ # With margin
      
      if(copy==TRUE){
         write.table(freq.tbl,"clipboard-16384",sep="\t",row.names=FALSE,col.names=TRUE)
      } 
      return(as.data.frame(freq.tbl))
   }
   
   
   if(margin==FALSE & pcn==FALSE){ # Without margin
      
      if(yvar!=FALSE){ 
         freq.tbl$Total <- NULL 
      }
      
      if(copy==TRUE){
         write.table(freq.tbl,"clipboard-16384",sep="\t",row.names=FALSE,col.names=TRUE)
      } 
      
      return(as.data.frame(freq.tbl))
   }
   
   # -- 5.  Percentage Output ----
   # v = vertical, h=horizontal
   
   if(yvar==FALSE){
      pcn.tbl <- freq.tbl
      pcn.tbl$Total <- as.numeric(pcn.tbl$Total)
      p.val <- round(pcn.tbl$Total/pcn.tbl$Total[1]*100, 2)
      p.char <- as.character(ifelse(is.na(p.val), "", paste0(" (", p.val, "%)")))
      fpcn.tbl <- paste0(pcn.tbl$Total, p.char)
      
   } else {
      
      pcn.tbl <- as.data.frame(lapply(freq.tbl[,-1], as.numeric))
      names(pcn.tbl) <- names(freq.tbl)[-1]
      
      if(pcn=="v"){ # Vertical % 
         fpcn.tbl <- t(apply(pcn.tbl, 1, 
                             function(x){
                                p.val <- x/(pcn.tbl[1,])
                                p.val <- round(p.val*100, 1)
                                p.char <- as.character(ifelse(is.na(p.val), "", paste0(" (", p.val, "%)")))
                                paste0(x, p.char)}))
      } else { # horizontal %
         fpcn.tbl <- apply(pcn.tbl, 2, 
                           function(x){
                              p.val <- x/(pcn.tbl$Total)
                              p.val <- round(p.val*100, 1)
                              p.char <- as.character(ifelse(is.na(p.val), "", paste0(" (", p.val, "%)")))
                              paste0(x, p.char)})
      }
      
   }
   
   # Re-combine
   fpcn.tbl <- as.data.frame(fpcn.tbl)
   fpcn.tbl <- as.data.frame(cbind(freq.tbl[,1,drop=F], fpcn.tbl))
   
   names(fpcn.tbl) <- names(freq.tbl)
   
   if(yvar==FALSE){
      
      fpcn.tbl$Total <- gsub("NA", "", fpcn.tbl$Total)
      return(fpcn.tbl)
      
   } else{
      
      fpcn.tbl[,-1] <- lapply(fpcn.tbl[,-1], 
                              function(x)
                                 gsub("NA", "", x)) 
   }
   
   # Output
   if(margin==FALSE){
      fpcn.tbl$Total <- NULL
   } 
   
   if(copy==TRUE){
      write.table(fpcn.tbl,"clipboard-16384",sep="\t",row.names=FALSE,col.names=TRUE, quote = F)
   } 
   
   return(fpcn.tbl)
   
}