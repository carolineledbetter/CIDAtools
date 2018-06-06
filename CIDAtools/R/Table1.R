#' Create a table one
#' 
#' 
#' This is a function created to provide characteristics of a study group with an option to stratify by some variable (usually an exposure) The output of this function is designed to be used with pander in rmarkdown, but all row name formatting can be removed with the option: emphasis = 'n'.
#' 
#' @param rowvars A vector of row positions or names.
#' @param colvariable The position or name of the varaible to stratify by, set to NULL for no stratification.
#' @param data the data frame or design object of the data you which to characterize
#' @param row_var_names An optional vector of row names to be used for variables. Must be the same length and in the same order as rowvars. 
#' @param incl_missing Set to T to include missing values
#' @param incl_pvalues Set to T to include p values (p values are only calculated on non missing observations)
#' @param emphasis Set to 's' for to indent categories for categorical values, 'b' to bold just variable names, and 'n' for no emphasis.
#' @param MedIQR optional vector of continuous variables to return median and IQR instead of mean and SD.
#' @return  a matrix with N and percentages for categorical variables and mean and sd for continuous ones. In addition, only the 2nd factor of binary categorical variables is displayed. All determinations of categorical, binary, or continuous are performed automatically based on factors. Only numeric(continuous) or factored(categorical) are permitted. Variables are displayed in the following order: binary, non-binary categorical, and continuous. If no continuous variable is provided, summary statistics on the entire sample are provided. No p-values can be provided in this case. If a design object is passed in lieu of a data frame, weighted numbers using the survey package are provided. (The survey package must be installed in this case.)
#' @keywords table1 tableone characteristic
#' @export
#' 


Table1 <- function(rowvars, colvariable, data, row_var_names = NULL, 
                   incl_missing = F, incl_pvalues = T, 
                   emphasis = c('s', 'b', 'n'), 
                   MedIQR = NULL) {
  # determing if data is a design object or data frame
  weighted <- F
  if (!is.data.frame(data)){
    classData <- class(data)
    if('survey.design' %in% classData) {
      if (!requireNamespace('survey', quietly = T)) {
        stop('Survey Package is required for weighted tables')
      }
      design <- data
      data <- design$variables
      weighted <- T
      # if (incl_missing == T) {
      #   warning('Missing is turned off for weighted tables')
      #   incl_missing <- F
      # }
    } else {
        stop('Data is not a data frame or design object')
      }
  }
  
  # do not include p_values if data is not stratified
  # setup dummy variable for unstratified data
  if (is.null(colvariable)) {
    incl_pvalues <- F
    data$dummy <- factor(rep('', nrow(data)))
    colvariable <- 'dummy'
    if (weighted == T){
      design$variables$dummy <- factor(rep('', nrow(design$variables)))
    }
  }

  # Warn users p_values are not calculated on missing obs
  if (incl_missing == T & incl_pvalues == T) {
    warning('P values are only calculated on non-missing observations')
  }
  
  #check that all arguments are valid 
  if (!is.atomic(rowvars)) stop("Please pass row variables as a vector")
  classes <- sapply(data[, rowvars], class)
  if (!all(classes %in% c('integer', 'factor', 'logical', 'double', 'numeric'))) 
    stop('Row variables must be numeric or factors')
  
  if(!is.null(MedIQR)) {
    if(!is.character(MedIQR)) stop('Median IQR requests must be variable names')
    classes <- sapply(data[, MedIQR, drop = F], class)
    if(!all(classes %in% c('integer', 'double', 'numeric')))
      stop('Median IQR requests must be continous variables')
  }
  
  if (weighted == T) {
    if (length(unique(design$variables[, colvariable])) > 20) {
      stop(paste0("Column Variable has more than 20 unique values,", 
                  "please pass a column variable with less", 
                  "than 20 unique values"))
    }
    
    if (!is.factor(design$variables[, colvariable])) {
      design$variables[, colvariable] <- 
            factor(design$variables[, colvariable])
      data[, colvariable] <- factor(design$variables[, colvariable])[0]
    }
  } 
  
  if (length(unique(data[, colvariable])) > 20) {
    stop(paste0("Column Variable has more than 20 unique values,", 
         "please pass a column variable with less than 20 unique values"))
  }
  
  if (!is.factor(data[, colvariable])) {
    data[, colvariable] <- factor(data[, colvariable])
  }
  
  if (!is.null(row_var_names) & length(rowvars) != length(row_var_names)){
    stop("Length of Row Variable Names is not equal to Row Variables")
  }
    
  if (length(unique(rowvars)) != length(rowvars)){
    stop('You may not pass duplicate row variables')
  }
  
  # set numeric colvariable and rownames to character names so they 
  # can be used in formula arguments also names will be used in table
  if (is.numeric(rowvars)){
    rowvars <- names(data)[rowvars]
  }
  
  if (is.numeric(colvariable))  colvariable <- names(data)[colvariable]

  #set column names and remove missing colvariable
  if (weighted == T) {
    Col_n <- survey::svytable(as.formula(paste0("~", colvariable)),
                      design, round = T)
  } else {
    Col_n <- table(data[, colvariable])
    data <- data[!is.na(data[, colvariable]), ]
  }
  
  p_str <- NULL
  if(incl_pvalues == T) p_str <- 'p_value'
  spacer <- ifelse(colvariable == 'dummy', '(N=', ' (n=')
  cnames <- c(paste0(levels(data[, colvariable]), spacer,  
                     format(Col_n, big.mark = ',', trim = T), 
                     ")"), p_str)
  
  #col dimensions
  col_dim <- length(levels(data[, colvariable]))
  
  # determine row types and names
  vartypes <- lapply(data[, rowvars], is.factor)
  catvars <- rowvars[vartypes == T]
               
  
  #add missing level for factors 
  if(incl_missing == T) {
    data[, catvars] <- lapply(data[, catvars, drop = F],
                                        addNA, ifany = T)
    data[, catvars] <- lapply(data[, catvars, drop = F], 
                              function(x){
      levels(x)[is.na(levels(x))] <- "Missing"
      x
    })
  }
  
  # set row name emphasis
  emphasis <- match.arg(emphasis)
  fxn <- function(i, title) {
    switch(emphasis, 
           s = c(title, paste0("\\  ",levels(data[,i]))), 
           b = c(paste0('**', title, '**'), levels(data[,i])), 
           n = c(title, levels(data[,i])))
  }
  
  # get number of levels for categorical variables and set rownames
  numlevels <- lapply(catvars, function(i) {length(levels(data[, i]))})
  
  binaryvars <- catvars[numlevels == 2]
  binarylabs <- sapply(binaryvars, function(i){
    title <- i
    lab <- fxn(i, title)[1:2]
    return(lab)
  })
  
  nonbinary <- catvars[!(numlevels == 2)]
  nonbinlab <- sapply(nonbinary, function(x){
    title <- x
    lab <- fxn(x, title)
    return(lab)
    })
  
  # continous variables 
  contvars <- rowvars[vartypes == F]
  contvars <- contvars[order(contvars %in% MedIQR)]
  continuous_labels <- contvars
  if(!length(MedIQR) == length(contvars) & !is.null(MedIQR))
    y <- min(which(contvars %in% MedIQR))
    continuous_labels <- c(contvars[1:(y-1)], ' ', 
                           contvars[y:length(contvars)])
  
  
  if(emphasis == 'b') {
    continuous_labels <- paste0('**', continuous_labels, '**')
  }
  
  # if missing are included add a line for the missing count
  if(incl_missing == T & length(contvars) != 0) {
    continuous_labels  <- unlist(
      sapply(1:length(contvars), function(x){
        if (sum(is.na(data[,contvars[x]])) >0){
          if(x >= y) x <- x+1
          emp <- ''
          # add slashes for indent if set
          if (emphasis == 's') emp <- '\\ '
          return(list(continuous_labels[x], 
                      paste0(emp, 'Missing N(%)')))
        }
        return(continuous_labels[x])
      }))
  }
  
  
  # put together all rownames
  rnames <- unlist(c(" ", binarylabs, nonbinlab," ",continuous_labels))
  
  #remove extra rows if no categorical/continous variables exist
  if (length(catvars) == 0) {
    rnames <- unlist(c(" ",continuous_labels))
  }
  if (length(contvars) == 0){
    rnames <- unlist(c(" ", binarylabs, nonbinlab))
  }
 
  # replace variable names with row variable names if they were provided
  if(!is.null(row_var_names)){
    tmp <- rowvars
    if (emphasis == 'b') {
      tmp <- paste0('**', rowvars, '**')
      row_var_names <- paste0('**', row_var_names, '**')
    }
    n <- match(tmp, rnames)
    rnames[n] <- row_var_names
  }
  
  RowCatWeighted <- function(){
    n <- survey::svytable(as.formula(paste0("~", var, ' + ', colvariable)), 
                          design, 
                          round = T, addNA = T)
    dimnames(n)[[1]][is.na(dimnames(n)[[1]])] <- 'Missing'
    p <- survey::svychisq(as.formula(paste0("~", var, ' + ', colvariable)), 
                          design, 
                          statistic = 'F')$p.value
    return(list(n = n, p = p))
  }
  
  RowCatUnweighted <- function(){
    n <- table(data[, var],data[, colvariable])
    p <- anova(glm(as.formula(paste0(colvariable, "~", var)), 
                   data = data, 
                   family = binomial()), test = 'LRT')$`Pr(>Chi)`[2]
    return(list(n = n, p = p))
  }

  # function to return rows for categorical variables
  returnRowCat <- function(var, r){
    levs <- length(levels(data[,var])) - r
    if (weighted){
      n <- survey::svytable(as.formula(paste0("~", var, ' + ', colvariable)), 
                            design, 
                            round = T, addNA = T)
      dimnames(n)[[1]][is.na(dimnames(n)[[1]])] <- 'Missing'
      p <- survey::svychisq(as.formula(paste0("~", var, ' + ', colvariable)), 
                            design, 
                            statistic = 'F')$p.value
      return(list(n = n, p = p))
    } else {
      n <- table(data[, var],data[, colvariable])
      p <- anova(glm(as.formula(paste0(colvariable, "~", var)), 
                     data = data, 
                     family = binomial()), test = 'LRT')$`Pr(>Chi)`[2]
    }
    pct <- round(prop.table(n, margin = 2) *100, 0)
    p <- ifelse(p < 0.01, '<0.01', sprintf('%.2f',p))
    repp <- levs
    if (!incl_pvalues) {
      p <- NULL
      repp <- 0
    }
    n_per <- cbind(matrix(paste0(format(n[1:levs,], big.mark = ',', trim = T), 
                                 "(", pct[1:levs,], ")"), nrow = levs, 
                          byrow = F), rep(" ", repp))
    returnRow <- rbind(c(rep(" ", col_dim), p), n_per)
    return(returnRow)
  }  

  # function to return continuous rows 
  returnRowContinuous <- function(var){
    # make table with mean and sd
    if (weighted == T){ 
      summ <- survey::svyby(formula = as.formula(paste0("~", var)),
                            by = as.formula(paste0("~", colvariable)), 
                            FUN = survey::svymean, design = design)
      # convert to same structure as unweighted summary
      summ <- matrix(c(summ[,2], summ[,3]), nrow = 2, byrow = T)
     } else {
      summ <- sapply(levels(data[, colvariable]), function(i) {
        mean <- mean(data[, var][data[, colvariable] == i], 
                   na.rm = T)
        sd <- sd(data[, var][data[, colvariable] == i], 
               na.rm = T)
        return(c(mean, sd))
      })
     }
    #round mean and sd appropriately
    if (abs(summ[2, 1]) >= 10){
      m_sd <- paste0(round(summ[1, ], digits = 0), "(", 
                     round(summ[2, ], digits = 0), ")")
    } else{
      if (abs(summ[2, 1]) >= 1){
        m_sd <- paste0(sprintf('%.1f', summ[1, ]), "(", 
                       sprintf('%.1f', summ[2, ]), ")")
      } else{
        if (abs(summ[2, 1]) >= 0.1){
          m_sd <- paste0(sprintf('%.2f', summ[1, ]), "(", 
                         sprintf('%.2f', summ[2, ]), ")")
        } else{
          if (abs(summ[2,1]) >= 0.01){
            m_sd <- paste0(sprintf('%.3f', summ[1, ]), "(", 
                           sprintf('%.3f', summ[2, ]), ")")
          }
          m_sd <- paste0(sprintf('%.2e', summ[1, ]), "(", 
                         sprintf('%.2e', summ[2, ]), ")")
        }}}
    summ <- m_sd
    if (var %in% MedIQR){
      if (weighted == T){
        summ <- survey::svyby(formula = as.formula(paste0("~", var)),
                              by = as.formula(paste0("~", colvariable)), 
                              FUN = survey::svyquantile, design = design, 
                              quantiles = c(0.5, 0.25, 0.75), keep.var = F)
        summ <- round(summ[2:4], 0)

      } else {
        summ <- aggregate(data[, var], by = list(data[, colvariable]), 
                          quantile, probs = c(0.5, 0.25, 0.75), na.rm = T)
        summ <- round(summ$x, 0)
      }
      summ <- paste0(summ[, 1], "(", summ[, 2], "-", summ[, 3], ")")
    }
    
    p <- NULL
    # return p-value if requested using anova
    if (incl_pvalues == T){
      if (weighted == T) {
        p <- summary(survey::svyglm(as.formula(paste0(colvariable, "~", var)),
                            design = design, 
                            family = 'quasibinomial'))$coefficients[2, 4]
      } else {
        p <- summary(aov(as.formula(paste0(var, "~", colvariable)), 
          data = data))[[1]][5][1, ]
      }
      p <- ifelse (p < 0.01, '<0.01', sprintf('%.2f', p))
    }
    
    returnRow <- matrix(c(summ, p), nrow = 1, byrow = T)
    
    # add row for missing if requested
    if (incl_missing == T & sum(is.na(data[, var])) > 0){
      N <- table(data[, colvariable][is.na(data[, var])])
      pct <- as.vector(round(
        (N/table(data[, colvariable])) * 100,0))
      spacer <- NULL
      if (incl_pvalues == T){
        spacer <- ' '
      }
      N_pct <- c(paste0(N[], '(', pct[], ")"), spacer)
      returnRow <- rbind(returnRow, N_pct)
    }
    return(returnRow)
  }
  
  #put together table
  rowheadercat <- NULL
  rowheadercont <- NULL
  cattable <- NULL
  conttable <- NULL
  if (length(catvars) != 0){
    cattable <- do.call(rbind, 
                        lapply(c(lapply(binaryvars, returnRowCat, r = 1), 
                                 lapply(nonbinary, returnRowCat, r = 0)), 
                               data.frame, stringsAsFactors=FALSE))
    names(cattable) <-  c(1:length(cattable))
    rowheadercat <- rep("N(%)", col_dim)
    if(incl_pvalues == T){
      rowheadercat <- c(rowheadercat, '')
    }
  }
  if (length(contvars) != 0){
    conttable <- do.call(rbind, 
                         lapply(lapply(contvars, returnRowContinuous), 
                                data.frame, stringsAsFactors=FALSE))
    names(conttable) <- c(1:length(conttable))
    add_p <- NULL
    if(incl_pvalues == T) add_p <- ''
    rowheadercont2 <- NULL
    if(length(MedIQR) == length(contvars)) {
      rowheadercont <- rep('Median(IQR)', col_dim)
      }
    rowheadercont <- rep('Mean(SD)', col_dim)
    if(!is.null(MedIQR)) {
      rowheadercont2 <- rep('Median(IQR)', col_dim)
      rowheadercont2 <- c(rowheadercont2, add_p)
    rowheadercont <- c(rowheadercont, add_p)
    }
    contvarsMed <- conttable[contvars %in% MedIQR, ]
    contvarsMSD <- conttable[!contvars %in% MedIQR, ]
  }
  
  
  finaltab <- as.matrix(rbind.data.frame(rowheadercat, 
                                         cattable, 
                                         rowheadercont, 
                                         contvarsMSD,
                                         rowheadercont2, 
                                         contvarsMed, 
                                         stringsAsFactors = F))


  #dimnames(finaltab) <- list(rnames, cnames)
  return(finaltab)
}



