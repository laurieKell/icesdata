#' Create a proper FLIndex for SAM assessment
#'
#' @description Creates an FLIndex object suitable for use with FLR_SAM.
#' This function properly structures survey index data for SAM assessment.
#'
#' @param stock FLStock object - used to extract dimensions and biological parameters
#' @param index_values FLQuant - survey index values (age x year). If NULL, creates dummy values from stock.n
#' @param name character - name for the index
#' @param index_q FLQuant - optional catchability values (default: constant 0.1)
#' @param sel_pattern FLQuant - optional selectivity pattern (default: constant 1.0)
#'
#' @return FLIndex object properly structured for SAM
#'
#' @export
samIndex<-function(stock, 
                             index_values = NULL,
                             name = "Survey",
                             index_q = NULL,
                             sel_pattern = NULL) {
  
  # Get dimensions from stock
  ages=dimnames(stock.n(stock))$age
  years=dimnames(stock.n(stock))$year
  iters=dims(stock)$iter
  
  # Create index values if not provided
  if (is.null(index_values)) {
    # Use stock numbers scaled down as a simple proxy
    index_values=stock.n(stock) * 0.1
    message("Warning: Using scaled stock.n as index values. Replace with actual survey data.")
  }
  
  # Ensure dimensions match
  if (!identical(dimnames(index_values)$age, ages) || 
      !identical(dimnames(index_values)$year, years)) {
    stop("index_values dimensions must match stock age and year dimensions")
  }
  
  # Create catchability if not provided
  if (is.null(index_q)) {
    index_q=FLQuant(0.1, dimnames = list(age = ages, year = years, iter = seq(iters)))
    if (iters > 1 && dims(index_q)$iter == 1) {
      index_q=propagate(index_q, iters)
    }
  }
  
  # Create selectivity pattern if not provided
  if (is.null(sel_pattern)) {
    sel_pattern=FLQuant(1.0, dimnames = list(age = ages, year = years, iter = seq(iters)))
    if (iters > 1 && dims(sel_pattern)$iter == 1) {
      sel_pattern=propagate(sel_pattern, iters)
    }
  }
  
  # Get age range
  ageMin=min(as.numeric(ages))
  ageMax=max(as.numeric(ages))
  
  # Create FLIndex with proper structure
  # Note: FLIndex doesn't have stock.wt slot - that's only in FLStock
  idx=FLIndex(
    name = name,
    index = index_values,  # This is the key slot SAM uses
    index.q = index_q,
    sel.pattern = sel_pattern,
    # Set proper range
    range = c(
      min = ageMin,
      max = ageMax,
      minfbar = ageMin,
      maxfbar = ageMax,
      startf = 0.5,  # Survey timing (mid-year)
      endf = 0.5
    )
  )
  
  # Ensure iterations match
  if (iters > 1 && dims(idx)$iter == 1) {
    idx=propagate(idx, iters)
  } else if (iters == 1 && dims(idx)$iter > 1) {
    idx=iter(idx, 1)
  }
  
  return(idx)
}


#' Validate FLIndex structure for SAM
#'
#' @param idx FLIndex object to validate
#' @return Logical - TRUE if valid, stops with error message if invalid
#'
#' @export
validateSamIndex<-function(idx) {
  
  if (!inherits(idx, "FLIndex") && !inherits(idx, "FLIndices")) {
    stop("idx must be FLIndex or FLIndices")
  }
  
  # Handle FLIndices
  if (inherits(idx, "FLIndices")) {
    for (i in seq_along(idx)) {
      validateSamIndex(idx[[i]])
    }
    return(TRUE)
  }
  
  # Check required slot
  if (is.null(index(idx))) {
    stop("FLIndex must have 'index' slot with survey index values")
  }
  
  # Check for NA/NaN values
  na_count=sum(is.na(index(idx)) | is.nan(index(idx)))
  if (na_count > 0) {
    total=length(index(idx))
    pct=round(100 * na_count / total, 2)
    warning("Index contains ", na_count, " NA/NaN values (", pct, "%)\n",
            "  This may cause SAM fitting to fail.")
  }
  
  # Check for zero/negative values (might be OK but warn)
  zero_count=sum(index(idx) <= 0, na.rm = TRUE)
  if (zero_count > 0) {
    warning("Index contains ", zero_count, " zero or negative values\n",
            "  SAM typically expects positive survey index values.")
  }
  
  # Check range is set
  if (is.na(range(idx)["min"]) || is.na(range(idx)["max"])) {
    warning("FLIndex range (min/max ages) not properly set")
  }
  
  return(TRUE)
}

