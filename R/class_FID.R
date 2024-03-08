# Define the FID (feature ID) S4 object

#' @description Stores feature ID class
#' @slot id character
#' @slot dataset_id character
#' @slot filepath character
#' @slot accessor character
#' 
#' @return a FEA class for use with MAMS
#' @export
#' @noRd

setClass("FID", slots = list(id = "CharOrNULL",
                             dataset_id = "CharOrNULL",
                             filepath = "CharOrNULL",
                             accessor = "CharOrNULL"))

# constructor for the FID S4 object

#' Constructor for the FID S4 object
#' @description Creates the FID object and populates its subfields
#' @param id Parent ID
#' @param dataset_id Parent dataset ID
#' @param filepath Path to the data file
#' @param accessor Accessors used
#' 
#' @return a FID S4 object for use with MAMS
#' @export

create_FID_object <- function(id = NA_character_, 
                              dataset_id = NA_character_, 
                              filepath = NA_character_,
                              accessor = NA_character_) {
    obj <- new("FID", 
               id = id, 
               dataset_id = dataset_id,                
               filepath = filepath,
               accessor = accessor)
    return(obj)
}


setMethod("id", signature("FID"), function(x) x@id)
setMethod("id<-", signature("FID"), function(x, value) {
    x@id <- value
    x
})

setMethod("dataset_id", signature("FID"), function(x) x@dataset_id)
setMethod("dataset_id<-", signature("FID"), function(x, value) {
    x@dataset_id <- value
    x
})

setMethod("filepath", "FID", function(x) x@filepath)
setMethod("filepath<-", "FID", function(x, value) { 
  x@filepath <- value
  x 
})

setMethod("accessor", "FID", function(x) x@accessor)
setMethod("accessor<-", "FID", function(x, value) { 
  x@accessor <- value
  x 
})

# collapse function to sub object
setMethod("collapse_to_list", "FID", function(x) {
  collapsed_list <- mapply(function(s) slot(x, s),
                           slotNames(x),
                           SIMPLIFY = FALSE)
  # Remove NULL values
  collapsed_list <- Filter(function(y) !is.null(y), collapsed_list)
  return(collapsed_list)
})

