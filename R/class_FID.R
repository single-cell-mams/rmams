#' Define the FID (feature ID) S4 object
#' @title class FID
#' @description Stores feature ID class
#' 
#' @importFrom methods is new slot slot<- slotNames
#' 
#' @slot id character
#' @slot dataset_id character
#' @slot filepath character
#' @slot accessor character
#' 
#' @return a FEA class for use with MAMS
#' @export

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

#' id
#' @description getter
#' @rdname id-FID-set
#' @param x FID object
#' @return the value
#' @export
setMethod("id", signature("FID"), function(x) x@id)
#' id<-
#' @description setter
#' @rdname id-FID-get
#' @param x FID object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("id<-", signature("FID"), function(x, value) {
    x@id <- value
    x
})
#' dataset_id
#' @description getter
#' @rdname dataset_id-FID-set
#' @param x FID object
#' @return the value
#' @export
setMethod("dataset_id", signature("FID"), function(x) x@dataset_id)
#' dataset_id<-
#' @description setter
#' @rdname dataset_id-FID-get
#' @param x FID object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("dataset_id<-", signature("FID"), function(x, value) {
    x@dataset_id <- value
    x
})
#' filepath
#' @description getter
#' @rdname filepath-FID-set
#' @param x FID object
#' @return the value
#' @export
setMethod("filepath", "FID", function(x) x@filepath)
#' filepath<-
#' @description setter
#' @rdname filepath-FID-get
#' @param x FID object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("filepath<-", "FID", function(x, value) { 
  x@filepath <- value
  x 
})
#' accessor
#' @description getter
#' @rdname accessor-FID-set
#' @param x FID object
#' @return the value
#' @export
setMethod("accessor", "FID", function(x) x@accessor)
#' accessor<-
#' @description setter
#' @rdname accessor-FID-get
#' @param x FID object
#' @param value value
#' @return nothing (setter)
#' @export
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

