#' Define the OID (observation ID) S4 object
#' @title class OID
#' @description Stores Observation ID class
#' 
#' @importFrom methods is new slot slot<- slotNames
#' 
#' @slot id character
#' @slot dataset_id character
#' @slot filepath character
#' @slot accessor character
#'
#' @return the OID class
#' @export
#'
#'

setClass("OID", slots = list(id = "CharOrNULL",
                             dataset_id = "CharOrNULL",                             
                             filepath = "CharOrNULL",
                             accessor = "CharOrNULL"))

#' id
#' @description setter
#' @rdname id-OID-get
#' @param x OID object
#' @return value
#' @export
setMethod("id", signature("OID"), function(x) x@id)
#' id<-
#' @description setter
#' @rdname id-OID-set
#' @param x OID object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("id<-", signature("OID"), function(x, value) {
    x@id <- value
    x
})
#' dataset_id
#' @description getter
#' @rdname dataset_id-OID-get
#' @param x OID object
#' @return value
#' @export
setMethod("dataset_id", signature("OID"), function(x) x@dataset_id)
#' dataset_id<-
#' @description setter
#' @rdname dataset_id-OID-set
#' @param x OID object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("dataset_id<-", signature("OID"), function(x, value) {
    x@dataset_id <- value
    x
})
#' filepath
#' @description getter
#' @rdname filepath-OID-get
#' @param x OID object
#' @return value
#' @export
setMethod("filepath", "OID", function(x) x@filepath)
#' filepath<-
#' @description setter
#' @rdname filepath-OID-set
#' @param x OID object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("filepath<-", "OID", function(x, value) { 
  x@filepath <- value
  x 
})
#' accessor
#' @description getter
#' @rdname accessor-OID-get
#' @param x OID object
#' @return value
#' @export
setMethod("accessor", "OID", function(x) x@accessor)
#' accessor<-
#' @description setter
#' @rdname accessor-OID-set
#' @param x OID object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("accessor<-", "OID", function(x, value) { 
  x@accessor <- value
  x 
})

# constructor for the OID S4 object

#' Constructor for the OID S4 object
#' @description Creates the OID object and populates its subfields
#' @param id character
#' @param dataset_id character
#' @param filepath character
#' @param accessor character
#' 
#' @return a OID S4 object for use with MAMS
#' @export

create_OID_object <- function(id = NA_character_, 
                              dataset_id = NA_character_,
                              filepath = NA_character_,
                              accessor = NA_character_) {
    obj <- new("OID", 
               id = id, 
               dataset_id = dataset_id, 
               filepath = filepath,
               accessor = accessor)
    return(obj)
}


# collapse function to sub object
setMethod("collapse_to_list", "OID", function(x) {
  collapsed_list <- mapply(function(s) slot(x, s),
                           slotNames(x),
                           SIMPLIFY = FALSE)
  # Remove NULL values
  collapsed_list <- Filter(function(y) !is.null(y), collapsed_list)
  return(collapsed_list)
})

