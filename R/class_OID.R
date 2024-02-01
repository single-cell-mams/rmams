# Define the OID S4 object

#' @description Stores Observation ID class
#' @slot id character
#' @slot dataset_id character
#' @slot oid_header character
#' @slot oid_header_delim character
#' @slot filepath character
#' @slot accessor character

setClass("OID", slots = list(id = "character",
                             dataset_id = "character",                             
                             filepath = "character",
                             accessor = "character",
                             oid_header = "character",
                             oid_header_delim = "character"))

setMethod("id", signature("OID"), function(x) x@id)
setMethod("id<-", signature("OID"), function(x, value) {
    x@id <- value
    x
})

setMethod("dataset_id", signature("OID"), function(x) x@dataset_id)
setMethod("dataset_id<-", signature("OID"), function(x, value) {
    x@dataset_id <- value
    x
})

setMethod("filepath", "OID", function(x) x@filepath)
setMethod("filepath<-", "OID", function(x, value) { 
  x@filepath <- value
  x 
})

setMethod("accessor", "OID", function(x) x@accessor)
setMethod("accessor<-", "OID", function(x, value) { 
  x@accessor <- value
  x 
})


setMethod("oid_header", signature("OID"), function(x) x@oid_header)
setMethod("oid_header<-", signature("OID"), function(x, value) {
    x@oid_header <- value
    x
})

setMethod("oid_header_delim", signature("OID"), function(x) x@oid_header_delim)
setMethod("oid_header_delim<-", signature("OID"), function(x, value) {
    x@oid_header_delim <- value
    x
})

setMethod("filepath", signature("OID"), function(x) x@filepath)
setMethod("filepath<-", signature("OID"), function(x, value) {
    x@filepath <- value
    x
})

setMethod("accessor", signature("OID"), function(x) x@accessor)
setMethod("accessor<-", signature("OID"), function(x, value) {
    x@accessor <- value
    x
})

# constructor for the OID S4 object

#' @param id
#' @param dataset_id
#' @param oid_header
#' @param oid_header_delim
#' @param filepath
#' @param accessor
#'
#' @return
#' @export

create_OID_object <- function(id = NA_character_, 
                              dataset_id = NA_character_,
                              filepath = NA_character_,
                              accessor = NA_character_,
                              oid_header = NA_character_, 
                              oid_header_delim = NA_character_,
                              filepath = NA_character_,
                              accessor = NA_character_) {
    obj <- new("OID", 
               id = id, 
               dataset_id = dataset_id, 
               filepath = filepath,
               accessor = accessor,
               oid_header = oid_header, 
               oid_header_delim = oid_header_delim,
               filepath = filepath,
               accessor = accessor)
    return(obj)
}



