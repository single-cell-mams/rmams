# Define the FID S4 object

#' @description Stores Feature ID class
#' @slot id character
#' @slot dataset_id character
#' @slot fid_header character
#' @slot fid_header_delim character
#' @slot filepath character
#' @slot accessor character

setClass("FID", slots = list(id = "character",
                             dataset_id = "character",
                             filepath = "character",
                             accessor = "character",
                             fid_header = "character",
                             fid_header_delim = "character",
                             filepath = "character",
                             accessor = "character"))

# constructor for the FID S4 object

create_FID_object <- function(id = NA_character_, 
                              dataset_id = NA_character_, 
                              filepath = NA_character_,
                              accessor = NA_character_,
                              fid_header = NA_character_, 
                              fid_header_delim = NA_character_,
                              filepath = NA_character_,
                              accessor = NA_character_) {
    obj <- new("FID", 
               id = id, 
               dataset_id = dataset_id,                
               filepath = filepath,
               accessor = accessor,
               fid_header = fid_header, 
               fid_header_delim = fid_header_delim,
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

setMethod("fid_header", signature("FID"), function(x) x@fid_header)
setMethod("fid_header<-", signature("FID"), function(x, value) {
    x@fid_header <- value
    x
})

setMethod("fid_header_delim", signature("FID"), function(x) x@fid_header_delim)
setMethod("fid_header_delim<-", signature("FID"), function(x, value) {
    x@fid_header_delim <- value
    x
})

setMethod("filepath", signature("FID"), function(x) x@filepath)
setMethod("filepath<-", signature("FID"), function(x, value) {
    x@filepath <- value
    x
})

setMethod("accessor", signature("FID"), function(x) x@accessor)
setMethod("accessor<-", signature("FID"), function(x, value) {
    x@accessor <- value
    x
})