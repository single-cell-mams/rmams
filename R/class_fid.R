# Define the FID S4 object

setClass("FID", slots = list(id = "character",
                             dataset_id = "character",
                             fid_header = "character",
                             fid_header_delim = "character"))

setMethod("id", signature("FID"), function(x) {
    paste0(x@id)
})

setMethod("id<-", signature("FID"), function(x, value) {
    x@id <- value
    x
})

setMethod("dataset_id", signature("FID"), function(x) {
    paste0(x@dataset_id)
})

setMethod("dataset_id<-", signature("FID"), function(x, value) {
    x@dataset_id <- value
    x
})

setMethod("fid_header", signature("FID"), function(x) {
    paste0(x$fid_header)
})

setMethod("fid_header<-", signature("FID"), function(x, value) {
    x@fid_header <- value
    x
})

setMethod("fid_header_delim", signature("FID"), function(x) {
    paste0(x$fid_header_delim)
})

setMethod("fid_header_delim<-", signature("FID"), function(x, value) {
    x@fid_header_delim <- value
    x
})

# constructor for the FID S4 object

create_FID_object <- function(id = NA_character_, 
                              dataset_id = NA_character_, 
                              fid_header = NA_character_, 
                              fid_header_delim = NA_character_) {
    obj <- new("FID", 
               id = id, 
               dataset_id = dataset_id, 
               fid_header = fid_header, 
               fid_header_delim = fid_header_delim)
    return(obj)
}