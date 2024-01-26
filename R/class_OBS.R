#' Class to create a OBS object
#'
#' @slot id character. 
#' @slot dataset_id character. 
#' @slot record_id character. 
#'
#' @return
#' @export
#'
#' @examples
setClass("OBS", slots = list(id = "character",
                             dataset_id = "character",
                             record_id = "character"))

create_OBS_object <- function(
    id = NA_character_,
    dataset_id = NA_character_,
    record_id = NA_character_
) {
  obj <- new("FEA",
             id = id,
             dataset_id = dataset_id,
             record_id = record_id
  )
  
  return(obj)
}

setMethod("id", "OBS", function(x) x@id)
setMethod("id<-", "OBS", function(x, value) {
  x@id <- value
  x
})

setMethod("dataset_id", "OBS", function(x) x@dataset_id)
setMethod("dataset_id<-", "OBS", function(x, value) {
  x@dataset_id <- value
  x
})

setMethod("record_id", "OBS", function(x) x@record_id)
setMethod("record_id<-", "OBS", function(x, value) {
  x@record_id <- value
  x
})