#' Class to create a FEA object
#'
#' @slot id character. 
#' @slot dataset_id character. 
#' @slot feature_name character. 
#' @slot reference_database character. 
#' @slot reference_organism character. 
#' @slot record_id character. 
#'
#' @return
#' @export
#'
#' @examples
setClass("FEA", slots = list(id = "character",
                             dataset_id = "character",
                             filepath = "character",
                             accessor = "character",
                             feature_name = "character",
                             reference_database = "character",
                             reference_organism = "character",
                             record_id = "character"))

create_FEA_object <- function(
    id = NA_character_,
    dataset_id = NA_character_,    
    filepath = NA_character_,
    accessor = NA_character_,
    feature_name = NA_character_,
    reference_database = NA_character_,
    reference_organism = NA_character_,
    record_id = NA_character_
) {
  obj <- new("FEA",
             id = id,
             dataset_id = dataset_id,
             accessor = accessor,
             data_type = data_type,
             feature_name = feature_name,
             reference_database = reference_database,
             reference_organism = reference_organism,
             record_id = record_id
  )
  
  return(obj)
}

setMethod("id", "FEA", function(x) x@id)
setMethod("id<-", "FEA", function(x, value) {
  x@id <- value
  x
})

setMethod("dataset_id", "FEA", function(x) x@dataset_id)
setMethod("dataset_id<-", "FEA", function(x, value) {
  x@dataset_id <- value
  x
})
setMethod("filepath", "FEA", function(x) x@filepath)
setMethod("filepath<-", "FEA", function(x, value) { 
  x@filepath <- value
  x 
})

setMethod("accessor", "FEA", function(x) x@accessor)
setMethod("accessor<-", "FEA", function(x, value) { 
  x@accessor <- value
  x 
})

setMethod("feature_name", "FEA", function(x) x@feature_name)
setMethod("feature_name<-", "FEA", function(x, value) {
  x@feature_name <- value
  x
})

setMethod("reference_database", "FEA", function(x) x@reference_database)
setMethod("reference_database<-", "FEA", function(x, value) {
  x@reference_database <- value
  x
})

setMethod("reference_organism", "FEA", function(x) x@reference_organism)
setMethod("reference_organism<-", "FEA", function(x, value) {
  x@reference_organism <- value
  x
})

setMethod("record_id", "FEA", function(x) x@record_id)
setMethod("record_id<-", "FEA", function(x, value) {
  x@record_id <- value
  x
})
