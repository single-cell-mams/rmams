# **ONG Class ####
#' @description Stores observation Neighborhood Graph class
#' @slot id 
#' @slot dataset_id 
#' @slot edge_metric 
#' @slot metric_type 
#' @keywords internal
#' @noRd
setClass(
  "ONG",
  slots = list(
    id = "character",
    dataset_id = "character",
    filepath = "character",
    accessor = "character",
    parent_id = "character",
    record_id = "character",
    edge_metric = "character",
    metric_type = "character"
  )
)

# Getter and setter functions 
setMethod("id", "ONG", function(x) x@id)
setMethod("id<-", "ONG", function(x, value) {
  x@id <- value
  x
})

setMethod("dataset_id", "ONG", function(x) x@dataset_id)
setMethod("dataset_id<-", "ONG", function(x, value) {
  x@dataset_id <- value
  x
})

setMethod("filepath", "ONG", function(x) x@filepath)
setMethod("filepath<-", "ONG", function(x, value) { 
  x@filepath <- value
  x 
})

setMethod("accessor", "ONG", function(x) x@accessor)
setMethod("accessor<-", "ONG", function(x, value) { 
  x@accessor <- value
  x 
})
setMethod("parent_id", "ONG", function(x) x@parent_id)
setMethod("parent_id<-", "ONG", function(x, value) {
  x@parent_id <- value
  x
})


setMethod("record_id", "ONG", function(x) x@record_id)
setMethod("record_id<-", "ONG", function(x, value) {
  x@record_id <- value
  x
})

setMethod("edge_metric", "ONG", function(x) x@edge_metric)
setMethod("edge_metric<-", "ONG", function(x, value) {
  x@edge_metric <- value
  x
})


setMethod("metric_type", "ONG", function(x) x@metric_type)
setMethod("metric_type<-", "ONG", function(x, value) {
  x@metric_type <- value
  x
})

#Create object function 
create_ONG_Object <- function(
    id = NA_character_,
    dataset_id = NA_character_,
    filepath = NA_character_,
    accessor = NA_character_,
    parent_id = NA_character_,
    record_id = NA_character_,
    edge_metric = NA_character_,
    metric_type = NA_character_
) {
  obj <- new("ONG",
             id = id,
             dataset_id = dataset_id,
             filepath = filepath,
             accessor = accessor,
             parent_id = parent_id,
             record_id = record_id,
             edge_metric = edge_metric,
             metric_type = metric_type
  )
  return(obj)
}

