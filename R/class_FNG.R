# **FNG Class ####
#' @description Stores Feature Neighborhood Graph class
#' @slot id 
#' @slot dataset_id 
#' @slot filepath
#' @slot accessor
#' @slot edge_metric 
#' @slot metric_type 
#' @keywords internal
#' @noRd
setClass(
  "FNG",
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
setMethod("id", "FNG", function(x) x@id)
setMethod("id<-", "FNG", function(x, value) {
  x@id <- value
  x
})

setMethod("dataset_id", "FNG", function(x) x@dataset_id)
setMethod("dataset_id<-", "FNG", function(x, value) {
  x@dataset_id <- value
  x
})

setMethod("filepath", "FNG", function(x) x@filepath)
setMethod("filepath<-", "FNG", function(x, value) { x@filepath <- value; x })

setMethod("accessor", "FNG", function(x) x@accessor)
setMethod("accessor<-", "FNG", function(x, value) { x@accessor <- value; x })

setMethod("parent_id", "FNG", function(x) x@parent_id)
setMethod("parent_id<-", "FNG", function(x, value) {
  x@parent_id <- value
  x
})

setMethod("record_id", "FNG", function(x) x@record_id)
setMethod("record_id<-", "FNG", function(x, value) {
  x@record_id <- value
  x
})

setMethod("edge_metric", "FNG", function(x) x@edge_metric)
setMethod("edge_metric<-", "FNG", function(x, value) {
  x@edge_metric <- value
  x
})

setMethod("metric_type", "FNG", function(x) x@metric_type)
setMethod("metric_type<-", "FNG", function(x, value) {
  x@metric_type <- value
  x
})

#Create object function 
create_FNG_object <- function(
    id = NA_character_,
    dataset_id = NA_character_,
    filepath = NA_character_,
    accessor = NA_character_,
    parent_id = NA_character_,
    record_id = NA_character_,
    edge_metric = NA_character_,
    metric_type = NA_character_
) {
  obj <- new("FNG",
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
