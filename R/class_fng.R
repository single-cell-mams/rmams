# **FNG Class ####
#' @description Stores Feature Neighborhood Graph class
#' @slot id 
#' @slot dataset_id 
#' @slot edge_metric 
#' @slot metric_type 
#' @keywords internal
#' @noRd
setClass(
  "FNG",
  slots = list(
    id = "character",
    dataset_id = "character",
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
create_FNG_Object <- function(
    id = NA_character_,
    dataset_id = NA_character_,
    edge_metric = NA_character_,
    metric_type = NA_character_
) {
  obj <- new("FNG",
             id = id,
             dataset_id = dataset_id,
             edge_metric = edge_metric,
             metric_type = metric_type
  )
  return(obj)
}
