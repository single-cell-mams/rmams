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
    edge_metric = "character",
    metric_type = "character"
  )
)

# Getter and setter functions for id
setMethod("id", "ONG", function(x) x@id)
setMethod("id<-", "ONG", function(x, value) {
  x@id <- value
  x
})

# Getter and setter functions for dataset_id
setMethod("dataset_id", "ONG", function(x) x@dataset_id)
setMethod("dataset_id<-", "ONG", function(x, value) {
  x@dataset_id <- value
  x
})


# Getter and setter functions for edge_metric
setMethod("edge_metric", "ONG", function(x) x@edge_metric)
setMethod("edge_metric<-", "ONG", function(x, value) {
  x@edge_metric <- value
  x
})


# Getter and setter functions for metric_type
setMethod("metric_type", "ONG", function(x) x@metric_type)
setMethod("metric_type<-", "ONG", function(x, value) {
  x@metric_type <- value
  x
})

#Create object function 
create_ONG_Object <- function(
    id = NA_character_,
    dataset_id = NA_character_,
    edge_metric = NA_character_,
    metric_type = NA_character_
) {
  obj <- new("ONG",
             id = id,
             dataset_id = dataset_id,
             edge_metric = edge_metric,
             metric_type = metric_type
  )
  return(obj)
}

