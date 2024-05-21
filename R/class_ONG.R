#' Define the ONG (observation neighborhood graph) S4 object
#' @title class ONG
#' @description Stores observation neighborhood graph data
#' 
#' @importFrom methods is new slot slot<- slotNames
#' 
#' @slot id character
#' @slot dataset_id character
#' @slot filepath character
#' @slot accessor character
#' @slot parent_id character
#' @slot record_id character
#' @slot edge_metric character
#' @slot metric_type character
#'
#' @return the ONG class for use with MAMS
#' @export


setClass(
  "ONG",
  slots = list(
    id = "CharOrNULL",
    dataset_id = "CharOrNULL",
    filepath = "CharOrNULL",
    accessor = "CharOrNULL",
    parent_id = "CharOrList",
    record_id = "CharOrNULL",
    edge_metric = "CharOrNULL",
    metric_type = "CharOrNULL"
  )
)

# Getter and setter functions 
#' id
#' @description getter
#' @rdname id-ONG-get
#' @param x ONG object
#' @return value
#' @export
setMethod("id", "ONG", function(x) x@id)
#' id<-
#' @description setter
#' @rdname id-ONG-set
#' @param x ONG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("id<-", "ONG", function(x, value) {
  x@id <- value
  x
})
#' dataset_id
#' @description getter
#' @rdname dataset_id-ONG-get
#' @param x ONG object
#' @return value
#' @export
setMethod("dataset_id", "ONG", function(x) x@dataset_id)
#' dataset_id<-
#' @description setter
#' @rdname dataset_id-ONG-set
#' @param x ONG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("dataset_id<-", "ONG", function(x, value) {
  x@dataset_id <- value
  x
})
#' filepath
#' @description getter
#' @rdname filepath-ONG-get
#' @param x ONG object
#' @return value
#' @export
setMethod("filepath", "ONG", function(x) x@filepath)
#' filepath<-
#' @description setter
#' @rdname filepath-ONG-set
#' @param x ONG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("filepath<-", "ONG", function(x, value) { 
  x@filepath <- value
  x 
})
#' accessor
#' @description getter
#' @rdname accessor-ONG-get
#' @param x ONG object
#' @return value
#' @export
setMethod("accessor", "ONG", function(x) x@accessor)
#' accessor<-
#' @description setter
#' @rdname accessor-ONG-set
#' @param x ONG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("accessor<-", "ONG", function(x, value) { 
  x@accessor <- value
  x 
})
#' parent_id
#' @description getter
#' @rdname parent_id-ONG-get
#' @param x ONG object
#' @return value
#' @export
setMethod("parent_id", "ONG", function(x) x@parent_id)
#' parent_id<-
#' @description setter
#' @rdname parent_id-ONG-set
#' @param x ONG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("parent_id<-", "ONG", function(x, value) {
  x@parent_id <- value
  x
})
#' record_id
#' @description getter
#' @rdname record_id-ONG-get
#' @param x ONG object
#' @return value
#' @export
setMethod("record_id", "ONG", function(x) x@record_id)
#' record_id<-
#' @description setter
#' @rdname record_id-ONG-set
#' @param x ONG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_id<-", "ONG", function(x, value) {
  x@record_id <- value
  x
})
#' edge_metric
#' @description getter
#' @rdname edge_metric-ONG-get
#' @param x ONG object
#' @return value
#' @export
setMethod("edge_metric", "ONG", function(x) x@edge_metric)
#' edge_metric<-
#' @description setter
#' @rdname edge_metric-ONG-set
#' @param x ONG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("edge_metric<-", "ONG", function(x, value) {
  x@edge_metric <- value
  x
})
#' metric_type
#' @description getter
#' @rdname metric_type-ONG-get
#' @param x ONG object
#' @return value
#' @export
setMethod("metric_type", "ONG", function(x) x@metric_type)
#' metric_type<-
#' @description setter
#' @rdname metric_type-ONG-set
#' @param x ONG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("metric_type<-", "ONG", function(x, value) {
  x@metric_type <- value
  x
})

#Create object function
#' Constructor for the ONG (observation neighborhood graph) S4 object
#' @description Creates the FID object and populates its subfields
#' @param id Main ID of MAMS object
#' @param dataset_id Parent dataset ID
#' @param filepath Path to the data file
#' @param accessor Accessor
#' @param parent_id Parent FOM object
#' @param record_id Record ID 
#' @param edge_metric Type of edge metric used
#' @param metric_type Details of the edge metric
#' 
#' @return an ONG S4 object for use with MAMS
#' @export
create_ONG_object <- function(
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

# collapse function to sub object
setMethod("collapse_to_list", "ONG", function(x) {
  collapsed_list <- mapply(function(s) slot(x, s),
                           slotNames(x),
                           SIMPLIFY = FALSE)
  # Remove NULL values
  collapsed_list <- Filter(function(y) !is.null(y), collapsed_list)
  return(collapsed_list)
})
