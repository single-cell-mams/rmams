#' Define the FNG (feature neighborhood graph) S4 object
#' @title class FNG
#' @description Stores feature neighborhood graph data
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
#' @return a FNG S4 object for use with MAMS
#' @export

setClass(
  "FNG",
  slots = list(
    id = "CharOrNULL",
    dataset_id = "CharOrNULL",
    filepath = "CharOrNULL",
    accessor = "CharOrNULL",
    parent_id = "CharOrNULL",
    record_id = "CharOrNULL",
    edge_metric = "CharOrNULL",
    metric_type = "CharOrNULL"
  )
)

# Getter and setter functions 

#' id
#' @description getter
#' @rdname id-FNG-get
#' @param x FNG object
#' @return the value
#' @export
setMethod("id", "FNG", function(x) x@id)
#' id<-
#' @description setter
#' @rdname id-FNG-set
#' @param x FNG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("id<-", "FNG", function(x, value) {
  x@id <- value
  x
})
#' dataset_id
#' @description getter
#' @rdname dataset_id-FNG-get
#' @param x FNG object
#' @return the value
#' @export
setMethod("dataset_id", "FNG", function(x) x@dataset_id)
#' dataset_id<-
#' @description setter
#' @rdname dataset_id-FNG-set
#' @param x FNG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("dataset_id<-", "FNG", function(x, value) {
  x@dataset_id <- value
  x
})
#' filepath
#' @description getter
#' @rdname filepath-FNG-get
#' @param x FNG object
#' @return the value
#' @export
setMethod("filepath", "FNG", function(x) x@filepath)
#' filepath<-
#' @description setter
#' @rdname filepath-FNG-set
#' @param x FNG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("filepath<-", "FNG", function(x, value) { x@filepath <- value; x })
#' accessor
#' @description getter
#' @rdname accessor-FNG-get
#' @param x FNG object
#' @return the value
#' @export
setMethod("accessor", "FNG", function(x) x@accessor)
#' accessor<-
#' @description setter
#' @rdname accessor-FNG-set
#' @param x FNG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("accessor<-", "FNG", function(x, value) { x@accessor <- value; x })
#' parent_id
#' @description getter
#' @rdname parent_id-FNG-get
#' @param x FNG object
#' @return the value
#' @export
setMethod("parent_id", "FNG", function(x) x@parent_id)
#' parent_id<-
#' @description setter
#' @rdname parent_id-FNG-set
#' @param x FNG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("parent_id<-", "FNG", function(x, value) {
  x@parent_id <- value
  x
})
#' record_id
#' @description getter
#' @rdname record_id-FNG-get
#' @param x FNG object
#' @return the value
#' @export
setMethod("record_id", "FNG", function(x) x@record_id)
#' record_id<-
#' @description setter
#' @rdname record_id-FNG-set
#' @param x FNG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_id<-", "FNG", function(x, value) {
  x@record_id <- value
  x
})
#' edge_metric
#' @description getter
#' @rdname edge_metric-FNG-get
#' @param x FNG object
#' @return the value
#' @export
setMethod("edge_metric", "FNG", function(x) x@edge_metric)
#' edge_metric<-
#' @description setter
#' @rdname edge_metric-FNG-set
#' @param x FNG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("edge_metric<-", "FNG", function(x, value) {
  x@edge_metric <- value
  x
})
#' metric_type
#' @description getter
#' @rdname metric_type-FNG-get
#' @param x FNG object
#' @return the value
#' @export
setMethod("metric_type", "FNG", function(x) x@metric_type)
#' metric_type<-
#' @description setter
#' @rdname metric_type-FNG-set
#' @param x FNG object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("metric_type<-", "FNG", function(x, value) {
  x@metric_type <- value
  x
})

#' Constructor for the FNG (feature neighborhood graph) S4 object
#' @description Creates the FID object and populates its subfields
#' @param id Main ID of MAMS object
#' @param dataset_id Parent dataset ID
#' @param filepath Path to the data file
#' @param accessor Accessor
#' @param parent_id Parent FNG object
#' @param record_id Record ID
#' @param edge_metric Type of edge metric used
#' @param metric_type Details of the edge metric
#' 
#' @return a FNG S4 object for use with MAMS
#' @export

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

# collapse function to sub object
setMethod("collapse_to_list", "FNG", function(x) {
  collapsed_list <- mapply(function(s) slot(x, s),
                           slotNames(x),
                           SIMPLIFY = FALSE)
  # Remove NULL values
  collapsed_list <- Filter(function(y) !is.null(y), collapsed_list)
  return(collapsed_list)
})
