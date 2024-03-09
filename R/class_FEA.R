#' Define the FEA (feature) S4 object
#' @title class FEA
#' @description Stores the feature metadata
#' 
#' @slot id character. 
#' @slot dataset_id character. 
#' @slot filepath character.
#' @slot accessor character.
#' @slot feature_modality character. 
#' @slot reference_database character. 
#' @slot reference_organism character. 
#' @slot record_id character. 
#'
#' @return a FEA class for use with MAMS
#' @export

setClass("FEA", slots = list(id = "CharOrNULL",
                             dataset_id = "CharOrNULL",
                             filepath = "CharOrNULL",
                             accessor = "CharOrNULL",
                             feature_modality = "CharOrNULL",
                             reference_database = "CharOrNULL",
                             reference_organism = "CharOrNULL",
                             record_id = "CharOrNULL"))

#' Constructor for the FEA S4 object
#' @description Creates the FEA (feature) object and populates its subfields
#' 
#' @param id Parent ID
#' @param dataset_id Parent dataset ID
#' @param filepath Path to the data file
#' @param accessor Accessors used
#' @param feature_modality Modality of the feature
#' @param reference_database Reference database used
#' @param reference_organism Reference organism used
#' @param record_id Record ID of object
#' 
#' @return a FEA S4 object for use with MAMS
#' @export
#' 
create_FEA_object <- function(
    id = NA_character_,
    dataset_id = NA_character_,    
    filepath = NA_character_,
    accessor = NA_character_,
    feature_modality = NA_character_,
    reference_database = NA_character_,
    reference_organism = NA_character_,
    record_id = NA_character_
) {
  obj <- new("FEA",
             id = id,
             dataset_id = dataset_id,
             filepath = filepath,
             accessor = accessor,
             feature_modality = feature_modality,
             reference_database = reference_database,
             reference_organism = reference_organism,
             record_id = record_id
  )
  
  return(obj)
}

#' id
#' @description getter
#' @rdname id-FEA-get
#' @param x FEA object
#' @return the value
#' @export
setMethod("id", "FEA", function(x) x@id)
#' id<-
#' @description setter
#' @rdname id-FEA-set
#' @param x FEA object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("id<-", "FEA", function(x, value) {
  x@id <- value
  x
})
#' dataset_id
#' @description getter
#' @rdname dataset_id-FEA-get
#' @param x FEA object
#' @return the value
#' @export
setMethod("dataset_id", "FEA", function(x) x@dataset_id)
#' dataset_id<-
#' @description setter
#' @rdname dataset_id-FEA-set
#' @param x FEA object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("dataset_id<-", "FEA", function(x, value) {
  x@dataset_id <- value
  x
})
#' filepath
#' @description getter
#' @rdname filepath-FEA-get
#' @param x FEA object
#' @return the value
#' @export
setMethod("filepath", "FEA", function(x) x@filepath)
#' filepath<-
#' @description setter
#' @rdname filepath-FEA-set
#' @param x FEA object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("filepath<-", "FEA", function(x, value) { 
  x@filepath <- value
  x 
})
#' accessor
#' @description getter
#' @rdname accessor-FEA-get
#' @param x FEA object
#' @return the value
#' @export
setMethod("accessor", "FEA", function(x) x@accessor)
#' accessor<-
#' @description setter
#' @rdname accessor-FEA-set
#' @param x FEA object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("accessor<-", "FEA", function(x, value) { 
  x@accessor <- value
  x 
})
#' feature_modality
#' @description getter
#' @rdname feature_modality-FEA-get
#' @param x FEA object
#' @return the value
#' @export
setMethod("feature_modality", "FEA", function(x) x@feature_modality)
#' feature_modality<-
#' @description setter
#' @rdname feature_modality-FEA-set
#' @param x FEA object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("feature_modality<-", "FEA", function(x, value) {
  x@feature_modality <- value
  x
})
#' reference_database
#' @description getter
#' @rdname reference_database-FEA-get
#' @param x FEA object
#' @return the value
#' @export
setMethod("reference_database", "FEA", function(x) x@reference_database)
#' reference_database<-
#' @description setter
#' @rdname reference_database-FEA-set
#' @param x FEA object
#' @param value value
#' @export
setMethod("reference_database<-", "FEA", function(x, value) {
  x@reference_database <- value
  x
})
#' reference_organism
#' @description getter
#' @rdname reference_organism-FEA-get
#' @param x FEA object
#' @return the value
#' @return nothing (setter)
#' @export
setMethod("reference_organism", "FEA", function(x) x@reference_organism)
#' reference_organism<-
#' @description setter
#' @rdname reference_organism-FEA-set
#' @param x FEA object
#' @param value value
#' @export
setMethod("reference_organism<-", "FEA", function(x, value) {
  x@reference_organism <- value
  x
})
#' record_id
#' @description getter
#' @rdname record_id-FEA-get
#' @param x FEA object
#' @return the value
#' @export
setMethod("record_id", "FEA", function(x) x@record_id)
#' record_id<-
#' @description setter
#' @rdname record_id-FEA-set
#' @param x FEA object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_id<-", "FEA", function(x, value) {
  x@record_id <- value
  x
})

# collapse function to sub object
setMethod("collapse_to_list", "FEA", function(x) {
  collapsed_list <- mapply(function(s) slot(x, s),
                           slotNames(x),
                           SIMPLIFY = FALSE)
  # Remove NULL values
  collapsed_list <- Filter(function(y) !is.null(y), collapsed_list)
  return(collapsed_list)
})
