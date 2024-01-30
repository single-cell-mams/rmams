#' Class to create a FOM object
#'
#' @slot id character. 
#' @slot dataset_id character. 
#' @slot data_type character. 
#' @slot representation character. 
#' @slot representation_description character. 
#' @slot obs_unit character. 
#' @slot processing character. 
#' @slot processing_description character. 
#' @slot analyte character. 
#' @slot analyte_description character. 
#' @slot modality character. 
#' @slot obs_subset character. 
#' @slot obs_subset_description character. 
#' @slot feature_subset character. 
#' @slot feature_subset_description character. 
#' @slot record_id character. 
#' @slot parent_id character. 
#' @slot parent_relationship character. 
#' @slot parent_relationship_description character. 
#'
#' @return
#' @export
#'
#' @examples
setClass(
  "FOM",
  slots = c(
    id = "character",
    dataset_id = "character",
    data_type = "character",
    representation = "character",
    representation_description = "character",
    obs_unit = "character",
    processing = "character",
    processing_description = "character",
    analyte = "character",
    analyte_description = "character",
    modality = "character",
    obs_subset = "character",
    obs_subset_description = "character",
    feature_subset = "character",
    feature_subset_description = "character",
    record_id = "character",
    parent_id = "character",
    parent_relationship = "character",
    parent_relationship_description = "character",
    filepath = "character",
    accessor = "character"
  )
)

#' Class to create a FOM object
#'
#' @slot id character. 
#' @slot dataset_id character. 
#' @slot data_type character. 
#' @slot representation character. 
#' @slot representation_description character. 
#' @slot obs_unit character. 
#' @slot processing character. 
#' @slot processing_description character. 
#' @slot analyte character. 
#' @slot analyte_description character. 
#' @slot modality character. 
#' @slot obs_subset character. 
#' @slot obs_subset_description character. 
#' @slot feature_subset character. 
#' @slot feature_subset_description character. 
#' @slot record_id character. 
#' @slot parent_id character. 
#' @slot parent_relationship character. 
#' @slot parent_relationship_description character. 
#'
#' @return
#' @export
#'
#' @examples
setClass(
  "FOM",
  slots = c(
    id = "character",
    dataset_id = "character",
    data_type = "character",
    representation = "character",
    representation_description = "character",
    obs_unit = "character",
    processing = "character",
    processing_description = "character",
    analyte = "character",
    analyte_description = "character",
    modality = "character",
    obs_subset = "character",
    obs_subset_description = "character",
    feature_subset = "character",
    feature_subset_description = "character",
    record_id = "character",
    parent_id = "character",
    parent_relationship = "character",
    parent_relationship_description = "character",
    filepath = "character",
    accessor = "character"
  )
)


#' Constructor function to create a FOM object
#'
#' @param id 
#' @param dataset_id 
#' @param data_type 
#' @param representation 
#' @param representation_description 
#' @param obs_unit 
#' @param processing 
#' @param processing_description 
#' @param analyte 
#' @param analyte_description 
#' @param modality 
#' @param obs_subset 
#' @param obs_subset_description 
#' @param feature_subset 
#' @param feature_subset_description 
#' @param record_id 
#' @param parent_id 
#' @param parent_relationship 
#' @param parent_relationship_description 
#'
#' @return
#' @export
#'
#' @examples
create_FOM_Object <- function(
    id = NA_character_,
    dataset_id = NA_character_,
    data_type = NA_character_,
    representation = NA_character_,
    representation_description = NA_character_,
    obs_unit = NA_character_,
    processing = NA_character_,
    processing_description = NA_character_,
    analyte = NA_character_,
    analyte_description = NA_character_,
    modality = NA_character_,
    obs_subset = NA_character_,
    obs_subset_description = NA_character_,
    feature_subset = NA_character_,
    feature_subset_description = NA_character_,
    record_id = NA_character_,
    parent_id = NA_character_,
    parent_relationship = NA_character_,
    parent_relationship_description = NA_character_,
    filepath = NA_character_,
    accessor = NA_character_
) {
  obj <- new("FOM",
             id = id,
             dataset_id = dataset_id,
             data_type = data_type,
             representation = representation,
             representation_description = representation_description,
             obs_unit = obs_unit,
             processing = processing,
             processing_description = processing_description,
             analyte = analyte,
             analyte_description = analyte_description,
             modality = modality,
             obs_subset = obs_subset,
             obs_subset_description = obs_subset_description,
             feature_subset = feature_subset,
             feature_subset_description = feature_subset_description,
             record_id = record_id,
             parent_id = parent_id,
             parent_relationship = parent_relationship,
             parent_relationship_description = parent_relationship_description
  )
  
  return(obj)
}

## setMethods
setMethod("id", "FOM", function(x) x@id)
setMethod("id<-", "FOM", function(x, value) { x@id <- value; x})
setMethod("dataset_id", "FOM", function(x) x@dataset_id)
setMethod("dataset_id<-", "FOM", function(x, value) { x@dataset_id <- value; x })
setMethod("data_type", "FOM", function(x) x@data_type)
setMethod("data_type<-", "FOM", function(x, value) { x@data_type <- value; x })
setMethod("representation", "FOM", function(x) x@representation)
setMethod("representation<-", "FOM", function(x, value) { x@representation <- value; x })
setMethod("representation_description", "FOM", function(x) x@representation_description)
setMethod("representation_description<-", "FOM", function(x, value) { x@representation_description <- value; x })
setMethod("obs_unit", "FOM", function(x) x@obs_unit)
setMethod("obs_unit<-", "FOM", function(x, value) { x@obs_unit <- value; x })
setMethod("processing", "FOM", function(x) x@processing)
setMethod("processing<-", "FOM", function(x, value) { x@processing <- value; x })
setMethod("processing_description", "FOM", function(x) x@processing_description)
setMethod("processing_description<-", "FOM", function(x, value) { x@processing_description <- value; x })
setMethod("analyte", "FOM", function(x) x@analyte)
setMethod("analyte<-", "FOM", function(x, value) { x@analyte <- value; x })
setMethod("analyte_description", "FOM", function(x) x@analyte_description)
setMethod("analyte_description<-", "FOM", function(x, value) { x@analyte_description <- value; x })
setMethod("modality", "FOM", function(x) x@modality)
setMethod("modality<-", "FOM", function(x, value) { x@modality <- value; x })
setMethod("obs_subset", "FOM", function(x) x@obs_subset)
setMethod("obs_subset<-", "FOM", function(x, value) { x@obs_subset <- value; x })
setMethod("obs_subset_description", "FOM", function(x) x@obs_subset_description)
setMethod("obs_subset_description<-", "FOM", function(x, value) { x@obs_subset_description <- value; x })
setMethod("feature_subset", "FOM", function(x) x@feature_subset)
setMethod("feature_subset<-", "FOM", function(x, value) { x@feature_subset <- value; x })
setMethod("feature_subset_description", "FOM", function(x) x@feature_subset_description)
setMethod("feature_subset_description<-", "FOM", function(x, value) { x@feature_subset_description <- value; x })
setMethod("record_id", "FOM", function(x) x@record_id)
setMethod("record_id<-", "FOM", function(x, value) { x@record_id <- value; x })
setMethod("parent_id", "FOM", function(x) x@parent_id)
setMethod("parent_id<-", "FOM", function(x, value) { x@parent_id <- value; x })
setMethod("parent_relationship", "FOM", function(x) x@parent_relationship)
setMethod("parent_relationship<-", "FOM", function(x, value) { x@parent_relationship <- value; x })
setMethod("parent_relationship_description", "FOM", function(x) x@parent_relationship_description)
setMethod("parent_relationship_description<-", "FOM", function(x, value) { x@parent_relationship_description <- value; x })



#' Constructor function to create a FOM object
#'
#' @param id 
#' @param dataset_id 
#' @param data_type 
#' @param representation 
#' @param representation_description 
#' @param obs_unit 
#' @param processing 
#' @param processing_description 
#' @param analyte 
#' @param analyte_description 
#' @param modality 
#' @param obs_subset 
#' @param obs_subset_description 
#' @param feature_subset 
#' @param feature_subset_description 
#' @param record_id 
#' @param parent_id 
#' @param parent_relationship 
#' @param parent_relationship_description 
#'
#' @return
#' @export
#'
#' @examples
create_FOM_Object <- function(
    id = NA_character_,
    dataset_id = NA_character_,
    data_type = NA_character_,
    representation = NA_character_,
    representation_description = NA_character_,
    obs_unit = NA_character_,
    processing = NA_character_,
    processing_description = NA_character_,
    analyte = NA_character_,
    analyte_description = NA_character_,
    modality = NA_character_,
    obs_subset = NA_character_,
    obs_subset_description = NA_character_,
    feature_subset = NA_character_,
    feature_subset_description = NA_character_,
    record_id = NA_character_,
    parent_id = NA_character_,
    parent_relationship = NA_character_,
    parent_relationship_description = NA_character_,
    filepath = NA_character_,
    accessor = NA_character_
) {
  obj <- new("FOM",
             id = id,
             dataset_id = dataset_id,
             data_type = data_type,
             representation = representation,
             representation_description = representation_description,
             obs_unit = obs_unit,
             processing = processing,
             processing_description = processing_description,
             analyte = analyte,
             analyte_description = analyte_description,
             modality = modality,
             obs_subset = obs_subset,
             obs_subset_description = obs_subset_description,
             feature_subset = feature_subset,
             feature_subset_description = feature_subset_description,
             record_id = record_id,
             parent_id = parent_id,
             parent_relationship = parent_relationship,
             parent_relationship_description = parent_relationship_description
  )
  
  return(obj)
}

## setMethods
setMethod("id", "FOM", function(x) x@id)
setMethod("id<-", "FOM", function(x, value) { x@id <- value; x})
setMethod("dataset_id", "FOM", function(x) x@dataset_id)
setMethod("dataset_id<-", "FOM", function(x, value) { x@dataset_id <- value; x })
setMethod("data_type", "FOM", function(x) x@data_type)
setMethod("data_type<-", "FOM", function(x, value) { x@data_type <- value; x })
setMethod("representation", "FOM", function(x) x@representation)
setMethod("representation<-", "FOM", function(x, value) { x@representation <- value; x })
setMethod("representation_description", "FOM", function(x) x@representation_description)
setMethod("representation_description<-", "FOM", function(x, value) { x@representation_description <- value; x })
setMethod("obs_unit", "FOM", function(x) x@obs_unit)
setMethod("obs_unit<-", "FOM", function(x, value) { x@obs_unit <- value; x })
setMethod("processing", "FOM", function(x) x@processing)
setMethod("processing<-", "FOM", function(x, value) { x@processing <- value; x })
setMethod("processing_description", "FOM", function(x) x@processing_description)
setMethod("processing_description<-", "FOM", function(x, value) { x@processing_description <- value; x })
setMethod("analyte", "FOM", function(x) x@analyte)
setMethod("analyte<-", "FOM", function(x, value) { x@analyte <- value; x })
setMethod("analyte_description", "FOM", function(x) x@analyte_description)
setMethod("analyte_description<-", "FOM", function(x, value) { x@analyte_description <- value; x })
setMethod("modality", "FOM", function(x) x@modality)
setMethod("modality<-", "FOM", function(x, value) { x@modality <- value; x })
setMethod("obs_subset", "FOM", function(x) x@obs_subset)
setMethod("obs_subset<-", "FOM", function(x, value) { x@obs_subset <- value; x })
setMethod("obs_subset_description", "FOM", function(x) x@obs_subset_description)
setMethod("obs_subset_description<-", "FOM", function(x, value) { x@obs_subset_description <- value; x })
setMethod("feature_subset", "FOM", function(x) x@feature_subset)
setMethod("feature_subset<-", "FOM", function(x, value) { x@feature_subset <- value; x })
setMethod("feature_subset_description", "FOM", function(x) x@feature_subset_description)
setMethod("feature_subset_description<-", "FOM", function(x, value) { x@feature_subset_description <- value; x })
setMethod("record_id", "FOM", function(x) x@record_id)
setMethod("record_id<-", "FOM", function(x, value) { x@record_id <- value; x })
setMethod("parent_id", "FOM", function(x) x@parent_id)
setMethod("parent_id<-", "FOM", function(x, value) { x@parent_id <- value; x })
setMethod("parent_relationship", "FOM", function(x) x@parent_relationship)
setMethod("parent_relationship<-", "FOM", function(x, value) { x@parent_relationship <- value; x })
setMethod("parent_relationship_description", "FOM", function(x) x@parent_relationship_description)
setMethod("parent_relationship_description<-", "FOM", function(x, value) { x@parent_relationship_description <- value; x })

