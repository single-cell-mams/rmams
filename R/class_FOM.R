#' Define the Feature Observation Matrix (FOM) S4 object
#' @title class FOM
#' @description Stores the main feature observation matrix
#' 
#' @importFrom methods is new slot slot<- slotNames
#' 
#' @slot id character. 
#' @slot dataset_id character.
#' @slot filepath character.
#' @slot accessor character. 
#' @slot data_type character. 
#' @slot representation character. 
#' @slot representation_description character. 
#' @slot obs_unit character. 
#' @slot obs_unit_description character.
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
#' @slot oid character.
#' @slot fid character.
#' @slot obs character.
#' @slot fea character.
#' @slot ong character.
#' @slot fng character.
#'
#' @return a FOM class for further use with other MAMS objects and sub-objects
#' @export

setClass(
  "FOM",
  slots = c(
    id = "CharOrNULL",
    dataset_id = "CharOrNULL",    
    filepath = "CharOrNULL",
    accessor = "CharOrNULL",
    data_type = "CharOrNULL",
    representation = "CharOrNULL",
    representation_description = "CharOrNULL",
    obs_unit = "CharOrNULL",
    obs_unit_description = "CharOrNULL",
    processing = "CharOrNULL",
    processing_description = "CharOrNULL",
    analyte = "CharOrNULL",
    analyte_description = "CharOrNULL",
    modality = "CharOrNULL",
    obs_subset = "CharOrNULL",
    obs_subset_description = "CharOrNULL",
    feature_subset = "CharOrNULL",
    feature_subset_description = "CharOrNULL",
    record_id = "CharOrNULL",
    parent_id = "CharOrNULL",
    parent_relationship = "CharOrNULL",
    parent_relationship_description = "CharOrNULL",
    oid = "CharOrNULL",
    fid = "CharOrNULL",
    obs = "CharOrNULL",
    fea = "CharOrNULL",
    ong = "CharOrNULL",
    fng = "CharOrNULL"
  )
)

#' Constructor function to create a FOM object
#'
#' @description Create the FOM object
#' @param id Denotes the unique id of the matrix, annotation data frame, or graph and should be unique
#' @param dataset_id ID of the dataset
#' @param data_type Explicitly describes the type of data stored in the FOM
#' @param filepath Path to the file
#' @param accessor Accessor
#' @param representation Preferred representation of the matrix
#' @param representation_description More detail about the representation 
#' @param obs_unit Biological unit of the observations
#' @param obs_unit_description Used to describe the obs_unit
#' @param processing Used to describe the nature of the data contained within the matrix
#' @param processing_description More detail about the nature of the data
#' @param analyte Used to describe the biological analytes being quantified in the matrix
#' @param analyte_description More details about the analytes 
#' @param modality Describes the modality of the matrix, may be the same as another field or combination of other fields
#' @param obs_subset Describes the subset of observations that are present in the FOM
#' @param obs_subset_description More about the subset of observations
#' @param feature_subset Describes the subset of features that are present in the FOM
#' @param feature_subset_description More about the subset of features
#' @param record_id Unique id to denote a combination of entries for record_package_name, record_package_version, record_function_name, and record_function_parameters
#' @param parent_id Denotes the id(s) of the parent matrices that were used to produce the matrix
#' @param parent_relationship 	Denotes the type of relationship with the parent matrix or matrices
#' @param parent_relationship_description More about the type of relationship with the parent matrix or matrices
#' @param oid Character vector or combination of character vectors used to denote the unique ID of each observation
#' @param fid Character vector or combination of character vectors used to denote the unique ID of each feature
#' @param obs Name of matrices or data frames with the same number of observations as its corresponding FOM
#' @param fea Name of matrices or data frames with the same number of features as its corresponding FOM
#' @param ong Name of observation neighborhood graph
#' @param fng Name of feature neighborhood graph
#'
#' @return a FOM S4 object for further use with other MAMS objects and sub-objects

create_FOM_object <- function(
    id = NA_character_,
    dataset_id = NA_character_,
    filepath = NA_character_,
    accessor = NA_character_,
    data_type = NA_character_,
    representation = NA_character_,
    representation_description = NA_character_,
    obs_unit = NA_character_,
    obs_unit_description = NA_character_,
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
    oid = NA_character_,
    fid = NA_character_,
    obs = NA_character_,
    fea = NA_character_,
    ong = NA_character_,
    fng = NA_character_
) {
  obj <- new("FOM",
             id = id,
             dataset_id = dataset_id,
             filepath = filepath,
             accessor = accessor,
             data_type = data_type,
             representation = representation,
             representation_description = representation_description,
             obs_unit = obs_unit,
             obs_unit_description = obs_unit_description,
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
             parent_relationship_description = parent_relationship_description,
             oid = oid,
             fid = fid,
             obs = obs,
             fea = fea,
             ong = ong,
             fng = fng
  )
  
  return(obj)
}

## setMethods

#' id
#' @description getter
#' @rdname id-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("id", "FOM", function(x) x@id)

#' id<-
#' @description setter
#' @rdname id-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("id<-", "FOM", function(x, value) { x@id <- value; x})
#' dataset_id
#' @description getter
#' @rdname dataset_id-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("dataset_id", "FOM", function(x) x@dataset_id)

#' dataset_id<-
#' @description setter
#' @rdname dataset_id-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("dataset_id<-", "FOM", function(x, value) { x@dataset_id <- value; x })

#' filepath
#' @description getter
#' @rdname filepath-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("filepath", "FOM", function(x) x@filepath)

#' filepath<-
#' @description setter
#' @rdname filepath-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("filepath<-", "FOM", function(x, value) { x@filepath <- value; x })

#' accessor
#' @description getter
#' @rdname accessor-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("accessor", "FOM", function(x) x@accessor)

#' accessor<-
#' @description setter
#' @rdname accessor-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("accessor<-", "FOM", function(x, value) { x@accessor <- value; x })

#' data_type
#' @description getter
#' @rdname data_type-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("data_type", "FOM", function(x) x@data_type)
#' data_type<-
#' @description setter
#' @rdname data_type-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("data_type<-", "FOM", function(x, value) { x@data_type <- value; x })
#' representation
#' @description getter
#' @rdname representation-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("representation", "FOM", function(x) x@representation)
#' representation<-
#' @description setter
#' @rdname representation-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("representation<-", "FOM", function(x, value) { x@representation <- value; x })
#' representation_description
#' @description getter
#' @rdname representation_description-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("representation_description", "FOM", function(x) x@representation_description)
#' representation_description<-
#' @description setter
#' @rdname representation_description-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("representation_description<-", "FOM", function(x, value) { x@representation_description <- value; x })
#' obs_unit
#' @description getter
#' @rdname obs_unit-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("obs_unit", "FOM", function(x) x@obs_unit)
#' obs_unit<-
#' @description setter
#' @rdname obs_unit-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("obs_unit_description", "FOM", function(x) x@obs_unit_description)
#' obs_unit_description<-
#' @description setter
#' @rdname obs_unit_description-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("obs_unit<-", "FOM", function(x, value) { x@obs_unit <- value; x })
#' processing
#' @description getter
#' @rdname processing-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("obs_unit_description<-", "FOM", function(x, value) { x@obs_unit_description <- value; x })
#' processing
#' @description getter
#' @rdname processing-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("processing", "FOM", function(x) x@processing)
#' processing<-
#' @description setter
#' @rdname processing-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("processing<-", "FOM", function(x, value) { x@processing <- value; x })
#' processing_description
#' @description getter
#' @rdname processing_description-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("processing_description", "FOM", function(x) x@processing_description)
#' processing_description<-
#' @description setter
#' @rdname processing_description-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("processing_description<-", "FOM", function(x, value) { x@processing_description <- value; x })
#' analyte
#' @description getter
#' @rdname analyte-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("analyte", "FOM", function(x) x@analyte)
#' analyte<-
#' @description setter
#' @rdname analyte-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("analyte<-", "FOM", function(x, value) { x@analyte <- value; x })
#' analyte_description
#' @description getter
#' @rdname analyte-description-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("analyte_description", "FOM", function(x) x@analyte_description)
#' analyte_description<-
#' @description setter
#' @rdname analyte_description-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("analyte_description<-", "FOM", function(x, value) { x@analyte_description <- value; x })
#' modality
#' @description getter
#' @rdname modality-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("modality", "FOM", function(x) x@modality)
#' modality<-
#' @description setter
#' @rdname modality-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("modality<-", "FOM", function(x, value) { x@modality <- value; x })
#' obs_subset
#' @description getter
#' @rdname obs_subset-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("obs_subset", "FOM", function(x) x@obs_subset)
#' obs_subset<-
#' @description setter
#' @rdname obs_subset-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("obs_subset<-", "FOM", function(x, value) { x@obs_subset <- value; x })
#' obs_subset_description
#' @description getter
#' @rdname obs_subset-description-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("obs_subset_description", "FOM", function(x) x@obs_subset_description)
#' obs_subset_description<-
#' @description setter
#' @rdname obs_subset_description-FOM-get
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("obs_subset_description<-", "FOM", function(x, value) { x@obs_subset_description <- value; x })
#' feature_subset
#' @description getter
#' @rdname feature_subset-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("feature_subset", "FOM", function(x) x@feature_subset)
#' feature_subset<-
#' @description setter
#' @rdname feature_subset-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("feature_subset<-", "FOM", function(x, value) { x@feature_subset <- value; x })
#' feature_subset_description
#' @description getter
#' @rdname feature_subset_description-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("feature_subset_description", "FOM", function(x) x@feature_subset_description)
#' feature_subset_description<-
#' @description setter
#' @rdname feature_subset_description-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("feature_subset_description<-", "FOM", function(x, value) { x@feature_subset_description <- value; x })
#' record_id
#' @description getter
#' @rdname record_id-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("record_id", "FOM", function(x) x@record_id)
#' record_id<-
#' @description setter
#' @rdname record_id-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_id<-", "FOM", function(x, value) { x@record_id <- value; x })
#' parent_id
#' @description getter
#' @rdname parent_id-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("parent_id", "FOM", function(x) x@parent_id)
#' parent_id<-
#' @description setter
#' @rdname parent_id-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("parent_id<-", "FOM", function(x, value) { x@parent_id <- value; x })
#' parent_relationship
#' @description getter
#' @rdname parent_relationship-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("parent_relationship", "FOM", function(x) x@parent_relationship)
#' parent_relationship<-
#' @description setter
#' @rdname parent_relationship-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("parent_relationship<-", "FOM", function(x, value) { x@parent_relationship <- value; x })
#' parent_relationship_description
#' @description getter
#' @rdname parent_relationship_description-FOM-get
#' @param x FOM object
#' @return the value
#' @export
setMethod("parent_relationship_description", "FOM", function(x) x@parent_relationship_description)
#' parent_relationship_description<-
#' @description setter
#' @rdname parent_relationship_description-FOM-set
#' @param x FOM object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("parent_relationship_description<-", "FOM", function(x, value) { x@parent_relationship_description <- value; x })


# collapse function to sub object
setMethod("collapse_to_list", "FOM", function(x) {
  collapsed_list <- mapply(function(s) slot(x, s),
                           slotNames(x),
                           SIMPLIFY = FALSE)
  # Remove NULL values
  collapsed_list <- Filter(function(y) !is.null(y), collapsed_list)
  return(collapsed_list)
})
