#' Class to create a FOM object
#'
#' @slot id character. 
#' @slot dataset_id character.
#' @slot filepath character.
#' @slot accessor character. 
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
#' @slot oid character.
#' @slot fid character.
#' @slot obs character.
#' @slot fea character.
#' @slot ong character.
#' @slot fng character.
#'
#' @return
#' @export
#'
#' @examples
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
#' @param id 
#' @param dataset_id 
#' @param data_type 
#' @param filepath
#' @param accessor
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
#' @param oid
#' @param fid
#' @param obs
#' @param fea
#' @param ong
#' @param fng
#'
#' @return
#' @export
#'
#' @examples
create_FOM_object <- function(
    id = NA_character_,
    dataset_id = NA_character_,
    filepath = NA_character_,
    accessor = NA_character_,
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
setMethod("id", "FOM", function(x) x@id)
setMethod("id<-", "FOM", function(x, value) { x@id <- value; x})
setMethod("dataset_id", "FOM", function(x) x@dataset_id)
setMethod("dataset_id<-", "FOM", function(x, value) { x@dataset_id <- value; x })
setMethod("filepath", "FOM", function(x) x@filepath)
setMethod("filepath<-", "FOM", function(x, value) { x@filepath <- value; x })
setMethod("accessor", "FOM", function(x) x@accessor)
setMethod("accessor<-", "FOM", function(x, value) { x@accessor <- value; x })


#setMethod("fid", "FOM", function(x) x@fid)
#setMethod("fid<-", "FOM", function(x, value) { x@fid <- value; x })
#setMethod("oid", "FOM", function(x) x@oid)
#setMethod("oid<-", "FOM", function(x, value) { x@oid <- value; x })
#setMethod("fea", "FOM", function(x) x@fea)
#setMethod("fea<-", "FOM", function(x, value) { x@fea <- value; x })
#setMethod("obs", "FOM", function(x) x@obs)
#setMethod("obs<-", "FOM", function(x, value) { x@obs <- value; x })
#setMethod("fng", "FOM", function(x) x@fng)
#setMethod("fng<-", "FOM", function(x, value) { x@fng <- value; x })
#setMethod("ong", "FOM", function(x) x@ong)
#setMethod("ong<-", "FOM", function(x, value) { x@ong <- value; x })

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


# collapse function to sub object
setMethod("collapse_to_list", "FOM", function(x) {
  collapsed_list <- mapply(function(s) slot(x, s),
                           slotNames(x),
                           SIMPLIFY = FALSE)
  # Remove NULL values
  collapsed_list <- Filter(function(y) !is.null(y), collapsed_list)
  return(collapsed_list)
})
