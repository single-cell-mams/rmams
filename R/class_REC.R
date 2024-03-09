# Define the REC (record) S4 object
#' @title class REC
#' @description Stores provenance about the dataset and also other objects
#' 
#' @importFrom methods is new slot slot<- slotNames
#' 
#' @slot id 
#' @slot dataset_id 
#' @slot record_id 
#' @slot record_package_name 
#' @slot record_package_version 
#' @slot record_function_name 
#' @slot record_function_parameters 
#' @slot record_workflow_link 
#' @slot record_runtime_start 
#' @slot record_runtime_end 
#' @slot record_runtime_duration 

setClass(
  "REC",
  slots = list(
    id = "CharOrNULL",
    dataset_id = "CharOrNULL",
    record_id = "CharOrNULL",
    record_package_name = "CharOrNULL",
    record_package_version = "CharOrNULL",
    record_function_name = "CharOrNULL",
    record_function_parameters = "CharOrList",
    record_workflow_link = "CharOrNULL",
    record_runtime_start = "CharOrNULL",
    record_runtime_end = "CharOrNULL",
    record_runtime_duration = "CharOrNULL"
  )
)

# Getter and setter functions for id
#' id
#' @description getter
#' @rdname id-REC-get
#' @param x REC object
#' @return value
#' @export
setMethod("id", "REC", function(x) x@id)
#' id<-
#' @description setter
#' @rdname id-REC-set
#' @param x REC object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("id<-", "REC", function(x, value) {
  x@id <- value
  x
})
#' dataset_id
#' @description getter
#' @rdname dataset_id-REC-get
#' @param x REC object
#' @return value
#' @export
setMethod("dataset_id", "REC", function(x) x@dataset_id)
#' dataset_id<-
#' @description setter
#' @rdname dataset_id-REC-set
#' @param x REC object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("dataset_id<-", "REC", function(x, value) {
  x@dataset_id <- value
  x
})
#' record_id
#' @description getter
#' @rdname record_id-REC-get
#' @param x REC object
#' @return value
#' @export
setMethod("record_id", "REC", function(x) x@record_id)
#' record_id<-
#' @description setter
#' @rdname record_id-REC-set
#' @param x REC object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_id<-", "REC", function(x, value) {
  x@record_id <- value
  x
})
#' record_package_name
#' @description getter
#' @rdname record_package_name-REC-get
#' @param x REC object
#' @return value
#' @export
setMethod("record_package_name", "REC", function(x) x@record_package_name)
#' record_package_name<-
#' @description setter
#' @rdname record_package_name-REC-set
#' @param x REC object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_package_name<-", "REC", function(x, value) {
  x@record_package_name <- value
  x
})
#' record_package_version
#' @description getter
#' @rdname record_package_version-REC-get
#' @param x REC object
#' @return value
#' @export
setMethod("record_package_version", "REC", function(x) x@record_package_version)
#' record_package_version<-
#' @description setter
#' @rdname record_package_version-REC-set
#' @param x REC object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_package_version<-", "REC", function(x, value) {
  x@record_package_version <- value
  x
})
#' record_function_name
#' @description getter
#' @rdname record_function_name-REC-get
#' @param x REC object
#' @return value
#' @export
setMethod("record_function_name", "REC", function(x) x@record_function_name)
#' record_function_name<-
#' @description setter
#' @rdname record_function_name-REC-set
#' @param x REC object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_function_name<-", "REC", function(x, value) {
  x@record_function_name <- value
  x
})
#' record_function_parameters
#' @description getter
#' @rdname record_function_parameters-REC-get
#' @param x REC object
#' @return value
#' @export
setMethod("record_function_parameters", "REC", function(x) x@record_function_parameters)
#' record_function_parameters<-
#' @description setter
#' @rdname record_function_parameters-REC-set
#' @param x REC object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_function_parameters<-", "REC", function(x, value) {
  x@record_function_parameters <- value
  x
})
#' record_workflow_link
#' @description getter
#' @rdname record_workflow_link-REC-get
#' @param x REC object
#' @return value
#' @export
setMethod("record_workflow_link", "REC", function(x) x@record_workflow_link)
#' record_workflow_link<-
#' @description setter
#' @rdname record_workflow_link-REC-set
#' @param x REC object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_workflow_link<-", "REC", function(x, value) {
  x@record_workflow_link <- value
  x
})
#' record_runtime_start
#' @description getter
#' @rdname record_runtime_start-REC-get
#' @param x REC object
#' @return value
#' @export
setMethod("record_runtime_start", "REC", function(x) x@record_runtime_start)
#' record_runtime_start<-
#' @description setter
#' @rdname record_runtime_start-REC-set
#' @param x REC object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_runtime_start<-", "REC", function(x, value) {
  x@record_runtime_start <- value
  x
})
#' record_runtime_end
#' @description getter
#' @rdname record_runtime_end-REC-get
#' @param x REC object
#' @return value
#' @export
setMethod("record_runtime_end", "REC", function(x) x@record_runtime_end)
#' record_runtime_end<-
#' @description setter
#' @rdname record_runtime_end-REC-set
#' @param x REC object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_runtime_end<-", "REC", function(x, value) {
  x@record_runtime_end <- value
  x
})
#' record_runtime_duration
#' @description getter
#' @rdname record_runtime_duration-REC-get
#' @param x REC object
#' @return value
#' @export
setMethod("record_runtime_duration", "REC", function(x) x@record_runtime_duration)
#' record_runtime_duration<-
#' @description setter
#' @rdname record_runtime_duration-REC-set
#' @param x REC object
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("record_runtime_duration<-", "REC", function(x, value) {
  x@record_runtime_duration <- value
  x
})


#' Constructor for the REC (provenance records) S4 object
#' @description Creates the FID object and populates its subfields
#' @param id Main ID of MAMS object
#' @param dataset_id Parent dataset ID
#' @param record_id Record ID
#' @param record_package_name Name of toolkit
#' @param record_package_version Version of toolkit
#' @param record_function_name Name of function(s) used
#' @param record_function_parameters Parameters passed to function(s)
#' @param record_workflow_link Link to the workflow of the toolkit
#' @param record_runtime_start Runtime start
#' @param record_runtime_end Runtime end
#' @param record_runtime_duration Run duration
#' 
#' @return a REC S4 object for use with MAMS
#' @export

#Create object function 
create_REC_object <- function(
    id = NA_character_,
    dataset_id = NA_character_,
    record_id = NA_character_,
    record_package_name = NA_character_,
    record_package_version = NA_character_,
    record_function_name = NA_character_,
    record_function_parameters = NA_character_,
    record_workflow_link = NA_character_,
    record_runtime_start = NA_character_,
    record_runtime_end = NA_character_,
    record_runtime_duration = NA_character_
) {
  obj <- new("REC",
             id = id,
             dataset_id = dataset_id,
             record_id = record_id,
             record_package_name = record_package_name,
             record_package_version = record_package_version,
             record_function_name = record_function_name,
             record_function_parameters = record_function_parameters,
             record_workflow_link = record_workflow_link,
             record_runtime_start = record_runtime_start,
             record_runtime_end = record_runtime_end,
             record_runtime_duration = record_runtime_duration
  )
  return(obj)
}


# collapse function to sub object
setMethod("collapse_to_list", "REC", function(x) {
  collapsed_list <- mapply(function(s) slot(x, s),
                           slotNames(x),
                           SIMPLIFY = FALSE)
  # Remove NULL values
  collapsed_list <- Filter(function(y) !is.null(y), collapsed_list)
  return(collapsed_list)
})
