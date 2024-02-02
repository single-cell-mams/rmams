# **REC Class ####
#' @description Stores provenance about the dataset and also other objects
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
#' @keywords internal
#' @noRd

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
setMethod("id", "REC", function(x) x@id)
setMethod("id<-", "REC", function(x, value) {
  x@id <- value
  x
})

setMethod("dataset_id", "REC", function(x) x@dataset_id)
setMethod("dataset_id<-", "REC", function(x, value) {
  x@dataset_id <- value
  x
})

setMethod("record_id", "REC", function(x) x@record_id)
setMethod("record_id<-", "REC", function(x, value) {
  x@record_id <- value
  x
})

setMethod("record_package_name", "REC", function(x) x@record_package_name)
setMethod("record_package_name<-", "REC", function(x, value) {
  x@record_package_name <- value
  x
})

setMethod("record_package_version", "REC", function(x) x@record_package_version)
setMethod("record_package_version<-", "REC", function(x, value) {
  x@record_package_version <- value
  x
})

setMethod("record_function_name", "REC", function(x) x@record_function_name)
setMethod("record_function_name<-", "REC", function(x, value) {
  x@record_function_name <- value
  x
})

setMethod("record_function_parameters", "REC", function(x) x@record_function_parameters)
setMethod("record_function_parameters<-", "REC", function(x, value) {
  x@record_function_parameters <- value
  x
})

setMethod("record_workflow_link", "REC", function(x) x@record_workflow_link)
setMethod("record_workflow_link<-", "REC", function(x, value) {
  x@record_workflow_link <- value
  x
})
setMethod("record_runtime_start", "REC", function(x) x@record_runtime_start)
setMethod("record_runtime_start<-", "REC", function(x, value) {
  x@record_runtime_start <- value
  x
})
setMethod("record_runtime_end", "REC", function(x) x@record_runtime_end)
setMethod("record_runtime_end<-", "REC", function(x, value) {
  x@record_runtime_end <- value
  x
})
setMethod("record_runtime_duration", "REC", function(x) x@record_runtime_duration)
setMethod("record_runtime_duration<-", "REC", function(x, value) {
  x@record_runtime_duration <- value
  x
})


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
