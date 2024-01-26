# generic functions for all objects

setGeneric("id", function(x) standardGeneric("id"))
setGeneric("id<-", function(x, value) standardGeneric("id<-"))
setGeneric("dataset_id", function(x) standardGeneric("dataset_id"))
setGeneric("dataset_id<-", function(x, value) standardGeneric("dataset_id<-"))

# Generic needed for Neighbor Graph S4 objects(ONG,FNG)

setGeneric("edge_metric", function(x) standardGeneric("edge_metric"))
setGeneric("edge_metric<-", function(x, value) standardGeneric("edge_metric<-"))
setGeneric("metric_type", function(x) standardGeneric("metric_type"))
setGeneric("metric_type<-", function(x, value) standardGeneric("metric_type<-"))

#Generics for REC S4 objects

setGeneric("record_id", function(x) standardGeneric("record_id"))
setGeneric("record_id<-", function(x, value) standardGeneric("record_id<-"))
setGeneric("record_package_name", function(x) standardGeneric("record_package_name"))
setGeneric("record_package_name<-", function(x, value) standardGeneric("record_package_name<-"))
setGeneric("record_package_version", function(x) standardGeneric("record_package_version"))
setGeneric("record_package_version<-", function(x, value) standardGeneric("record_package_version<-"))
setGeneric("record_function_name", function(x) standardGeneric("record_function_name"))
setGeneric("record_function_name<-", function(x, value) standardGeneric("record_function_name<-"))
setGeneric("record_function_parameters", function(x) standardGeneric("record_function_parameters"))
setGeneric("record_function_parameters<-", function(x, value) standardGeneric("record_function_parameters<-"))
setGeneric("record_workflow_link", function(x) standardGeneric("record_workflow_link"))
setGeneric("record_workflow_link<-", function(x, value) standardGeneric("record_workflow_link<-"))
setGeneric("record_runtime_start", function(x) standardGeneric("record_runtime_start"))
setGeneric("record_runtime_start<-", function(x, value) standardGeneric("record_runtime_start<-"))
setGeneric("record_runtime_end", function(x) standardGeneric("record_runtime_end"))
setGeneric("record_runtime_end<-", function(x, value) standardGeneric("record_runtime_end<-"))
setGeneric("record_runtime_duration", function(x) standardGeneric("record_runtime_duration"))
setGeneric("record_runtime_duration<-", function(x, value) standardGeneric("record_runtime_duration<-"))
