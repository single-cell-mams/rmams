# generic function for fom
setGeneric("fom", function(mams, ...) standardGeneric("fom"))
setGeneric("fom<-", function(mams, ..., value) standardGeneric("fom<-"))


#' @title character or list class union
#' @description class to allow either NULL or list
#' @keywords internal
#' @noRd
setClassUnion("CharOrList", c("character", "list"))

# generic functions for all objects

setGeneric("id", function(x) standardGeneric("id"))
setGeneric("id<-", function(x, value) standardGeneric("id<-"))
setGeneric("dataset_id", function(x) standardGeneric("dataset_id"))
setGeneric("dataset_id<-", function(x, value) standardGeneric("dataset_id<-"))
setGeneric("filepath", function(x) standardGeneric("filepath"))
setGeneric("filepath<-", function(x, value) standardGeneric("filepath<-"))
setGeneric("accessor", function(x) standardGeneric("accessor"))
setGeneric("accessor<-", function(x, value) standardGeneric("accessor<-"))

# generic functions for Cross Reference to the subobjects
setGeneric("fom", function(x) standardGeneric("fom"))
setGeneric("fom<-", function(x, value) standardGeneric("fom<-"))
setGeneric("oid", function(x) standardGeneric("oid"))
setGeneric("oid<-", function(x, value) standardGeneric("oid<-"))
setGeneric("fid", function(x) standardGeneric("fid"))
setGeneric("fid<-", function(x, value) standardGeneric("fid<-"))
setGeneric("fea", function(x) standardGeneric("fea"))
setGeneric("fea<-", function(x, value) standardGeneric("fea<-"))
setGeneric("obs", function(x) standardGeneric("obs"))
setGeneric("obs<-", function(x, value) standardGeneric("obs<-"))
setGeneric("ong", function(x) standardGeneric("ong"))
setGeneric("ong<-", function(x, value) standardGeneric("ong<-"))
setGeneric("fng", function(x) standardGeneric("fng"))
setGeneric("fng<-", function(x, value) standardGeneric("fng<-"))

# generic functions for FOM objects

# Define a generic function and method for 'data_type'
setGeneric("data_type", function(x) standardGeneric("data_type"))
setGeneric("data_type<-", function(x, value) standardGeneric("data_type<-"))
# Define a generic function and method for 'representation'
setGeneric("representation", function(x) standardGeneric("representation"))
setGeneric("representation<-", function(x, value) standardGeneric("representation<-"))
# Define a generic function and method for 'representation_description'
setGeneric("representation_description", function(x) standardGeneric("representation_description"))
setGeneric("representation_description<-", function(x, value) standardGeneric("representation_description<-"))
# Define a generic function and method for 'obs_unit'
setGeneric("obs_unit", function(x) standardGeneric("obs_unit"))
setGeneric("obs_unit<-", function(x, value) standardGeneric("obs_unit<-"))
# Define a generic function and method for 'processing'
setGeneric("processing", function(x) standardGeneric("processing"))
setGeneric("processing<-", function(x, value) standardGeneric("processing<-"))
# Define a generic function and method for 'processing_description'
setGeneric("processing_description", function(x) standardGeneric("processing_description"))
setGeneric("processing_description<-", function(x, value) standardGeneric("processing_description<-"))
# Define a generic function and method for 'analyte'
setGeneric("analyte", function(x) standardGeneric("analyte"))
setGeneric("analyte<-", function(x, value) standardGeneric("analyte<-"))
# Define a generic function and method for 'analyte_description'
setGeneric("analyte_description", function(x) standardGeneric("analyte_description"))
setGeneric("analyte_description<-", function(x, value) standardGeneric("analyte_description<-"))
# Define a generic function and method for 'modality'
setGeneric("modality", function(x) standardGeneric("modality"))
setGeneric("modality<-", function(x, value) standardGeneric("modality<-"))
# Define a generic function and method for 'obs_subset'
setGeneric("obs_subset", function(x) standardGeneric("obs_subset"))
setGeneric("obs_subset<-", function(x, value) standardGeneric("obs_subset<-"))
# Define a generic function and method for 'obs_subset_description'
setGeneric("obs_subset_description", function(x) standardGeneric("obs_subset_description"))
setGeneric("obs_subset_description<-", function(x, value) standardGeneric("obs_subset_description<-"))
# Define a generic function and method for 'feature_subset'
setGeneric("feature_subset", function(x) standardGeneric("feature_subset"))
setGeneric("feature_subset<-", function(x, value) standardGeneric("feature_subset<-"))
# Define a generic function and method for 'feature_subset_description'
setGeneric("feature_subset_description", function(x) standardGeneric("feature_subset_description"))
setGeneric("feature_subset_description<-", function(x, value) standardGeneric("feature_subset_description<-"))
# Define a generic function and method for 'record_id'
setGeneric("record_id", function(x) standardGeneric("record_id"))
setGeneric("record_id<-", function(x, value) standardGeneric("record_id<-"))
# Define a generic function and method for 'parent_id'
setGeneric("parent_id", function(x) standardGeneric("parent_id"))
setGeneric("parent_id<-", function(x, value) standardGeneric("parent_id<-"))
# Define a generic function and method for 'parent_relationship'
setGeneric("parent_relationship", function(x) standardGeneric("parent_relationship"))
setGeneric("parent_relationship<-", function(x, value) standardGeneric("parent_relationship<-"))
# Define a generic function and method for 'parent_relationship_description'
setGeneric("parent_relationship_description", function(x) standardGeneric("parent_relationship_description"))
setGeneric("parent_relationship_description<-", function(x, value) standardGeneric("parent_relationship_description<-"))

# generic functions for neighbor graph objects (ONG,FNG)

setGeneric("edge_metric", function(x) standardGeneric("edge_metric"))
setGeneric("edge_metric<-", function(x, value) standardGeneric("edge_metric<-"))
setGeneric("metric_type", function(x) standardGeneric("metric_type"))
setGeneric("metric_type<-", function(x, value) standardGeneric("metric_type<-"))

# generic functions for REC objects

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

# generic functions for OID objects

setGeneric("oid_header", function(x) standardGeneric("oid_header"))
setGeneric("oid_header<-", function(x, value) standardGeneric("oid_header<-"))
setGeneric("oid_header_delim", function(x) standardGeneric("oid_header_delim"))
setGeneric("oid_header_delim<-", function(x, value) standardGeneric("oid_header_delim<-"))

# generic functions for FID objects
setGeneric("fid_header", function(x) standardGeneric("fid_header"))
setGeneric("fid_header<-", function(x, value) standardGeneric("fid_header<-"))
setGeneric("fid_header_delim", function(x) standardGeneric("fid_header_delim"))
setGeneric("fid_header_delim<-", function(x, value) standardGeneric("fid_header_delim<-"))

# generic functions for FEA objects
setGeneric("feature_modality", function(x) standardGeneric("feature_modality"))
setGeneric("feature_modality<-", function(x, value) standardGeneric("feature_modality<-"))
setGeneric("reference_database", function(x) standardGeneric("reference_database"))
setGeneric("reference_database<-", function(x, value) standardGeneric("reference_database<-"))
setGeneric("reference_organism", function(x) standardGeneric("reference_organism"))
setGeneric("reference_organism<-", function(x, value) standardGeneric("reference_organism<-"))
