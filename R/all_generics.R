# generic function for high level setters/getters
#' fom
#' @rdname FOM-get-generic
#' @description MAMS generic getter for FOM objects
#' @param mams the MAMS object
#' @param fom_id the FOM ID
#' @param key key of the value wanted
#' @return the value
setGeneric("fom", function(mams, fom_id, key) standardGeneric("fom"))
#' fom<-
#' @rdname FOM-set-generic
#' @description MAMS generic setter for FOM objects
#' @param mams the MAMS object
#' @param fom_id the FOM ID
#' @param key key of the value wanted
#' @param value the new value
#' @return nothing (setter)
setGeneric("fom<-", function(mams, fom_id, key, value) standardGeneric("fom<-"))
#' fid
#' @rdname FID-get-generic
#' @description MAMS generic getter for FID objects
#' @param mams the MAMS object
#' @param fid_id the FID ID
#' @param key key of the value wanted
#' @return the value
setGeneric("fid", function(mams, fid_id, key) standardGeneric("fid"))
#' fid<-
#' @rdname FID-set-generic
#' @description MAMS generic setter for FID objects
#' @param mams the MAMS object
#' @param fid_id the FID ID
#' @param key key of the value wanted
#' @param value the new value
#' @return nothing (setter)
setGeneric("fid<-", function(mams, fid_id, key, value) standardGeneric("fid<-"))
#' oid
#' @rdname OID-get-generic
#' @description MAMS generic getter for OID objects
#' @param mams the MAMS object
#' @param oid_id the OID ID
#' @param key key of the value wanted
#' @return the value
setGeneric("oid", function(mams, oid_id, key) standardGeneric("oid"))
#' oid<-
#' @rdname OID-set-generic
#' @description MAMS generic setter for OID objects
#' @param mams the MAMS object
#' @param oid_id the OID ID
#' @param key key of the value wanted
#' @param value the new value
#' @return nothing (setter)
setGeneric("oid<-", function(mams, oid_id, key, value) standardGeneric("oid<-"))
#' fea
#' @rdname FEA-get-generic
#' @description MAMS generic getter for FEA objects
#' @param mams the MAMS object
#' @param fea_id the FEA ID
#' @param key key of the value wanted
#' @return the value
setGeneric("fea", function(mams, fea_id, key) standardGeneric("fea"))
#' fea<-
#' @rdname FEA-set-generic
#' @description MAMS generic setter for FEA objects
#' @param mams the MAMS object
#' @param fea_id the FEA ID
#' @param key key of the value wanted
#' @param value the new value
#' @return nothing (setter)
setGeneric("fea<-", function(mams, fea_id, key, value) standardGeneric("fea<-"))
#' fng
#' @rdname FNG-get-generic
#' @description MAMS generic getter for FNG objects
#' @param mams the MAMS object
#' @param fng_id the FNG ID
#' @param key key of the value wanted
#' @return the value
setGeneric("fng", function(mams, fng_id, key) standardGeneric("fng"))
#' fng<-
#' @rdname FNG-set-generic
#' @description MAMS generic setter for FNG objects
#' @param mams the MAMS object
#' @param fng_id the FNG ID
#' @param key key of the value wanted
#' @param value the new value
#' @return nothing (setter)
setGeneric("fng<-", function(mams, fng_id, key, value) standardGeneric("fng<-"))
#' ong
#' @rdname ONG-get-generic
#' @description MAMS generic getter for ONG objects
#' @param mams the MAMS object
#' @param ong_id the ONG ID
#' @param key key of the value wanted
#' @return the value
setGeneric("ong", function(mams, ong_id, key) standardGeneric("ong"))
#' ong<-
#' @rdname ONG-set-generic
#' @description MAMS generic setter for ONG objects
#' @param mams the MAMS object
#' @param ong_id the ONG ID
#' @param key key of the value wanted
#' @param value the new value
#' @return nothing (setter)
setGeneric("ong<-", function(mams, ong_id, key, value) standardGeneric("ong<-"))
#' obs
#' @rdname OBS-get-generic
#' @description MAMS generic getter for OBS objects
#' @param mams the MAMS object
#' @param obs_id the OBS ID
#' @param key key of the value wanted
#' @return the value
setGeneric("obs", function(mams, obs_id, key) standardGeneric("obs"))
#' obs<-
#' @rdname OBS-set-generic
#' @description MAMS generic setter for OBS objects
#' @param mams the MAMS object
#' @param obs_id the OBS ID
#' @param key key of the value wanted
#' @param value the new value
#' @return nothing (setter)
setGeneric("obs<-", function(mams, obs_id, key, value) standardGeneric("obs<-"))
#' rec
#' @rdname REC-get-generic
#' @description MAMS generic getter for REC objects
#' @param mams the MAMS object
#' @param rec_id the REC ID
#' @param key key of the value wanted
#' @return the value
setGeneric("rec", function(mams, rec_id, key) standardGeneric("rec"))
#' rec<-
#' @rdname REC-set-generic
#' @description MAMS generic setter for REC objects
#' @param mams the MAMS object
#' @param rec_id the REC ID
#' @param key key of the value wanted
#' @param value the new value
#' @return nothing (setter)
setGeneric("rec<-", function(mams, rec_id, key, value) standardGeneric("rec<-"))

#' @title character or list class union
#' @description class to allow either NULL or list
#' @keywords internal
#' @noRd
setClassUnion("CharOrList", c("character", "list", "NULL"))

#' @title character or NULL class union
#' @description class to allow either NULL or character
#' @keywords internal
#' @noRd
setClassUnion("CharOrNULL", c("character", "NULL"))

#' @title character or list class union
#' @description class to allow either NULL or list
#' @keywords internal
#' @noRd
setClassUnion("ListOrNULL", c("list", "NULL"))

# generic functions for all objects

#' id
#' @rdname id-get-generic
#' @description generic getter for id field
#' @param x the id to get
#' @return the value
#' @keywords internal           
setGeneric("id", function(x) standardGeneric("id"))
#' id<-
#' @rdname id-set-generic
#' @description generic getter for id field
#' @param x the id to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("id<-", function(x, value) standardGeneric("id<-"))
#' dataset_id
#' @rdname dataset_id-get-generic
#' @description generic getter for dataset_id field
#' @param x the dataset_id to get
#' @return the value
#' @keywords internal           
setGeneric("dataset_id", function(x) standardGeneric("dataset_id"))
#' dataset_id<-
#' @rdname dataset_id-set-generic
#' @description generic getter for dataset_id field
#' @param x the dataset_id to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("dataset_id<-", function(x, value) standardGeneric("dataset_id<-"))
#' filepath
#' @rdname filepath-get-generic
#' @description generic getter for filepath field
#' @param x the filepath to get
#' @return the value
#' @keywords internal           
setGeneric("filepath", function(x) standardGeneric("filepath"))
#' filepath<-
#' @rdname filepath-set-generic
#' @description generic getter for filepath field
#' @param x the filepath to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("filepath<-", function(x, value) standardGeneric("filepath<-"))
#' accessor
#' @rdname accessor-get-generic
#' @description generic getter for accessor field
#' @param x the accessor to get
#' @return the value
#' @keywords internal           
setGeneric("accessor", function(x) standardGeneric("accessor"))
#' accessor<-
#' @rdname accessor-set-generic
#' @description generic getter for accessor field
#' @param x the accessor to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("accessor<-", function(x, value) standardGeneric("accessor<-"))

# Define a generic function and method for 'data_type'
#' data_type
#' @rdname data_type-get-generic
#' @description generic getter for data_type field
#' @param x the data_type to get
#' @return the value
#' @keywords internal           
setGeneric("data_type", function(x) standardGeneric("data_type"))
#' data_type<-
#' @rdname data_type-set-generic
#' @description generic getter for data_type field
#' @param x the data_type to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("data_type<-", function(x, value) standardGeneric("data_type<-"))
# Define a generic function and method for 'representation'
#' representation
#' @rdname representation-get-generic
#' @description generic getter for representation field
#' @param x the representation to get
#' @return the value
#' @keywords internal           
setGeneric("representation", function(x) standardGeneric("representation"))
#' representation<-
#' @rdname representation-set-generic
#' @description generic getter for representation field
#' @param x the representation to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("representation<-", function(x, value) standardGeneric("representation<-"))
# Define a generic function and method for 'representation_description'
#' representation_description
#' @rdname representation_description-get-generic
#' @description generic getter for representation_description field
#' @param x the representation_description to get
#' @return the value
#' @keywords internal           
setGeneric("representation_description", function(x) standardGeneric("representation_description"))
#' representation_description<-
#' @rdname representation_description-set-generic
#' @description generic getter for representation_description field
#' @param x the representation_description to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("representation_description<-", function(x, value) standardGeneric("representation_description<-"))
# Define a generic function and method for 'obs_unit'
#' obs_unit
#' @rdname obs_unit-get-generic
#' @description generic getter for obs_unit field
#' @param x the obs_unit to get
#' @return the value
#' @keywords internal           
setGeneric("obs_unit", function(x) standardGeneric("obs_unit"))
#' obs_unit<-
#' @rdname obs_unit-set-generic
#' @description generic getter for obs_unit field
#' @param x the obs_unit to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("obs_unit<-", function(x, value) standardGeneric("obs_unit<-"))

# Define a generic function and method for 'processing'
#' processing
#' @rdname processing-get-generic
#' @description generic getter for processing field
#' @param x the processing to get
#' @return the value
#' @keywords internal           
setGeneric("processing", function(x) standardGeneric("processing"))
#' processing<-
#' @rdname processing-set-generic
#' @description generic getter for processing field
#' @param x the processing to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("processing<-", function(x, value) standardGeneric("processing<-"))

# Define a generic function and method for 'processing_description'
#' processing_description
#' @rdname processing_description-get-generic
#' @description generic getter for processing_description field
#' @param x the processing_description to get
#' @return the value
#' @keywords internal           
setGeneric("processing_description", function(x) standardGeneric("processing_description"))
#' processing_description<-
#' @rdname processing_description-set-generic
#' @description generic getter for processing_description field
#' @param x the processing_description to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("processing_description<-", function(x, value) standardGeneric("processing_description<-"))

# Define a generic function and method for 'analyte'
#' analyte
#' @rdname analyte-get-generic
#' @description generic getter for analyte field
#' @param x the analyte to get
#' @return the value
#' @keywords internal           
setGeneric("analyte", function(x) standardGeneric("analyte"))
#' analyte<-
#' @rdname analyte-set-generic
#' @description generic getter for analyte field
#' @param x the analyte to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("analyte<-", function(x, value) standardGeneric("analyte<-"))

# Define a generic function and method for 'analyte_description'
#' analyte_description
#' @rdname analyte_description-get-generic
#' @description generic getter for analyte_description field
#' @param x the analyte_description to get
#' @return the value
#' @keywords internal           
setGeneric("analyte_description", function(x) standardGeneric("analyte_description"))
#' analyte_description<-
#' @rdname analyte_description-set-generic
#' @description generic getter for analyte_description field
#' @param x the analyte_description to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("analyte_description<-", function(x, value) standardGeneric("analyte_description<-"))

# Define a generic function and method for 'modality'
#' processing_modalitydescription
#' @rdname modality-get-generic
#' @description generic getter for modality field
#' @param x the modality to get
#' @return the value
#' @keywords internal           
setGeneric("modality", function(x) standardGeneric("modality"))
#' modality<-
#' @rdname modality-set-generic
#' @description generic getter for modality field
#' @param x the modality to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("modality<-", function(x, value) standardGeneric("modality<-"))

# Define a generic function and method for 'obs_subset'
#' obs_subset
#' @rdname obs_subset-get-generic
#' @description generic getter for obs_subset field
#' @param x the obs_subset to get
#' @return the value
#' @keywords internal           
setGeneric("obs_subset", function(x) standardGeneric("obs_subset"))
#' obs_subset<-
#' @rdname obs_subset-set-generic
#' @description generic getter for obs_subset field
#' @param x the obs_subset to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("obs_subset<-", function(x, value) standardGeneric("obs_subset<-"))

# Define a generic function and method for 'obs_subset_description'
#' obs_subset_description
#' @rdname obs_subset_description-get-generic
#' @description generic getter for obs_subset_description field
#' @param x the obs_subset_description to get
#' @return the value
#' @keywords internal           
setGeneric("obs_subset_description", function(x) standardGeneric("obs_subset_description"))
#' obs_subset_description<-
#' @rdname obs_subset_description-set-generic
#' @description generic getter for obs_subset_description field
#' @param x the obs_subset_description to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("obs_subset_description<-", function(x, value) standardGeneric("obs_subset_description<-"))

# Define a generic function and method for 'feature_subset'
#' feature_subset
#' @rdname feature_subset-get-generic
#' @description generic getter for feature_subset field
#' @param x the feature_subset to get
#' @return the value
#' @keywords internal           
setGeneric("feature_subset", function(x) standardGeneric("feature_subset"))
#' feature_subset<-
#' @rdname feature_subset-set-generic
#' @description generic getter for feature_subset field
#' @param x the feature_subset to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("feature_subset<-", function(x, value) standardGeneric("feature_subset<-"))

# Define a generic function and method for 'feature_subset_description'
#' feature_subset_description
#' @rdname feature_subset_description-get-generic
#' @description generic getter for feature_subset_description field
#' @param x the obs_subset_descrifeature_subset_descriptionption to get
#' @return the value
#' @keywords internal           
setGeneric("feature_subset_description", function(x) standardGeneric("feature_subset_description"))
#' feature_subset_description<-
#' @rdname feature_subset_description-set-generic
#' @description generic getter for feature_subset_description field
#' @param x the feature_subset_description to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("feature_subset_description<-", function(x, value) standardGeneric("feature_subset_description<-"))

# Define a generic function and method for 'record_id'
#' record_id
#' @rdname record_id-get-generic
#' @description generic getter for record_id field
#' @param x the record_id to get
#' @return the value
#' @keywords internal           
setGeneric("record_id", function(x) standardGeneric("record_id"))
#' record_id<-
#' @rdname record_id-set-generic
#' @description generic getter for record_id field
#' @param x the record_id to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("record_id<-", function(x, value) standardGeneric("record_id<-"))

# Define a generic function and method for 'parent_id'
#' parent_id
#' @rdname parent_id-get-generic
#' @description generic getter for parent_id field
#' @param x the parent_id to get
#' @return the value
#' @keywords internal           
setGeneric("parent_id", function(x) standardGeneric("parent_id"))
#' parent_id<-
#' @rdname parent_id-set-generic
#' @description generic getter for parent_id field
#' @param x the parent_id to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("parent_id<-", function(x, value) standardGeneric("parent_id<-"))

# Define a generic function and method for 'parent_relationship'
#' parent_relationship
#' @rdname parent_relationship-get-generic
#' @description generic getter for parent_relationship field
#' @param x the parent_relationship to get
#' @return the value
#' @keywords internal           
setGeneric("parent_relationship", function(x) standardGeneric("parent_relationship"))
#' parent_relationship<-
#' @rdname parent_relationship-set-generic
#' @description generic getter for parent_relationship field
#' @param x the parent_relationship to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("parent_relationship<-", function(x, value) standardGeneric("parent_relationship<-"))

# Define a generic function and method for 'parent_relationship_description'
#' parent_relationship_description
#' @rdname parent_relationship_description-get-generic
#' @description generic getter for parent_relationship_description field
#' @param x the parent_relationship_description to get
#' @return the value
#' @keywords internal           
setGeneric("parent_relationship_description", function(x) standardGeneric("parent_relationship_description"))
#' parent_relationship_description<-
#' @rdname parent_relationship_description-set-generic
#' @description generic getter for parent_relationship_description field
#' @param x the parent_relationship_description to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("parent_relationship_description<-", function(x, value) standardGeneric("parent_relationship_description<-"))

#' edge_metric
#' @rdname edge_metric-get-generic
#' @description generic getter for edge_metric field
#' @param x the edge_metric to get
#' @return the value
#' @keywords internal           
setGeneric("edge_metric", function(x) standardGeneric("edge_metric"))
#' edge_metric<-
#' @rdname edge_metric-set-generic
#' @description generic getter for edge_metric field
#' @param x the edge_metric to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("edge_metric<-", function(x, value) standardGeneric("edge_metric<-"))

#' metric_type
#' @rdname metric_type-get-generic
#' @description generic getter for metric_type field
#' @param x the metric_type to get
#' @return the value
#' @keywords internal           
setGeneric("metric_type", function(x) standardGeneric("metric_type"))
#' metric_type<-
#' @rdname metric_type-set-generic
#' @description generic getter for metric_type field
#' @param x the metric_type to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("metric_type<-", function(x, value) standardGeneric("metric_type<-"))

#' record_id
#' @rdname record_id-get-generic
#' @description generic getter for record_id field
#' @param x the record_id to get
#' @return the value
#' @keywords internal           
setGeneric("record_id", function(x) standardGeneric("record_id"))
#' record_id<-
#' @rdname record_id-set-generic
#' @description generic getter for record_id field
#' @param x the record_id to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("record_id<-", function(x, value) standardGeneric("record_id<-"))

#' record_package_name
#' @rdname record_package_name-get-generic
#' @description generic getter for record_package_name field
#' @param x the record_package_name to get
#' @return the value
#' @keywords internal           
setGeneric("record_package_name", function(x) standardGeneric("record_package_name"))
#' record_package_name<-
#' @rdname record_package_name-set-generic
#' @description generic getter for record_package_name field
#' @param x the record_package_name to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("record_package_name<-", function(x, value) standardGeneric("record_package_name<-"))

#' record_package_version
#' @rdname record_package_version-get-generic
#' @description generic getter for record_package_version field
#' @param x the record_package_version to get
#' @return the value
#' @keywords internal           
setGeneric("record_package_version", function(x) standardGeneric("record_package_version"))
#' record_package_version<-
#' @rdname record_package_version-set-generic
#' @description generic getter for record_package_version field
#' @param x the record_package_version to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("record_package_version<-", function(x, value) standardGeneric("record_package_version<-"))

#' record_function_name
#' @rdname record_function_name-get-generic
#' @description generic getter for record_function_name field
#' @param x the record_function_name to get
#' @return the value
#' @keywords internal           
setGeneric("record_function_name", function(x) standardGeneric("record_function_name"))
#' record_function_name<-
#' @rdname record_function_name-set-generic
#' @description generic getter for record_function_name field
#' @param x the record_function_name to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("record_function_name<-", function(x, value) standardGeneric("record_function_name<-"))

#' record_function_parameters
#' @rdname record_function_parameters-get-generic
#' @description generic getter for record_function_parameters field
#' @param x the record_function_parameters to get
#' @return the value
#' @keywords internal           
setGeneric("record_function_parameters", function(x) standardGeneric("record_function_parameters"))
#' record_function_parameters<-
#' @rdname record_function_parameters-set-generic
#' @description generic getter for record_function_parameters field
#' @param x the record_function_parameters to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("record_function_parameters<-", function(x, value) standardGeneric("record_function_parameters<-"))

#' record_workflow_link
#' @rdname record_workflow_link-get-generic
#' @description generic getter for record_workflow_link field
#' @param x the record_workflow_link to get
#' @return the value
#' @keywords internal           
setGeneric("record_workflow_link", function(x) standardGeneric("record_workflow_link"))
#' record_workflow_link<-
#' @rdname record_workflow_link-set-generic
#' @description generic getter for record_workflow_link field
#' @param x the record_workflow_link to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("record_workflow_link<-", function(x, value) standardGeneric("record_workflow_link<-"))

#' record_runtime_start
#' @rdname record_runtime_start-get-generic
#' @description generic getter for record_runtime_start field
#' @param x the record_runtime_start to get
#' @return the value
#' @keywords internal           
setGeneric("record_runtime_start", function(x) standardGeneric("record_runtime_start"))
#' record_runtime_start<-
#' @rdname record_runtime_start-set-generic
#' @description generic getter for record_runtime_start field
#' @param x the record_runtime_start to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("record_runtime_start<-", function(x, value) standardGeneric("record_runtime_start<-"))

#' record_runtime_end
#' @rdname record_runtime_end-get-generic
#' @description generic getter for record_runtime_end field
#' @param x the record_runtime_end to get
#' @return the value
#' @keywords internal           
setGeneric("record_runtime_end", function(x) standardGeneric("record_runtime_end"))
#' record_runtime_end<-
#' @rdname record_runtime_end-set-generic
#' @description generic getter for record_runtime_end field
#' @param x the record_runtime_end to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("record_runtime_end<-", function(x, value) standardGeneric("record_runtime_end<-"))

#' record_runtime_duration
#' @rdname record_runtime_duration-get-generic
#' @description generic getter for record_runtime_duration field
#' @param x the record_runtime_duration to get
#' @return the value
#' @keywords internal           
setGeneric("record_runtime_duration", function(x) standardGeneric("record_runtime_duration"))
#' record_runtime_duration<-
#' @rdname record_runtime_duration-set-generic
#' @description generic getter for record_runtime_duration field
#' @param x the record_runtime_duration to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("record_runtime_duration<-", function(x, value) standardGeneric("record_runtime_duration<-"))

#' feature_modality
#' @rdname feature_modality-get-generic
#' @description generic getter for feature_modality field
#' @param x the feature_modality to get
#' @return the value
#' @keywords internal           
setGeneric("feature_modality", function(x) standardGeneric("feature_modality"))
#' feature_modality<-
#' @rdname feature_modality-set-generic
#' @description generic getter for feature_modality field
#' @param x the feature_modality to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("feature_modality<-", function(x, value) standardGeneric("feature_modality<-"))

#' reference_database
#' @rdname reference_database-get-generic
#' @description generic getter for reference_database field
#' @param x the reference_database to get
#' @return the value
#' @keywords internal           
setGeneric("reference_database", function(x) standardGeneric("reference_database"))
#' reference_database<-
#' @rdname reference_database-set-generic
#' @description generic getter for reference_database field
#' @param x the reference_database to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("reference_database<-", function(x, value) standardGeneric("reference_database<-"))

#' reference_organism
#' @rdname reference_organism-get-generic
#' @description generic getter for reference_organism field
#' @param x the reference_organism to get
#' @return the value
#' @keywords internal           
setGeneric("reference_organism", function(x) standardGeneric("reference_organism"))
#' reference_organism<-
#' @rdname reference_organism-set-generic
#' @description generic getter for reference_organism field
#' @param x the reference_organism to get
#' @param value the new value
#' @return nothing (setter)
#' @keywords internal           
setGeneric("reference_organism<-", function(x, value) standardGeneric("reference_organism<-"))


# Define the S4 generic function for to S3 list converter
setGeneric("collapse_to_list", function(x,...) {
  standardGeneric("collapse_to_list")
})

