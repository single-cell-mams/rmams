## FOM generics

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
