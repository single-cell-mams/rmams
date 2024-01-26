# generic functions for all objects

setGeneric("id", function(x) standardGeneric("id"))
setGeneric("id<-", function(x, value) standardGeneric("id<-"))
setGeneric("dataset_id", function(x) standardGeneric("dataset_id"))
setGeneric("dataset_id<-", function(x, value) standardGeneric("dataset_id<-"))

# generic functions for the OID S4 object

setGeneric("oid_header", function(x) standardGeneric("oid_header"))
setGeneric("oid_header<-", function(x, value) standardGeneric("oid_header<-"))
setGeneric("oid_header_delim", function(x) standardGeneric("oid_header_delim"))
setGeneric("oid_header_delim<-", function(x, value) standardGeneric("oid_header_delim<-"))

# generic functions for the FID S4 object

setGeneric("fid_header", function(x) standardGeneric("fid_header"))
setGeneric("fid_header<-", function(x, value) standardGeneric("fid_header<-"))
setGeneric("fid_header_delim", function(x) standardGeneric("fid_header_delim"))
setGeneric("fid_header_delim<-", function(x, value) standardGeneric("fid_header_delim<-"))
