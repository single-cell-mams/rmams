#' Converts Seurat object to a MAMS object
#' 
#' @param object_list A named list of Seurat objects to be converted to MAMS format
#' @param observation_subsets A vector with same length as object_list indicating the
#' observation subset name for each Seurat object. One of: full, filtered, threshold, detected,
#' nonartifact, clean.
#' @param dataset_id Specify id of the dataset
#' @return A MAMS object containing all the extracted metadata fields.
#' @export
#'
#' @examples
#' library(Seurat)
#' options(Seurat.object.assay.version = "v3")
#' counts <- matrix(rpois((500*200), 1), nrow = 500, ncol = 200, 
#'     dimnames = list(paste0("Row", 1:500), paste0("Col", 1:200)))
#' srt <- CreateSeuratObject(counts = counts)
#' srt <- NormalizeData(srt)
#' subset_srt <- srt[, 1:100]
#' mams <- convert_seurat_to_MAMS(object_list = list(srt = srt, 
#'     subset_srt = subset_srt), observation_subsets = c("full", "subset"), 
#'     dataset_id = "dataset1")
#' print(mams)
#' 
convert_seurat_to_MAMS <- function(object_list,observation_subsets,dataset_id){
    #FOMs <- list()
    #ONG <- list()
    FIDs <- c()
    MAMS <- create_MAMS_object()
    for(i in 1:length(object_list)){
        object <- object_list[[i]]
        filepath <- paste0(names(object_list)[[i]], ".rds")
        oid <- paste0("oid", i)
        obs <- paste0("obs", i)
        obs_subset <- observation_subsets[[i]]
        
        for(mod in SeuratObject::Assays(object)){
            fid <- paste0("fid", length(FIDs)+1)
            fea <- paste0("fea", length(FIDs)+1)
            FIDs <- c(FIDs, fid)
            
            if(mod == "RNA"){
                modality <- "rna"
                analyte <- "rna"
            }else{
                modality <- "protein"
                analyte <- "protein"
            }
            
            for(assay in slotNames(object@assays[[mod]])){
                fom <- paste0("fom", length(MAMS@FOM)+1)
                accessor <- paste0("GetAssayData(object = ", substr(filepath, 1, nchar(filepath)-4), ', slot = \"', assay, '\" assay = \"', mod, '\")')
                if(assay == "counts"){
                    data_type <- "int"
                    representation <- "sparse"
                    processing <- "counts"
                    feature_subset <- "full"
                    MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath=filepath, accessor=accessor, oid=oid, processing=processing, modality=modality, analyte=analyte, data_type = data_type,dataset_id = dataset_id)
                    
                }else if(assay == "data"){
                    data_type <- "double"
                    representation <- "sparse"
                    processing <- "lognormalized"
                    feature_subset <- "full"
                    MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath=filepath, accessor=accessor, oid=oid, processing=processing, modality=modality, analyte=analyte, data_type = data_type,dataset_id = dataset_id)
                    
                }else if(assay == "scale.data"){
                    data_type <- "double"
                    representation <- "dense"
                    processing <- "scaled"
                    feature_subset <- "variable"
                    MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath=filepath, accessor=accessor, oid=oid, processing=processing, modality=modality, analyte=analyte, data_type = data_type,dataset_id = dataset_id)
                    
                }
               # MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath=filepath, accessor=accessor, oid=oid, processing=processing, modality=modality, analyte=analyte, obs_subset = obs_subset)
              #  FOMs[[fom]] <- create_FOM_object(id = fom, filepath = filepath, accessor = accessor, representation = representation, analyte = analyte, modality = modality, obs_subset = obs_subset, feature_subset = feature_subset, oid = oid, fid = fid, obs = obs, fea = fea)
            }
        }
        
        for(dimred in names(object@reductions)){ 
            fom <- paste0("fom", length(MAMS@FOM)+1)
            reduction <- object@reductions[[dimred]]
            data_type = "double"
            if(grepl("pca|ica", dimred, ignore.case = TRUE)){
                processing<- "Reduction"
            } else if (grepl("tsne|umap", dimred, ignore.case = TRUE)){
                processing <- "Embedding"
            }
            accessor <- paste0(processing, "(object = ", substr(filepath, 1, nchar(filepath)-4), ', reduction = \"', dimred, '\")')
            MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath=filepath, accessor=accessor, oid=oid, processing=processing, modality=modality, analyte=analyte, obs_subset = obs_subset,dataset_id = dataset_id,data_type = data_type)
           # FOMs[[fom]] <- create_FOM_object(id = fom, filepath=filepath, accessor=accessor, oid=oid, processing=processing, modality=modality, analyte=analyte)
         }
        ## Graph
        for(graph in SeuratObject::Graphs(object)){
            #filepath <- paste0(names(object_list)[[i]], ".rds")
            ogr <- paste0("ogr", length(MAMS@ONG)+1)
            graphname <- paste("FindNeighbors", mod, dimred, sep = ".")
            edge_metric <- object@commands[[graphname]]$annoy.metric
            metric_type <- "distance"
            accessor <- paste0("Graphs(", substr(filepath, 1, nchar(filepath)-4), ', \"', graph, '\")')
            if(substr(graph, 1, 3) == "RNA"||substr(graph, 1, 3) == "ADT"){
                record_id <- paste("FindNeighbors", mod, dimred, substr(filepath, 15, nchar(filepath)-4), sep = ".")
            }
            else if(substr(graph, 1, 3) == "wsn"||substr(graph, 1, 3) == "wkn"){
                record_id <- paste("FindMultiModalNeighbors", substr(filepath, 15, nchar(filepath)-4), sep = ".")
            }
            MAMS@ONG[[ogr]] <- create_ONG_object(id = ogr, filepath = filepath, accessor = accessor, record_id = record_id, edge_metric = edge_metric, metric_type = metric_type,dataset_id = dataset_id)
           # ONG[[ogr]] <- create_ONG_object(id = ogr, filepath = filepath, accessor = accessor, record_id = record_id, edge_metric = edge_metric, metric_type = metric_type)
        }
        ## Neighbor
        for(neighbor in SeuratObject::Neighbors(object)){
            ogr <- paste0("ogr", length(MAMS@ONG)+1)
            graphname <- paste("FindNeighbors", mod, dimred, sep = ".")
            edge_metric <- object@commands[[graphname]]$annoy.metric
            metric_type <- "distance"
            accessor <- paste0("Neighbors(", substr(filepath, 1, nchar(filepath)-4), ', \"', neighbor, '\")')
            record_id <- paste("FindMultiModalNeighbors", substr(filepath, 15, nchar(filepath)-4), sep = ".")
            MAMS@ONG[[ogr]] <- create_ONG_object(id = ogr, filepath = filepath, accessor = accessor, record_id = record_id, edge_metric = edge_metric, metric_type = metric_type,dataset_id = dataset_id)
            #ONG[[ogr]] <- create_ONG_object(id = ogr, filepath = filepath, accessor = accessor, record_id = record_id, edge_metric = edge_metric, metric_type = metric_type)
        }
    }
   # MAMS <- create_MAMS_object(FOM = FOMs, ONG = ONGs)
    return(MAMS)
}


#' Converts a SingleCellExperiment object to a MAMS object 
#' 
#' @param object_list A named list of SingleCellExperiment 
#'  objects to be converted to MAMS format
#' @param observation_subsets A vector with same length as object_list 
#'  indicating the observation subset name for each 
#'  SingleCellExperiment object. One of: full, filtered, threshold, detected,
#'  nonartifact, clean.
#' @param dataset_id Specify id of the dataset
#' @return A MAMS object containing all the extracted metadata fields.
#' @export
#'
#' @examples
#' library(SingleCellExperiment)
#' counts <- matrix(rpois((500*200), 1), nrow = 500, ncol = 200, 
#'     dimnames = list(paste0("Row", 1:500), paste0("Col", 1:200)))
#' sce <- SingleCellExperiment(list(counts=counts))
#' sce <- scuttle::logNormCounts(sce)
#' subset_sce <- sce[, 1:100]
#' mams <- convert_SCE_to_MAMS(object_list = list(sce = sce, 
#'     subset_sce = subset_sce), observation_subsets = c("full", "subset"), 
#'     dataset_id = "dataset1")
#' print(mams)
convert_SCE_to_MAMS <- function(object_list, observation_subsets, dataset_id) {
    # Create empty mams object
    MAMS <- create_MAMS_object()
    FIDs <- c()
    
    # Iterate over input objects in object_list
    for (i in 1:length(object_list)) {
        object <- object_list[[i]]
        filepath <- paste0(names(object_list)[[i]], ".rds")
        oid <- paste0("oid", i)
        obs_subset <- observation_subsets[[i]]
        
        # Iterate over assays
        for (mod in SummarizedExperiment::assayNames(object)) {
            fid <- paste0("fid", length(FIDs) + 1)
            fea <- paste0("fea", length(FIDs) + 1)
            FIDs <- c(FIDs, fid)
            
            if (mod == "counts") {
                modality <- "rna"
                analyte <- "rna"
            } else if (mod == "logcounts") {
                modality <- "rna"
                analyte <- "rna"
            } else if (mod == "cpm" || mod == "tpm" || mod == "fpkm") {
                modality <- "rna"
                analyte <- "rna"
            } else {
                modality <- "protein"
                analyte <- "protein"
            }
            
            assay_data <- SummarizedExperiment::assay(object, mod)
            fom <- paste0("fom", length(MAMS@FOM) + 1)
            accessor <- paste0("assay(object = ", substr(filepath, 1, nchar(filepath) - 4), ", \"", mod, "\")")
            
            if (mod == "counts") {
                data_type <- "int"
                representation <- "sparse"
                processing <- "counts"
                feature_subset <- "full"
            } else if (mod == "logcounts") {
                data_type <- "double"
                representation <- "sparse"
                processing <- "lognormalized"
                feature_subset <- "full"
            } else if (mod == "cpm" || mod == "tpm" || mod == "fpkm") {
                data_type <- "double"
                representation <- "dense"
                processing <- "scaled"
                feature_subset <- "variable"
            }
            
            MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath = filepath, accessor = accessor, oid = oid, processing = processing, modality = modality, analyte = analyte, data_type = data_type, dataset_id = dataset_id)
        }
        
        # Iterate over reducedDims
        if (length(reducedDimNames(object)) > 0) {
            for (dimred in names(SingleCellExperiment::reducedDims(object))) {
                fom <- paste0("fom", length(MAMS@FOM) + 1)
                reduction <- SingleCellExperiment::reducedDim(object, dimred)
                data_type <- "double"
                processing <- ifelse(grepl("pca|ica", dimred, ignore.case = TRUE), "Reduction", ifelse(grepl("tsne|umap", dimred, ignore.case = TRUE), "Embedding", NA))
                accessor <- paste0(processing, "(object = ", substr(filepath, 1, nchar(filepath) - 4), ", reduction = \"", dimred, "\")")
                
                MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath = filepath, accessor = accessor, oid = oid, processing = processing, modality = modality, analyte = analyte, obs_subset = obs_subset, dataset_id = dataset_id, data_type = data_type)
            }
        }
    }
    
    return(MAMS)
}


#' Converts a AnnData object to a MAMS object 
#' 
#' @param object_list A named list of AnnData objects 
#'  objects to be converted to MAMS format
#' @param observation_subsets A vector with same length as object_list 
#'  indicating the observation subset name for each 
#'  AnnData object. One of: full, filtered, threshold, detected,
#'  nonartifact, clean.
#' @param dataset_id Specify id of the dataset
#' @return A MAMS object containing all the extracted metadata fields.
#' @export
#'
#' @examples
#' \dontrun{
#' library(reticulate)
#' anndata <- import("anndata")
#' counts <- matrix(rpois((500*200), 1), nrow = 500, ncol = 200, 
#'     dimnames = list(paste0("Row", 1:500), paste0("Col", 1:200)))
#' adata <- anndata$AnnData(X = counts)
#' adata$obsm[["X_pca"]] <- counts[, 1:2]
#' adata$obsm[["X_tsne"]] <- counts[, 3:4]
#' mams <- convert_AnnData_to_MAMS(object_list = list(adata = adata), 
#'     observation_subsets = c("full"), dataset_id = "dataset1")
#' print(mams)
#' }
convert_AnnData_to_MAMS <- function(object_list, observation_subsets, dataset_id) {
    # Create emoty mams object
    MAMS <- create_MAMS_object()
    FIDs <- c()
    
    # Iterate over each input object in the object_list
    for (i in 1:length(object_list)) {
        object <- object_list[[i]]
        filepath <- paste0(names(object_list)[[i]], ".h5ad")
        oid <- paste0("oid", i)
        obs_subset <- observation_subsets[[i]]
        
        # Iterate over assays
        assays <- list("X" = object$X, "raw.X" = if (!is.null(object$raw)) object$raw$X else NULL)
        for (mod in names(assays)) {
            if (!is.null(mod)) {
                fid <- paste0("fid", length(FIDs) + 1)
                fea <- paste0("fea", length(FIDs) + 1)
                FIDs <- c(FIDs, fid)
                
                if (mod == "X") {
                    modality <- "rna"
                    analyte <- "rna"
                } else if (mod == "raw.X") {
                    modality <- "rna"
                    analyte <- "rna"
                } else {
                    modality <- "protein"
                    analyte <- "protein"
                }
                
                fom <- paste0("fom", length(MAMS@FOM) + 1)
                accessor <- paste0("object[['", mod, "']]")
                
                data_type <- if (mod == "X" || mod == "raw.X") "double" else "int"
                representation <- if (is(object$X, "sparse")) "sparse" else "dense"
                processing <- if (mod == "X") "lognormalized" else "counts"
                feature_subset <- "full"
                
                MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath = filepath, accessor = accessor, oid = oid, processing = processing, modality = modality, analyte = analyte, data_type = data_type, dataset_id = dataset_id)
            }
        }
        
        # Iterate over reducedDims
        if ("obsm" %in% names(object)) {
            for (dimred in object$obsm_keys()) {
                fom <- paste0("fom", length(MAMS@FOM) + 1)
                reduction <- object$obsm[[dimred]]
                data_type <- "double"
                processing <- ifelse(grepl("pca|ica", dimred, ignore.case = TRUE), "Reduction", ifelse(grepl("tsne|umap", dimred, ignore.case = TRUE), "Embedding", NA))
                accessor <- paste0("object$obsm[['", dimred, "']]")
                
                MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath = filepath, accessor = accessor, oid = oid, processing = processing, modality = modality, analyte = analyte, obs_subset = obs_subset, dataset_id = dataset_id, data_type = data_type)
            }
        }
    }
    
    return(MAMS)
}
        