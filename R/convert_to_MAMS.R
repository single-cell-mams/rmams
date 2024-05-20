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
        obs_subset_description<-obs_sub_desc[[obs_subset]]
        
        for(mod in SeuratObject::Assays(object)){
            
            
           
            
            if(mod == "RNA"){
                modality <- "rna"
                analyte <- "rna"
            }else{
                modality <- "protein"
                analyte <- "protein"
            }
            analyte_description<- analyte_desc[[analyte]]
            for(assay in slotNames(object@assays[[mod]])){
                fom <- paste0("fom", length(MAMS@FOM)+1)
                accessor <- paste0("GetAssayData(object = ", substr(filepath, 1, nchar(filepath)-4), ', slot = \"', assay, '\" assay = \"', mod, '\")')
                if(assay == "counts"){
                    data_type <- "int"
                    if (SeuratObject::IsSparse(object@assays[[mod]][assay]) == TRUE){
                        representation<- "sparse"
                        representation_description<- "The matrix contains zeros for the majority of the measurements"
                        
                    }
                    else { 
                           representation<- "dense"
                           representation_description<- "The matrix contains zeros for the majority of the measurements"
                           
                    }
                    
                    processing <- "counts"
                    processing_description <- process_desc[[processing]]
                    feature_subset <- "full"
                    feature_subset_description<- fea_desc[[feature_subset]]
                    obs_unit<- "cell"
                    obs_unit_description<- obs_desc[[obs_unit]]
                    fid <- paste0("fid", length(FIDs)+1)
                    fea <- paste0("fea", length(FIDs)+1)
                    FIDs <- c(FIDs, fid)
                    MAMS@FOM[[fom]] <- create_FOM_object(id = fom, 
                                                         filepath=filepath, 
                                                         accessor=accessor, 
                                                         fid=fid,
                                                         fea=fea,
                                                         oid=oid, 
                                                         processing=processing, 
                                                         processing_description=processing_description,
                                                         modality=modality, 
                                                         analyte=analyte, 
                                                         analyte_description=analyte_description,
                                                         feature_subset=feature_subset,
                                                         feature_subset_description = feature_subset_description,
                                                         data_type = data_type,
                                                         representation= representation,
                                                         representation_description = representation_description,
                                                         obs_unit=obs_unit,
                                                         obs_unit_description=obs_unit_description,
                                                         obs_subset=obs_subset,
                                                         obs_subset_description=obs_subset_description,
                                                         dataset_id = dataset_id)
                    
                }else if(assay == "data"){
                    data_type <- "double"
                    if (SeuratObject::IsSparse(object@assays[[mod]][assay]) == TRUE){
                        representation<- "sparse"
                        representation_description<- "The matrix contains zeros for the majority of the measurements"
                    }
                    else { 
                        representation<- "dense"
                        representation_description<- "The matrix contains non-zeros for the majority of the measurements"
                    }
                    processing <- "lognormalized"
                    processing_description <- process_desc[[processing]]
                    feature_subset <- "full"
                    feature_subset_description<- fea_desc[[feature_subset]]
                    obs_unit<- "cell"
                    obs_unit_description<- obs_desc[[obs_unit]]
                    MAMS@FOM[[fom]] <- create_FOM_object(id = fom, 
                                                         filepath=filepath, 
                                                         accessor=accessor,
                                                         fid = fid,
                                                         fea = fea,
                                                         oid=oid, 
                                                         processing=processing, 
                                                         processing_description=processing_description,
                                                         modality=modality, 
                                                         analyte=analyte, 
                                                         analyte_description=analyte_description,
                                                         feature_subset=feature_subset,
                                                         feature_subset_description = feature_subset_description,
                                                         data_type = data_type,
                                                         representation= representation,
                                                         representation_description = representation_description,
                                                         obs_unit=obs_unit,
                                                         obs_unit_description=obs_unit_description,
                                                         obs_subset=obs_subset,
                                                         obs_subset_description=obs_subset_description,
                                                         dataset_id = dataset_id)
                    
                }else if(assay == "scale.data"){
                    data_type <- "double"
                    if (SeuratObject::IsSparse(object@assays[[mod]][assay]) == TRUE){
                        representation<- "sparse"
                        representation_description<- "The matrix contains zeros for the majority of the measurements"
                    }
                    else { 
                        representation<- "dense"
                        representation_description<- "The matrix contains non-zeros for the majority of the measurements"
                    }
                    processing <- "scaled"
                    processing_description <- process_desc[[processing]]
                    feature_subset <- "variable"
                    feature_subset_description<- fea_desc[[feature_subset]]
                    obs_unit<- "cell"
                    obs_unit_description<- obs_desc[[obs_unit]]
                    MAMS@FOM[[fom]] <- create_FOM_object(id = fom, 
                                                         filepath=filepath, 
                                                         accessor=accessor, 
                                                         oid=oid, 
                                                         fid=fid,
                                                         fea=fea,
                                                         processing=processing, 
                                                         processing_description=processing_description,
                                                         modality=modality, 
                                                         analyte=analyte, 
                                                         representation = representation,
                                                         feature_subset=feature_subset,
                                                         feature_subset_description = feature_subset_description,
                                                         representation_description = representation_description,
                                                         analyte_description=analyte_description,
                                                         obs_unit=obs_unit,
                                                         obs_unit_description=obs_unit_description,
                                                         obs_subset=obs_subset,
                                                         obs_subset_description=obs_subset_description,
                                                         data_type = data_type,
                                                         dataset_id = dataset_id)
                    
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

process_desc<- list("raw"= "Original measurements have not been altered",
                    "counts"="Raw data for assays that produce integer-like data such as scRNA-seq.Child of raw",	
                    "intensities" = "Raw data for assays that produce continuous data. Child of raw	",
                    "decontaminated"="Measurements have been corrected for background signal.Ambient RNA removal in single-cell RNA-seq.",
                    "lograw"	= "The log of the raw data",	
                    "logcounts" = "The log of the raw counts",	
                    "logintensities" = "The log of the raw intensity values.",
                    "corrected"	= "Measurements have been corrected for observation-level covariates",
                    "normalized"="	Data that has been normalized for differences in overall signal abundance between observations.	Correcting for total number of UMIs or reads in each cell.",
                    "lognormalized" = 	"Data that has been log transformed after normalizing for differences in overall signal abundance between observations. Child of normalized.",
                    "centered"= "Data with features have been made to center around a standard quantity such as the mean or median.	Mean-centered data",
                    "scaled" = "Data with features have been centered around a standard quantity and standardized to have similar variances or ranges.	Z-scored data",
                    "reduction" =" A matrix containing a data dimensionality reduction generally useful for input into tools for downstream analysis such as clustering or 2D-embedding.	PCA, ICA, Autoencoders",
                    "embedding"	= "A matrix containing a low dimensional embedding (usually 2D or 3D) generally used for visualization. Child of reduction UMAP, tSNE")


analyte_desc<- list("rna" =" Used for technologies that measure RNA expression levels.This should generally be used for assays listed under “RNA assay” (EFO_0001457) from the OLS.
                    dna	Used for technologies that measure features of DNA.	This should generally be used for assays listed under “DNA assay” (EFO_0001456) from the OLS.",
                    "chromatin"	=" Used for technologies that measure open chromatin regions of DNA.",
                    "protein" = "Used for technologies that measure protein expression levels.	This should generally be used for assays listed under “protein assay” (EFO_0001458) from the OLS. Examples include CITE-seq, Total-seq, CODEX, MIBI.",
                    "morphology" = "Used for morphological measurements often derived from imaging technologies (e.g. cell size or shape).",	
                    "lipid"	= "Used for technologies that measure lipid levels.",
                    "metabolite" = "Used for technologies that measure metabolite levels",
                    "methylation" = "Used for technologies that measure methtylation levels.")

obs_desc<- list("bulk" = "Features are quantified for a collection of cells (e.g. bulk) such as tissue or culture	Bulk RNA-seq or ATAC-seq",
                "cell" = "Features are quantified for individual cells, ex: Single-cell RNA-seq",
                "nucleus" =	"Features are quantified for individual nuclei")

fea_desc<-list("full" = "Features have not been filtered or subsetted.",	
               "threshold" = "Features that have a total signal above a certain threshold, ex: Only including features with a total UMI or read count above a certain threshold across observations.",
               "detected"= "Features that have a minimum level of detection across observations. Only including features with at least 3 counts in at least 3 observations.",	
               "variable" = "Features that have minimum level of variability across all cells")

obs_sub_desc<-list("full"=	"Observations have not been filtered or subsetted.",	
                   'filtered' =	"Observations that have enough signal above background.Droplets (or cell barcodes) that have enough counts to be considered to be non-empty. Similar to the “filtered” matrix from CellRanger.",
                   "threshold" = "Observations that have a total signal above a certain threshold. Filtering to include cells with a total UMI or read count above a certain threshold across features.",
                   "detected" = "Observations that have minimum levels of detection across features.Filtering include cells with at least 3 counts in at least 3 genes.",
                   "nonartifact"= "A general term to describe filtering that may occur due other quality control metrics.	Artifacts in single cell RNA-seq data include high contamination from ambient material, high mitochondrial percentage, or doublets/multiplets.",
                   "clean"=	"An “analysis ready” set of observations.")


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
#' \dontrun{
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
#' }
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
            
            # manual annotation
            # if (mod == "counts") {
            #     data_type <- "int"
            #     representation <- "sparse"
            #     processing <- "counts"
            #     feature_subset <- "full"
            # } else if (mod == "logcounts") {
            #     data_type <- "double"
            #     representation <- "sparse"
            #     processing <- "lognormalized"
            #     feature_subset <- "full"
            # } else if (mod == "cpm" || mod == "tpm" || mod == "fpkm") {
            #     data_type <- "double"
            #     representation <- "dense"
            #     processing <- "scaled"
            #     feature_subset <- "variable"
            # }
            
            # auto annotation
            data_type <- .data_type(assay_data)
            representation <- .representation(assay_data)
            processing <- .processing(assay_data)
            feature_subset <- .feature_subset(object_list)[i]
            
            MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath = filepath, accessor = accessor, oid = oid, processing = processing, modality = modality, analyte = analyte, data_type = data_type, dataset_id = dataset_id)
        }
        
        # Iterate over reducedDims
        if (length(reducedDimNames(object)) > 0) {
            for (dimred in names(SingleCellExperiment::reducedDims(object))) {
                fom <- paste0("fom", length(MAMS@FOM) + 1)
                reduction <- SingleCellExperiment::reducedDim(object, dimred)
                data_type <- "double"
                processing <- .reduction_or_embedding(reduction)
                # processing <- ifelse(grepl("pca|ica", dimred, ignore.case = TRUE), "Reduction", ifelse(grepl("tsne|umap", dimred, ignore.case = TRUE), "Embedding", NA))
                accessor <- paste0("reducedDim(x = ", substr(filepath, 1, nchar(filepath) - 4), ", type = \"", dimred, "\")")
                
                MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath = filepath, accessor = accessor, oid = oid, processing = processing, modality = modality, analyte = analyte, obs_subset = obs_subset, dataset_id = dataset_id, data_type = data_type)
            }
        }
    }
    
    return(MAMS)
}

.reduction_or_embedding <- function(matrix_input) {
    if (!is.matrix(matrix_input)) {
        stop("Input must be a matrix.")
    }
    
    num_columns <- ncol(matrix_input)
    
    if (num_columns > 2) {
        return("Reduction")
    } else if (num_columns == 2) {
        return("Embedding")
    } else {
        return(NULL)
    }
}

.representation <- function(mat){
    num_zeros <- sum(mat == 0)
    total_elements <- length(mat)
    proportion_zeros <- num_zeros / total_elements
    
    threshold <- 0.5
    if (proportion_zeros > threshold) {
        return("sparse")
    } else {
        return("dense")
    }
}

.data_type <- function(mat) {
    if (typeof(mat) == "integer") {
        return("int")
    } else if (typeof(mat) == "double") {
        if (all(mat == as.integer(mat))) {
            return("int")
        } else {
            return("double")
        }
    } else {
        return(typeof(mat))
    }
}

.processing <- function(mat){
    if (all(mat == as.integer(mat)) && all(mat >= 0)) {
        return("counts")
    }
    
    if (all(mat >= 0) && any(mat < 1)) {
        return("lognormalized")
    }
    
    if (mean(mat) > -0.01 && mean(mat) < 0.01 && sd(mat) > 0.9 && sd(mat) < 1.1) {
        return("scaled")
    }
    return(NULL)
}

.feature_subset <- function(sce_list) {
    n_rows <- sapply(sce_list, function(sce) nrow(sce))
    max_rows <- max(n_rows)
    result <- sapply(n_rows, function(n) if (n == max_rows) "full" else "variable")
    return(result)
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
