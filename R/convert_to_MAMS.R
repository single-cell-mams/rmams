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
                    MAMS@FOM[[fom]] <- create_FOM_object(id = fom, 
                                                         filepath=filepath, 
                                                         accessor=accessor, 
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

        