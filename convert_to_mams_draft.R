library(Seurat)
## Import objects and create list of objects
## (Assumes the rds files are in current working directory)
file_paths <- c("pbmc1k_seurat_filtered.rds", "pbmc1k_seurat_nonempty.rds", "pbmc1k_seurat_tcell.rds")
file_names <-  gsub(pattern = "\\.rds$", replacement = "", x = basename(file_paths))
object_list <- lapply(file_paths, readRDS)
names(object_list) <- file_names
observation_subsets <- c("filtered", "filtered", "clean")

## Input list of objects, named by the filename
## Input vector of observation subsets, one for each file
## Needs completion: FEA, OBS, ONG, FNG, REC
convert_seurat_to_MAMS <- function(object_list, observation_subsets){
    FEA <- c()
    OBS <- c() 
    FOMs <- list()
    FIDs <- c()
    obs_unit <- "cell" # should probably be a user input
    for(i in 1:length(object_list)){
        object <- object_list[[i]]
        filepath <- paste0(names(object_list)[[i]], ".rds")
        oid <- paste0("oid", i)
        obs <- paste0("obs", i)
        obs_subset <- observation_subsets[[i]]
        
        for(mod in SeuratObject::Assays(object)){
            fid <- paste0("fid", length(FIDs)+1)
            fea <- paste0("fea", length(FIDs)+1) # should fea values be the same as fid? 
            FIDs <- c(FIDs, fid)
            
            if(mod == "RNA"){
                modality <- "rna"
                analyte <- "rna"
            }else{
                modality <- "protein"
                analyte <- "protein"
            }
            
            ## Loop over Assays
            ## Needs completion: record ID
            for(assay in SeuratObject::Layers(object)){
                fom <- paste0("fom", length(FOMs)+1)
                accessor <- paste0("GetAssayData(object = ", substr(filepath, 1, nchar(filepath)-4), ', slot = \"', assay, '\" assay = \"', mod, '\")')
                if(assay == "counts"){
                    data_type <- "int"
                    representation <- "sparse"
                    processing <- "counts"
                    feature_subset <- "full"
                }else if(assay == "data"){
                    data_type <- "double"
                    representation <- "sparse"
                    processing <- "lognormalized"
                    feature_subset <- "full"
                }else if(assay == "scale.data"){
                    data_type <- "double"
                    representation <- "dense"
                    processing <- "scaled"
                    feature_subset <- "variable"
                }
                FOMs[[fom]] <- list(filepath=filepath, accessor=accessor, oid=oid, fid=fid, obs=obs, fea=fea, data_type=data_type, representation=representation, obs_unit=obs_unit, processing=processing, feature_subset=feature_subset, modality=modality, analyte=analyte, obs_subset=obs_subset)#, record_id=record_id)
            }
            
            ## Loop over Graphs & Neighbors
            ## Needs Completion
            for(graph in Graphs(object)){
                ## Neighbors
                graphname <- paste("FindNeighbors",mod,dimred, sep = ".")
                edge_metric <- object@commands[[graphname]]$annoy.metric
            }
            
            #if(length(names(object[[mod]][[]])) > 1){
            #    FEA <- c(FEA, paste(names(object[[mod]][[]]), mod, sep = "."))
            #}
            #OBS <- c(OBS, paste(names(object@meta.data), mod, sep = "."))
        }
        
        ## Loop over Reduced Dimensions
        ## Needs completion: tracking obs_subset, parent_relationship, record_id
        for(dimred in names(object@reductions)){ 
            fom <- paste0("fom", length(FOMs)+1)
            reduction <- object@reductions[[dimred]]
            if(grepl("pca|ica", dimred, ignore.case = TRUE)){
                processing<- "Reduction"
            } else if (grepl("tsne|umap", dimred, ignore.case = TRUE)){
                processing <- "Embedding"
            }
            accessor <- paste0(processing, "(object = ", substr(filepath, 1, nchar(filepath)-4), ', reduction = \"', dimred, '\")')
            FOMs[[fom]] <- list(filepath=filepath, accessor=accessor, oid=oid, processing=processing, modality=modality, analyte=analyte)
            # Full list of params: #FOMs[[fom]] <- list(filepath=filepath, accessor=accessor, oid=oid, obs=obs, data_type=data_type, representation=representation, obs_unit=obs_unit, processing=processing, feature_subset=feature_subset, modality=modality, analyte=analyte, obs_subset=obs_subset, parent_id=parent_id, parent_relationship=parent_relationship, record_id=record_id)
        }
        
    }
    return(FOMs)
}

test <- convert_seurat_to_MAMS(object_list, observation_subsets)
test


