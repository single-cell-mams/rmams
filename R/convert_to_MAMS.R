#' Converts Seurat object to a MAMS object
#'
#' @param object_list 
#'
#' @return
#' @export
#'
#' @examples
convert_seurat_to_MAMS <- function(object_list){
    FOMs <- list()
    FIDs <- c()
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
                FOMs[[fom]] <- create_FOM_object(id = fom, filepath = filepath, accessor = accessor, representation = representation, analyte = analyte, modality = modality, obs_subset = obs_subset, feature_subset = feature_subset, oid = oid, fid = fid, obs = obs, fea = fea)
            }
        }
        
        for(dimred in names(object@reductions)){ 
            fom <- paste0("fom", length(FOMs)+1)
            reduction <- object@reductions[[dimred]]
            if(grepl("pca|ica", dimred, ignore.case = TRUE)){
                processing<- "Reduction"
            } else if (grepl("tsne|umap", dimred, ignore.case = TRUE)){
                processing <- "Embedding"
            }
            accessor <- paste0(processing, "(object = ", substr(filepath, 1, nchar(filepath)-4), ', reduction = \"', dimred, '\")')
            FOMs[[fom]] <- create_FOM_object(id = fom, filepath=filepath, accessor=accessor, oid=oid, processing=processing, modality=modality, analyte=analyte)
        }
    }
    MAMS <- create_MAMS_object(FOM = FOMs)
    return(MAMS)
}

