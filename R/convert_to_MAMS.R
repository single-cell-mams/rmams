# Converts Seurat object to a MAMS object

#' Converts a Seurat object to a MAMS object.
#' @description Convert a Seurat object to a MAMS object, with metadata preserved and imported.
#' @param object_list a Seurat object to be converted.
#'
#' @return a MAMS object with the data from the Seurat object
#' @export
#'
#' @examples
#' \dontrun{mams <- convert_seurat_to_mams("pbmc_seurat")}
#' 
convert_seurat_to_MAMS <- function(object_list){
    FOMs <- list()
convert_seurat_to_MAMS <- function(object_list,observation_subsets){
   # FOMs <- list()
  #  ONG <- list()
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
            
            for(assay in SeuratObject::Layers(object)){
                fom <- paste0("fom", length(MAMS@FOM)+1)
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
                MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath=filepath, accessor=accessor, oid=oid, processing=processing, modality=modality, analyte=analyte)
              #  FOMs[[fom]] <- create_FOM_object(id = fom, filepath = filepath, accessor = accessor, representation = representation, analyte = analyte, modality = modality, obs_subset = obs_subset, feature_subset = feature_subset, oid = oid, fid = fid, obs = obs, fea = fea)
            }
        }
        
        for(dimred in names(object@reductions)){ 
            fom <- paste0("fom", length(MAMS@FOM)+1)
            reduction <- object@reductions[[dimred]]
            if(grepl("pca|ica", dimred, ignore.case = TRUE)){
                processing<- "Reduction"
            } else if (grepl("tsne|umap", dimred, ignore.case = TRUE)){
                processing <- "Embedding"
            }
            accessor <- paste0(processing, "(object = ", substr(filepath, 1, nchar(filepath)-4), ', reduction = \"', dimred, '\")')
            MAMS@FOM[[fom]] <- create_FOM_object(id = fom, filepath=filepath, accessor=accessor, oid=oid, processing=processing, modality=modality, analyte=analyte)
           # FOMs[[fom]] <- create_FOM_object(id = fom, filepath=filepath, accessor=accessor, oid=oid, processing=processing, modality=modality, analyte=analyte)
         }
        ## Graph
        for(graph in Graphs(object)){
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
            MAMS@ONG[[ogr]] <- create_ONG_object(id = ogr, filepath = filepath, accessor = accessor, record_id = record_id, edge_metric = edge_metric, metric_type = metric_type)
           # ONG[[ogr]] <- create_ONG_object(id = ogr, filepath = filepath, accessor = accessor, record_id = record_id, edge_metric = edge_metric, metric_type = metric_type)
        }
        ## Neighbor
        for(neighbor in Neighbors(object)){
            ogr <- paste0("ogr", length(MAMS@ONG)+1)
            graphname <- paste("FindNeighbors", mod, dimred, sep = ".")
            edge_metric <- object@commands[[graphname]]$annoy.metric
            metric_type <- "distance"
            accessor <- paste0("Neighbors(", substr(filepath, 1, nchar(filepath)-4), ', \"', neighbor, '\")')
            record_id <- paste("FindMultiModalNeighbors", substr(filepath, 15, nchar(filepath)-4), sep = ".")
            MAMS@ONG[[ogr]] <- create_ONG_object(id = ogr, filepath = filepath, accessor = accessor, record_id = record_id, edge_metric = edge_metric, metric_type = metric_type)
            #ONG[[ogr]] <- create_ONG_object(id = ogr, filepath = filepath, accessor = accessor, record_id = record_id, edge_metric = edge_metric, metric_type = metric_type)
        }
    }
   # MAMS <- create_MAMS_object(FOM = FOMs, ONG = ONGs)
    return(MAMS)
}
        