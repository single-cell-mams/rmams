# initial file


library(Seurat)
#library(rmams)
## Import objects and create list of objects
## (Assumes the rds files are in current working directory)

#file_paths <- c("data/pbmc1k_seurat_filtered.rds", "data/pbmc1k_seurat_nonempty.rds", "data/Downloads/pbmc1k_seurat_tcell.rds")


#file_names <-  gsub(pattern = "\\.rds$", replacement = "", x = basename(file_paths))
#object_list <- lapply(file_paths, readRDS)
#names(object_list) <- file_names
#observational_subsets<-c("filtered","nonempty","subset")
#result<- convert_seurat_to_MAMS(object_list,observational_subsets )
