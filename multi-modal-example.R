# libs from R 4.2.3
# Seurat 4.3.0
# SeuratObject 4.1.3

library(dplyr)
library(Seurat)
library(patchwork)

# IMPORT
raw.matrices <- Read10X(data.dir = "/restricted/projectnb/camplab/home/isarfraz/rmams_mar8/raw_feature_bc_matrix")
data.raw <- CreateSeuratObject(counts = raw.matrices[[1]], assay = "RNA", project = "pbmc1k")
adt <- CreateAssayObject(counts = raw.matrices[[2]])
data.raw[["ADT"]] <- adt

nonempty.matrices <- Read10X(data.dir = "/restricted/projectnb/camplab/home/isarfraz/rmams_mar8/filtered_feature_bc_matrix")
data.nonempty <- CreateSeuratObject(counts = nonempty.matrices[[1]], assay = "RNA", project = "pbmc1k")
adt <- CreateAssayObject(counts = nonempty.matrices[[2]])
data.nonempty[["ADT"]] <- adt


# QC
data.nonempty[["percent.mt"]] <- PercentageFeatureSet(data.nonempty, pattern = "^MT-")
data <- subset(data.nonempty, subset = nFeature_RNA > 500 & percent.mt < 5)

# CLUSTERING

### RNA only clustering of all cells
DefaultAssay(data) <- 'RNA'
data <- NormalizeData(data) %>% FindVariableFeatures() %>% ScaleData() %>% RunPCA()
data <- FindNeighbors(data, dims = 1:10)
data <- FindClusters(data, resolution = 2, verbose = FALSE)
data <- RunUMAP(data, reduction = 'pca', dims = 1:10, assay = 'RNA', 
                reduction.name = 'rna.umap', reduction.key = 'rnaUMAP_')
DimPlot(data, reduction = 'rna.umap')

### ADT only clustering of all cells
DefaultAssay(data) <- 'ADT'
VariableFeatures(data) <- rownames(data[["ADT"]])
data <- NormalizeData(data, normalization.method = 'CLR', margin = 2, verbose = FALSE) %>% 
    ScaleData() %>% RunPCA(reduction.name = 'apca', dims = 1:10)
data <- FindNeighbors(data, reduction = "apca", dims = 1:10)
data <- FindClusters(data, graph.name = "ADT_snn", resolution = 2, verbose = FALSE)
data <- RunUMAP(data, reduction = 'apca', dims = 1:10, 
                reduction.name = 'adt.umap', reduction.key = 'adtUMAP_')
DimPlot(data, reduction = 'adt.umap')


### Multimodal clustering of all cells
data <- FindMultiModalNeighbors(
    data, reduction.list = list("pca", "apca"), 
    dims.list = list(1:20, 1:10), modality.weight.name = "RNA.weight"
)

data <- RunUMAP(data, nn.name = "weighted.nn", reduction.name = "wnn.umap", reduction.key = "wnnUMAP_")
data <- FindClusters(data, graph.name = "wsnn", algorithm = 3, resolution = 0.5, verbose = FALSE)

DimPlot(data, reduction = 'wnn.umap', label = TRUE, repel = TRUE)
FeaturePlot(data, features = c("CD3-TotalSeqC","rna_CD3E"), reduction = 'wnn.umap')

# T-SUBSET
data.tcell <- subset(data, seurat_clusters %in% c(1,2,10,5,6,7))

### RNA only clustering of T-cells
DefaultAssay(data.tcell) <- 'RNA'
data.tcell <- NormalizeData(data.tcell, verbose = FALSE) %>% FindVariableFeatures() %>% ScaleData() %>% RunPCA(reduction.name = "pca.tcell")
data.tcell <- FindNeighbors(data.tcell, reduction = "pca.tcell", dims = 1:10)
data.tcell <- FindClusters(data.tcell, resolution = 2, verbose = FALSE)
data.tcell <- RunUMAP(data.tcell, reduction = 'pca.tcell', dims = 1:10, assay = 'RNA', 
                      reduction.name = 'rna.tcell.umap', reduction.key = 'rnaTcellUMAP_')
DimPlot(data.tcell, reduction = 'rna.tcell.umap')


### ADT only clustering of T-cells
DefaultAssay(data.tcell) <- 'ADT'
VariableFeatures(data.tcell) <- rownames(data.tcell[["ADT"]])
data.tcell <- NormalizeData(data.tcell, normalization.method = 'CLR', margin = 2, verbose = FALSE) %>% 
    ScaleData() %>% RunPCA(reduction.name = 'apca.tcell', dims = 1:10)
data.tcell <- RunUMAP(data.tcell, reduction = 'apca.tcell', dims = 1:10, assay = 'ADT', 
                      reduction.name = 'adt.tcell.umap', reduction.key = 'adtTcellUMAP_')
data.tcell <- FindNeighbors(data.tcell, reduction = "apca.tcell", dims = 1:10)
data.tcell <- FindClusters(data.tcell, graph.name = "ADT_snn", resolution = 2, verbose = FALSE)
data.tcell <- FindClusters(data.tcell, algorithm = 3, resolution = 2, verbose = FALSE)
DimPlot(data.tcell, reduction = 'adt.tcell.umap')

### Multimodal clustering of T-cells
data.tcell <- FindMultiModalNeighbors(
    data.tcell, reduction.list = list("pca.tcell", "apca.tcell"),
    dims.list = list(1:10, 1:10), modality.weight.name = "RNA.weight"
)
data.tcell <- RunUMAP(data.tcell, nn.name = "weighted.nn", reduction.name = "wnn.tcell.umap", reduction.key = "wnnTcellUMAP_")
data.tcell <- FindClusters(data.tcell, graph.name = "wsnn", algorithm = 3, resolution = 2, verbose = FALSE)
DimPlot(data.tcell, reduction = 'wnn.tcell.umap', label = TRUE, repel = TRUE)


#save objects
# saveRDS(data.raw, "pbmc8k_seurat_raw.rds")
# saveRDS(data.nonempty, "pbmcs8k_seurat_nonempty.rds")
# saveRDS(data, "pbmc8k_seurat_filtered.rds")
# saveRDS(data.tcell, "pbmc8k_seurat_tcell.rds")


# pass objects through mams seurat parser to generate mams object

# mams object to json 

