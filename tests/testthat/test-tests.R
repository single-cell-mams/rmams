library(Seurat)
options(Seurat.object.assay.version = "v3")

test_that("Test MAMS functions", {
    # create seurat object
    counts <- matrix(rnorm(50000), nrow = 5000, ncol = 10, dimnames = list(paste0("Row", 1:5000), paste0("Col", 1:10)))
    srt <- CreateSeuratObject(counts = counts)
    srt <- NormalizeData(srt)
    srt <- FindVariableFeatures(srt)
    srt <- ScaleData(srt)
    
    # subset
    subset_srt <- srt[1:2000, ]
    print(srt)
    print(subset_srt)
    # Test convert_seurat_to_MAMS
    mams <- convert_seurat_to_MAMS(object_list = list(srt = srt, subset_srt = subset_srt), 
                                   observation_subsets = c("full", "subset"), dataset_id = "dataset1",parent_list = c("yes","no"))
    # test if its a mam object
    expect_is(mams, "MAMS")
    print(mams)
    # test if first fom is counts
    expect_equal(mams@FOM$fom1@processing, "counts")
    # test if third fom is scaled
    expect_equal(mams@FOM$fom3@processing, "scaled")
    
    
    # Test check_MAMS
    fom(mams, "fom1", "data_type") <- "dt1"
    fom(mams, "fom2", "data_type") <- "dt2"
    fom(mams, "fom3", "data_type") <- "dt3"
    fom(mams, "fom4", "data_type") <- "dt4"
    fom(mams, "fom5", "data_type") <- "dt5"
    fom(mams, "fom6", "data_type") <- "dt6"
    expect_equal(mams@FOM$fom6@data_type, "dt6")
    
    # Test export to JSON
    
    # Test read from JSON
})
