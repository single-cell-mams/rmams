library(Seurat)

test_that("Test MAMS functions", {
    # create seurat object
    counts <- matrix(rnorm(50000), nrow = 5000, ncol = 10, dimnames = list(paste0("Row", 1:5000), paste0("Col", 1:10)))
    srt <- CreateSeuratObject(counts = counts)
    srt <- NormalizeData(srt)
    srt <- FindVariableFeatures(srt)
    srt <- ScaleData(srt)
    
    # subset
    subset_srt <- srt[1:2000, ]
    
    # Test convert_seurat_to_MAMS
    mams <- convert_seurat_to_MAMS(object_list = list(srt = srt, subset_srt = subset_srt), 
                                   observation_subsets = c("full", "subset"))
    # test if its a mam object
    expect_is(mams, "MAMS")
    # test if first fom is counts
    # expect_equal(mams@FOM$fom1@processing, "counts")
    # test if third fom is scaled
    # expect_equal(mams@FOM$fom3@processing, "scaled")
    
    
    # Test check_MAMS
    # fom(mams, "fom1", "dataset_id") <- "1"
    # fom(mams, "fom2", "dataset_id") <- "1"
    # fom(mams, "fom3", "dataset_id") <- "1"
    # fom(mams, "fom4", "dataset_id") <- "1"
    # fom(mams, "fom5", "dataset_id") <- "1"
    # fom(mams, "fom6", "dataset_id") <- "1"
    # fom(mams, "fom1", "data_type") <- "dt"
    # fom(mams, "fom2", "data_type") <- "dt"
    # fom(mams, "fom3", "data_type") <- "dt"
    # fom(mams, "fom4", "data_type") <- "dt"
    # fom(mams, "fom5", "data_type") <- "dt"
    # fom(mams, "fom6", "data_type") <- "dt"
    
    # expect_error(check_MAMS(mams), 'fom6(ID:fom6) is missing the field(s): c("dataset_id", "data_type")')
    
    
    # Test export to JSON
    
    # Test read from JSON
})
