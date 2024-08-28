# rmams
### R package for Matrix and Analysis Metadata Standards

Many molecular datasets are being generated across cancer types that contain multi-modal data collected from longitudinally- and spatially-related biological specimens. A major roadblock to this goal is that the data is stored in a wide variety of file formats or programming language-specific libraries, classes, or data structures. Although a wide range of experimental protocols and platforms are available, an important commonality across these technologies is that they often produce a matrix of features that are measured in a set of observations. In order to facilitate data sharing across groups and technologies, and assays, and to promote interoperability between down-stream analysis tools, a detailed data schema describing the characteristics of FOMs has been developed and will serve a standard useful for the community.


### Installation
The `rmams` package can be installed with the `remotes` package using the following command:
```
install.packages("devtools")
library(devtools)
install_github("single-cell-mams/rmams")

```
or with a conda-compatible package manager using the [bioconda](https://bioconda.github.io/recipes/r-mams/README.html) channel.

### Quick Usage

```
library(Seurat)
options(Seurat.object.assay.version = "v3")
counts <- matrix(rpois((500*200), 1), nrow = 500, ncol = 200, dimnames = list(paste0("Row", 1:500), paste0("Col", 1:200)))
srt <- CreateSeuratObject(counts = counts)
srt <- NormalizeData(srt)
subset_srt <- srt[, 1:100]
mams <- convert_seurat_to_MAMS(object_list = list(srt = srt, subset_srt = subset_srt),
  observation_subsets = c("full", "subset"), dataset_id = "dataset1")
print(mams)
```

**You can access a detailed tutorial on how to use the rmams package [here](https://single-cell-mams.github.io/rmams/articles/rmams_tutorial.html).**

### Citation
> Sarfraz, I., Wang, Y., Shastry, A. et al. MAMS: matrix and analysis metadata standards to facilitate harmonization and reproducibility of single-cell data. Genome Biol 25, 205 (2024). https://doi.org/10.1186/s13059-024-03349-w
