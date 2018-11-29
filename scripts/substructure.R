## Prepare covariate data for EWAS analysis ##

suppressMessages(silent <- lapply(c("tidyverse","minfi","flashpcaR"), library, character.only=T))


rgSet <- readRDS("../int/rgSet.rds")
betas <- readRDS("../int/betas.qc.norm.filt.rds")

# Estimate cell counts based on cell type-specific reference methylation data
rgSet_450k_mimic <- convertArray(rgSet, outType="IlluminaHumanMethylation450k")
estCellCounts <- estimateCellCounts(rgSet_450k_mimic)
saveRDS(estCellCounts, "../int/estCellCounts.rds")

# PCA
probeVariances <- apply(betas, 1, var)
highVarIndices <- order(probeVariances, decreasing=T)[1:10000]  # Top 10k most variable probes for PCA
highVarMat <- betas[highVarIndices,]
pca.fit <- flashpca(t(highVarMat), stand="sd", ndim=20)
EVs <- setNames(data.frame(pca.fit$vectors), paste0("PC", 1:20))
PCs <- cbind(sampleKey=as.character(colnames(highVarMat)), EVs)
save("pca.fit", "PCs", file="../int/pca.RData")

# CPACOR
run_cpacor <- function() {
  # CPACOR (Lehne et al. 2015) to find control probe PCs that adjust for 
  # technical confounding
  print("...CPA method (control probe PCA)...")
  rg_set <- readRDS("../int/rgSet.rds")
  source("helpers.R")
  cpacor_fit <- run_CPA(rgSet)
  CP_PCs <- data.frame(cpacor_fit$x) %>%
    select(one_of(paste0("PC", 1:20))) %>%
    rename_all(funs(paste0("cp", .))) %>%  # To distinguish from standard PCA components
    rownames_to_column(var="sampleKey") %>%
    dplyr::filter(sampleKey %in% colnames(betas))  
  # B/c used rg_set as input, must trim samples that were filtered
  save("cpacor_fit", "CP_PCs", file="../int/CPACOR.RData")
}
run_cpacor()


sampleSheet <- read_csv("../int/sampleSheet.csv") %>%
  mutate(sampleKey=paste(Sentrix_ID, Sentrix_Position, sep="_"))