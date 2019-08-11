suppressMessages(silent <- lapply(c("tidyverse","minfi","wateRmelon","doParallel"), library, character.only=T))

rgSet <- readRDS("../int/rgSet.rds")

mSet <- preprocessNoob(rgSet)  # Noob normalization procedure for background correction and dye bias adjustment

print("...sample QC...")
detP <- readRDS("../int/detP.rds")
lowDetection <- colSums(detP>1e-16)/nrow(detP) > 0.1  # Don't want >10% of probes whose detection failed at p=0.01
medIntensities <- getQC(mSet)
lowIntensity <- (medIntensities$mMed < 10 | medIntensities$uMed < 10)  # Informed by visual inspection of U vs. M intensity plot
sexMismatch <- pData(mSet)$sex!=getSex(mapToGenome(mSet))$predictedSex
keepSamples <- !(lowDetection | lowIntensity | sexMismatch)
mSet.qc <- mSet[,keepSamples]
print(paste0("...QC: Removed ", sum(!keepSamples), " samples (", sum(lowDetection), " detection, ", 
             sum(lowIntensity), " intensity, ", sum(sexMismatch), " sex mismatch)."))

make_dplots <- function(betas, types, ttl) {
  # Type I/II comparison plot
  plot(0, xlim=c(-0.2,1.2), ylim=c(0,10), xlab="Beta values", ylab="Density", main=ttl)
  typeI_densities <- apply(betas[types=="I",], 2, density)
  for (samp in typeI_densities) lines(samp, col="red")
  typeII_densities <- apply(betas[types=="II",], 2, density)
  for (samp in typeII_densities) lines(samp, col="green")
  legend("topright", c("Type I", "Type II"), col=c("red","green"), lty=1)
}

# Within-sample normalization using BMIQ
print("...normalization using BMIQ...")
jpeg("../output/probeTypePlot_prenormalization.jpg")
make_dplots(getBeta(mSet.qc), getAnnotation(mSet.qc)$Type, ttl="Pre-normalization")
dev.off()

betas.qc.norm <- BMIQ(mSet.qc)

jpeg("../output/probeTypePlot_postnormalization.jpg")
make_dplots(betas.qc.norm, getAnnotation(mSet.qc)$Type, ttl="Post-normalization")
dev.off()
print("BMIQ normalization done.")


# Probe filtering
print("...probe filtering...")
undetectedProbes <- rownames(detP)[rowSums(detP>1e-16)/ncol(detP) > 0.1]  # Don't want >10% of samples whose detection failed at p=0.01
ann450k <- getAnnotation(mSet.qc)
sexChromProbes <- ann450k$Name[ann450k$chr %in% c("chrX","chrY")]  # Probes in sex chromosomes
crossReactiveProbesDF <- read.csv("../data/literature/48639-non-specific-probes-Illumina450k.csv", stringsAsFactors=F)  # From Chen 2013
snpProbes <- grep("^rs", featureNames(mSet.qc), value=T)  # Probes measuring SNPs (may be none)
chProbes <- grep("^ch\\.", featureNames(mSet.qc), value=T)  # Probes measuring CpH (non-CpG) methylation
snpInfo <- getSnpInfo(mSet.qc)
probesWithSNPs <- rownames(snpInfo)[!is.na(snpInfo$CpG_maf) | !is.na(snpInfo$SBE_maf)]  # Probes with SNPs at CpG site or single-base extension site
keepProbes <- !(rownames(mSet.qc) %in% c(undetectedProbes, sexChromProbes, crossReactiveProbesDF$TargetID,
                                         snpProbes, chProbes, probesWithSNPs))
betas.qc.norm.filt <- betas.qc.norm[keepProbes,]
print(paste("...filtering: removed", sum(!keepProbes), "probes."))

print(paste("...dimensions of final methylation set:", paste(dim(betas.qc.norm.filt), collapse=" x ")))
saveRDS(betas.qc.norm.filt, file="../int/betas.qc.norm.filt.rds", compress=F)

