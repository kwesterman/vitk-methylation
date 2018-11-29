suppressMessages(silent <- lapply(c("tidyverse","minfi","doParallel","readxl"), library, character.only=T))


phenos <- read_csv("../data/phen/finalTbl_withCovariates.csv") %>%
  mutate(sex=ifelse(M1F0==1, "M", "F")) %>%
  dplyr::rename(age=age_0) %>%
  select(hnrcid, sex, age)

sampleSheet <- read_excel("../data/meth/MANIFEST.xlsx", sheet=2) %>%
  mutate(Basename=paste0("../data/meth/", Sentrix_ID, "/", Sentrix_ID, "_", Sentrix_Position)) %>%
  dplyr::rename(hnrcid=HNRCID) %>%
  inner_join(phenos, by="hnrcid")

rgSet <- read.metharray.exp(targets=sampleSheet)
saveRDS(rgSet, file=paste0("../int/rgSet.rds"), compress=F)

detP <- detectionP(rgSet)
saveRDS(detP, file="../int/detP.rds")

jpeg(paste0("../output/qcIntensityPlot.jpg"))  # Plot of Unmeth vs. Meth median intensities
plotQC(getQC(preprocessRaw(rgSet)))
dev.off()
