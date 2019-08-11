STEP_SIZE=50
P_COL=4
DATASET=$1
RAW_BED=${DATASET}.bed

cd ../int/combp

#CPV_DIR=../../opt/combined-pvalues/cpv

comb-p acf -d 1:1000:$STEP_SIZE -c $P_COL $RAW_BED > acf_$DATASET.txt
comb-p slk --acf acf_${DATASET}.txt -c $P_COL ${DATASET}.bed > acf_${DATASET}.bed
comb-p peaks --dist 500 --seed 0.1 acf_${DATASET}.bed > regions_${DATASET}.bed
comb-p region_p -p $RAW_BED -r regions_${DATASET}.bed -s $STEP_SIZE -c $P_COL > regions.sig_${DATASET}.bed

cd ../../scripts
