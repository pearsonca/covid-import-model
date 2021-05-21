
WGET = wget -c -O $@
R = Rscript $^ $@

COGURL := https://cog-uk.s3.climb.ac.uk/phylogenetics/latest/cog_metadata.csv
DLDIR ?= ~/Downloads

data/$(notdir ${COGURL}):
	${WGET} ${COGURL}

#' NB must manual download the mutation report from https://outbreak.info/location-reports?loc=IND
data/outbreakinfo_mutation_report_data.tsv: $(shell ls ${DLDIR}/outbreakinfo_mutation_report_data_*.tsv | tail -1)
	cp $< $@

fetch: data/$(notdir ${COGURL}) data/outbreakinfo_mutation_report_data.tsv

data/data_proportion.rds: R/gen_data_proportion.R data/$(notdir ${COGURL})
	${R}

data/traveller_cases.rds: R/gen_traveller_cases.R data/voc_imports_2021_05_13.csv
	${R}

loaded: $(patsubst %,data/%.rds,data_proportion)

