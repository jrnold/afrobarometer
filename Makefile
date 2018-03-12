# Settings
RSCRIPT ?= RScript
AWS ?= aws

S3_BUCKET = s3://jrnold-data/afrobarometer

# Paths
OUTDIR = data

all: build

.PHONY: download
download:
	$(RSCRIPT) bin/download.R

.PHONY: build
build: data/afrobarometer.rds

data/afrobarometer.rds: bin/afrobarometer.R download
	$(RSCRIPT) $<

.PHONY: dist
dist: data/afrobarometer.rds
	aws s3 sync --exclude ".gitkeep" data $(S3_BUCKET)
