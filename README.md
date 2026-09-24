# Preregistering Data Donation Studies
This repository contains the complete analysis pipeline for the preregistration template, including the related shiny app. Currently hosted here: https://valeriehase-data-donation.share.connect.posit.cloud/

## Project Structure

```text
.
├── app/                 # code for running the Shiny App
├── data/
│   ├── interviews/      # data from qualitative interviews
│   ├── survey/          # data from quantitative survey
└── scripts/             # analysis pipeline via subscripts
```
The workflow follows a linear, script-based pipeline with clearly defined inputs and outputs. To run the full pipeline, open the R project to execute the main script:

```r
source("main.R")
```