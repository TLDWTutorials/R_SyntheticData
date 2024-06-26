# Synthetic Data Generator

This repository contains an R script for generating synthetic patient data, including demographic information, A1C levels, and associated medication and ICD-10 codes. This project is designed for research and educational purposes.

## Prerequisites

Before running the script, make sure you have R installed on your system. You can download it from [CRAN](https://cran.r-project.org/).

Download a dataset of ICD10 codes - I used this one: https://www.kaggle.com/datasets/mrhell/icd10cm-codeset-2023?select=ICDCodeSet.csv

## Features

- Generates names, ages, races, and ethnicities for 1000 synthetic patients.
- Generates medical data such as A1c, systolic/diastolic blood pressure
- Calculates Metformin dosage based on A1C levels.
- Assigns ICD-10 codes related to specific health conditions.
- Exports the data to a CSV file for further analysis.

## Contributing

Contributions to this project are welcome. Please fork the repository and submit a pull request with your improvements.
