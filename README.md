# Overview

This repository provides a workflow for microbiome data preprocessing, batch effect correction, statistical analysis, and visualization. The pipeline consists of the following steps:

## **1. Data Preprocessing and Batch Effect Correction**  
- **Data Preprocessing:** The **`MOSAIC`** folder contains three steps: **`step0.ipynb`**, **`step1.Rmd`**, and **`step2.ipynb`**, which sequentially clean the dataset.  
- **Batch Effect Correction:** **ConQuR** is applied using **`Benchmarking_ConQuR.Rmd`** in **`MOSAIC`** folder to correct batch effects.  
- **Output:** The cleaned and batch-effect-corrected data is stored as **`Data_After_MOSAIC` ** folder.

## **2. Statistical Analysis**  
- Statistical computations are performed using the scripts in the **`Main.Rmd`** directory.  
- Various analyses are conducted to identify significant microbiome features.

## **3. Data Visualization**  
- The visualization scripts in the **`Plots`** folder generate figures and graphs based on the processed data.  
- **`Plots`** folder include two files: **`forest_plot_and_tables.Rmd`** and **`other_plots.Rmd`**.

This structured workflow ensures reproducibility and consistency in microbiome data analysis.

