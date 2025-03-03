# Overview

This repository contains the code used in the manuscript "Deciphering gut microbiome in colorectal cancer via robust learning methods". The code is provided for transparency, reproducibility, and to allow reviewers and readers to better understand the methods and analysis presented in the paper.

## **1. Data Preprocessing and Batch Effect Correction**  
- **Data Preprocessing:** The **`MOSAIC`** folder contains three steps: **`step0.ipynb`**, **`step1.Rmd`**, and **`step2.ipynb`**, which sequentially clean the dataset.  
- **Batch Effect Correction:** **ConQuR** is applied using **`Benchmarking_ConQuR.Rmd`** in **`MOSAIC`** folder to correct batch effects.  
- **Output:** The cleaned and batch-effect-corrected data is stored as **`Data_After_MOSAIC`** folder.

## **2. Statistical Analysis**  
- Statistical computations are performed using the scripts **`Main.Rmd`**.  
- Various analyses are conducted to identify microbial signatures for colorectal cancer.

## **3. Data Visualization**  
- The visualization scripts in the **`Plots`** folder generate figures and graphs based on the processed data.  
- **`Plots`** folder include two files: **`forest_plot_and_tables.Rmd`** and **`other_plots.Rmd`**.


