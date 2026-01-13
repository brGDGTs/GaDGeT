 # GaDGeT
 *GaDGeT- GDGT calculations simplified: an adaptable R-toolbox for rapid index calculations*
 GaDGeT is an adaptable R script designed to calculate various fractional abundances and GDGT-indices from isoGDGTs, OHGDGTs, brGDGTs, GMGTs, and GDDs. The script processes data from Excel (.xlsx; .csv) files located in the 
 'Input' directory and outputs CSV files with the calculated results into the 'Output' directory.
 This tool was developed as part of the research presented in the article:
 
 Schneider, T., & Castañeda, I.S. (2025). *GaDGeT: An open-source R-workflow for fast and flexible GDGT index calculations*. SoftwareX (32, 102374). DOI: 10.1016/j.softx.2025.102374. https://doi.org/10.1016/j.softx.2025.102374
 
 Schneider, T., & Castañeda, I. (2025). *GaDGeT: An open-source R-workflow for fast and flexible GDGT index calculations*. Initial Public Release. (v.1.0.0). Zenodo. https://doi.org/10.5281/zenodo.15827945

 # Features
 *Fractional Abundance Calculations:* Calculates fractional abundances for a wide range of GDGTs.
 *GDGT Index Calculations:* Computes multiple GDGT indices based on published methods.
 *Output Files:* Generates CSV files for each calculation, organized by dataset and index type.
 *Error Handling:* Includes robust error handling for file reading and data processing.

 # Original publication
 Please find the original, open access publication in Organic Geochemistry: Schneider and Castañeda (2024): *GaDGeT- GDGT calculations simplified: an adaptable R-toolbox for rapid index calculations*
 
 # User manual
 Please follow the instructions in the user manual.

 # Requirements
 R Version: 3.5 or above
 
 R Packages: stringr, readxl, readr

 # Installation
 Clone or Download the Repository: Download the script and associated function files to your local machine and don't change the folder structure.
 Follow the explanation in the user manual.

 # Usage
## 1. Prepare Input Data
Place your Excel files containing the HPLC areas in the 'Input' directory.
Ensure that each Excel file contains a sheet named "GDGTs" with the correct structure as per the template.
If concentration calculations are required, include the dry sediment weight and internal standard (IS) data.

## 2. Run the Script
Open the GaDGeT.R script in R or RStudio and run the script. Ensure that the working directory is set correctly (you can modify it at the start of the script).

## 3. Output
The script will generate CSV files in the 'Output' directory.
Each output will be saved in a subfolder corresponding to the dataset name, and further organized by calculation type.

# License
This script is provided "as is" without any warranties. You are free to use, modify, and distribute it under a Creative Commons Attribution 4.0 International License.
Users must give appropriate credit (cite the original publication, Schneider and Castañeda (2024), provide a link to the license, and indicate if changes were made. 
Please find more information in the license file.


# Contributing
If you'd like to contribute to the development of GaDGeT, please reach out to the author at the contact information below.

# Contact
For troubleshooting, suggestions, or modifications, please contact:

Tobias Schneider

Email: tobiaschnei@gmail.com
Website: www.drtobiasschneider.com



