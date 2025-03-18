This is the repository for the 2024 study "Social Media Polls on Twitter and Mastodon: Rapid Data Collection for Public Health".

It contains all materials to reproduce our analyses. Anonymized data is available at our OSF repository: https://osf.io/8wvy3/

## Folder structure

-   `data/`: Put the preprocessed data from OSF here. If you have access to the MuSPAD data, put the MuSPAD data here, too.
-   `R`: Contains the analysis of the data.
-   `Complete Surveys`: Contains the figures.


## Top level files

-  `Preprocessing.Rmd`: This file contains the code for preprocessing the data.
-  `00_read_labeled_data.R`: This file contains the automatic labeling of data from the LimeSurvey export
-  `01_rename.R`: This file contains the code for renaming the variables in the data.

If you have access to the MuSPAD data, you may run the following scripts:

- `MuSPADPreprocessing.R`: Preprocessing of MuSPAD data. Automatically called when executing the scripts below.
- `Timeline.R`: Creates timeline subplots. Automatically called when necessary in the scripts below. 
- `NumberOfInfectionsMuSPADavail.R`: This file contains code to produce Figure 2 and Figure 5.
- `Plot7DayIncidence_MuSPADavail.R`: This file contains code to produce Figure 3 and Figure Figure 7.
- `VaccinationComparisons_MuSPADavail.R`: This file contains code to produce Figure 4 and Figure S4.
- `DemographicComparison_MuSPADavail.R`: This file contains code to produce Figure 6.
- `TimingOfInfection_MuSPADavail.R`: This file contains code to produce Figure S1.
- `DemographicComparison_Recruiter.R`: This file contains code to produce Figure S2 and S3.

If you do _not_ have access to the MuSPAD data, you may run the following scripts:

- `Timeline.R`: Creates timeline subplots. Automatically called when necessary in the scripts below. 
- `NumberOfInfectionsMuSPADunavail.R`: This file contains code to produce Figure 2 and Figure 5 (minus the MuSPAD data).
- `Plot7DayIncidence_MuSPADunavail.R`: This file contains code to produce Figure 3 and Figure Figure 7 (minus the MuSPAD data).
- `VaccinationComparisons_MuSPADunavail.R`: This file contains code to produce Figure 4 and Figure S4 (minus the MuSPAD data).
- `DemographicComparison_MuSPADunavail.R`: This file contains code to produce Figure 6 (minus the MuSPAD data).
- `TimingOfInfection_MuSPADunavail.R`: This file contains code to produce Figure S1 (minus the MuSPAD data).
- `DemographicComparison_Recruiter.R`: This file contains code to produce Figure S2 and S3.

### Instructions to reproduce the plots

This project uses `renv` for reproducibility.
To reproduce the plots found in the paper, follow these steps:

1. Clone the repository
2. Call `renv::restore()` from the R terminal
3. Put the preprocessed data from OSF into the folder called "data"
    * The cleaned data set called `cleaned_data.rds`
    * The XLSX file called `employment-germany-by-occupation-2023.xlsx`
    * If you have access to the MuSPAD data, put the `muspad_22-Nov-2022.rds` and `MuSPAD_data_subset.csv` files in the data folder
4. Now you can run the scripts in the `R` folder to reproduce the plots
5. The resulting plots can be found in the `plots` folder. Each scripts generates a PDF and PNG version of the plot



