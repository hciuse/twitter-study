This is the repository for the 2024 study "Social Media Polls on Twitter and Mastodon:
Rapid Data Collection for Public Health".

It contains all materials to reproduce our analyses. Anonymized data is available at our OSF repository: https://osf.io/8wvy3/

## Folder structure

-   `data/`: Put the preprocessed data from OSF here.
-   `R`: Contains the analysis of the data.
-   `Complete Surveys`: Contains the figures.


## Top level files

-  `Preprocessing.Rmd`: This file contains the code for preprocessing the data.
-  `survey_415684_R_syntax_file.R`: This file contains the automatic labeling of data from the LimeSurvey export
-  `rename.R`: This file contains the code for renaming the variables in the data.

If you have access to the MuSPAD data, you may run the following scripts:

- `MuSPADPreprocessing.R`: Preprocessing of MuSPAD data. Automatically called when executing the scripts below.
- `Timeline.R`: Creates timeline subplots. Automatically called when necessary in the scripts below. 
- `NumberOfInfectionsMuSPADavail.R`: This file contains code to produce Figure 2 and Figure 5.
- `DemographicComparison_MuSPADavail.R`: This file contains code to produce Figure 6.
- `Plot7DayIncidence_MuSPADavail.R`: This file contains code to produce Figure 3 and Figure Figure 7.
- `TimingOfInfection_MuSPADavail.R`: This file contains code to produce Figure S1.
-`VaccinationComparisons_MuSPADavail.R`: This file contains code to produce Figure 4 and Figure S4.

If you do _not_ have access to the MuSPAD data, you may run the following scripts:

- `NumberOfInfectionsMuSPADunavail.R`: This file contains code to produce Figure 2 and Figure 5 (minus the MuSPAD data).
- `DemographicComparison_MuSPADunavail.R`: This file contains code to produce Figure 6 (minus the MuSPAD data).
- `Plot7DayIncidence_MuSPADunavail.R`: This file contains code to produce Figure 3 and Figure Figure 7 (minus the MuSPAD data).
- `TimingOfInfection_MuSPADunavail.R`: This file contains code to produce Figure S1 (minus the MuSPAD data).
-`VaccinationComparisons_MuSPADunavail.R`: This file contains code to produce Figure 4 and Figure S4 (minus the MuSPAD data).

## Instructions for reproducibility

This project uses `renv` for reproducibility. You can install the libraries we used by calling `renv::restore()` from the R terminal.

1. Clone the repository.
2. Call `renv::restore()` from the R terminal.
3. Create a folder called "data" and put the preprocessed data from OSF there.
4. Depending on your access to the MuSPAD data, the according subset of scripts in the `R` folder should run now.
