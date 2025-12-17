# PBPK_Model_Template_Mod
Version of the PBPK Model Template project created to work with the MCSimMod modeling package, Paul Schlosser, December, 2025.

Descriptive help files are provided in the "Documentation" folder. These are R-markdown (.Rmd) files that can "Knit"ed to create html or pdf versions for easier reading.

To use the included R functions, the R packages MCSimMod and readxl must be installed (with dependent packages). For details, see Documentation/Software_Installation_Instructions.

-------------------------
run_template_model.R

This R source code file includes functions to run the PBPK Model Template. An overview on use is provided in Documentation/How_To_Run_PBPK_Template and detailed help on use of the functions defined by this script is provided in Documentation/PBPK_Template_Description_of_Functions.

**The current format for input parameter spreadsheets can be seen in:
1. model parameters: BLANK_template_parameters_Model.xlsx
1. exposure parameters: BLANK_template_parameters_Exposure.xlsx

-------------------------
PBPK_template.model

This MCSim source code file defines the current model template.

-------------------------
Other .R scripts
Most of the .R scripts contained in this package contain functions which in turn create plots or tables from either the original PBPK Model Template paper for PFAS PBPK models (Bernstein et al., 2021, (https://doi.org/10.1093/toxsci/kfab063) or the second PBPK Model Template paper extending the model to various volatilie orgranic compounds (VOCS) (Bernstein et al., 2023, https://doi.org/10.1093/toxsci/kfad021). The specific chemical (class) addressed by each script is indicated in the script name, with more details in the script comments. (Yoon_scripts.R has function to create table results to match those for several chemicals in the Yoon et al. (2007) paper, per the comments in this script.)

plot_PFAS_template_man.R contains functions for plotting results for the PFAS simulations and does not need to be run directly.

PFOA_inhalation_sim_combined.R contains test code for evaluating the model's ability to predict PFOA dosimetry after inhalation exposure.

PFOA_Loccisno_Kemper.R contains functions for specific simulations used by the PFOA_inhalation... script.

test.R runs a single simulation to demonstrate very small scale numerical instability that occurs with some simulations, not yet resolved.

-------------------------
Documentation
This is a directory containing documentation on installation and use of the PBPK Model Template package.

-------------------------
Data
This is a directory containing data files for each of the chemicals and example models. The directory contains sub-folders for each chemical named "Digitized_Data_X" where X is the name of the chemical. The sub-folders contain .csv and .xlsx files containing data digitized from the source publications.

-------------------------
Inputs
This is a directory containing input data files for each chemical. To run the current model template there must be two input spreadsheets: one for parameters in the model, and one for parameters related to the exposure scenario. Note, the top of this file indicates an example file containing the current format for the input spreadsheets. Previously implemented chemical models may need to have their input spreadsheets modified in order to be compatible with the current version of the template.
