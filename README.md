# NormEnforcement
Datasets and scripts for "Norm enforcement in small-scale societies depends on coordinated third parties and pre-existing relationships"
Moya et. al. 2016

The script (.do) file was written in Stata 12.1 for Mac.
It requires data from 2 data (.dta) files:
  1. NormEnforcementDataSupplement.dta - Each row corresponds to a person's responses on one Story.
  2. NormEnforcementDataLongSupplement.dta - Each row corresponds to a person's response to questions about one protagonist on one Story.
The second file can be reshaped from the first.

The script has three main sections roughly corresponding to:
  1. analyses in the main text
  2. analyses in the supplementary materials
  3. analyses not mentioned in either
Note however, that some data reported in the supplement is based on analyses shown in the first section. 
The final section includes code to run multi-level models in MLwiN through the runmlwin commands Stata. These are commented out and require as they require software and package installations, and a specification of a directory for MLwiN. 
