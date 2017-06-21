# SolarSurveyConjointAnalysis
R codes for discrete choice modeling of a solar adoption survey

This repository contains the codes and data you need to create preference models for solar installers and solar systems.

The design of the conjoint surveys are originally stored in:
  design_InstallerConjoint.csv
  design_SystemConjoint.csv

The choice results are in:
  data_InstallerConjoint.csv
  data_SystemConjoint.csv

There are three sets of code files for each of the conjoint survey (X could be Installer or System):
  code1_GenerateDesignMatrix_X.R
  code2_OrganizeData_X.R
  code2_OrganizeData_Training_X.R
  code3_ModelEstimation_mlogit_X.R
  code3_ModelEstimation_MIXL_X.R
  code3_ModelEstimation_HB_X.R

code1 extract data from the "design_X.csv" and save to designM_X.csv with desired format
code2 extract data from the choice files "XConjoint_data.csv"
code3 build the logit models.

Run these three codes in sequence to get the modeling results.
