# SolarSurveyConjointAnalysis
R codes for discrete choice modeling of a solar adoption survey

This repository contains the codes and data you need to create preference models for solar installers and solar systems.

The design of the conjoint surveys are stored in:
  design_InstallerConjoint.csv
  design_SystemConjoint.csv

The choice results are in:
  data_InstallerConjoint.csv
  data_SystemConjoint.csv

There are three code files for each of the conjoint survey (X could be Installer or System):
  code1_GenerateDesignMatrix_X.R
  code2_OrganizeData_X.R
  code3_LogitModeling_X.R

code1 and code2 compile the data to the desired format for building logit models in R. 
code3 build the logit models.

Run these three codes in sequence to get the modeling results.