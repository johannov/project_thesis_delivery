### Code and data for the project thesis "Estimating elasticities of natural gas demand in Europe" ###

Authors:
Johanne Øderud Vatne
David Jamissen

Content:
- data_prep.R : Prepares data for analysis
- mg_estimator.R : Estimates country-specific ARDL models and combine them in a mean group estimate. Provides custom standard error estimations of the mean group multipliers
- panel_unit_root_testing.R : Conducts a set of panel unit root tests on the data
- pmg_hmtest.wf1 : EViews workfile on estimating a pooled mean group model and conducting a Hausman specification test

Pre-running instructions:
1. Unzip the contents of "data.zip" into a folder in the directory named "data"
2. Unzip the contents of "output.zip" into a folder in the directory named "output"

The files in the "output" folder can be deleted, or overwritten when data_prep.R runs.
