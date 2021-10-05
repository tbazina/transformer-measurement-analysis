# transformer-measurement-analysis

Quantitative and graphical analysis of power transformer measurements.

## Summary

Analysis of measurement results including descriptive, time-domain, spectral, stationarity, distribution, correlation, regression, spatial, significant difference, damping, coordinate-amplitude relationship, and tank modification effect analyses.

## Content

* Script `vibro.job`
  * Job batch request for using supercomputing resources.
* Script `functions.R`
  * Main script with functions/objects for performing analyses/plotting. Always source script.
* Scripts `measurement_analysis.R` and `experiment_design.R`
  * Experimental design and analysis of first project measurements.
* Scripts `analysis.R` and `analysis_incidence_angle.R`
  * Analysis of test measurements.
* Folder 110MVA
  * Folder Testovi
    * Analysis of impact test preliminary measurements.
  * Folder DoE
    * Experimental design of 110MVA measurements.
  * Scripts `stege_jezgra_analysis.R`, `oplata_analysis.R`, `namotaji_analysis.R`, `impact_test_analysis.R`
    * Analysis of all measurements on 110MVA power transformer.
* Folder 2MVA
  * Folder DoE
    * Experimental design of 2MVA measurements.
  * Scripts `analysis_impact_2MVA.R`, `analysis_jezgra_2MVA.R`, `analysis_namotaji_2MVA.R`, `analysis_oplata_2MVA.R`, `analysis_oplata_ukrute_2MVA.R`
    * Preliminary analysis of all measurements on 2MVA power transformer.
  * Scripts `analysis_finalna_namotaji.R` and `analysis_finalna_oplata.R`
    * Final analysis of all measurements on 2MVA power transformer.
