# sismar 1.1.0
* migration to Portuguese documentation
* overall improvements to function documentation
* addition of function `parse_sisma_bes` to support feature engineering of standard SISMA BES export

# sismar 1.0.0
* Addition of function `process_sisma_export` which automatically detects SISMA export type passed in by user
* Addition of helper function `clean_sisma_df` to clean SISMA dataframes passed in by `process_sisma_export`
* Addition of utility `setup_sisma_folder` to create a folder structure for data processing
* Addition of example data for unit testing and for sandbox use
* Internalize parsing data

# sismar 0.4.0
* Translation of pkgdown site to Portuguese.

# sismar 0.3.1
* Fix to AHD (DAH) feature engineering for age_coarse.

# sismar 0.3.0
* Redesign of AHD (DAH) feature engineering for pregnant women (MG).  This population disaggregation is now removed from indicator names and is encoded in the disaggregate variable.

# sismar 0.2.0

* Addition of data to engineer features for Family Planning data elements from the 1) Integrated Family Planning Summary, 2) the SAAJ Summary, and 3) the APE Summary.
* `parse_sisma_smi_pf_int` has been incorporated and `parse_sisma_csv` modified to feature engineer above Family Planning data elements.

# sismar 0.1.0

* Prior to version 0.1.0, updates were not captured.
