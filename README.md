## CREATING EMEP4UK INPUT FILES

######################################################################################
#### **These scripts create emission input files for the EMEP4UK model for the EU (0.1&deg;) and the UK (0.01&deg;)**
######################################################################################

*Info:*
----------------

1. EU emissions data; 

   * gridded data taken from https://www.ceip.at/
   * data is 0.1&deg; for GNFR sectors
   * data is masked using "Emissions_mask.tif" - this removes UK terrestrial cells

2. UK emissions data;

   * 1km and 0.01&deg; emissions surfaces are created for UK & Eire in the 'UK Emissions Model' (https://github.com/NERC-CEH/UK_emissions_model)
   * Data is processed from the NAEI, MapEire, EMEP and E-PRTR
   * This workflow takes that data and creates EMEP4UK input files, masked to "Emissions_mask.tif"


-----------------------------------------------------------------------------------------------------------------


_All UK emissions data processed and stored in:_

//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/Emissions_grids_plain


