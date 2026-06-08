# Data Appendix

This appendix provides more detail on how the dataset used in this thesis was constructed and how issues were resolved in compiling it from over 1,100 individual documents.

## Data Sources

The figures in this dataset were constructed from tables of unit cost estimates published in the annual SAR for each MDAP active in the reporting period. These figures were cross-referenced with the SAR Summary Tables published by the DoD to resolve discrepancies and obtain the baseline type for each program during its reporting period. Unless otherwise stated, all observations in this dataset are from the individual SARs.

## Variables

`report_date` - The current year  
`service` - The service branch responsible for the program  
`program` - Name of the program  
`base_year` - Base year of the program  
`baseline_type` - Whether the program is in the development phase (DE) or production phase (PdE)  
`prime_contractor` - The prime contractor in charge of the program  
`secondary_contractor` - An applicable major secondary contractor or large subcontractor involved in the program  
`platform_portfolio` - MDAP product market classification  
`cohort` - Year cohort for a treated program  
`merger_exposure` - Specific merger a treated program is exposed to  
`treatment_rule` - The channel treatment assignment operates through; For instance, `sub-ATK` means the program is treated by Alliant TechSystems (ATK) at the subcontractor level, while `prime-Raytheon` means treatment operates through Raytheon at the prime contractor level.  

## Manual Adjustments

### Program Transitions

Several programs transitioned from the development phase (DE) to the production phase (PdE) during the reporting cycle. In these instances, the base year gets updated, and all dollar estimates in subsequent SARs are reported with respect to the new base year. To ensure consistency in cost estimates, I manually reanchor all estimates after the baseline change to the original base year using deflator tables published in the *National Defense Budget Estimates* for FY2024, also known as the DoD "Green Book". The following programs have had estimates rebased:

* AMPV: All estimates after 2018 rebased to 2015 dollars.

* B-2 EHF SATCOM: All estimates after 2011 rebased to 2007 dollars.

* B61 Mod 12 LEP TKA: All estimates after 2017 rebased to 2012 dollars.

* CHEM DEMIL-ACWA: All estimates after 2010 rebased to 1994 dollars.

* CH-53K: All estimates after 2015 rebased to 2006 dollars.

* CRH: All estimates after 2018 rebased to 2014 dollars.

* CIRCM: All estimates after 2017 rebased to 2015 dollars.

* F-22 Inc 3.2B Mod: All estimates after 2015 rebased to 2013 dollars.

* GPS IIIF: All estimates after 2020 rebased to 2018 dollars.

* ICBM Fuze Mod: All entries after 2020 rebased to 2014 dollars.

* JAGM: All estimates after 2017 rebased to 2015 dollars.

* JTRS HMS: All estimates after 2010 rebased to 2004 dollars.

* JLTV: All estimates after 2014 rebased to 2012 dollars.

* KC-46A: All estimates after 2015 rebased to 2011 dollars.

* MQ-4C: All estimates after 2015 rebased to 2008 dollars.

* PAC-3 MSE: All estimates after 2013 rebased to 2004 dollars.

* PIM: All estimates after 2012 rebased to 2011 dollars.

* RQ-4A/B: All estimates after 2013 rebased to 2000 dollars.

* SDB II: All estimates after 2014 rebased to 2010 dollars.

Three programs also suffered serious breaches of thresholds for unit cost growth and were subsequently restructured with reset baselines and a new base year. For these programs, I continue using the original baseline cost and quantity estimates and re-anchor all subsequent cost estimates to the original base year. This applies to the following programs:

* GPS OCX
* IDECM Block 4
* JPALS

### Coverage Gaps

Selected Acquisition Reports were not published in 2020 due to changes in reporting requirements and budgetary authorizations. For all active programs in this year, current estimates of program costs and quantities have been imputed using linear interpolation.

Two programs were officialy designated as MDAPs in 2020: NGJ Low Band and LRSO, but they only first appear in SARs in 2021. For these two programs, I use the original baseline estimates of costs and quantities as the current estimates for 2020.

### Note on the F-35 Program

Reports for the F-35 program between 2011 and 2020 and 2023 provide separate cost estimates for the airframe and engine subprograms. However, SARs for 2021 and 2022 only report aggregate estimates of program costs. To reconstruct an approximate estimate of subprogram costs for those years, I calculate each subprogram's fraction of total estimated costs in 2023 and apply those weights to the aggregate estimates reported in 2021 and 2022.

## References

Acquisition Innovation Research Center. (2026). Data sources for analyzing acquisition. <https://acqirc.org/external-links/#resources>

Office of the Under Secretary of Defense (Comptroller). (2023, May). *National defense budget estimates for FY 2024*. U.S. Department of Defense. <https://comptroller.defense.gov/Portals/45/Documents/defbudget/FY2024/FY24_Green_Book.pdf>

Washington Headquarters Services. (2026). Selected acquisition reports — FOIA reading room. U.S. Department of Defense. <https://www.esd.whs.mil/Records-Declass/FOIA/Reading-Room/Reading-Room-List_2/Selected_Acquisition_Reports/>
