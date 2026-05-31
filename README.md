# Heat and wildfire smoke exposures among farmworkers in the U.S.
**Authors**: Jamie Ponmattam<sup>1</sup> and Daniel Carrión*<sup>2,3</sup>

Publication: [_link_]
> Manuscrupt in preparation

---
# Research Overview
US farmworkers face rising co-exposure to extreme heat and wildfire smoke without federal occupational protection. Using county-level heat index, wildfire smoke, and farmworker counts from 2008–2023, we show that emerging hotspots of co-exposure, and the steepest increases for migrant farmworkers, concentrate in states without heat or smoke standards. These climate-driven occupational hazards are increasingly concentrated in jurisdictions without worker protection.

## Data Sources
| Source                                                                                                                                                                  | Description                                                                                                                                                             |
|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [USDA Crop Sequence Boundaries (CSB)][https://www.nass.usda.gov/Research_and_Science/Crop-Sequence-Boundaries/index.php]                                                | Polygons of single crop boundaries over a set period of time in the contiguous US.  Download: CSB 2008-2015, 2013-2020 and 2017-2024 datasets.                          |
| [USDA Census of Agriculture] [https://www.nass.usda.gov/AgCensus/] ([Download][https://quickstats.nass.usda.gov/])                                                      | Count of US farms, ranches and the workers that occurs every 5 years Download: County hired, unpaid and migrant workers, 2002 - 2022                                    |
| [PRISM Weather Data][https://prism.oregonstate.edu/downloads/]                                                                                                          | Historical spatial weather dataset Download (via FTP): daily nationwide 800m resolution mean temperature and maximum vapour pressure density (VPDmax) from 2008 to 2023 |
| [Childs et al, “Growing wildfire-derived PM2.5 across the contiguous U.S. and implications for air quality regulation”][https://www.stanfordecholab.com/wildfire_smoke] | Daily 10km-gridded wildfire smoke PM^2.5 data from 2008-2023                                                                                                            |

## Software
### R / R studio
Analysis was conducted in R 4.4.1-foss-2022b using R studio. 02_code files 02_*** - 05_*** were run in parallel on High Performance Computing clusters.

**Note:** Large data files were handled with qs package in R for fast writing / reading. It was removed from [CRAN repository in Jan 2026]([url](https://cran.r-project.org/web/packages/qs/index.html)).  Please manually install qs from the CRAN archives, use [qs2]([url]([https://github.com/qsbase/qs](https://cran.r-project.org/web/packages/qs2/index.html))) or another package suitable for large data handling.

### ArcGIS - Emerging Hot Spot Analysis (EHSA)
[EHSA]([url](https://doc.esri.com/en/arcgis-pro/latest/tool-reference/space-time-pattern-mining/emerginghotspots.html?tabs=dialog)) is space-time pattern mining tool available in ESRI’s ArcGIS that identifies both hotspot clusters and temporal trends in spatiotemporal data. It calculates a Getis-Ord Gi* statistic for each location at each time point; significant Getis-Ord Gi* statistic values, after a false discover rate (FDR) correction, indicate the presence of significant spatial cluster of high or low values. It then evaluates trends in the farmworker-co-exposure day values at these hot- and cold- spots using the Mann-Kendall trend test. Combining the hot-/cold-spot classification and trend results, EHSA then categorizes each location into [one of 17 patterns]([url](https://doc.esri.com/en/arcgis-pro/latest/tool-reference/space-time-pattern-mining/learnmoreemerging.html)).

**Steps:** See 02_code> 07a_EHSA parameters

---
# Citation
> Manuscrupt in preparation

# Affiliations
> Affiliations:
> <br>1 Occupational & Environmental Medicine Program, Yale School of Medicine, New Haven, CT 
> <br>2 Department of Environmental Health Sciences, Yale School of Public Health, New Haven, CT 
> <br>3 Yale Center on Climate Change and Health, Yale School of Public Health, New Haven, CT
> <br>*Corresponding author; daniel.carrion@yale.edu 
