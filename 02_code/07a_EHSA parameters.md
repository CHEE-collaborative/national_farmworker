1. Load output of 07_prep_EHSAdatacube.R into ArcGIS.

2. Apply [Convert Temporal Field (Data Management]([url](https://pro.arcgis.com/en/pro-app/3.4/tool-reference/data-management/convert-time-field.htm)) Tool to Year and convert to date column. Label as Year.2

3. Create [netCDF Space-Time Cube in ArcGIS]([url](https://doc.esri.com/en/arcgis-pro/latest/tool-reference/space-time-pattern-mining/learnmorecreatecube.html))

4. Apply Emerging Hot Spot Analysis to fields n_days80F, n_days90F, n_days100F, n_days110F. Additional parameters are:
	- Analysis Variable: Year.2
	- Conceptualization of Spatial Relationships: Contiguity edges corners
	- Time trend: 1 year
	- Define Global Window: entire cube
