# VISA_tool

Single stock advice html visualizations were created through R markdown documents. Advantages of this method are:
- Rmd has the potential to become a model document through which to develop as many single stock html pages as needed. It allows to connect with the different data sources, and to create the tables or graphs in this same document. 
- R is a more user friendly tool than others, so it could be used by more people. It is also quite flexible so makes easy to modify. (Something about space requirements?).
The information presented is the same as in the advice sheets. Improved accessibility to the information of the Advice sheets can be resumed as follows:
- Outlook of the document structure, and accessibility to the different sections through the side menu. 
- Links to available information in ICES on the stock (Latest advice, SID, SAG, TAF).
- All tables are downloadable as csv. 
- Graphs from SAG are active and allow to identify single points and its value. 
- Stock Status Table is presented in a simplified way, compared to that in the Advice sheets. Only the used option (Maximum sustainable yield, precautionary approach or management plan) is shown.
Following the request, we present options to visualize catch scenarios: 
- For advice where only next years options are 

## Data preparation:

Prior to the development of the Html pages trough Rmd, the data have been retrieved from different sources and following different strategies:
The data populating the html comes mostly from the same advice sheet word documents. However a script to extract the information in this sheets is available. The more homogeneous the advice sheets are, the easier is the extraction of the information.
Stock assessment graphs and assessment summary are downloaded from SAG, so this part is quite homogeneous and easy to collect.
The most difficult parts because of their heterogeneity are the catch scenarios. An initial extraction can be automated, but as these come only from the advice sheets for the time being, it is necessary a clean up step, which is difficult to automate. A solution to this would be to include the information on this tables in some of the already existing ICES databases.

## Comparison with other related institutions dissemination tools: 

Similar but not identical sites, devoted to publish assessment information are:
- Marine Institute (Ireland) Stock book. 
https://shiny.marine.ie/speciesdash/
Here they give info on species together with some weight-length from surveys. Downloadable.
https://shiny.marine.ie/stockbook/
Here they provide the assessments of the current year. Not downloadable. Quite complete but also complex.
Plain text and some graphs. The present the catch scenarios in separated graphs of F, SSB and Landings. Colours identify the relative position regarding reference points.
- STECF dashboard and others, provides direct download of data, 
Only interactive graphs for Mediterranean Stock Assessments 2016, https://stecf.jrc.ec.europa.eu/dd/medbs/ram allows download of data and images.
All reports available, also download of data with no further registration.
- RAM legacy database, they provide pdfs but requires registration to download data. The most explicative and interactive visualization is a time-series of kobe plots, quite interesting, but it works in Flash, so it is not updated anymore, and hence, not recommended its use. 
-NOAA, introductory pages are quite user friendly:
https://www.fisheries.noaa.gov/topic/population-assessments#fish-stocks
They have a fish description similar to the old popular advice documents:
https://www.fisheries.noaa.gov/species/acadian-redfish
But at the very end there is a pdf, not downloadable data
https://www.afsc.noaa.gov/refm/stocks/assessments.htm

Other pages using similar tools to this one:
-Oceana https://eu.oceana.org/en/catchy-data
-Scott’s product for benchmarks, is demo, and audience are experts, but tools are the same.

**Advantages:**
-All code used is available in github. 

**Disadvantages:**
- Word files used to extract most text and tables are not available, only pdfs are made public.
- Reliance on word files is tricky, only data coming from standardized fields in databases are fully replicable and easily updated.
- Another option: transform pdfs to html, and after that do the opposite, write the html and from there transform into pdfs, word etc (?!)
Some table contents will become easier to collect when TAF is fully running, but not text, which depends on experts.

