# VISA_tool
Single stock advice visualizations were created as single R markdown documents. For future development, a script to create all .RMD documents should be created. 
The data populating the html comes mostly from the same advice sheet word documents. However a script to extract the information in this sheets is available. The more homogeneous the advice sheets are, the easier is the extraction of the information.
Stock assessment graphs and assessment summary are downloaded from SAG, so this part is quite homogeneous and easy to collect.
The most difficult parts because of their heterogeneity are the catch scenarios. An initial extraction can be automated, but as these come only from the advice sheets for the time being, it is necessary a clean up step, which is difficult to automate. A solution to this would be to include the information on this tables in some of the already existing ICES databases.
Similar but not identical sites, devoted to publish assessment information are:
- IMR Stock book. Plain text and some graphs.
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
-Oceana
-Scott’s last one

Advantages:
-All code used is available in github. 
Disadvantages:
- Word files used to extract most text and tables are not available, only pdfs are made public.
- Reliance on word files is tricky, only data coming from standardized fields in databases are fully replicable and easily updated.
- Another option: transform pdfs to html, and after that do the opposite, write the html and from there transform into pdfs, word etc (?!)
Some table contents will become easier to collect when TAF is fully running, but not text, which depends on experts.
