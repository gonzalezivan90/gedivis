This tool allow users to explore GEDI measurements across Colombian regions and ecosystems. The project is led by Dr. Patrick Jantz (Patrick.Jantz@nau.edu) and developed by Ph.D. student Ivan Gonzalez (ig299@nau.edu)

This code requires an R file that contains the front-end. Due the data size, most of files used in this app are pre-processed and stored in RData format.
Files can de downloaded from [here] (https://drive.google.com/open?id=12IDvY3dG64yGz2kq1BEdpxjUdmK_KsQt&usp=drive_fs)

Files:
- app.R: R functions
- 16_blue.png: icon to use in leaflet for protected areas
- 17_blue.png: icon to use in leaflet for non-protected areas

- intro.png : image describing instructions to use the similarity tool
- simintro.png: image describing instructions to use the similarity tool

- Fagua_5km_sampledGEDI.tif: binary raster of 5km with presence of GEDI point
- Fagua_5km_sampledNGEDI.tif: count of GEDI point in each cell for 5km raster
- Fagua_et_al_2019_Byte_1km.tif: Fagua et al. vegetation heigth 5km raster (resampled for viz)
- Fagua_et_al_2019_Byte_5km.tif: Original Fagua et al. vegetation heigth 1km raster
- Fagua_et_al_2019_Byte_compDEF.tif: Compressed Fagua et al. vegetation heigth 1km raster
- Fagua_ID_5km_54227cells.tif: Raster ID used as a key to load GEDI sections
- ID_sampled_5km_gedi.tif: Raster ID used as a key to load GEDI sections, only with values on pixels with GEDI information

- md_intro.md: Markdown with instrucctions 
- md_introsimi.md: Markdown with instructions for similarity module 

- tempSQlite.sqlite: Temporal database used to support GDAL crop of height map

- uEcoDf.RData: Data frame of ecosystems and ecoregions
- regionsSimpl.RData: Simplified ecoregions polygon layer to load on the leaflet viz panel
- orbits_col_crop_892.RData: Vector layer of GEDI orbits in Colombia
- leafSim.RData: Simplified starting layers for leaflet 


Folders: 
- www: With visual accessory files for front-end design
- gedi2019_id_5km: Clip of GEDI points for each 5km pixel
- rast_maps_veg: 
- rdat_2a: Preprocessed files with vertical profiles, L2a GEDI points and spatial layers for each combination of ecosystem + region
- rdat_2b: Preprocessed files with vertical profiles and L2b GEDI points metrics for each combination of ecosystem + region
- rdat_eco: Spatial layers (original and simplified) for plot on the geographical panel
- umap_2a: UMAP (dimension reduction) L2a data for several sample sizes stored as data frames
- umap_2b: UMAP (dimension reduction) L2a data for several sample sizes stored as data frames

Original data sources:
- Biotic regions: Vectorial layer from Environmental Studies Institute in Colombia (Instituto de Hidrología, Meteorología y Estudios Ambientales - IDEAM). Available [here](http://www.siac.gov.co/catalogo-de-mapas) (1.7GB).
- Ecosystems: Vectorial layer from Environmental Studies Institute in Colombia (Instituto de Hidrología, Meteorología y Estudios Ambientales - IDEAM). Available [here](http://www.siac.gov.co/catalogo-de-mapas) (40MB).
- GEDI points: HDF5 files from NASA level 2 products repositories. Check more [here](https://lpdaac.usgs.gov/products/gedi02_av001/) (~10 GB)
