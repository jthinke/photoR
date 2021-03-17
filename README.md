# photoR
R Shiny app for estimating phenology and reproductive success from annotated time lapse image data.
Code was last updated 27 April 2020 to fix bugs for downloading results.

## Quick instructions 

Simple instruction for running the photoR app from RStudio (1-6) or online [here](https://jefferson.shinyapps.io/photor2/) (3-6).

1) install the 'shiny' and 'lubridate' packages from a CRAN repository, if needed
  
```r
install.packages(c("shiny", "lubridate")
```
2) open `app.r` in Rstudio and run the app (green arrow icon 'RunApp')

3) There will be 2 data files to load. The first is the attendance data (e.g., test_att.csv) and the second is reproductive data (e.g, test_repro.csv). Navigate to where the data are housed and allow upload. The app will automatically identify errors in your data formatting and print messages to the screen to help identify where data errors are likely.

4) Specify how the date field is input in the raw data files you just specified. For the test data, it is "m/d/y'.

5) Select which data output you want to see. 

6) Use the download button to download results.

## Reference

Hinke JT, A Barbosa, L Emmerson, T Hart, M Juáres, M Korczak-Abshire, G Milinevsky, M Santos, P Trathan, G Watters, C Southwell. 2018. Estimating nest-level phenology and reproductive success of colonial seabirds using time-lapse cameras. Methods in Ecology and Evolution. 9:1853-1863. [doi:10.1111/2041-210X.13015](https://doi.org/10.1111/2041-210X.13015)
