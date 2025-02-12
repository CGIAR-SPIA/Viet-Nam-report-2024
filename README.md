
# SPIA Viet Nam Report: Global Ambitions, Sustainable Pathways (2024)

Welcome to this repository, which contains the reproducible code used in the [SPIA report]() on the adoption of agricultural innovations in Viet Nam. The report is based on data from the Vietnam Household Living Standards Survey (VHLSS), collected by the General Statistics Office in 2022, 2023, and 2024. It offers insights into the adoption patterns of 19 CGIAR-related agricultural innovations across the country.

Additional materials from the report are available in an OpenICPSR repository [here](https://www.openicpsr.org/openicpsr/project/212901).

# General Instructions

- To run the code, navigate to the desired script file in the [script](script) folder. A [summary](#Summary) of each script file is provided below.
In the top-right corner of the script file, click on the three-dot button and select "Download" (or use Ctrl + Shift + S) to save the script to your computer.
Run the downloaded script in R. The datasets are automatically fetched directly from GitHub within the script, so you do not need to download the data files manually.
If you wish to download the datasets manually, go to the [data](data) folder. In the top-right corner, you will find an arrow icon to "Download raw file."
  
# Summary
Section | Description| Script | Output |
|:-----:|:------:|:------:| :-----:|
|-|Dataset for VH22 innovations and correlates|[VH22 data](script/VH22_data.R)|[VH22 dataset](data/processed/VH22_data.csv), [VH22 codebook](other/codebook%20for%20processed%20data/VH22_data.dic.csv)|
|-|Dataset for VH23 innovations and correlates| [VH23 data](script/VH23_data.R)|[VH23 dataset](data/processed/VH23_data.csv), [VH23 codebook](other/codebook%20for%20processed%20data/VH23_data.dic.csv)|
|-|Create all maps in the report|[Maps_all](script/Maps_all.RmD) |Figures 15, 17, 33, 35, 40 |
|Executive Summary| CGIAR Reach estimates| [Reach](script/Reach.R) | Figure 1 | 
|3. Methods and Data|Overview of socio-economic variables|[Table 5](script/Table%205.R)|Table 5|
|4. Overview of Results|Adoption rate|[Table 7](script/Table%207.R)|Table 7|
|4. Overview of Results|Adoption rate by region|[Table 8](script/Table%208.R)|Table 8|
|4. Overview of Results|OLS results|[Table 9](script/Table%209.R)|Table 9; Figures 13, 17, 27, 29, 31, 33, 37, 42, and 47; Appendix E|
|5. Aquaculture|Section results |[Aquaculture](script/5.%20Aquaculture.R)|Tables 10, 11; Figures 8, 11, 12 |
|6. Breeding Innovations|Section results |[Breeding Innovations](script/6.%20Breeding%20Innov.R)|Tables 13, 15, 16; Figures 15, 16, 18|
|7. Climate Change Adaptation Options|Section results|[CC adaptation](script/7.%20CC%20adaptation.R)|Tables 17, 18, 20; Figures 21, 22, 23, 24|
|8. Environmental Conservation|Section results|[Environment](script/8.%20Environment.R)|Figures 25a,b|
|9. Mechanisation|Section results|[Mechanisation](script/9.%20Mechanization.R)|Figures 26, 28, 30, 32|
|10. Sustainable Intensification Practices|Section results|[SI practices](script/10.%20SI%20practices.R)|Table 21, Figures 34, 35, 36, 38, 39, 40, 46|
|Appendix B|Dataset with calculated weights|[Weights](https://github.com/CGIAR-SPIA/Viet-Nam-report-2024/blob/main/script/Report_weights.R)|[Weights dataset](Output/Report_weights.csv)|

# Software Implementation:
The report uses R version 4.3.2 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 22631)


To install R:

- Download R for [Windows](https://cran.r-project.org/bin/windows/base/) or [macOS](https://cran.r-project.org/bin/macosx/) and install it
- Download and install [RStudio](https://posit.co/download/rstudio-desktop/)
- To check the current version of R, open RStudio and enter "R.version"
  
For more information, please refer to [The Comprehensive R Archive Network](https://cran.r-project.org/)
