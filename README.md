
# SPIA Viet Nam Report: Global Ambitions, Sustainable Pathways (2024)

Welcome to this repository, which contains the materials and reproducible code used in the SPIA report on the adoption of agricultural innovations in Vietnam. The report is based on data from the Vietnam Household Living Standards Survey (VHLSS), collected by the General Statistics Office in 2022, 2023, and 2024. It offers insights into the adoption patterns of 19 CGIAR-related agricultural innovations across the country.

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
|3. Methods and Data|Overview of socio-economic variables|[Table 5](script/Table.5.R)|Table 5|
|4. Overview of Results|Adoption rate|[Table 7](script/Table.7.R)|Table 7|
|4. Overview of Results|Adoption rate by region|[Table 8](script/Table.8.R)|Table 8|
|4. Overview of Results|OLS results|[Table 9](script/Table.9.R)|Table 9; Figures 11, 15, 26, 29, 31, 33, 35, 39, 44, and 46; Appendix C|
|5. Aquaculture|Section results |[Aquaculture](script/5.%20Aquaculture.R)|Table 10; Fig 9,11,12 |
|6. Breeding Innovations|Section results |[Breeding Innovations](script/6.%20Breeding%20Innov.R)|Tables 13 and 15|
|7. Climate Change Adaptation Options|Section results|[CC adaptation](script/7.%20CC%20adaptation.R)|Table 20, Bulletin dataset|
|8. Environmental Conservation|Section results|[Environment](script/8.%20Environment.R)|Figures 24a,b|
|9. Mechanisation|Section results|[Mechanisation](script/9.%20Mechanization.R)|Figures 24, 26, 28, 30|
|10. Sustainable Intensification Practices|Section results|[SI practices](script/10.%20SI%20practices.R)|Figures 3, 4, 7, 8, 36|
|Appendix B|Dataset with calculated weights|[Weights](https://github.com/CGIAR-SPIA/Viet-Nam-report-2024/blob/main/script/Report_weights.R)|[Weights dataset](Output/Report_weights.csv)|

# Software Implementation:
The report uses R version 4.3.2 to generate the results. However, other versions of R may be compatible as well.

To install R:

- Step 1: Download R for [Windows](https://cran.r-project.org/bin/windows/base/) or [macOS](https://cran.r-project.org/bin/macosx/) and install it
- Step 2: Download and install [RStudio](https://posit.co/download/rstudio-desktop/)
- To check the current version of R, open RStudio and enter R.version
  
For more information, please refer to [The Comprehensive R Archive Network](https://cran.r-project.org/)
