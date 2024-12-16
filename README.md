# General Instruction: 
- Navigate to script file needed under the [script](script) folder. A [summary](#Summary) of each script file is listed below.
- On the top right corner of the script file, click on the three-dot button, choose "Download" or Ctrl + Shift + S and save the script to your computer.
- Run the downloaded script with R. Data sets are curled directly from GitHub using the code, so you don't need to download data files manually. 

# Summary:
Section | Subsection | Description| Script | Output | Status (will be deleted when published) | 
|:-----:|:----------:|:------:|:------:| :-----:|:-----:|
|General|VH22 data|Dataset for VH22 innovations and correlates|[VH22 data](script/VH22_data.R)|[VH22 dataset](data/processed/VH22_data.csv), [VH22 codebook](other/codebook%20for%20processed%20data/VH22_data.dic.csv)|complete|
|General|VH23 data|Dataset for VH22 innovations and correlates| [VH23 data](script/VH23_data.R)|[VH23 dataset](data/processed/VH23_data.csv), [VH23 codebook](other/codebook%20for%20processed%20data/VH23_data.dic.csv)|complete|
|General| - |Create all maps in the report|[Maps_all.Rmd](script/Maps_all.RmD) |-|complete|
|Executive Summary| CGIAR Reach estimate| Introduction.R | 
|3. Methods and Data|-|Produce Table 5. Overview of socio-economic variables|[Table 5](script/Table.5.R)|Table 5|complete|
|4. Overview of Results|-|Produce adoption rate table|[Table 7](script/Table.7.R)|Table 7|complete|
|4. Overview of Results|-|Produce adoption rate by region table|[Table 8](script/Table.8.R)|Table 8|complete|
|4. Overview of Results|-|Produce Table 9 and OLS plots|[Table 9](script/Table.9.R)|Table 9; Figures 11, 15, 26, 29, 31, 33, 35, 39, 44, and 46; Appendix C| complete|
|5. Aquaculture|-|Produce Table 10|[Aquaculture](script/3.%20Aquaculture.R)|Table 10|complete|
|6. Breeding Innovations|-|Produce Tables 13 and 15|[Breeding Innovations](script/4.%20Breeding%20Innov.R)|Tables 13 and 15|complete|
|7. Climate Change Adaptation Options|-|-|[CC adaptation](script/5.%20CC%20adaptation.R)|-|complete|
|8. Environmental Conservation|7. Environment|Payment for Forest Environmental Services (PFES)|[7.Environment](script/7. Environment.R)|Figures 24a,b|complete|
|9. Mechanisation|-|Produce Maps|[Mechanisation](script/9.%20Mechanization.R)|Figures 24, 26, 28, 30|complete|
|10. Sustainable Intensification Practices|3R3G, AWD|1M5R, 3R3G, AWD|[8. SI practices](script/8.%20SI%20practices.R)|Figures 3, 4, 7, 8, 36|complete|
|Appendix B|Weights|Calculated weights|[Weight](https://github.com/CGIAR-SPIA/Viet-Nam-report-2024/blob/main/script/Report_weights.R)|[Weight](Output/Report_weights.csv)|complete|

# Software Implementation:
The report uses R version 4.3.2 to generate the results. However, other versions of R may be compatible as well.
To install R:
- Step 1: Download R for [Windows](https://cran.r-project.org/bin/windows/base/) or [macOS](https://cran.r-project.org/bin/macosx/) and install it
- Step 2: Download and install [RStudio](https://posit.co/download/rstudio-desktop/)
- To check the current version of R, open RStudio and enter R.version
For more information, please refer to [The Comprehensive R Archive Network](https://cran.r-project.org/)
