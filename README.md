# General Instruction: 
- If you want to download the data only, direct to [data](data), click on the data file. On the top-right corner, you will see an arrow indicating "Downnload raw file". Download and save to your local directory. 
- To run the code, navigate to script file needed under the [script](script) folder. A [summary](#Summary) of each script file is listed below.
- On the top right corner of the script file, click on the three-dot button, choose "Download" or Ctrl + Shift + S and save the script to your computer.
- Run the downloaded script with R. Data sets are curled directly from GitHub using the code, so you don't need to download data files manually. 

# Summary:
Section | Description| Script | Output | Status (will be deleted when published) | 
|:-----:|:------:|:------:| :-----:|:-----:|
|-|Dataset for VH22 innovations and correlates|[VH22 data](script/VH22_data.R)|[VH22 dataset](data/processed/VH22_data.csv), [VH22 codebook](other/codebook%20for%20processed%20data/VH22_data.dic.csv)|complete|
|-|Dataset for VH23 innovations and correlates| [VH23 data](script/VH23_data.R)|[VH23 dataset](data/processed/VH23_data.csv), [VH23 codebook](other/codebook%20for%20processed%20data/VH23_data.dic.csv)|complete|
|-|Create all maps in the report|[Maps_all](script/Maps_all.RmD) |Figures 15, 17, 33, 35, 40 |complete|
|Executive Summary| CGIAR Reach estimates| [Reach](script/Reach.R) | Figure 1 | complete
|3. Methods and Data|Overview of socio-economic variables|[Table 5](script/Table.5.R)|Table 5|complete|
|4. Overview of Results|Adoption rate|[Table 7](script/Table.7.R)|Table 7|complete|
|4. Overview of Results|Adoption rate by region|[Table 8](script/Table.8.R)|Table 8|complete|
|4. Overview of Results|OLS results|[Table 9](script/Table.9.R)|Table 9; Figures 11, 15, 26, 29, 31, 33, 35, 39, 44, and 46; Appendix C| complete|
|5. Aquaculture|Section results |[Aquaculture](script/3.%20Aquaculture.R)|Table 10; Fig 9,11,12 |complete|
|6. Breeding Innovations|Section results |[Breeding Innovations](script/4.%20Breeding%20Innov.R)|Tables 13 and 15|complete|
|7. Climate Change Adaptation Options|Section results|[CC adaptation](script/5.%20CC%20adaptation.R)|Table 20, Bulletin dataset|complete|
|8. Environmental Conservation|Section results|[Environment](script/7.%20Environment.R)|Figures 24a,b|complete|
|9. Mechanisation|Section results|[Mechanisation](script/9.%20Mechanization.R)|Figures 24, 26, 28, 30|complete|
|10. Sustainable Intensification Practices|Section results|[SI practices](script/8.%20SI%20practices.R)|Figures 3, 4, 7, 8, 36|complete|
|Appendix B|Dataset with calculated weights|[Weights](https://github.com/CGIAR-SPIA/Viet-Nam-report-2024/blob/main/script/Report_weights.R)|[Weights dataset](Output/Report_weights.csv)|complete|

# Software Implementation:
The report uses R version 4.3.2 to generate the results. However, other versions of R may be compatible as well.

To install R:

- Step 1: Download R for [Windows](https://cran.r-project.org/bin/windows/base/) or [macOS](https://cran.r-project.org/bin/macosx/) and install it
- Step 2: Download and install [RStudio](https://posit.co/download/rstudio-desktop/)
- To check the current version of R, open RStudio and enter R.version
  
For more information, please refer to [The Comprehensive R Archive Network](https://cran.r-project.org/)
