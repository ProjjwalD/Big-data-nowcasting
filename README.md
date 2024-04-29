
# Forecasting Indian GDP Growth with ARIMA and Google Trends

## Project Description
This repository contains R scripts for forecasting Indian GDP growth by integrating Google Trends data into an Autoregressive Integrated Moving Average (ARIMA) model. The aim is to enhance the predictive accuracy of the quarterly GDP growth rates using online search behavior as an external regressor.

## Repository Structure
- **/data**: Folder to place the dataset files (not included in the repository for privacy reasons).
- **Code.R**: Main script file containing the data preparation, analysis, and visualization code.

## Getting Started
### Prerequisites
Ensure you have R installed on your computer. You can download it from [CRAN](https://cran.r-project.org/). Additionally, RStudio as an IDE is recommended for running R scripts. Download it from [RStudio's website](https://rstudio.com/products/rstudio/download/).

### Installation
1. Clone the repository or download the ZIP file and extract it.
2. Open the RStudio and set the working directory to the location of the extracted files:
   ```R
   setwd("path/to/folder")
   ```
3. Install the necessary R packages if you haven't already:
   ```R
   install.packages(c("readr", "dplyr", "lubridate", "ggplot2", "scales", "forecast", "tidyr", "stringr", "readxl"))
   ```
4. Place your dataset files in the `/data` folder. The script expects the following files:
   - `multiTimeline.csv` for Google Trends data.
   - `Statement_Quarterly_Constant_01.03.2024.xlsx` for GDP data.

### Running the Script
Load and execute the script in RStudio:
```R
source("Code.R")
```

## Contributing
Contributions to this project are welcome. Please fork the repository and submit a pull request with your suggested changes.

## Acknowledgments
- Ministry of Statistics and Programme Implementation, Government of India, for providing GDP data.
- Google Trends for providing access to search trend data.
