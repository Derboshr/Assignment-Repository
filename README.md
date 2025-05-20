# README file

The files used in this assignment were written and executed in R version 4.3.2. It is recommended that this version of R is used for the files to be properly run.

## The original data

The data set is a free set procured from the United States' DATA.GOV site, specifically at the url <https://catalog.data.gov/dataset/motor-vehicle-collisions-crashes>.


## Packages

Within this project, the following packages have been used:

-   readr
-   tidyverse
-   ISLR
-   ggridges
-   formatR

If you have not installed any/all of these packages, please do so by executing:

``` python
install.packages("<package name>")
```

In the console.

## Downloading the data

There are 4 key files required for the project:

-   Planning tidy.R ~ (The file in which all data tidying has been done, and where Prelim Report.Rmd sources from)
-   Motor_Vehicle_Collisions\_-\_Crashes.zip ~ (The original data in zip form, so that it can be uploaded and downloaded efficiently)
-   Prelim Report.Rmd ~ (The file required to knit the report)
-   bibliography.bib ~ (The file containing all bibliography entries referenced in the Rmd file)

These files can be obtained either through Moodle or through the following github repository:

<https://github.com/Derboshr/Assignment-Repository>



## Compiling the data

Once the data is downloaded, ensure all 4 key files are located in the same folder. After this open Prelim Report.Rmd, and set the working directory to the folder path of the4 files. Setting the working directory can be done through entering the following in the R console:

``` python
setwd("<folder path name>")
```

Please note, the default when copying address on windows is in the following format:

``` python
C:\Users\Place\Folder\Subfolder
```

When entering an address in the R terminal, this format will return an error, please instead use either format:

``` python
C:\\Users\\Place\\Folder\\Subfolder
```

or

``` python
C:/Users/Place/Folder/Subfolder
```

With the working directory now set, the report file should be able to be run. This can be done by pressing the 'knit' button on the middle top of the script panel.

Once the pdf has been created, it is recommended that the file is opened with either Google Chrome or Adobe Acrobat. This is due to the volume of data within the project, which Microsoft Edge seems to have trouble loading.
