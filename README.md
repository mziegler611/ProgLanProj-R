# ProgLanProj-R
# Purpose: 
Read an excel file and get statistical information from it, including minimum/maximum, average, frequency, and find.

# Steps to install/run:
Download R-Studio (or Vscode and R/and a R extension)

Open the project

Change the path in Line 23 in main.r, line 10 in test.r, and line 18 in functions.r  to where the excel file catsvsdogs is located on your computer

Then Change the path in Line 7 in main.r to where the functions.r is located on your computer

Then change the path in line 7 in test.r to where the main.r file is located on your computer

And Run! 

If it is not working you may need to install all the libraries first which can be done by running the following lines at the beginning of the code

install.packages(readxl)

install.packages(tidyselect)

install.packages(dplyr)

install.packages(tidyr)

install.packages(magrittr)

install.packages(readr)

install.packages(testthat)


# Some links to good resources:
https://towardsdatascience.com/unit-testing-in-r-68ab9cc8d211 

good for unit testing

https://readxl.tidyverse.org/ 

for reading excel sheets in general

https://www.geeksforgeeks.org/r-object-oriented-programming/

shows some simple object creation


# Notes:
# Challenges of New Programming Language
Things get hard to read, pretty fast

Path navigation for files is a bit odd, but maybe there is a better way to do this we just haven’t found

# Benefits of New Programming Language
Has many built in libraries that make writing more manageable

Has unit testing to help while writing code

