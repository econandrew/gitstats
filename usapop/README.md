USA Total Resident Population 1901 to today
-------------------------------------------

An R program to generate a single consistent historical population series for the USA from 1901 to the most recent estimate. It is based on a compilation of multiple US Census Bureau files, namely
- an estimate from 1901-1999
- revised estimates 1990-1999 based on 2000 census
- estimates 2000-2009 based on 2010 census
- 2010s estimates

This, oddly, doesn't seem to exist as a single official data set. My numbers are consistent with http://www.multpl.com/united-states-population/table, but here you have the R code to validate the numbers.

My numbers are also more or less the same as those given by the OECD (this is tested in the code), but don't you prefer having a dataset you can source back to USCB?