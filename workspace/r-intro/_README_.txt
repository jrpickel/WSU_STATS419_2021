(1) Write a handshake function:

handShake = function(n=1, plotMe=FALSE)
  {
  
  
  }

(2) Write code that reads in the "original draft [original.txt]" of the declaration of independence (the Thomas Jefferson draft).  Write code that reads in the "unanimous draft [final.txt]" (signed by Congress).  [You will have to find this data on your own.  'Big Brother' Google will likely help.] For each draft, count how many letters of the alphabet used.  (lower case a-z and OTHER as the bucket for any non-white space symbol).  Write a generic function [create a very verbose name] that does this where the input is the raw string and the output is a dataframe that has one row with a-z,OTHER as the column names and the counts of the each character in the cell.  Summarize these results as a table and as "one visualization".  

EASTER EGG (10 points): post code that would take the two text inputs (original.string and final.string) and does a string diff comparison.

(3) Read in the "measures-students-cleansed.txt" file.  Convert all measurements to "centimeters".  Identify "replicators" and "fabricators" of data.  Remove a "data-collector" that was a "fabricator" from the dataset.  Remove the "replicate" records form the dataset (e.g., if row A1 and A2 are replicates, only keep one of them).
 
(4) Write a function "computeDeterminant" for a 3x3 matrix.  The function take a 3x3 matrix as the input and "manually" loop through and compute the Determinant.