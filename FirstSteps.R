# Import the TSV File into R.
# install.packages("readr")
library(readr)
rawdatadf <- readr::read_tsv("Survey_Symptoms_US.tsv")
print(rawdatadf)

# Test if code works
# One day I will understand what a data frame is.
# Conditional count: https://www.r-bloggers.com/2021/07/countif-function-in-r/
# na.omit, na.rm: https://statisticsglobe.com/na-omit-r-example/
# Some guy's Tutorial: https://statsandr.com/blog/fisher-s-exact-test-in-r-independence-test-for-a-small-sample/
# Check if this will work. Then just rinse and repeat across all possible combinations of columns?
workingdf <- data.frame(
  "symptomA_yes" = c(sum(rawdatadf$Asthma_Symptom_Present == 1 & rawdatadf$COPD_Symptom_Present == 1, na.rm=TRUE), sum(rawdatadf$Asthma_Symptom_Present == 1 & rawdatadf$COPD_Symptom_Present == 0, na.rm=TRUE)),
  "symptomA_no" = c(sum(rawdatadf$Asthma_Symptom_Present == 0 & rawdatadf$COPD_Symptom_Present == 1, na.rm=TRUE), sum(rawdatadf$Asthma_Symptom_Present == 0 & rawdatadf$COPD_Symptom_Present == 0, na.rm=TRUE)),
  row.names = c("symptomB_yes", "symptom_B_no"),
  stringsAsFactors = FALSE # I don't know what this does.
)
workingdf

# Mosaic Plot Code
mosaicplot(workingdf,
           main = "Test Plot",
           color = TRUE
           )

# Expected Frequencies? Should I include code to conditionally run chi-squared instead of Fisher's Exact Test? In this case we can but for other columns we might not.
chisq.test(workingdf)$expected # Unsure of what this type of syntax is, is this how R uses macros?

# Fisher's Test here we go
test <- fisher.test(workingdf)
test

# - - - - - - - - - - - Actual Code
# Gameplan: Iterate over all combinations of columns with Fisher's Exact and print out the data.
# Output Dataframe will have the name of the symptoms compared, and the results of the fisher's exact test.
# Should provide decent practice.

# Initialize Globals
symptomslist <- data.frame(colnames(rawdatadf[, 2:(ncol(rawdatadf) - 9)])) # Get rid of patient number, CSHQ, and Disease Name.
symptomslist # Indexing into this and adding one will map onto the rawdatadf's columns. 214 should be the magic number.

# This uses a matrix which means everything is initialized as the same data type.
# outputdf <- data.frame(matrix(ncol = 4, nrow = 0)) # Did this a lot in MATLAB but doesn't look nice in R https://stackoverflow.com/questions/32712301/create-empty-data-frame-with-column-names-by-assigning-a-string-vector
# colnames(outputdf) <- c("Symptom A", "Symptom B", "p-value", "Odds Ratio")
# outputdf

# Instead I need to use a vector to allow specification of data types. This is why it reports numbers as chrs instead of numbers.
# Actually may not have been the reason, see below where character types are reported as NA when adding numerical data types before being properly added.
# https://sparkbyexamples.com/r-programming/r-create-an-empty-dataframe/
outputdf <- data.frame(SymptomA=character(0), SymptomB=character(0), pvalue=numeric(0), oddsratio=numeric(0))
print(outputdf)

# Counter and While Loop.
for(i in 1:213) { # External loop that runs over all columns.
  for(j in (i+1):214) { # Internal loop that bounces from i to the end for all possible combinations of columns.
    # Create the Contingency Table
    workingdf <- data.frame(
      "symptomA_yes" = c(sum(rawdatadf[, i + 1] == 1 & rawdatadf[, j + 1] == 1, na.rm=TRUE), sum(rawdatadf[, i + 1] == 1 & rawdatadf[, j + 1] == 0, na.rm=TRUE)),
      "symptomA_no" = c(sum(rawdatadf[, i + 1] == 0 & rawdatadf[, j + 1] == 1, na.rm=TRUE), sum(rawdatadf[, i + 1] == 0 & rawdatadf[, j + 1] == 0, na.rm=TRUE)),
      row.names = c(paste("Yes", symptomslist[j, ]), paste("No", symptomslist[j, ])),
      stringsAsFactors = FALSE # I don't know what this does.
    )
    colnames(workingdf) <- c(paste("Yes", symptomslist[i, ]), paste("No", symptomslist[i, ]))
    
    # Run Fisher's Test and Print to the Output Data Frame
    testoutput <- fisher.test(workingdf)
    # Observe that we put the numeric data in first because for some reason c() coerces numerical types to chars if at least one value is a char.
    outputdf[nrow(outputdf) + 1,] <- c(NA, NA, testoutput$p.value, testoutput$estimate)
    outputdf[nrow(outputdf), 1:2] <- c(symptomslist[i,], symptomslist[j,])
  }
}
