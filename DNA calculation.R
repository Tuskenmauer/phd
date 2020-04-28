library(stringr)
library(tidyverse)
library(dplyr)

#Uploading data
data <- pdf_text("DNA_conc_20200313.pdf")
tab <- str_split(data, "\n")

#Selecting 1st page
tab <- tab[[1]]

#Selecting column names
colnames <- tab[1]
length(colnames)

#Setting patterns for string splitting
pattern1 <- "([a-z])\\s([^U])"
pattern2 <- "(\\d*)\\s(\\()"
pattern3 <- "(\\.)\\s([A-Z])"
pattern4 <- "(\\d*/\\d*/\\d*)\\s(\\d*:\\d*:\\d*)\\s([AM])"

#Clearing up and splitting the string
colnames2 <- colnames %>%
  str_replace_all(pattern1, "\\1_\\2") %>%
  str_replace_all(pattern2, "\\1_\\2") %>%
  str_replace_all(pattern3, "\\1_\\2") %>% 
  str_split("\\s+", simplify = TRUE)
colnames2

#String into data frame
dna <- tab[2:20] %>%
  str_replace_all(pattern4, "\\1.\\2.\\3") %>%
  str_split("\\s+", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(colnames2)
head(dna)

#Selecting only the DNA conc.
pcr <- dna %>% select(Sample_ID, Nucleic_Acid)
pcr <- pcr[-1,]
pcr <- transform(pcr, Nucleic_Acid = as.numeric(Nucleic_Acid))

#Setting up the experiment
v <- 4 #Final volume of DNA in µL
n <- 3 #Repetitions
p <- 5 #Pipeting error in µL
c <- 0.2 #final DNA concentration

dilution <- function(x, n, c, v, p){
  water <- x/c
  total <- water + x
  var <- v*n+p
  if(total > var)
  {pcr %>% mutate(DNA = 1, dilution = water)
  }
  else {
    pcr %>% mutate(DNA = 10, dilution = 10*x/c)
  }
}

#Calculating dilutions
dilution(pcr$Nucleic_Acid, n = n, c = c, v = v, p = p)

#Mixing up the standard
conc = 10 #concentration of S1 in µg/mL
std <- dilution(pcr$Nucleic_Acid, n = n, c = conc, v = v, p = p)
head(std[order(std$Nucleic_Acid, decreasing = TRUE),], n = 5)
sum(std$dilution[1:5]) #total water needed