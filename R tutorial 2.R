### ------------------------------------- ###
###            Data Management            ###
### ------------------------------------- ###


# 0. Set directory

setwd("/YOUR DATA LOCATION/")


# 1. Example 1: combine data files based on subjects' ID

## Import files:

library(readxl)

WhichFile<- "Data 2/hdp_abc.xlsx"
excel_sheets(path = WhichFile)
# [1] "hdp_a" "hdp_b" "hdp_c"


df_a=read_xlsx(WhichFile, sheet="hdp_a"  )
df_b=read_xlsx(WhichFile, sheet="hdp_b"  )

## Combine data 
### If we want to obtain the frequency of remission in married and not married groups, 
### we need to combine data from df_a and df_b:
df_ab=merge(df_a,df_b, by="ID" )

table(df_ab$remission)
#     0    1 
# 6004 2521 
table(df_ab$Married)
#     0           1     married     Married Not Married  NotMarried 
# 3408        5113           1           1           1           1 
class(df_ab$Married)
# [1] "character"


### Clean data:
#### Convert all letters to lower case:
df_ab$Married_new=tolower(df_ab$Married)
table(df_ab$Married_new)
#     0           1     married not married  notmarried 
# 3408        5113           2           1           1 

df_ab$Married_new1=gsub(" ", "", x=df_ab$Married_new, fixed = TRUE)
table(df_ab$Married_new1)
#     0          1    married notmarried 
# 3408       5113          2          2 

df_ab$Married_new2[df_ab$Married_new1 %in% c("married")]="Married"
table(df_ab$Married_new2)
# Married 
# 2 

df_ab$Married_new3[df_ab$Married_new1 %in% c("married", "1")]="Married"
table(df_ab$Married_new3)
# Married 
# 5115 

df_ab$Married_new3[df_ab$Married_new1 %in% c("notmarried", "0")]="Not Married"
table(df_ab$Married_new3)
# Married Not Married 
# 5115        3410 

sum(table(df_ab$Married_new3))
# [1] 8525


### Obtain the frequency table of remission in marriage groups:
table(df_ab$remission, df_ab$Married_new3)
#    Married Not Married
# 0    3566        2438
# 1    1549         972

### Add labels:
df_ab$remission_label[df_ab$remission==0]="NO"
df_ab$remission_label[df_ab$remission==1]="Yes"
table(df_ab$remission_label, df_ab$Married_new3)
#     Married Not Married
# NO     3566        2438
# Yes    1549         972



# 2. Example 2: convert data between wide and long formats

tolerance=read.csv("Data 1/tolerance1.csv", header = T, stringsAsFactors = F )
head(tolerance)
## This data is a so called "WIDE" data - each row contains the values for one individual
# id tol11 tol12 tol13 tol14 tol15 male exposure
# 1   9  2.23  1.79  1.90  2.12  2.66    0     1.54
# 2  45  1.12  1.45  1.45  1.45  1.99    1     1.16
# 3 268  1.45  1.34  1.99  1.79  1.34    1     0.90
# 4 314  1.22  1.22  1.55  1.12  1.12    0     0.81
# 5 442  1.45  1.99  1.45  1.67  1.90    0     1.13
# 6 514  1.34  1.67  2.23  2.12  2.44    1     0.90


## Some times we need to convert the "WIDE" data to "LONG" data for analysis.
## There are several ways to convert data between wide and long.

### a. Use "reshape2": wide to long
library(reshape2)

tolerance.long.1=reshape(data = tolerance, idvar = c("id", "male", "exposure"), 
                         varying = c( "tol11","tol12","tol13","tol14","tol15"), 
                         timevar = "year", v.names = "tolerance", direction = "long" )


## Let's check if the data was correctly tranformed
tolerance.long.1[tolerance.long.1$id==9, ]
#             id male exposure year tolerance
# 9.0.1.54.1  9    0     1.54    1      2.23
# 9.0.1.54.2  9    0     1.54    2      1.79
# 9.0.1.54.3  9    0     1.54    3      1.90
# 9.0.1.54.4  9    0     1.54    4      2.12
# 9.0.1.54.5  9    0     1.54    5      2.66


### b. Use "tidyr": wide to long
library(tidyr)
tolerance.long.2=gather(data = tolerance, key = "year", value = "tolerance",
                        c( "tol11","tol12","tol13","tol14","tol15"),
                        factor_key=TRUE )
tolerance.long.2[tolerance.long.2$id==9, ]
# id male exposure  year tolerance
# 1   9    0     1.54 tol11      2.23
# 17  9    0     1.54 tol12      1.79
# 33  9    0     1.54 tol13      1.90
# 49  9    0     1.54 tol14      2.12
# 65  9    0     1.54 tol15      2.66


## If we have a long data and want to tranform it to wide data, we can also use the same packages.

### c. Use "reshape2": long to wide
tolerance.wide.1=reshape(data = tolerance.long.1, idvar = c("id", "male", "exposure"), 
                         timevar = "year",
                         direction = "wide" )


### d. Use "tidyr": long to wide
tolerance.wide.2=spread(data = tolerance.long.2, key = "year", value = "tolerance")




# 3. Example 3: duplicate data
tolerance2=read.csv("Data 2/tolerance2.csv" , header = T, stringsAsFactors = F )
head(tolerance2)
#     id tol11 tol12 tol13 tol14  tol15 male exposure
# 1   9  2.23  1.79  1.90  2.12   2.66    0     1.54
# 2   9  2.23    NA  1.90  2.12 2.66 a    0     1.54
# 3  45  1.12  1.45  1.45  1.45   1.99    1     1.16
# 4 268  1.45  1.34  1.99  1.79   1.34    1     0.90
# 5 314  1.22  1.22  1.55  1.12   1.12    0     0.81
# 6 442  1.45  1.99  1.45  1.67    1.9    0     1.13
sapply(tolerance2, class)
#       id       tol11       tol12       tol13       tol14       tol15        male    exposure 
# "integer"   "numeric"   "numeric"   "numeric"   "numeric" "character"   "integer"   "numeric" 
## Why tol15 is character but not numeric?
## The second observation is 2.66 a, causing the whole column imported as character. 
## Also, the data have duplicates (e.g., two observations found for subject 9). 

## First, let's clean the letters in the numeric variable:
tolerance2$tol15=gsub("[a-zA-Z ]", "", tolerance2$tol15)
tolerance2$tol15
# [1] 2.66 2.66 1.99 1.34 1.12 1.90 2.44   NA 1.99 1.22 1.12 1.22 1.55 3.32 3.32 2.12
# [17] 2.12 1.55 2.12
class(tolerance2$tol15)
# [1] "character"
## "tol15" is still character after removing letters in the column values, so we have to convert it to numeric:
tolerance2$tol15=as.numeric(tolerance2$tol15)
class(tolerance2$tol15)
# [1] "numeric"


## Second, let's check out how many and which subjects have duplicates:
tolerance2[duplicated(tolerance2$id), ]
#     id tol11 tol12 tol13 tol14  tol15 male exposure
# 2    9  2.23    NA   1.9  2.12 2.66 a    0     1.54
# 9  569  1.79  1.90   1.9  1.99   1.99    0     1.99
# 15 978  1.22  1.34    NA    NA   3.32    1     1.59
dim(tolerance2[duplicated(tolerance2$id), ])
# [1] 3 8
nrow(tolerance2[duplicated(tolerance2$id), ])
# [1] 3

## 3 subjects have duplicated observations
## Let's find out all the duplicated observations for them. First, we identify their IDs:
ind=tolerance2$id[duplicated(tolerance2$id)]
ind
tolerance2[which(tolerance2$id %in% ind ), ]
## The duplicates all have same values for each subject except some of them have NAs. 
## So we can keep those with complete data (i.e., rows have all column values available).
## We identify that the rows need to be removed are row 2,8,and 15. 
tolerance2.clean=tolerance2[-c(2,8,15),]
## We check again if there is duplicate.
nrow(tolerance2.clean[duplicated(tolerance2.clean$id), ])
# [1] 0
## 0 row shows that there is no duplicate any more.
write.csv(tolerance2.clean, "Data 2/tolerance2_clean.csv", row.names = F)




