### ------------------------------------- ###
###            Data Management            ###
### ------------------------------------- ###


# 0. Set directory

setwd("C:/Users/sarbe/OneDrive/Desktop/R Data")
# setwd("/YOUR DATA LOCATION/")


# 1. Need to combine data files based on subjects' ID

## Import files:

library(readxl)

WhichFile<- "hdp_abc.xlsx"
excel_sheets(path = WhichFile)
# [1] "hdp_a" "hdp_b" "hdp_c"


df_a=read.xlsx(WhichFile, sheet="hdp_a"  )
df_b=read.xlsx(WhichFile, sheet="hdp_b"  )

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


