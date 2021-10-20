### ------------------------------------- ###
###               Intro to R              ###
### ------------------------------------- ###


# rm(list = ls()) ## this is the code to empty the global environment in R


### ------------------------------------- ###
# 1. R installation                         #
### ------------------------------------- ###
# https://www.r-project.org/



### ------------------------------------- ###
# 2. R studio                               #
### ------------------------------------- ###
# https://www.rstudio.com/



### ------------------------------------- ###
# 3. Common data types                      #
### ------------------------------------- ###

## Numeric:
x1<-c(1:10)
print(x1)
x1
x1[1]

## Character:
x2<-c("A","B","C","*","/30-+gle","1","6")
print(x2)
x2
x2[5]

## Logical:
class(FALSE)
class(NA) 
# NA is a data type/indicator represents the absence of a value, and is represented by the keyword NA (without quotes) 

## Vector: x1 and x2 are both a vector

## List: an object contains heterogeneous elements. The elements can be matrices, data frames, functions.
## For example, we can save x1 and x2 with different lengths to a list:
length(x1)
length(x2)
x1x2_list=list(var1=x1,var2=x2) # var1 and var2 are the variable/column names given to x1 and x2, respectively.



### ------------------------------------- ###
# 4. Operators for basic calculation:       #
### ------------------------------------- ###

## Addition
20+50

## Subtraction
8-5

## Multiplication
2*10

## Division
10/5

## Exponentiation
10^3


## Modulo: returns the reminder of a division
25%%7



## Assign data to variables:

x1=c(1,1,1)
x2=c(2,1,1)
x3=c(1,0.5,1)
x4="five"
y1=1
y2=0.5
y3=3

x1+x2
x1*y1
x1+x4


## Multiplication in matrices

### Build a matrix from vectors
m1=rbind(x1,x2,x3)

### Build a matrix from scratch
m2=matrix(data = 2, nrow = 3, ncol = 2 )

### Build a vector from elements
v1=c(y1,y2,y3)

m1/2
m1[,1]/2
m1[1,]/2

m1/v1[1]
m1/v1[2]
m1[,1]/v1[1]
m1[1,]/v1[2]
m1[3,3]/v1[3]

m1 * v1
m1 %*% v1
m1 * m2
m1 %*% m2




### ------------------------------------- ###
# 5. Import data sets                       #
### ------------------------------------- ###

setwd("/YOUR DATA LOCATION/")

## Import CSV data file
data_csv=read.csv("binary.csv", header = TRUE, stringsAsFactors = FALSE )
## Import CSV data from an online link, e.g., GitHub repository
data_csv.GitHub=read.csv("https://raw.githubusercontent.com/cmomo/IntroR/main/binary.csv", header = TRUE, stringsAsFactors = FALSE )

## Import text data file
data_txt=read.table("binary.txt", header = TRUE, stringsAsFactors = FALSE )


## Import Excel data file (need a package called "openxlsx" to import)
### Install required package:
install.packages("openxlsx")
### Check whether the required package has already been installed. If not, install the package:
if(length("openxlsx")) install.packages("openxlsx")
### 2 ways to call the installed package
require(openxlsx)
library(openxlsx)

## Import Excel data file:
data_xlsx=read.xlsx("binary.xlsx", sheet=1  ) # import the 1st sheet by its order number
data_xlsx=read.xlsx("binary.xlsx", sheet="binary"  ) # import the 1st sheet by its name




### ------------------------------------- ###
# 6. Data inspection                        #
### ------------------------------------- ###

## Let's check the dimension of the dataset using data_csv:
dim(data_csv)
nrow(data_csv)
ncol(data_csv)


## We can obtain the column and row names, also change the names accordingly.
colnames(data_csv)
colnames(data_csv)[1]="status"
colnames(data_csv)
colnames(data_csv)[c(2:4)]=c("GRE", "GPA", "ranking")
colnames(data_csv)
colnames(data_csv)=c("admit","gre","gpa","rank" )
colnames(data_csv)

rownames(data_csv)



## Let's find out the class of each variable (i.e., column):
class(data_csv)
class(data_csv$admit)
class(data_csv$gre)
class(data_csv$gpa)
class(data_csv$rank)

class(data_csv[,1])
class(data_csv[,"admit"])

## A handy function to check the class for all variables in the dataset:
lapply(data_csv, class)
sapply(data_csv, class)
# lappy - When you want to apply a function to each element of a list in turn and get a list back.
# sapply - When you want to apply a function to each element of a list in turn, but you want a vector back, rather than a list.


## Change the class of variable
class(data_csv$admit)
adm_numeric=data_csv$admit
adm_factor=as.factor(data_csv$admit)
adm_character=as.character(data_csv$admit)
head(adm_numeric)
head(adm_factor)
head(adm_character)

adm_numeric1=as.numeric(adm_factor)
head(adm_numeric1)
adm_character1=as.character(adm_factor)
head(adm_character1)
adm_numeric2=as.numeric(as.character(adm_factor))
head(adm_numeric2)




### ------------------------------------- ###
# 7. Try some calculations using "data_csv" #
### ------------------------------------- ###


## Calculate the difference between GRE score and the mean GRE score: 
mu=mean(data_csv$gre)
data_csv$gre1=data_csv$gre-mu


## Calculate the difference between GRE score and the mean GRE score in each Admit group:
mu1=mean(data_csv$gre[data_csv$admit==1])
mu0=mean(data_csv$gre[data_csv$admit==0])
data_csv$gre1_byAdmit=ifelse(data_csv$admit==1, data_csv$gre-mu1, data_csv$gre-mu0) 





### ------------------------------------- ###
# 8. Descriptive of data                    #
### ------------------------------------- ###

## If a vector of IDs are available for all subjects, and they are saved in the order as the data set.
## We want to combine the IDs with the data set. 
ID=ids::random_id(n = 400, bytes = 3, use_openssl = TRUE)

data=data_csv

data1=data
data1$ID=ID
data2=cbind(ID,data)

head(data2)
sapply(data2, class)

## admit: whether a subject was admitted or not (0 = Not Admit; 1 = Admit)
data2$admit_group[data2$admit==0]="Not Admit"
head(data2)
table(data2$admit_group)
data2$admit_group[is.na(data2$admit_group)]="Admit"
head(data2)
table(data2$admit_group)

data2_1=data2[data2$admit==1,]
data2_0a=data2[data2$admit==0,]
data2_0b=data2[data2$admit!=1,]


mean(data2$gre)
mean(data2$gpa)
mean(data2_1$gre)
mean(data2_0a$gre)
mean(data2$gre[data2$admit_group=="Admit"] )
mean(data2$gre[data2$admit_group!="Admit"] )
aggregate(data2$gre, by=list(data2$admit_group), FUN=mean )

sd(data2$gpa)
aggregate(data2$gre, by=list(data2$admit_group), FUN=sd )


meanSD=function(x){
  a=mean(x)
  b=sd(x)
  return(c(Mean=a, SD=b ))
} 

aggregate(data2$gre, by=list(data2$admit_group), 
          FUN=meanSD)
sapply(data2[,-c(1,6)], meanSD)
sapply(data2[, !colnames(data2) %in% c("ID","admit_group")], meanSD)


## Group GPA to A+ >= 3.5, 3<= A- <3.5, 2.5<= B+ <3, 2 <= B- < 2.5
data2$gpa_group=NA
data2$gpa_group[data2$gpa>=3.5]="A+"
data2$gpa_group[data2$gpa<3.5 & data2$gpa>=3]="A-"
data2$gpa_group[data2$gpa<3 & data2$gpa>=2.5]="B+"
data2$gpa_group[data2$gpa<2.5]="B-"
## Check the number of subjects (frequency) in each group:
table(data2$gpa_group)
sum(table(data2$gpa_group))




### ------------------------------------- ###
# 9. Data saving, management, and cleaning  #
### ------------------------------------- ###

aggregate(data2[, c("gre")], by=list(data2$admit_group), meanSD)
aggregate(data2[, c("gpa")], by=list(data2$admit_group), meanSD)

gre=aggregate(data2[, c("gre")], by=list(data2$admit_group), meanSD)
gpa=aggregate(data2[, c("gpa")], by=list(data2$admit_group), meanSD)
gre
gpa

## save all results as a list:
res_list=list(GREbyAdmit=gre, GPAbyAdmit=gpa )
write.csv(res_list, file = "res_list.csv")
write.table(res_list, file = "res_list.txt")
saveRDS(res_list, file = "res_list.rds")

sink("res_list_sink.txt")
res_list
sink()

gre$Score="gre"
gre
gpa$Score="gpa"
gpa

gre
gre$x
gre_x=gre$x
gpa_x=gpa$x
gre_x
gpa_x

gre1=cbind.data.frame(Group=gre$Group.1, gre_x )
gpa1=cbind.data.frame(Group=gpa$Group.1, gpa_x )
gre1
gpa1

## Add a column: indicator of test
gre1$Test="GRE"
gpa1$Test="GPA"
gre1
gpa1

res_gre_gpa_c=cbind.data.frame(gre1,gpa1 )
res_gre_gpa_c
res_gre_gpa_r=rbind.data.frame(gre1,gpa1 )
res_gre_gpa_r
res_gre_gpa_r1=res_gre_gpa_r
res_gre_gpa_r1$Mean=round(res_gre_gpa_r1$Mean, digits = 2)
res_gre_gpa_r1$SD=round(res_gre_gpa_r1$SD, digits = 2)
res_gre_gpa_r1



## save cleaned results as a table:
write.csv(res_gre_gpa_r1, file = "res_gre_gpa_r.csv" )
write.csv(res_gre_gpa_r1, file = "res_gre_gpa_r(1).csv", row.names = F )





### ------------------------------------------------------- ###
# 10. Test between two admission groups: admit vs. not admit  #
### ------------------------------------------------------- ###

t.test(data2_1$gre, data2_0a$gre, alternative = "two.sided" )
t.test(data2_1$gpa, data2_0a$gpa, alternative = "two.sided" )
wilcox.test(data2_1$gre, data2_0a$gre )
wilcox.test(data2_1$gpa, data2_0a$gpa )



