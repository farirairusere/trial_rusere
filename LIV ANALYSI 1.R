home=setwd("C:\\Users\\frusere\\Desktop\\DATA")

A<-read.csv("RYTTTA.csv")



View(A)
library(networkD3)
library(dplyr)
library(lme4)
library(jtools)
library(lmerTest)
library(tidyverse)
library(rockchalk)
library(dplyr)
library(gtsummary)
library(car)
library(ordinal)
library(finalfit)
library(dplyr)
library(mice)
library(ggplot2)
library(plm)
library(flextable)
library(gt)
library(kableExtra)
library(pglm)
library(broom.mixed)
library(huxtable)
library(cluster)
library(NbClust)
library(factoextra)
library(lcmm)
library(flexmix)
library(mclust)
library(poLCA)
library(lavaan)
library(msm)
library(stats19)
library(agricolae)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(vcd)
library(networkD3)
library(poLCA)
library(psych)
library(nnet)
library(randomForest)
library(xfun)
library(pdp)
library(dunn.test)
library(RColorBrewer)
library(scales)

library(ggpubr)
library(ggExtra)
library(gridExtra)

summary(A)
A$gender<-as.factor(A$gender)
A$villagename<-as.factor(A$villagename)

A$year<-as.factor(A$year)
A$crop_plant_last_year<-as.factor(A$crop_plant_last_year)
A$cattle_own_any<-as.factor(A$cattle_own_any)
A$goat_own_any<-as.factor(A$goat_own_any)
summary(A)

A$gender<-as.factor(A$gender)
summary(A$villagename)
levels(A$villagename)
A <- A %>%mutate(villagename = factor(trimws(tolower(as.character(villagename)))))

# View the cleaned levels to confirm the changes
print(levels(A$villagename))

A <- A %>%mutate(villagename = trimws(tolower(as.character(villagename))))

# Replace "kumani" and "khaya lami" with "agincourt" by converting to character first
A <- A %>%mutate(villagename = ifelse(villagename %in% c("kumani", "khaya lami"), "agincourt", villagename))

# Convert back to factor and drop unused levels
A$villagename <- factor(A$villagename)

# Check the levels to ensure the changes
print(levels(A$villagename))

# Check the summary to see the counts
summary(A$villagename)

A <- A %>%mutate(villagename = trimws(tolower(as.character(villagename))))

# Replace "kumani" and "khaya lami" with "agincourt" by converting to character first
A <- A %>%mutate(villagename = ifelse(villagename %in% c("rholane"), "lillydale b", villagename))

# Convert back to factor and drop unused levels
A$villagename <- factor(A$villagename)

# Check the levels to ensure the changes
print(levels(A$villagename))

# Check the summary to see the counts
summary(A$villagename)

summary(A)
summary(A$crop_plant_last_year)

A$crop_plant_last_year[A$crop_plant_last_year == "-"] <- 0
levels(A$crop_plant_last_year)<-list("0"=c("0","-"),"1"=c("1"))
summary(A$crop_plant_last_year)

A<-A%>%mutate(LIVESTOCK=case_when(cattle_own_any=="0"&goat_own_any=="0"~"0", cattle_own_any=="1"|goat_own_any=="1"~"1"))
A%>%count(LIVESTOCK)
View(A)

A<-A%>%mutate(LBL=case_when(hh_field_number=="0"&LIVESTOCK=="0"~"0", hh_field_number=="1"|hh_field_number=="2"|hh_field_number=="3"|hh_field_number=="4"|hh_field_number=="5"|hh_field_number=="6"|LIVESTOCK=="1"~"1"))
A%>%count(LBL)
View(A)
summary(A$hh_field_number)
A$hh_field_number<-as.factor(A$hh_field_number)
levels(A$hh_field_number)<-list("0"=c("0"),"1"=c("1","2","3","4","5","6"))
summary(A$hh_field_number)


summary(A)
A$AGED_count <- as.numeric(as.character(A$AGED_count))

A$CHN_count <- as.numeric(as.character(A$CHN_count))


categorical_vars <-c("hh_field_number","LIVESTOCK","crop_plant_last_year","gender","HVA","QUE","FID","LBL")
continuous_vars <- c("Household_size","DR","GR","DA","age","CHN_count","AGED_count","AA","NRU","gov_grant_value","MER","OFER","NRC")
str(A)

A1<-A%>%dplyr::select(household_size,DR,GR,DA,age,HVA,gender,AGED_count,AA,CHN_count,HVA,DA,OFER,MER,gov_grant_value,NRC,NRU,LBL,hh_field_number,LIVESTOCK,crop_plant_last_year,year)%>%tbl_summary(by=year)
A1

A21<-A%>%dplyr::select(AGED_count,AA,CHN_count,year,FID,QUE)
View(A21)

A1<-A%>%dplyr::select(household_size,DR,GR,DA,age,HVA,gender,AGED_count,AA,CHN_count,HVA,DA,OFER,MER,gov_grant_value,NRC,NRU,LBL,hh_field_number,LIVESTOCK,crop_plant_last_year,year)%>%tbl_summary(by=year,statistic = list(all_continuous()~"{mean}({sd})",all_categorical()~"{n}/({N})"),digits=all_continuous()~2,type = list(CHN_count ~ "continuous",AGED_count~"continuous"),label = list(year~"Year",household_size~"Household size",DR~"Dependency ratio",GR~"Gender ratio",AA~"Number of active adults",CHN_count~"Number of Children",AGED_count~"Number of adults aged65+",age~"Age of household head",gender~"Gender of the household head",HVA~"High value assests",DA~"Domestic assets",OFER~"Local off farm employment",MER~"Migrant employment",gov_grant_value~"Total value of social grants earned",NRU~"Natural resources use score",NRC~"Natural resources collection score",LBL~"Crop and/or livestock farming",hh_field_number~"Number of external fields",LIVESTOCK~"Livestock ownership",crop_plant_last_year~"Crop Production"))%>%add_stat_label()%>%add_stat_label()%>%add_p()%>%as_flex_table()
A1

save_as_docx(A1,path="summaryA23.docx")


AB1<-A%>%dplyr::select(gov_grant_value,household_size,DR,GR,DA,age,AA,HVA,AGED_count,CHN_count,gender,hh_field_number,NRU,crop_plant_last_year,LIVESTOCK,year,FID,QUE,villagename)
View(AB1)
attach(AB1)



F1<-A%>%dplyr::select(gov_grant_value,household_size,DR,GR,DA,age,AA,HVA,AGED_count,CHN_count,gender,hh_field_number,NRU,crop_plant_last_year,LIVESTOCK,year,FID,QUE,villagename)
View(F1)
attach(F1)

F1.1<-A%>%dplyr::select(household_size,DR,GR,DA,age,HVA,gender,crop_plant_last_year)
View(F1.1)
attach(F1.1)

F2<-A%>%dplyr::select(gov_grant_value,hh_field_number,NRU,NRC,LIVESTOCK,MER,OFER,LBL,year,FID,QUE,villagename)
View(F2)
attach(F2)

F3<-A%>%dplyr::select(year,FID,QUE)
View(F3)
attach(F3)

A$gov_grant_value<-scale(A$gov_grant_value)
A$NRU<-scale(A$NRU)
A$NRC<-scale(A$NRC)
A$DA<-scale(A$DA)
A$age<-scale(A$age)

F4<-A%>%dplyr::select(gov_grant_value,NRC,MER,OFER,LBL)
F4$LBL<-as.numeric(F4$LBL)
View(F4)
attach(F4)
F1c<-F4

Data1<-F1c
set.seed(123)
Data1<-as.data.frame.matrix(Data1)
View(Data1)

pca_result <- prcomp(Data1, center = TRUE, scale = TRUE)
summary(pca_result)
#screeplot(pca_result,type="l")
print(pca_result$rotation)
eigenvectors<-pca_result$loadings
eigenvalues<-pca_result$sdev*pca_result$sdev
eigenvalues

pca_result <- principal(Data1, nfactors = 3, rotate = "varimax")
pca_result



fit.varimax<-principal(Data1,nfactors=3,rotate="varimax")
fit.varimax
loadings <- fit.varimax$loadings
loadings1<-loadings(fit.varimax)
loadings1
component_scores <- as.matrix(Data1) %*% loadings
normalized_scores <- scale(component_scores)
composite_data <- data.frame(RC1 = normalized_scores[, 1], RC2 = normalized_scores[, 2], RC3 = normalized_scores[, 3])
View(composite_data)
standardized_data <- scale(composite_data)

fviz_nbclust(standardized_data,kmeans,method="wss")+labs(subtitle="Elbow method")
fviz_nbclust(standardized_data,kmeans,method="silhouette")+labs(subtitle="Sil method")
set.seed(200)
gap_stat<-clusGap(standardized_data,FUN=kmeans,nstart=25,K.max=10,B=50)
print(gap_stat,method="firstmax")
fviz_gap_stat(gap_stat)

set.seed(20)
data.matrix(standardized_data)
TIPOC3c<-(na.omit(data.matrix(standardized_data)))
Tipo3c<-dist(standardized_data,method="euclidean")
hclust(Tipo3c,method="ward.D")
tipo3C<-hclust(Tipo3c,method="ward.D")
plot(tipo3C)
rect.hclust(tipo3C,3)
kc<-kmeans(standardized_data,3)
kc
kc$centers
write.csv(kc$centers,file="clusterpca4.csv")
combined_data<-cbind(F2,composite_data)
View(combined_data)
dataset<-cbind(combined_data,kc$cluster) 
View(dataset)
dataset$cluster<-kc$cluster
View(dataset)


dataset1<-dataset%>%dplyr::select(gov_grant_value,NRC,LBL,MER,OFER,cluster)
View(dataset1)
attach(dataset1)

View(dataset1)
dataset1$LBL<-as.numeric(dataset1$LBL)
cluster_summary <- dataset1 %>%group_by(cluster) %>%summarise(Mean_Variable1 = mean(LBL),SD_Variable1 = sd(LBL),Mean_Variable2 = mean(gov_grant_value),SD_Variable2 = sd(gov_grant_value),Mean_Variable3= mean(OFER),SD_Variable3 = sd(OFER),Mean_Variable4 = mean(NRC),SD_Variable4 = sd(NRC),Mean_Variable5 = mean(MER),SD_Variable5 = sd(MER),Count = n())
cluster_summary
print(cluster_summary)
write.csv(cluster_summary,file="clustersummary.csv")


F5<-cbind(F2,dataset)
View(F5)



View(F5)

kruskal_result <- kruskal.test(gov_grant_value~cluster, data = dataset1)
print(kruskal_result)
posthoc_dunn <- dunn.test(dataset1$gov_grant_value, g = dataset1$cluster, method = "bonferroni")
print(posthoc_dunn)

kruskal_result1 <- kruskal.test(NRC~cluster, data = dataset1)
print(kruskal_result1)
posthoc_dunn1 <- dunn.test(dataset1$NRC, g = dataset1$cluster, method = "bonferroni")
print(posthoc_dunn1)

kruskal_result2 <- kruskal.test(LBL~cluster, data = dataset1)
print(kruskal_result2)
posthoc_dunn2 <- dunn.test(dataset1$LBL, g = dataset1$cluster, method = "bonferroni")
print(posthoc_dunn2)

kruskal_result3 <- kruskal.test(MER~cluster, data = dataset1)
print(kruskal_result3)
posthoc_dunn3 <- dunn.test(dataset1$MER, g = dataset1$cluster, method = "bonferroni")
print(posthoc_dunn3)

kruskal_result4 <- kruskal.test(OFER~cluster, data = dataset1)
print(kruskal_result4)
posthoc_dunn4 <- dunn.test(dataset1$OFER, g = dataset1$cluster, method = "bonferroni")
print(posthoc_dunn4)

Y6<- cbind(F5,F1.1)
View(Y6)

colnames(Y6)

# Check column names in Y6
colnames(Y6)

# Identify duplicated column names
duplicated_columns <- colnames(Y6)[duplicated(colnames(Y6))]
print("Duplicated columns in Y6:")
print(duplicated_columns)

# Rename duplicated columns in Y6 by appending a suffix
colnames(Y6)[duplicated(colnames(Y6))] <- paste0(duplicated_columns, "_dup")

# Check updated column names
print("Updated column names in Y6 after renaming duplicates:")
print(colnames(Y6))



F7<-Y6%>%dplyr::select(gov_grant_value,NRU,NRC,LBL,MER,OFER,cluster,household_size,DR,GR,DA,age,HVA,gender,crop_plant_last_year,LIVESTOCK,year,FID,QUE)
View(F7)


F7$cluster<-as.factor(F7$cluster)



wide_data<- pivot_wider(data=F7,id_cols = QUE,names_from = year,values_from = cluster)
View(wide_data)

WD1<-wide_data%>%dplyr::select(QUE,'2010','2014','2021')

View(WD1)

WD11<-wide_data%>%dplyr::select(QUE,'2010')
View(WD11)
WD11$'2010'[WD11$'2010'=="NULL"]<-"NA"


WD11$'2010'<-as.character(WD11$'2010')
WD11$'2010'<-as.factor(WD11$'2010')


levels(WD11$'2010')
WD11$'2010' <- ifelse(WD11$'2010' == "NA", NA, WD11$'2010')

WD11<-WD11[!is.na(WD11$'2010'), ]

View(WD11)

factor_counts <- table(WD11$'2010')
factor_counts

factor_percentages <- prop.table(factor_counts/589) * 100
factor_percentages






WD2<-wide_data%>%dplyr::select(QUE,'2010','2014')

View(WD2)
WD2$'2010'[WD2$'2010'=="NULL"]<-"NA"
WD2$'2014'[WD2$'2014'=="NULL"]<-"NA"

WD2$'2010'<-as.character(WD2$'2010')
WD2$'2010'<-as.factor(WD2$'2010')
WD2$'2014'<-as.character(WD2$'2014')
WD2$'2014'<-as.factor(WD2$'2014')

levels(WD2$'2010')
WD2$'2010' <- ifelse(WD2$'2010' == "NA", NA, WD2$'2010')
WD2$'2014' <- ifelse(WD2$'2014' == "NA", NA, WD2$'2014')
WD2<-WD2[!is.na(WD2$'2010'), ]
WD2<-WD2[!is.na(WD2$'2014'), ]
View(WD2)

transition_counts <- table(WD2$'2010', WD2$'2014')

transition_counts
row_totals <- rowSums(transition_counts)
col_totals <- colSums(transition_counts)

row_totals
col_totals

transition_percentages <- prop.table(row_totals/520) * 100
transition_percentages

transition_percentages <- prop.table(col_totals/520) * 100
transition_percentages


# Create transition count table
transition_counts <- matrix(c(12, 19, 15, 17, 120, 34, 30, 63, 144), 
                            nrow = 3, byrow = TRUE, 
                            dimnames = list('2010' = c(1, 2, 3), 
                                            '2014' = c(1, 2, 3)))

# Convert to data frame
transition_counts_df <- as.data.frame(as.table(transition_counts))
colnames(transition_counts_df) <- c("From_2010", "To_2014", "Count")

# Calculate the overall total of all transitions
total_transitions <- sum(transition_counts_df$Count)

# Convert each cell to a percentage of the overall total
transition_counts_df$Percentage <- (transition_counts_df$Count / total_transitions) * 100

transition_counts_df$From_2010 <- factor(transition_counts_df$From_2010, 
                                         levels = c(1, 2, 3), 
                                         labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))


# Specify factor levels and labels for To_2014
unique_values <- unique(transition_counts_df$To_2014)
transition_counts_df$To_2014 <- factor(transition_counts_df$To_2014, 
                                       levels = unique_values, 
                                       labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))

# Print the data frame to verify
print(transition_counts_df)

# Create the bar plot with percentage labels
library(ggplot2)
B1<-ggplot(transition_counts_df, aes(x = To_2014, y = Percentage, fill = factor(From_2010))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Transition of Households from 2010 to 2014",
       x = "To 2014",
       y = "Percentage of Total",
       fill = "From 2010") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()

B1



WD8<-wide_data%>%dplyr::select(QUE,'2010','2019')

View(WD8)
WD8$'2010'[WD8$'2010'=="NULL"]<-"NA"
WD8$'2019'[WD8$'2019'=="NULL"]<-"NA"

WD8$'2010'<-as.character(WD8$'2010')
WD8$'2010'<-as.factor(WD8$'2010')
WD8$'2019'<-as.character(WD8$'2019')
WD8$'2019'<-as.factor(WD8$'2019')

levels(WD2$'2010')
WD8$'2010' <- ifelse(WD8$'2010' == "NA", NA, WD8$'2010')
WD8$'2019' <- ifelse(WD8$'2019' == "NA", NA, WD8$'2019')
WD8<-WD8[!is.na(WD8$'2010'), ]
WD8<-WD8[!is.na(WD8$'2019'), ]
View(WD8)

transition_counts <- table(WD8$'2010', WD8$'2019')

transition_counts
row_totals <- rowSums(transition_counts)
col_totals <- colSums(transition_counts)

row_totals
col_totals

transition_percentages <- prop.table(row_totals/504) * 100
transition_percentages

transition_percentages <- prop.table(col_totals/504) * 100
transition_percentages


transition_counts <- matrix(c(30, 98, 127, 4, 41, 10, 18, 44, 132), 
                            nrow = 3, byrow = TRUE, 
                            dimnames = list('2010' = c(1, 2, 3), 
                                            '2019' = c(1, 2, 3)))

# Convert to data frame
transition_counts_df <- as.data.frame(as.table(transition_counts))
colnames(transition_counts_df) <- c("From_2010", "To_2019", "Count")

# Calculate the overall total of all transitions
total_transitions <- sum(transition_counts_df$Count)

# Convert each cell to a percentage of the overall total
transition_counts_df$Percentage <- (transition_counts_df$Count / total_transitions) * 100



transition_counts_df$From_2010 <- factor(transition_counts_df$From_2010, 
                                         levels = c(1, 2, 3), 
                                         labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))


# Specify factor levels and labels for To_2019
unique_values <- unique(transition_counts_df$To_2019)
transition_counts_df$To_2019 <- factor(transition_counts_df$To_2019, 
                                       levels = unique_values, 
                                       labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))

# Print the data frame to verify
print(transition_counts_df)

# Create the bar plot with percentage labels

B2<-ggplot(transition_counts_df, aes(x = To_2019, y = Percentage, fill = factor(From_2010))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Transition of Households from 2010 to 2019",
       x = "To 2019",
       y = "Percentage of Total",
       fill = "From 2010") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()
B2



WD9<-wide_data%>%dplyr::select(QUE,'2014','2019')



View(WD9)
View(WD9)


WD9$'2014'[WD9$'2014'=="NULL"]<-"NA"
WD9$'2019'[WD9$'2019'=="NULL"]<-"NA"

WD9$'2014'<-as.character(WD9$'2014')
WD9$'2014'<-as.factor(WD9$'2014')
WD9$'2019'<-as.character(WD9$'2019')
WD9$'2019'<-as.factor(WD9$'2019')

levels(WD9$'2014')
WD9$'2014' <- ifelse(WD9$'2014' == "NA", NA, WD9$'2014')
WD9$'2019' <- ifelse(WD9$'2019' == "NA", NA, WD9$'2019')
WD9<-WD9[!is.na(WD9$'2014'), ]
WD9<-WD9[!is.na(WD9$'2019'), ]
View(WD9)



transition_counts <- table(WD9$'2014', WD9$'2019')

transition_counts

transition_counts
row_totals <- rowSums(transition_counts)
col_totals <- colSums(transition_counts)

row_totals
col_totals

transition_percentages <- prop.table(row_totals/473) * 100
transition_percentages

transition_percentages <- prop.table(col_totals/473) * 100
transition_percentages


transition_percentages <- prop.table(transition_counts/473) * 100
transition_percentages


# Create transition count table for 2014 to 2019
transition_counts <- matrix(c(19, 52, 80, 12, 80, 28, 17, 47, 138), 
                            nrow = 3, byrow = TRUE, 
                            dimnames = list('2014' = c(1, 2, 3), 
                                            '2019' = c(1, 2, 3)))

# Convert to data frame
transition_counts_df <- as.data.frame(as.table(transition_counts))
colnames(transition_counts_df) <- c("From_2014", "To_2019", "Count")

# Calculate the overall total of all transitions
total_transitions <- sum(transition_counts_df$Count)

# Convert each cell to a percentage of the overall total
transition_counts_df$Percentage <- (transition_counts_df$Count / total_transitions) * 100


transition_counts_df$From_2014 <- factor(transition_counts_df$From_2014, 
                                         levels = c(1, 2, 3), 
                                         labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))


# Specify factor levels and labels for To_2019
unique_values <- unique(transition_counts_df$To_2019)
transition_counts_df$To_2019 <- factor(transition_counts_df$To_2019, 
                                       levels = unique_values, 
                                       labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))

# Print the data frame to verify
print(transition_counts_df)

# Create the bar plot with percentage labels

B3<-ggplot(transition_counts_df, aes(x = To_2019, y = Percentage, fill = factor(From_2014))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Transition of Households from 2014 to 2019",
       x = "To 2019",
       y = "Percentage of Total",
       fill = "From 2014") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()

B3





WD4<-wide_data%>%dplyr::select(QUE,'2014','2021')

View(WD4)
View(WD4)
WD4$'2014'[WD4$'2014'=="NULL"]<-"NA"
WD4$'2021'[WD4$'2021'=="NULL"]<-"NA"

WD4$'2014'<-as.character(WD4$'2014')
WD4$'2014'<-as.factor(WD4$'2014')
WD4$'2021'<-as.character(WD4$'2021')
WD4$'2021'<-as.factor(WD4$'2021')

levels(WD4$'2014')
WD4$'2014' <- ifelse(WD4$'2014' == "NA", NA, WD4$'2014')
WD4$'2021' <- ifelse(WD4$'2021' == "NA", NA, WD4$'2021')
WD4<-WD4[!is.na(WD4$'2014'), ]
WD4<-WD4[!is.na(WD4$'2021'), ]
View(WD4)

transition_counts <- table(WD4$'2014', WD4$'2021')

transition_counts

row_totals <- rowSums(transition_counts)
col_totals <- colSums(transition_counts)

row_totals
col_totals

transition_percentages <- prop.table(row_totals/454) * 100
transition_percentages

transition_percentages <- prop.table(col_totals/454) * 100
transition_percentages

transition_percentages <- prop.table(transition_counts/454) * 100
transition_percentages

# Create transition count table for 2014 to 2021
transition_counts <- matrix(c(29, 65, 58, 8, 76, 27, 21, 65, 105), 
                            nrow = 3, byrow = TRUE, 
                            dimnames = list('2014' = c(1, 2, 3), 
                                            '2021' = c(1, 2, 3)))

# Convert to data frame
transition_counts_df <- as.data.frame(as.table(transition_counts))
colnames(transition_counts_df) <- c("From_2014", "To_2021", "Count")

# Calculate the overall total of all transitions
total_transitions <- sum(transition_counts_df$Count)

# Convert each cell to a percentage of the overall total
transition_counts_df$Percentage <- (transition_counts_df$Count / total_transitions) * 100



transition_counts_df$From_2014 <- factor(transition_counts_df$From_2014, 
                                         levels = c(1, 2, 3), 
                                         labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))


# Specify factor levels and labels for To_2021
unique_values <- unique(transition_counts_df$To_2021)
transition_counts_df$To_2021 <- factor(transition_counts_df$To_2021, 
                                       levels = unique_values, 
                                       labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))

# Print the data frame to verify
print(transition_counts_df)

# Create the bar plot with percentage labels

B4<-ggplot(transition_counts_df, aes(x = To_2021, y = Percentage, fill = factor(From_2014))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Transition of Households from 2014 to 2021",
       x = "To 2021",
       y = "Percentage of Total",
       fill = "From 2014") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()
B4

WD111<-wide_data%>%dplyr::select(QUE,'2010','2021')

View(WD111)
WD111$'2010'[WD111$'2010'=="NULL"]<-"NA"
WD111$'2021'[WD111$'2021'=="NULL"]<-"NA"

WD111$'2010'<-as.character(WD111$'2010')
WD111$'2010'<-as.factor(WD111$'2010')
WD111$'2021'<-as.character(WD111$'2021')
WD111$'2021'<-as.factor(WD111$'2021')

levels(WD111$'2010')
WD111$'2010' <- ifelse(WD111$'2010' == "NA", NA, WD111$'2010')
WD111$'2021' <- ifelse(WD111$'2021' == "NA", NA, WD111$'2021')
WD111<-WD111[!is.na(WD111$'2010'), ]
WD111<-WD111[!is.na(WD111$'2021'), ]
View(WD111)

transition_counts <- table(WD111$'2010', WD111$'2021')


transition_counts
row_totals <- rowSums(transition_counts)
col_totals <- colSums(transition_counts)

row_totals
col_totals

transition_percentages <- prop.table(row_totals/489) * 100
transition_percentages

transition_percentages <- prop.table(col_totals/489) * 100
transition_percentages

# Create transition count table for 2010 to 2021
transition_counts <- matrix(c(46, 106, 95, 2, 35, 14, 13, 67, 111), 
                            nrow = 3, byrow = TRUE, 
                            dimnames = list('2010' = c(1, 2, 3), 
                                            '2021' = c(1, 2, 3)))

# Convert to data frame
transition_counts_df <- as.data.frame(as.table(transition_counts))
colnames(transition_counts_df) <- c("From_2010", "To_2021", "Count")

# Calculate the overall total of all transitions
total_transitions <- sum(transition_counts_df$Count)

# Convert each cell to a percentage of the overall total
transition_counts_df$Percentage <- (transition_counts_df$Count / total_transitions) * 100

transition_counts_df$From_2010 <- factor(transition_counts_df$From_2010, 
                                         levels = c(1, 2, 3), 
                                         labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))


# Specify factor levels and labels for To_2021
unique_values <- unique(transition_counts_df$To_2021)
transition_counts_df$To_2021 <- factor(transition_counts_df$To_2021, 
                                       levels = unique_values, 
                                       labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))

# Print the data frame to verify
print(transition_counts_df)

# Create the bar plot with percentage labels
library(ggplot2)
B5<-ggplot(transition_counts_df, aes(x = To_2021, y = Percentage, fill = factor(From_2010))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Transition of Households from 2010 to 2021",
       x = "To 2021",
       y = "Percentage of Total",
       fill = "From 2010") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic()
B5


WD112<-wide_data%>%dplyr::select(QUE,'2019','2021')

View(WD112)
WD112$'2019'[WD112$'2019'=="NULL"]<-"NA"
WD112$'2021'[WD112$'2021'=="NULL"]<-"NA"

WD112$'2019'<-as.character(WD112$'2019')
WD112$'2019'<-as.factor(WD112$'2019')
WD112$'2021'<-as.character(WD112$'2021')
WD112$'2021'<-as.factor(WD112$'2021')

levels(WD112$'2019')
WD112$'2019' <- ifelse(WD112$'2019' == "NA", NA, WD112$'2019')
WD112$'2021' <- ifelse(WD112$'2021' == "NA", NA, WD112$'2021')
WD112<-WD112[!is.na(WD112$'2019'), ]
WD112<-WD112[!is.na(WD112$'2021'), ]
View(WD112)

transition_counts <- table(WD112$'2019', WD112$'2021')

transition_counts
row_totals <- rowSums(transition_counts)
col_totals <- colSums(transition_counts)

row_totals
col_totals

transition_percentages <- prop.table(row_totals/454) * 100
transition_percentages

transition_percentages <- prop.table(col_totals/454) * 100
transition_percentages


# Transition count table
transition_counts <- matrix(c(12, 19, 15, 17, 120, 34, 30, 63, 144), 
                            nrow = 3, byrow = TRUE, 
                            dimnames = list('2019' = c(1, 2, 3), 
                                            '2021' = c(1, 2, 3)))

# Convert the table to a data frame
transition_counts_df <- as.data.frame(as.table(transition_counts))
colnames(transition_counts_df) <- c("From_2019", "To_2021", "Count")

# Calculate the overall total of all transitions
total_transitions <- sum(transition_counts_df$Count)

# Convert each cell to a percentage of the overall total
transition_counts_df$Percentage <- (transition_counts_df$Count / total_transitions) * 100

# Rename the categories
transition_counts_df$From_2019 <- factor(transition_counts_df$From_2019, 
                                         levels = c(1, 2, 3), 
                                         labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))
transition_counts_df$To_2021 <- factor(transition_counts_df$To_2021, 
                                       levels = c(1, 2, 3), 
                                       labels = c("Diversified", "Social grant-dependent", "Migrant-labour_dependent"))


# Print the data frame with percentages and new labels
print(transition_counts_df)

# Create the bar plot with percentage labels
library(ggplot2)
B6<-ggplot(transition_counts_df, aes(x = To_2021, y = Percentage, fill = From_2019)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "Transition of Households from 2019 to 2021",
       x = "To 2021",
       y = "Percentage of Total",
       fill = "From 2019") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme_classic()
B6

T66<-grid.arrange(B1,B2,B5,B3,B4,B6, nrow = 3, ncol = 2)
T66
ggsave("T66.png",T66,width=40,height=25,units=c("cm"))


AD1<-Y6%>%dplyr::select(year,FID,QUE,villagename,cluster)
View(AD1)
print(AD1)
AD1$cluster<-as.character(AD1$cluster)
AD1$cluster[AD1$cluster=="1"]<-"Diversified"
AD1$cluster[AD1$cluster=="2"]<-"Social Grants Dependent"
AD1$cluster[AD1$cluster=="3"]<-"Migrant Dependent"
AD1$cluster<-as.factor(AD1$cluster)

AD1$villagename<-as.character(AD1$villagename)
AD1$villagename[AD1$villagename=="agincourt"]<-"Agincourt"
AD1$villagename[AD1$villagename=="cunningmore b"]<-"Cunningmore B"
AD1$villagename[AD1$villagename=="huntington"]<-"Huntington"
AD1$villagename[AD1$villagename=="ireagh a"]<-"Ireagh A"
AD1$villagename[AD1$villagename=="justicia a"]<-"Justicia A"
AD1$villagename[AD1$villagename=="lillydale b"]<-"Lillydale B"
AD1$villagename[AD1$villagename=="xanthia"]<-"Xanthia"
AD1$villagename[AD1$villagename=="ireagh b"]<-"Ireagh B"
AD1$villagename[AD1$villagename=="justicia b"]<-"Justicia B"

AD1$villagename[AD1$villagename=="kildare b"]<-"Kildare B"
AD1$villagename[AD1$villagename=="kildare a"]<-"Kildare A"

AD1$villagename[AD1$villagename=="kildare c"]<-"Kildare C"
AD1$villagename<-as.factor(AD1$villagename)


AD1 <- AD1 %>%mutate(cluster_name = case_when(cluster == 1 ~ "Diversified",cluster == 2 ~ "Social Grants Dependent",cluster == 3 ~ "Migrant Dependent",TRUE ~ as.character(cluster)))

levels(AD1$villagename)



fun_color_range=colorRampPalette(c("#0000FF","#00FF00","#000000"))
myColors=fun_color_range(3)



# Calculate the percentage of each cluster within each year
cluster_distribution_percent <- AD1 %>%group_by(year, cluster) %>%summarise(count = n(), .groups = 'drop') %>%group_by(year) %>% mutate(total = sum(count)) %>%mutate(percentage = (count / total) * 100)
fun_color_range=colorRampPalette(c("#0000FF","#00FF00","#000000"))
myColors=fun_color_range(3)



# Plot the distribution of clusters by year as percentages
T1<-ggplot(cluster_distribution_percent, aes(x = year, y = percentage, fill = as.factor(cluster)))+geom_bar(stat = "identity", position = "dodge")+geom_smooth(aes(group = cluster,colour = as.factor(cluster)), method = "lm", se = FALSE)+labs(title = "Distribution of livelihood classes by Year (Percentage)",x = "Year",y = "Percentage of Households",fill = "Cluster")+theme_minimal()+scale_y_continuous(labels = scales::percent_format(scale = 1)) +scale_fill_manual(values=myColors,name="Livelihood class") +scale_color_manual(values = myColors, name = "Livelihood class")+theme_classic()         
T1
ggsave("T1.tiff",width=40,height=25,units=c("cm"),dpi=1200,compression="lzw")





fun_color_range=colorRampPalette(c("#0000FF","#00FF00","#000000"))

myColors=fun_color_range(3)





# Calculate the percentage of each cluster within each year and village
cluster_distribution_village_percent <- AD1 %>%group_by(villagename, year, cluster) %>%summarise(count = n(), .groups = 'drop') %>%group_by(villagename, year) %>%mutate(total = sum(count)) %>%mutate(percentage = (count / total) * 100)

# Plot the distribution of clusters by year and village as percentages
T2<-ggplot(cluster_distribution_village_percent, aes(x = year, y = percentage, fill = as.factor(cluster))) +geom_bar(stat = "identity", position = "dodge") +geom_smooth(aes(group = cluster, color = as.factor(cluster)), method = "lm", se = FALSE) + facet_wrap(~ villagename, scales = "free_y") +labs(title = "Distribution of livelihood classes by year and Village (Percentage)",x = "Year",y = "Percentage of Households",fill = "cluster") +scale_y_continuous(labels = percent_format(scale = 1)) +scale_fill_manual(values = myColors, name = "Livelihood class") +scale_color_manual(values = myColors, name = "Livelihood class") + theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(strip.text.x = element_text(size = 8)) +theme(legend.position = "bottom")+ theme_classic()
T2
ggsave("T2.tiff",width=40,height=25,units=c("cm"),dpi=1200,compression="lzw")

F8<- cbind(F7,A21)
View(F8)




F8$cluster<-as.factor(F8$cluster)
View(F8)


model1 <- multinom(cluster ~ gender+household_size+GR+age+GR+AGED_count+CHN_count+AA+NRU+DA+HVA, data = F8)
print(model1)
summary(model1)
tidy(model1)|>mutate_if(is.numeric,round,2)|> print(n = 30)
glance(model1)


pred.probs <-  predict(model1,type="probs")
class1 <- if_else(F8$cluster=="1",1,0) 
resid.class1 <- class1 - pred.probs[,1]


class2 <- if_else(F8$cluster=="2",1,0) 
resid.class2 <- class2 - pred.probs[,2]


class3 <- if_else(F8$cluster=="3",1,0) 
resid.class3 <- class3 - pred.probs[,3]



odds_ratios <- exp(coef(model1))
odds_ratios
model_summary <- summary(model1)
z<-summary(model1)$coefficients/summary(model1)$standard.errors
p<-(1-pnorm(abs(z),0,1))*2
p











levels(F8$cluster)
F8_news <- F8
F8_news$cluster <- relevel(F8_new$cluster, ref = "3")
model_new <- multinom(cluster ~household_size + DR + GR + age + GR+AGED_count+CHN_count+AA+NRU+DA+HVA, data = F8_news)
model_new
model_summary_new <- summary(model_new)
model_summary_new
coefficients_and_p_values_new <- model_summary_new$coefficients
coefficients_and_p_values_new
z<-summary(model_new)$coefficients/summary(model_new)$standard.errors
p<-(1-pnorm(abs(z),0,1))*2
p

model_summary <- tidy(model1)
p_values <- model_summary$p.value
coefficients_and_p_values <- data.frame(
  Coefficient = model_summary$term,
  P_Value = p_values
)
print(coefficients_and_p_values)
significant_coefficients <- coefficients_and_p_values[coefficients_and_p_values$P_Value < 0.05, ]
print(significant_coefficients)

