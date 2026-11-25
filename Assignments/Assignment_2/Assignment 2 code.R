read.csv("C:/Users/dftat/Dropbox/PC/Downloads/data_course_HAZEL/Valhalla's_CogniTrackX_-_TBI_an2026-final.csv")
read.csv("Valhalla's_CogniTrackX_-_TBI_an2026-final.csv")
csv_files="csv files"
Data_Course_HAZEL/Data/wingspan_vs_mass.csv
## Reading all the data in the wingspan file through a relative path ####
read.csv('Data/wingspan_vs_mass.csv')
df<-read.csv('Data/wingspan_vs_mass.csv')
head(df,5)
##Finding all birds that begin with "b" and listing them####
b_files<-list.files("Data/",pattern = "^b",recursive = TRUE, full.names = TRUE)
for (i in b_files) {
  print(i)
}
## Finding and listing all data that ends with .csv ####
csv<- list.files("Data/",pattern = ".csv$",recursive = T, full.names = T)
for (i in csv) {
  print(i)
}