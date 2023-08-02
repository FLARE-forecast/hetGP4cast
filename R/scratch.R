library(devtools)
df = data.table::fread("C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Important_code/aquatics-2023-03-20-xgboost_parallel.csv.gz")
unique(df$datetime)
head(df)
df = df[, c("datetime",   "site_id" ,"variable", "prediction")]
colnames(df)[4] = "observation"
df = df[!duplicated(df$datetime), ]
df2 = df
df2$site_id = "FCR"
df = rbind(df, df2)
sample_lake_data = df
df = read.csv("C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Important_code/datforpackage.csv")
df1 = dplyr::filter(df, Depth == 1)
sample_lake_data_withDepth = df
sample_lake_data_1mdepth = df1
head(sample_lake_data_1mdepth)
sample_lake_data_1mdepth$Depth = NULL
colnames(sample_lake_data_withDepth)[1] = "depth"
usethis::use_data(sample_lake_data_withDepth, overwrite = TRUE)
usethis::use_data(sample_lake_data_1mdepth, overwrite = TRUE)
plot(df2$datetime, df2$observation)
str(df)
write.csv(sample_lake_data_1mdepth,
          "C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Important_code/TEST/data/sample_lake_data_1mdepth.csv")
library(hetGP)
dat = read.csv("sample_lake_data_1mdepth.csv")
dat$X.1=NULL
dat$X = NULL
head(obs_data)
class(obs_data$date)
obs_data$datetime = NULL
obs_data$datetime = obs_data$date
obs_data$YEAR = NULL
obs_data$MONTH = NULL
obs_data$DAY = NULL
colnames(obs_data)[1] = "depth"
obs_data$DOY  = NULL
obs_data$site_id = "FCR"
obs_data$variable = "temperature"
colnames(obs_data)[3] = "datetime"
colnames(obs_data)[2] = "observation"
write.csv(obs_data, "data/sample_lake_data_withDepth.csv")
df1 = read.csv("data/sample_lake_data_1mdepth.csv")
df2 = read.csv("data/sample_lake_data_withDepth.csv")
head(df1)
head(df2)
df1$X.1=NULL
df1$X = NULL
df2$X=NULL
head(df2)
