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
obs_data = read_obs_data()
obs_data = filter(obs_data, date > as.Date("2021-01-01"))
head(obs_data)
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
write.csv(obs_data, "data/withDepth.csv")
head(df1)
head(df2)
head(obs_data)
df1$X.1=NULL
df1$X = NULL
df2$X=NULL
head(df2)
obs_data$datetime = NULL
str(obs_data)
#################################################################################################################
sample_lake_data_1mdepth = read.csv("data/sample_lake_data_1mdepth.csv")
sample_lake_data_1mdepth$X=NULL
sample_lake_data_1mdepth$X.1=NULL

head(sample_lake_data_1mdepth)

# use only DOY as covariate
model1 = fit_hetgp(X = "DOY", Y = "temperature",
                   site_id = "FCR", df = sample_lake_data_1mdepth)
# won't work
fit_hetgp(X = "DOY", Y = "blach",
          site_id = "FCR", df = sample_lake_data_1mdepth)

fit_hetgp(X = "D", Y = "temperature",
          site_id = "FCR", df = sample_lake_data_1mdepth)

fit_hetgp(X = "DOY", Y = "temperature",
          site_id = "F", df = sample_lake_data_1mdepth)

# works
# do not save covmat
preds1 = predict_hetgp(het_gp_object = model1, reference_date = "2023-09-01")
head(preds1$pred_df)

preds2 = predict_hetgp(het_gp_object = model1, reference_date = "2023-09-01", save_covmat = TRUE)
preds2$covmat[1:10,1:10]
meandf = preds2$pred_df
meandf = meandf[meandf$parameter == "mu", ]
plot(meandf$datetime, meandf$prediction, type = "l")

# does not work
preds = predict_hetgp(het_gp_object = model1, reference_date = "2023-9-1")


# DOY / Depth
df2 = read.csv("data/withDepth.csv")
df2$X=NULL
# don't run it will take like an hour
modeld = fit_hetgp(X = c("DOY","depth"), Y = "temperature",
                   site_id = "FCR", df = df2)

# warning
temp=fit_hetgp(X = c("DOY"), Y = "temperature",
          site_id = "FCR", df = df2)

# won't work
fit_hetgp(X = c("DOY","d"), Y = "temperature",
          site_id = "FCR", df = df2)

# will work
predict_hetgp(modeld,
              reference_date = "2023-09-01",
              depths = 1:10) # depths 1-10 are default

predict_hetgp(modeld,
              reference_date = "2023-09-01",
              depths = c(1,3,5)) # use whatever depths you want

# won't work
predict_hetgp(modeld,
              reference_date = "2023-09-01",
              depths = c(-1, -2))

