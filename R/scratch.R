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
filter(ogp, DOY == 250)
test=readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz")
#head(test)
#unique(test$variable)
lakedat=readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-expanded-observations.csv.gz")
lakedat = lakedat[complete.cases(lakedat), ]

test_UTC_split = strsplit(as.character(lakedat$datetime), " ")

UTC_df = as.data.frame(do.call("rbind", test_UTC_split))
names(UTC_df) = c("Date", "Time")
idx = which(UTC_df$Time == "00:00:00")
lakedat = lakedat[idx, ]

lakedat1$depth = NULL
lakedat1 = lakedat1[complete.cases(lakedat1), ]
remove(x)

temp = lakedat1
temp$DOY = as.integer(format(lakedat1$datetime, "%j"))
aves1 = temp %>% group_by(DOY) %>% summarise(meantemp = mean(observation, na.rm=TRUE), mysd = sd(observation))
head(aves1)
temp = lakedat1[lakedat1$site_id == "SUGG", ]
temp$DOY = as.integer(format(temp$datetime, "%j"))
avesdf = aggregate(observation ~ DOY, data = temp,
                   FUN = function(x){
                     Mean=mean(x, na.rm = TRUE)
                     SD = sd(x)
                     return(c(Mean, SD))
                   }
)

avesdf <- do.call("data.frame", avesdf)
colnames(avesdf) = c("DOY", "Mean", "SD")
avesdf = avesdf[avesdf$DOY %in% 245:280, ]
# get 90% prediction intervals
avesdf$Upper = avesdf$Mean + 1.645*avesdf$SD
avesdf$Lower = avesdf$Mean - 1.645*avesdf$SD
plot(avesdf$DOY, avesdf$Mean, type = "l", ylab = "Mean", ylim = c(22, 32))
lines(avesdf$DOY, avesdf$Upper, lty = 2)
lines(avesdf$DOY, avesdf$Lower, lty = 2)

#################################################################################################################

# use only DOY as covariate
str(lakedat1)
library(hetGP)

model1 = fit_hetgp(X = "DOY", Y = "temperature",
                   site_id = "SUGG", df = lakedat1)

# works
# do not save covmat
preds1 = predict_hetgp(het_gp_object = model1, reference_datetime = "2023-09-01")

preds2 = predict_hetgp(het_gp_object = model1, reference_date = "2023-09-01", save_covmat = TRUE)

lakedat2 = filter(lakedat, depth %in% 1:2)
lakedat2 = lakedat2[complete.cases(lakedat2), ]
# don't run it will take like an hour
modeld = fit_hetgp(X = c("DOY","depth"), Y = "temperature",
                   site_id = "BARC", df = lakedat2)


