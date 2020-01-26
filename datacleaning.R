data <- read.csv('MODIS_data.csv')

library(ggpubr)

data$ParticulateMicrocystin <-  replace(data$ParticulateMicrocystin, is.na(data$ParticulateMicrocystin), mean(data$ParticulateMicrocystin, na.rm = TRUE))
data$DissolvedMicrocystin <-  replace(data$DissolvedMicrocystin, is.na(data$DissolvedMicrocystin), mean(data$DissolvedMicrocystin, na.rm = TRUE))
data$CTDBeamAttenuation <-  replace(data$CTDBeamAttenuation, is.na(data$CTDBeamAttenuation), mean(data$CTDBeamAttenuation, na.rm = TRUE))
data$StationDepth <-  replace(data$StationDepth, is.na(data$StationDepth), mean(data$StationDepth, na.rm = TRUE))
data$SampleDepth <-  replace(data$SampleDepth, is.na(data$SampleDepth), mean(data$SampleDepth, na.rm = TRUE))
data$WaveHeight <-  replace(data$WaveHeight, is.na(data$WaveHeight), mean(data$WaveHeight, na.rm = TRUE))
data$SecchiDepth <-  replace(data$SecchiDepth, is.na(data$SecchiDepth), mean(data$SecchiDepth, na.rm = TRUE))
data$CTDTemperature <-  replace(data$CTDTemperature, is.na(data$CTDTemperature), mean(data$CTDTemperature, na.rm = TRUE))
data$CTDTramission <-  replace(data$CTDTramission, is.na(data$CTDTramission), mean(data$CTDTramission, na.rm = TRUE))
data$CTDDissolvedOxygen <-  replace(data$CTDDissolvedOxygen, is.na(data$CTDDissolvedOxygen), mean(data$CTDDissolvedOxygen, na.rm = TRUE))
data$CTDPhotosyntheticallyActiveRadiation <-  replace(data$CTDPhotosyntheticallyActiveRadiation, is.na(data$CTDPhotosyntheticallyActiveRadiation), mean(data$CTDPhotosyntheticallyActiveRadiation, na.rm = TRUE))
data$Turbidity <-  replace(data$Turbidity, is.na(data$Turbidity), mean(data$Turbidity, na.rm = TRUE))
data$ExtractedPhycocyanin <-  replace(data$ExtractedPhycocyanin, is.na(data$ExtractedPhycocyanin), mean(data$ExtractedPhycocyanin, na.rm = TRUE))
data$ExtractedChlorophylla <-  replace(data$ExtractedChlorophylla, is.na(data$ExtractedChlorophylla), mean(data$ExtractedChlorophylla, na.rm = TRUE))
data$TotalPhosphorus <-  replace(data$TotalPhosphorus, is.na(data$TotalPhosphorus), mean(data$TotalPhosphorus, na.rm = TRUE))
data$TotalDissolvedPhosphorus <-  replace(data$TotalDissolvedPhosphorus, is.na(data$TotalDissolvedPhosphorus), mean(data$TotalDissolvedPhosphorus, na.rm = TRUE))
data$SolubleReactivePhosphorus <-  replace(data$SolubleReactivePhosphorus, is.na(data$SolubleReactivePhosphorus), mean(data$SolubleReactivePhosphorus, na.rm = TRUE))
data$Ammonia <-  replace(data$Ammonia, is.na(data$Ammonia), mean(data$Ammonia, na.rm = TRUE))
data$Nitrate <-  replace(data$Nitrate, is.na(data$Nitrate), mean(data$Nitrate, na.rm = TRUE))
data$ParticulateOrganicCarbon <-  replace(data$ParticulateOrganicCarbon, is.na(data$ParticulateOrganicCarbon), mean(data$ParticulateOrganicCarbon, na.rm = TRUE))
data$ParticulateOrganicNitrogen <-  replace(data$ParticulateOrganicNitrogen, is.na(data$ParticulateOrganicNitrogen), mean(data$ParticulateOrganicNitrogen, na.rm = TRUE))
data$DissolvedOrganicCarbon <-  replace(data$DissolvedOrganicCarbon, is.na(data$DissolvedOrganicCarbon), mean(data$DissolvedOrganicCarbon, na.rm = TRUE))
data$ColoredDissolvedOrganicMaterialAbsorbance <-  replace(data$ColoredDissolvedOrganicMaterialAbsorbance, is.na(data$ColoredDissolvedOrganicMaterialAbsorbance), mean(data$ColoredDissolvedOrganicMaterialAbsorbance, na.rm = TRUE))
data$TotalSuspendedSolids <-  replace(data$TotalSuspendedSolids, is.na(data$TotalSuspendedSolids), mean(data$TotalSuspendedSolids, na.rm = TRUE))
data$VolatileSuspendedSolids <-  replace(data$VolatileSuspendedSolids, is.na(data$VolatileSuspendedSolids), mean(data$VolatileSuspendedSolids, na.rm = TRUE))


correlation <- cor(data[sapply(data, is.numeric)])

cor(data$ParticulateMicrocystin, data$ExtractedChlorophylla, method = c("kendall"))

write.csv(data, file = "data.csv")

y <- data$ExtractedChlorophylla
x1 <- data$Turbidity
x2 <- data$ParticulateMicrocystin
x3 <- data$DissolvedMicrocystin
fit <- lm(y ~ x1 + x2 + x3, data=data)
summary(fit) # show results

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics



library(DAAG)
library(bootstrap)
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}

# matrix of predictors
X <- as.matrix(as.data.frame(data[c(x1,x2,x3)]))
# vector of predicted values
y <- as.matrix(data[c("y")])
