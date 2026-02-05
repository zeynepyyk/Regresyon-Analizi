# Regresyon-Analizi
YAŞAM TARZI VERİLERİ İLE ÇOK DEĞİŞKENLİ REGRESYON ANALİZİ   
install.packages("readxl"); library(readxl); veri <- read_excel("R FD0NAL PROJECT.xlsx", sheet = "Final_data (1)"); head(veri); str(veri)
data.frame(
  Age_mean = mean(veri$Age),
  Age_sd   = sd(veri$Age),
  Age_min  = min(veri$Age),
  Age_max  = max(veri$Age),
  
  Max_BPM_mean = mean(veri$Max_BPM),
  Max_BPM_sd   = sd(veri$Max_BPM),
  Max_BPM_min  = min(veri$Max_BPM),
  Max_BPM_max  = max(veri$Max_BPM),
  
  ekran_suresi_mean = mean(veri$ekran_suresi),
  ekran_suresi_sd   = sd(veri$ekran_suresi),
  ekran_suresi_min  = min(veri$ekran_suresi),
  ekran_suresi_max  = max(veri$ekran_suresi),
  
  yakilan_kalori_mean = mean(veri$yakilan_kalori),
  yakilan_kalori_sd   = sd(veri$yakilan_kalori),
  yakilan_kalori_min  = min(veri$yakilan_kalori),
  yakilan_kalori_max  = max(veri$yakilan_kalori),
  
  antreman_sikligi_mean = mean(veri$antreman_sikligi),
  antreman_sikligi_sd   = sd(veri$antreman_sikligi),
  antreman_sikligi_min  = min(veri$antreman_sikligi),
  antreman_sikligi_max  = max(veri$antreman_sikligi),
  
  vucutkitleendksi_mean = mean(veri$vucutkitleendksi),
  vucutkitleendksi_sd   = sd(veri$vucutkitleendksi),
  vucutkitleendksi_min  = min(veri$vucutkitleendksi),
  vucutkitleendksi_max  = max(veri$vucutkitleendksi)
)

hist(veri$vucutkitleendksi,
     probability = TRUE,
     main = "BMI HistogramD1 ve Normal DaDD1lD1m EDrisi",
     xlab = "VC<cut Kitle D0ndeksi",
     col = "lightgray")

lines(density(veri$vucutkitleendksi), lwd = 2)

hist(veri$Age,
     main = "Age HistogramD1",
     xlab = "Age",
     col = "lightblue")

hist(veri$Max_BPM,
     main = "Max BPM HistogramD1",
     xlab = "Max BPM",
     col = "lightgreen")

hist(veri$ekran_suresi,
     main = "Ekran Suresi HistogramD1",
     xlab = "Ekran Suresi",
     col = "lightpink")

hist(veri$yakilan_kalori,
     main = "YakD1lan Kalori HistogramD1",
     xlab = "YakD1lan Kalori",
     col = "orange")

hist(veri$antreman_sikligi,
     main = "HaftalD1k Antrenman SD1klD1DD1 HistogramD1",
     xlab = "Antrenman / Hafta",
     col = "lightyellow")

sayisal_veri <- veri[, c("Age", "Max_BPM", "ekran_suresi",
                         "yakilan_kalori",
                         "antreman_sikligi",
                         "vucutkitleendksi")]

cor_matris <- cor(sayisal_veri, use = "complete.obs")
cor_matris

install.packages("corrplot")
library(corrplot)

corrplot(cor_matris, method = "circle", type = "upper")
install.packages("car")
library(car)
vif(veri)
model <- lm(vucutkitleendksi ~ Age + Max_BPM + ekran_suresi +
              yakilan_kalori + antreman_sikligi,
            data = veri)

install.packages("car")
library(car)

vif(model)

plot(veri$Age, veri$vucutkitleendksi,
     xlab = "Age",
     ylab = "BMI",
     main = "Age ve BMI Scatter Plot")
abline(lm(vucutkitleendksi ~ Age, data = veri), col = "red")

plot(veri$Max_BPM, veri$vucutkitleendksi,
     xlab = "Max BPM",
     ylab = "BMI",
     main = "Max BPM ve BMI Scatter Plot")
abline(lm(vucutkitleendksi ~ Max_BPM, data = veri), col = "red")

plot(veri$ekran_suresi, veri$vucutkitleendksi,
     xlab = "Ekran Suresi",
     ylab = "BMI",
     main = "Ekran Suresi ve BMI Scatter Plot")
abline(lm(vucutkitleendksi ~ ekran_suresi, data = veri), col = "red")

plot(veri$yakilan_kalori, veri$vucutkitleendksi,
     xlab = "YakD1lan Kalori",
     ylab = "BMI",
     main = "YakD1lan Kalori ve BMI Scatter Plot")
abline(lm(vucutkitleendksi ~ yakilan_kalori, data = veri), col = "red")

plot(veri$antreman_sikligi, veri$vucutkitleendksi,
     xlab = "Antrenman SD1klD1DD1",
     ylab = "BMI",
     main = "Antrenman SD1klD1DD1 ve BMI Scatter Plot")
abline(lm(vucutkitleendksi ~ antreman_sikligi, data = veri), col = "red")

model <- lm(vucutkitleendksi ~ Age + Max_BPM + ekran_suresi +
              yakilan_kalori + antreman_sikligi,
            data = veri)

veri$BMI_tahmin <- fitted(model)
head(veri[, c("vucutkitleendksi", "BMI_tahmin")])

summary(model)

model <- lm(vucutkitleendksi ~ Age + Max_BPM + ekran_suresi +
              yakilan_kalori + antreman_sikligi, data = veri)

set.seed(123)
shapiro.test(sample(model$residuals, 5000))

hist(model$residuals,
     main = "ArtD1klarD1n HistogramD1",
     xlab = "ArtD1klar")

qqnorm(model$residuals)
qqline(model$residuals, col = "red")

library(lmtest)
bptest(model)

install.packages("sandwich")
install.packages("lmtest")

library(sandwich)
library(lmtest)

coeftest(model, vcov = vcovHC(model, type = "HC1"))

plot(model$fitted.values, model$residuals,
     xlab = "Tahmin Edilen Degerler",
     ylab = "ArtD1klar",
     main = "ArtD1klar vs Tahmin Edilen Degerler")
abline(h = 0, col = "red")

install.packages("lmtest")
library(lmtest)
dwtest(model)

par(mfrow = c(2, 3))

plot(veri$Age, model$residuals,
     xlab = "Age", ylab = "ArtD1klar", main = "Age")
abline(h = 0, col = "red")

plot(veri$Max_BPM, model$residuals,
     xlab = "Max BPM", ylab = "ArtD1klar", main = "Max BPM")
abline(h = 0, col = "red")

plot(veri$ekran_suresi, model$residuals,
     xlab = "Ekran SC<resi", ylab = "ArtD1klar", main = "Ekran SC<resi")
abline(h = 0, col = "red")

plot(veri$yakilan_kalori, model$residuals,
     xlab = "YakD1lan Kalori", ylab = "ArtD1klar", main = "YakD1lan Kalori")
abline(h = 0, col = "red")

plot(veri$antreman_sikligi, model$residuals,
     xlab = "Antrenman SD1klD1DD1", ylab = "ArtD1klar", main = "Antrenman")
abline(h = 0, col = "red")

plot(model$fitted.values, model$residuals,
     xlab = "Tahmin Edilen", ylab = "ArtD1klar", main = "Fitted")
abline(h = 0, col = "red")

par(mfrow = c(1, 1))

plot(cooks.distance(model),
     type = "h",
     main = "Cookb
