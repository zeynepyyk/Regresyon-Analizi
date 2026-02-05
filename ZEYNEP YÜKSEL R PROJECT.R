install.packages("readxl"); library(readxl); veri <- read_excel("R FİNAL PROJECT.xlsx", sheet = "Final_data (1)"); head(veri); str(veri)
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
     main = "BMI Histogramı ve Normal Dağılım Eğrisi",
     xlab = "Vücut Kitle İndeksi",
     col = "lightgray")

lines(density(veri$vucutkitleendksi), lwd = 2)

hist(veri$Age,
     main = "Age Histogramı",
     xlab = "Age",
     col = "lightblue")

hist(veri$Max_BPM,
     main = "Max BPM Histogramı",
     xlab = "Max BPM",
     col = "lightgreen")

hist(veri$ekran_suresi,
     main = "Ekran Suresi Histogramı",
     xlab = "Ekran Suresi",
     col = "lightpink")

hist(veri$yakilan_kalori,
     main = "Yakılan Kalori Histogramı",
     xlab = "Yakılan Kalori",
     col = "orange")

hist(veri$antreman_sikligi,
     main = "Haftalık Antrenman Sıklığı Histogramı",
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
     xlab = "Yakılan Kalori",
     ylab = "BMI",
     main = "Yakılan Kalori ve BMI Scatter Plot")
abline(lm(vucutkitleendksi ~ yakilan_kalori, data = veri), col = "red")

plot(veri$antreman_sikligi, veri$vucutkitleendksi,
     xlab = "Antrenman Sıklığı",
     ylab = "BMI",
     main = "Antrenman Sıklığı ve BMI Scatter Plot")
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
     main = "Artıkların Histogramı",
     xlab = "Artıklar")

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
     ylab = "Artıklar",
     main = "Artıklar vs Tahmin Edilen Degerler")
abline(h = 0, col = "red")

install.packages("lmtest")
library(lmtest)
dwtest(model)

par(mfrow = c(2, 3))

plot(veri$Age, model$residuals,
     xlab = "Age", ylab = "Artıklar", main = "Age")
abline(h = 0, col = "red")

plot(veri$Max_BPM, model$residuals,
     xlab = "Max BPM", ylab = "Artıklar", main = "Max BPM")
abline(h = 0, col = "red")

plot(veri$ekran_suresi, model$residuals,
     xlab = "Ekran Süresi", ylab = "Artıklar", main = "Ekran Süresi")
abline(h = 0, col = "red")

plot(veri$yakilan_kalori, model$residuals,
     xlab = "Yakılan Kalori", ylab = "Artıklar", main = "Yakılan Kalori")
abline(h = 0, col = "red")

plot(veri$antreman_sikligi, model$residuals,
     xlab = "Antrenman Sıklığı", ylab = "Artıklar", main = "Antrenman")
abline(h = 0, col = "red")

plot(model$fitted.values, model$residuals,
     xlab = "Tahmin Edilen", ylab = "Artıklar", main = "Fitted")
abline(h = 0, col = "red")

par(mfrow = c(1, 1))

plot(cooks.distance(model),
     type = "h",
     main = "Cook’s Distance",
     ylab = "Cook’s Distance",
     xlab = "Gözlem Sırası")
abline(h = 4/length(model$residuals), col = "red")

AIC(model)
BIC(model)

model2 <- lm(vucutkitleendksi ~ Age + Max_BPM, data = veri)

AIC(model, model2)
BIC(model, model2)


