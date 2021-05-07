library(readr)
library(lavaan)
library(semPlot)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(GGally)

# Modelo 1b, sacar yield, agregar variable intermedia, para tener 3: Directness, Lima o no, Regional o no.
# Usar el Estimator WLSM como en el modelo 1a NO DA RESULTADO

data01b <- read_csv("https://public.minsky.cc/maria/market_channel_factors_db/model/01ModelDataframeOfWhitePotato.csv")
data01b$isPriceInformation <- ifelse((data01b$formalPriceInformation == 1) | (data01b$informalPriceInformation == 1), 1, 0)
data01b$marketLima <- ifelse((data01b$marketSize == 3), 1, 0)
data01b$marketRegional <- ifelse((data01b$marketSize == 2), 1, 0)

model01b <- "# direct effect
              farmGatePricePerKg ~ c0*districtTimeToLimaMarket + c1*districtTimeToProvinceCapitalOfRegion + c3*isPriceInformation + c7*volumeOfDistrict + c8*soldProduction + c10*associative + c11*pointsOfSale
            # mediators
              marketChannelDirectedness ~ a10*districtTimeToLimaMarket + a11*districtTimeToProvinceCapitalOfRegion + a13*isPriceInformation + a17*volumeOfDistrict + a18*soldProduction + a1a*associative + a1b*pointsOfSale
              marketLima ~ a20*districtTimeToLimaMarket + a21*districtTimeToProvinceCapitalOfRegion + a23*isPriceInformation + a27*volumeOfDistrict + a28*soldProduction + a2a*associative + a2b*pointsOfSale
              marketRegional ~ a30*districtTimeToLimaMarket + a31*districtTimeToProvinceCapitalOfRegion + a33*isPriceInformation + a37*volumeOfDistrict + a38*soldProduction + a3a*associative + a3b*pointsOfSale
            # indirect effect
              farmGatePricePerKg ~ b1*marketChannelDirectedness + b2*marketLima + b3*marketRegional
            # mediation through market channel directedness (Collector, Wholesaler, Retailer, Final consumer)
              i_10 := a10*b1
              i_11 := a11*b1
              i_13 := a13*b1
              i_17 := a17*b1
              i_18 := a18*b1
              i_1a := a1a*b1
              i_1b := a1b*b1
            # mediation through Lima (0, 1) from Market size
              i_20 := a20*b2
              i_21 := a21*b2
              i_23 := a23*b2
              i_27 := a27*b2
              i_28 := a28*b2
              i_2a := a2a*b2
              i_2b := a2b*b2
            # mediation through Regional (0, 1) from Market size
              i_30 := a30*b3
              i_31 := a31*b3
              i_33 := a33*b3
              i_37 := a37*b3
              i_38 := a38*b3
              i_3a := a3a*b3
              i_3b := a3b*b3
            "

fit01b <- sem(model01b, data = data01b)
summary(fit01b, fit.measures=TRUE)
fitMeasures(fit01b, c("cfi","rmsea","srmr"))
varTable(fit01b)
semPaths(fit01b, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)

ggcorrplot(fitted(fit01b, type="vcov")$cov, type="upper")
corrplot(cor(data01b[, 11:25]))
ggpairs(data01b[, 11:25])

ggqqplot(data01b, x="districtTimeToLimaMarket")
ggqqplot(data01b, x="districtTimeToProvinceCapitalOfRegion")
ggqqplot(data01b, x="provinceCapitalToLimaMarket")
ggqqplot(data01b, x="informalPriceInformation")
ggqqplot(data01b, x="formalPriceInformation")
ggqqplot(data01b, x="informalMetereologicalInformation")
ggqqplot(data01b, x="formalMetereologicalInformation")
ggqqplot(data01b, x="volumeOfDistrict")
ggqqplot(data01b, x="soldProduction")
ggqqplot(data01b, x="yield")
ggqqplot(data01b, x="associative")
ggqqplot(data01b, x="pointsOfSale")
ggqqplot(data01b, x="marketSize")
ggqqplot(data01b, x="simplifiedMarketChannelDirectedness")
ggqqplot(data01b, x="farmGatePricePerKg")
