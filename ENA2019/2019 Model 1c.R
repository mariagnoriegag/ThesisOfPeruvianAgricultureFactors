library(readr)
library(lavaan)
library(semPlot)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(GGally)

# Modelo 1c, igual que modelo 1b pero quitando la variable mediadora Directedness
# Se deja el estimador del modelo 1a (WLSM)

data01c <- read_csv("https://public.minsky.cc/maria/market_channel_factors_db/model/2019_01ModelDataframeOfWhitePotato.csv")
data01c$isPriceInformation <- ifelse((data01c$formalPriceInformation == 1) | (data01c$informalPriceInformation == 1), 1, 0)
data01c$marketLima <- ifelse((data01c$marketSize == 3), 1, 0)
data01c$marketRegional <- ifelse((data01c$marketSize == 2), 1, 0)

model01c <- "# direct effect
              farmGatePricePerKg ~ c0*districtTimeToLimaMarket + c1*districtTimeToProvinceCapitalOfRegion + c3*isPriceInformation + c7*volumeOfDistrict + c8*soldProduction + c10*associative + c11*pointsOfSale
            # mediators
              marketLima ~ a10*districtTimeToLimaMarket + a11*districtTimeToProvinceCapitalOfRegion + a13*isPriceInformation + a17*volumeOfDistrict + a18*soldProduction + a1a*associative + a1b*pointsOfSale
              marketRegional ~ a20*districtTimeToLimaMarket + a21*districtTimeToProvinceCapitalOfRegion + a23*isPriceInformation + a27*volumeOfDistrict + a28*soldProduction + a2a*associative + a2b*pointsOfSale
            # indirect effect
              farmGatePricePerKg ~ b1*marketLima + b2*marketRegional
            # mediation through Lima (0, 1) from Market size
              i_10 := a10*b1
              i_11 := a11*b1
              i_13 := a13*b1
              i_17 := a17*b1
              i_18 := a18*b1
              i_1a := a1a*b1
              i_1b := a1b*b1
            # mediation through Regional (0, 1) from Market size
              i_20 := a20*b2
              i_21 := a21*b2
              i_23 := a23*b2
              i_27 := a27*b2
              i_28 := a28*b2
              i_2a := a2a*b2
              i_2b := a2b*b2
            "

fit01c <- sem(model01c, data = data01c, estimator="WLSM")
summary(fit01c, fit.measures=TRUE)
fitMeasures(fit01c, c("cfi","rmsea","srmr", "cfi.robust", "rmsea.robust"))
varTable(fit01c)
semPaths(fit01c, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)

ggcorrplot(fitted(fit01c, type="vcov")$cov, type="upper")
corrplot(cor(data01c[, 11:25]))
ggpairs(data01c[, 11:25])

ggqqplot(data01c, x="districtTimeToLimaMarket")
ggqqplot(data01c, x="districtTimeToProvinceCapitalOfRegion")
ggqqplot(data01c, x="provinceCapitalToLimaMarket")
ggqqplot(data01c, x="informalPriceInformation")
ggqqplot(data01c, x="formalPriceInformation")
ggqqplot(data01c, x="informalMetereologicalInformation")
ggqqplot(data01c, x="formalMetereologicalInformation")
ggqqplot(data01c, x="volumeOfDistrict")
ggqqplot(data01c, x="soldProduction")
ggqqplot(data01c, x="yield")
ggqqplot(data01c, x="associative")
ggqqplot(data01c, x="pointsOfSale")
ggqqplot(data01c, x="marketSize")
ggqqplot(data01c, x="simplifiedMarketChannelDirectedness")
ggqqplot(data01c, x="farmGatePricePerKg")
