library(readr)
library(lavaan)
library(semPlot)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(GGally)

# Modelo 1a, quitar yield y quitar el destino (Market size)

data01a <- read_csv("https://public.minsky.cc/maria/market_channel_factors_db/model/01ModelDataframeOfWhitePotato.csv")
data01a$isPriceInformation <- ifelse((data01a$formalPriceInformation == 1) | (data01a$informalPriceInformation == 1), 1, 0)

model01a <- "# direct effect
              farmGatePricePerKg ~ c0*districtTimeToLimaMarket + c1*districtTimeToProvinceCapitalOfRegion + c3*isPriceInformation + c7*volumeOfDistrict + c8*soldProduction + c10*associative + c11*pointsOfSale
            # mediators
              marketChannelDirectedness ~ a20*districtTimeToLimaMarket + a21*districtTimeToProvinceCapitalOfRegion + a23*isPriceInformation + a27*volumeOfDistrict + a28*soldProduction + a2a*associative + a2b*pointsOfSale
            # indirect effect
              farmGatePricePerKg ~ b2*marketChannelDirectedness
            # mediation through market channel directedness (Collector, Wholesaler, Retailer, Final consumer)
              i_20 := a20*b2
              i_21 := a21*b2
              i_23 := a23*b2
              i_27 := a27*b2
              i_28 := a28*b2
              i_2a := a2a*b2
              i_2b := a2b*b2
            "

fit01a <- sem(model01a, data = data01a, estimator="WLSM")
summary(fit01a, fit.measures=TRUE)
fitMeasures(fit01a, c("cfi","rmsea","srmr", "cfi.robust", "rmsea.robust"))
varTable(fit01a)
semPaths(fit01a, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)

ggcorrplot(fitted(fit01a, type="vcov")$cov, type="upper")
corrplot(cor(data01a[, 11:25]))
ggpairs(data01a[, 11:25])

ggqqplot(data01a, x="districtTimeToLimaMarket")
ggqqplot(data01a, x="districtTimeToProvinceCapitalOfRegion")
ggqqplot(data01a, x="provinceCapitalToLimaMarket")
ggqqplot(data01a, x="informalPriceInformation")
ggqqplot(data01a, x="formalPriceInformation")
ggqqplot(data01a, x="informalMetereologicalInformation")
ggqqplot(data01a, x="formalMetereologicalInformation")
ggqqplot(data01a, x="volumeOfDistrict")
ggqqplot(data01a, x="soldProduction")
ggqqplot(data01a, x="yield")
ggqqplot(data01a, x="associative")
ggqqplot(data01a, x="pointsOfSale")
ggqqplot(data01a, x="marketSize")
ggqqplot(data01a, x="simplifiedMarketChannelDirectedness")
ggqqplot(data01a, x="farmGatePricePerKg")
