library(readr)
library(lavaan)
library(semPlot)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(GGally)

data03 <- read_csv("https://public.minsky.cc/maria/market_channel_factors_db/model/2019_03ModelDataframeOfWhitePotato.csv")

model03 <- "# direct effect
              farmGatePricePerKg ~ c0*districtTimeToLimaMarket + c1*districtTimeToProvinceCapitalOfRegion + c3*informalPriceInformation + c4*formalPriceInformation + c5*informalMetereologicalInformation + c6*formalMetereologicalInformation + c7*volumeOfDistrict + c8*soldProduction + c9*yield + c10*associative + c11*pointsOfSale
            # mediators
              marketSize ~ a10*districtTimeToLimaMarket + a11*districtTimeToProvinceCapitalOfRegion + a13*informalPriceInformation + a14*formalPriceInformation + a15*informalMetereologicalInformation + a16*formalMetereologicalInformation + a17*volumeOfDistrict + a18*soldProduction + a19*yield + a1a*associative + a1b*pointsOfSale
              localMarketChannelDirectedness ~ a20*districtTimeToLimaMarket + a21*districtTimeToProvinceCapitalOfRegion + a23*informalPriceInformation + a24*formalPriceInformation + a25*informalMetereologicalInformation + a26*formalMetereologicalInformation + a27*volumeOfDistrict + a28*soldProduction + a29*yield + a2a*associative + a2b*pointsOfSale
            # indirect effect
              farmGatePricePerKg ~ b1*marketSize + b2*localMarketChannelDirectedness
            # mediation throught market size
              i_10 := a10*b1
              i_11 := a11*b1
              i_13 := a13*b1
              i_14 := a14*b1
              i_15 := a15*b1
              i_16 := a16*b1
              i_17 := a17*b1
              i_18 := a18*b1
              i_19 := a19*b1
              i_1a := a1a*b1
              i_1b := a1b*b1
            # mediation throught local market channel directedness
              i_20 := a20*b2
              i_21 := a21*b2
              i_23 := a23*b2
              i_24 := a24*b2
              i_25 := a25*b2
              i_26 := a26*b2
              i_27 := a27*b2
              i_28 := a28*b2
              i_29 := a29*b2
              i_2a := a2a*b2
              i_2b := a2b*b2
            "

fit03 <- sem(model03, data = data03, estimator="WLSM")
summary(fit03, fit.measures=TRUE)
fitMeasures(fit03, c("cfi","rmsea","srmr", "cfi.robust", "rmsea.robust"))
varTable(fit03)
semPaths(fit03, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)

ggcorrplot(fitted(fit03, type="cov")$cov, type="upper")
corrplot(cor(data03[, 11:25]))
ggpairs(data03[, 11:25])

ggqqplot(data03, x="districtTimeToProvinceCapitalOfRegion")
ggqqplot(data03, x="provinceCapitalToLimaMarket")
ggqqplot(data03, x="informalPriceInformation")
ggqqplot(data03, x="formalPriceInformation")
ggqqplot(data03, x="informalMetereologicalInformation")
ggqqplot(data03, x="formalMetereologicalInformation")
ggqqplot(data03, x="volumeOfDistrict")
ggqqplot(data03, x="soldProduction")
ggqqplot(data03, x="yield")
ggqqplot(data03, x="associative")
ggqqplot(data03, x="pointsOfSale")
ggqqplot(data03, x="marketSize")
ggqqplot(data03, x="localMarketChannelDirectedness")
ggqqplot(data03, x="farmGatePricePerKg")
