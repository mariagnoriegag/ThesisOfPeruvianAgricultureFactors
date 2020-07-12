library(readr)
library(lavaan)
library(semPlot)
library(ggcorrplot)
library(ggpubr)
library(corrplot)
library(GGally)

data02 <- read_csv("https://public.minsky.cc/maria/market_channel_factors_db/model/02ModelDataframeOfWhitePotato.csv")

model <- "# direct effect
          farmGatePricePerKg ~ c0*districtTimeToLimaMarket + c1*districtTimeToProvinceCapitalOfRegion + c3*informalPriceInformation + c4*formalPriceInformation + c5*informalMetereologicalInformation + c6*formalMetereologicalInformation + c7*volumeOfDistrict + c8*soldProduction + c10*associative + c11*pointsOfSale
          # mediators
          marketSize ~ a10*districtTimeToLimaMarket + a11*districtTimeToProvinceCapitalOfRegion + a13*informalPriceInformation + a14*formalPriceInformation + a15*informalMetereologicalInformation + a16*formalMetereologicalInformation + a17*volumeOfDistrict + a18*soldProduction + a1a*associative + a1b*pointsOfSale
          simplifiedMarketChannelDirectedness ~ a20*districtTimeToLimaMarket + a21*districtTimeToProvinceCapitalOfRegion + a23*informalPriceInformation + a24*formalPriceInformation + a25*informalMetereologicalInformation + a26*formalMetereologicalInformation + a27*volumeOfDistrict + a28*soldProduction + a29*yield + a2a*associative + a2b*pointsOfSale
          # indirect effect
          farmGatePricePerKg ~ b1*marketSize + b2*simplifiedMarketChannelDirectedness
          # covariance
          #marketSize ~~ simplifiedMarketChannelDirectedness
          # mediation throught market size
          i_10 := a10*b1
          i_11 := a11*b1
          i_13 := a13*b1
          i_14 := a14*b1
          i_15 := a15*b1
          i_16 := a16*b1
          i_17 := a17*b1
          i_18 := a18*b1
          i_1a := a1a*b1
          i_1b := a1b*b1
          # mediation throught market channel directedness
          i_20 := a20*b2
          i_21 := a21*b2
          i_23 := a23*b2
          i_24 := a24*b2
          i_25 := a25*b2
          i_26 := a26*b2
          i_27 := a27*b2
          i_28 := a28*b2
          i_2a := a2a*b2
          i_2b := a2b*b2
          "

fit02 <- sem(model, data = data02, estimator="WLSM")
summary(fit02, fit.measures=TRUE)
fitMeasures(fit02, c("cfi","rmsea","srmr", "cfi.robust", "rmsea.robust"))
varTable(fit02)
semPaths(fit02, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)

ggcorrplot(fitted(fit02, type="cov")$cov, type="upper")
corrplot(cor(data02[, 11:25]))
vcov(fit02)

ggqqplot(data02, x="districtTimeToLimaMarket")
ggqqplot(data02, x="districtTimeToProvinceCapitalOfRegion")
ggqqplot(data02, x="provinceCapitalToLimaMarket")
ggqqplot(data02, x="informalPriceInformation")
ggqqplot(data02, x="formalPriceInformation")
ggqqplot(data02, x="informalMetereologicalInformation")
ggqqplot(data02, x="formalMetereologicalInformation")
ggqqplot(data02, x="volumeOfDistrict")
ggqqplot(data02, x="soldProduction")
ggqqplot(data02, x="yield")
ggqqplot(data02, x="associative")
ggqqplot(data02, x="pointsOfSale")
ggqqplot(data02, x="marketSize")
ggqqplot(data02, x="simplifiedMarketChannelDirectedness")
ggqqplot(data02, x="farmGatePricePerKg")

ggpairs(data02[, 11:25])

