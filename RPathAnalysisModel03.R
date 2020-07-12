library(readr)
library(lavaan)
library(semPlot)
library(ggcorrplot)

data03 <- read_csv("https://public.minsky.cc/maria/market_channel_factors_db/model/03ModelDataframeOfWhitePotato.csv")

model <- "# direct effect
          farmGatePricePerKg ~ c1*districtTimeToProvinceCapitalOfRegion + c2*provinceCapitalToLimaMarket + c3*informalPriceInformation + c4*formalPriceInformation + c5*informalMetereologicalInformation + c6*formalMetereologicalInformation + c7*volumeOfDistrict + c8*soldProduction + c9*yield + c10*associative + c11*pointsOfSale
          # mediators
          marketSize ~ a11*districtTimeToProvinceCapitalOfRegion + a12*provinceCapitalToLimaMarket + a13*informalPriceInformation + a14*formalPriceInformation + a15*informalMetereologicalInformation + a16*formalMetereologicalInformation + a17*volumeOfDistrict + a18*soldProduction + a19*yield + a1a*associative + a1b*pointsOfSale
          localMarketChannelDirectedness ~ a21*districtTimeToProvinceCapitalOfRegion + a22*provinceCapitalToLimaMarket + a23*informalPriceInformation + a24*formalPriceInformation + a25*informalMetereologicalInformation + a26*formalMetereologicalInformation + a27*volumeOfDistrict + a28*soldProduction + a29*yield + a2a*associative + a2b*pointsOfSale
          # indirect effect
          farmGatePricePerKg ~ b1*marketSize + b2*localMarketChannelDirectedness
          # covariance
          #marketSize ~~ localMarketChannelDirectedness
          # mediation throught market size
          i_11 := a11*b1
          i_12 := a12*b1
          i_13 := a13*b1
          i_14 := a14*b1
          i_15 := a15*b1
          i_16 := a16*b1
          i_17 := a17*b1
          i_18 := a18*b1
          i_19 := a19*b1
          i_1a := a1a*b1
          i_1b := a1b*b1
          # mediation throught market channel directedness
          i_21 := a21*b2
          i_22 := a22*b2
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

fit03 <- sem(model, data = data03, estimator="WLSM")
summary(fit03, fit.measures=TRUE)
fitMeasures(fit03, c("cfi","rmsea","srmr", "cfi.robust", "rmsea.robust"))
varTable(fit03)
semPaths(fit03, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
