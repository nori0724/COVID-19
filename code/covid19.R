#library(readr)
library(survival)
#library(tidyverse)
library(MASS)
library(flexsurv)
library(openxlsx)
install.packages("flexsurv")
# install.packages("survival")
# remove.packages("survival")

#df <- read.csv("/Users/masanoritakahashi/Documents/講義/ゼミ/M2/空間生存時間/論文/covid19/data_covid.csv")
df <- read.xlsx("/Users/masanoritakahashi/Documents/講義/ゼミ/M2/空間生存時間/論文/covid19/data_covid_fix_name.xlsx")
df <- na.omit(df)
head(df)
df
# 生存時間の分布
hist(df$date_first,xlab="time", main="")

#生存時間object
Y = Surv(df$date_first, df$status ==1)

# KM
kmfit1 = survfit(Y ~ 1)
summary(kmfit1)
plot(kmfit1,  mark.t=TRUE, xlab="time", ylab = "survival probabilities", main="")

# KM_pop
kmfit_pop = survfit(Y ~ df$pop_median)
par(family = "HiraKakuProN-W3")
plot(kmfit_pop,mark.t=TRUE,xlab="survival time in days", ylab = "survival probabilities",
     col = colors_type)
survdiff(Y ~ df$pop_median)

Y = Surv(df$date_first, df$status ==1)
fs1 <- flexsurvreg(Y ~ log(pf)+log(ef)+log(pop)+log(gdp)+log(urb)+log(dist)+log(air)+log(detection),
                   data = df,dist = "Gompertz")


fs2 <- flexsurvreg(Y ~ log(hf)+log(pop)+log(gdp)+log(urb)+log(dist)+log(air)+log(detection),
                   data = df,dist = "Gompertz")
fs2
###glm.nb

mean(df$date_first) #->63.72667

var(df$date_first) #->331.9449

library(MASS)
# nb
model_nb = glm.nb(date_first ~ log(pf)+log(ef)+log(pop)+log(gdp)+log(urb)+log(dist)+log(air)+log(detection),data = df)
summary(model_nb)
# 正規分布
model = glm(date_first ~ log(pf)+log(ef)+log(pop)+log(gdp)+log(urb)+log(dist)+log(air)+log(detection),data = df)
summary(model) #1176.5
# poisson
model_p = glm(date_first ~ log(pf)+log(ef)+log(pop)+log(gdp)+log(urb)+log(dist)+log(air)+log(detection),data = df, family="poisson")
summary(model_p) #1333.6

####################
# mapping
install.packages("rworldmap")
library(rworldmap)
df <- read.xlsx("/Users/masanoritakahashi/Documents/講義/ゼミ/M2/空間生存時間/論文/covid19/data_covid_fix_name.xlsx")
map_obj <- joinCountryData2Map(df, joinCode="NAME", nameJoinColumn="country")
png("/Users/masanoritakahashi/Documents/講義/ゼミ/M2/空間生存時間/論文/covid19/map/data_first.png", width=960, height=540)
par(family="Osaka")
mapCountryData(map_obj, nameColumnToPlot="date_first"
               , catMethod="fixedWidth"
               , mapTitle = "中国でコロナウイルスが発生してから各国で初感染するまでの日数"
               , colourPalette=heat.colors(7)
               , addLegend = TRUE)
dev.off()




# mapの名前の一致
library(maps)
x <- map("world", plot=FALSE)
x$names[grep("Timor", x$names)]

##################################3
df0712 <- read.csv("/Users/masanoritakahashi/Documents/講義/ゼミ/M2/空間生存時間/論文/covid19/covid19_0712_cases.csv")
df0712 = df0712[df0712$Cumulative_number_for_14_days_of_COVID.19_cases_per_100000>0,]
map_obj <- joinCountryData2Map(df0712, joinCode="NAME", nameJoinColumn="fix_country")
png("/Users/masanoritakahashi/Documents/講義/ゼミ/M2/空間生存時間/論文/covid19/map/covid19_0712_cases.png", width=960, height=540)
par(family="Osaka")
mapCountryData(map_obj, nameColumnToPlot="Cumulative_number_for_14_days_of_COVID.19_cases_per_100000"
               , catMethod="fixedWidth"
               , mapTitle = "Cumulative_number_for_14_days_of_COVID.19_cases_per_100000"
               , addLegend = TRUE)
dev.off()

##################################3
# コロナ収束までの日数
converge <- read.csv("/Users/masanoritakahashi/Documents/講義/ゼミ/M2/空間生存時間/論文/covid19/corona_converge_days.csv")
map_obj <- joinCountryData2Map(converge, joinCode="NAME", nameJoinColumn="country")
png("/Users/masanoritakahashi/Documents/講義/ゼミ/M2/空間生存時間/論文/covid19/map/corona_converge_days.png", width=960, height=540)
par(family="Osaka")
mapCountryData(map_obj, nameColumnToPlot="corona_converge_days"
               , catMethod="fixedWidth"
               , mapTitle = "The time it takes for the coronavirus to converge"
               , addLegend = TRUE)
dev.off()

# cox
converge <- na.omit(converge)
# 生存時間の分布
hist(converge$corona_converge_days,xlab="time", main="")
#生存時間object
Y = Surv(converge$corona_converge_days, converge$status ==1)
# KM
kmfit1 = survfit(Y ~ 1)
summary(kmfit1)
plot(kmfit1,  mark.t=TRUE, xlab="time", ylab = "survival probabilities", main="")

mod = coxph(Y ~ pop + urb + gdp + dist + hf + detection + air + date_first, data=converge)
summary(mod)
AIC(mod)
plot(mod)

# 予測
mod.fit<-survfit(mod)
plot(mod.fit)
par(new=T)
plot(kmfit1,  mark.t=TRUE, xlab="survival time in days", ylab = "survival probabilities")
cox.zph(mod, transform = rank)



