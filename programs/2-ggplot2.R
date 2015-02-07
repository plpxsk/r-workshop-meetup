library(ggplot2)
college <- read.csv("../data/College.csv")

## BASIC PLOTS
## base R:
plot(college$S.F.Ratio, college$Grad.Rate)

## ggplot2:
qplot(college$S.F.Ratio, college$Grad.Rate)
qplot(S.F.Ratio, Grad.Rate, data = college)
qplot(S.F.Ratio, Grad.Rate, data = college,
      colour=Private)
## what patterns do we see?
qplot(S.F.Ratio, Grad.Rate, data = college,
      geom=c("point", "smooth"), method=lm)

## histograms are univariate: one variable required
qplot(Grad.Rate, data = college,
      geom="histogram")
qplot(Grad.Rate, data = college,
      geom="histogram", binwidth = 2)


## USING ggplot() function and CUSTOMIZING PLOTS
## aes = aesthetics
p <- ggplot(college, aes(x=S.F.Ratio, y=Grad.Rate))

p + geom_point()
p + geom_point(aes(colour = Private))
##p + geom_point(colour = Private) 
p + geom_point(colour = "green")

## nice palette
p + geom_point(aes(colour = Private)) +
    scale_color_brewer()
p + geom_point(aes(colour = Private)) +
    scale_color_brewer(type="qual", palette=2)

## variations on histogram
ggplot(college) +
    geom_histogram(aes(x=S.F.Ratio))

p <- ggplot(college, aes(x=S.F.Ratio))

p + geom_histogram()
p + stat_bin(geom="area")
p + stat_bin(geom="point")
p + stat_bin(geom="line")


## OVERLAYS
qplot(Private, Grad.Rate, data = college)
qplot(Private, Grad.Rate, data = college, geom="jitter")
qplot(Private, Grad.Rate, data = college, geom=c("jitter", "boxplot"))
qplot(Private, Grad.Rate, data = college, geom=c("boxplot", "jitter"))

p <- ggplot(college, aes(x=Private, y=Grad.Rate))

p + geom_point()
p + geom_boxplot() + geom_jitter()
p + geom_boxplot() + geom_jitter() + coord_flip()
p + geom_boxplot(notch = TRUE, notchwidth = .5) +
    geom_jitter(colour="sienna1")

## modifying theme
theme_set(theme_bw())
## theme_set(theme_gray())
p + geom_boxplot(notch = TRUE, notchwidth = .5) +
    geom_jitter(colour="sienna1")


## 'FACETS'
library(dplyr)
college$Top10quartile <- ntile(college$Top10perc, 4)

p <- ggplot(college, aes(x=S.F.Ratio, y=Grad.Rate)) + geom_point()
p

p + facet_grid(. ~ Top10quartile)

## tip: don't need to iterate too much. 
ggplot(college, aes(x=S.F.Ratio, y=Grad.Rate)) + geom_boxplot() +
    geom_point() + facet_grid(. ~ Top10quartile)
