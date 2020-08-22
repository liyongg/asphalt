source('Functions.R')
source('Anomalies_List.R')
source('Status_List.R')
source('Create_Weg.R')

# DF_1 <- Create_LCMS_DF_RAW(LCMS_folder = '20200617_LCMS_Steenverlies_ALL_ZOAB/')
# DF_2 <- Create_LCMS_DF_RAW(LCMS_folder = '20200309_LCMS_Steenverlies_ALL_ZOAB/')
# All_DF <- unique(rbind(DF_1, DF_2))

#### Chapter 7: Parametric Approach ####

# Specify the prototype
Weg <- R044
Segment <- Specified_Segment(Weg = Weg, Baan = '1HRR', HmStart = 7.1, Strook = '1RR')

PD <- BVS_DF(Weg, Segment$`Index List`)
PDLong <- PD_Long(PD, 'All')

PD.Transformed <- DataTransform(PD, f = log, s = s.MAD)
PDLong.Transformed <- PD_Long(PD.Transformed, 'All')


# ACF
set.seed(10)
ggacf(runif(100)) + 
  labs(title = TeX('\\textbf{Sample autocorrelation for an i.i.d. sample} (\\textit{n} = 100) \\textbf{from} \\textit{U}(0,1)'))

ACF_Plot(PDLong, Segment$`ID List`)

# Histogram
Plot_Histogram(Weg, Segment, Position = 'All')

Plot_Histogram(Weg, Segment, Position = 'All', f = log, s = s.MAD) + theme(legend.position = 'none')

SquareT <- PD_Long(DataTransform(PD, f = function(x) sqrt(x), s = s.MAD), Position = 'Left'); SquareT$Position <- gsub('Left', 'Square Root', SquareT$Position)
CubeT <- PD_Long(DataTransform(PD, f = function(x) x^(1/3), s = s.MAD), Position = 'Left'); CubeT$Position <- gsub('Left', 'Cube Root', CubeT$Position)
LogT <- PD_Long(DataTransform(PD, f = log, s = s.MAD), Position = 'Left'); LogT$Position <- gsub('Left', 'Log Transform', LogT$Position)
Create_Histogram_Multiple(rbind(SquareT, CubeT, LogT), Segment$`ID List`, Positions = c('Cube Root',  'Log Transform', 'Square Root')) + 
  theme(legend.position = 'none')

# Goodness of Fit
Plot_ECDF(subset(PDLong.Transformed, Position == 'Left'), Segment$`ID List`)
Plot_QQ(subset(PDLong.Transformed, Position == 'Left'), Segment$`ID List`)


LogisECDF <- Plot_ECDF(subset(PDLong.Transformed, Position == 'Left'), Segment$`ID List`, p = plogis, args = list(scale = sqrt(3/pi^2)))
LogisQQ <- Plot_QQ(subset(PDLong.Transformed, Position == 'Left'), col = 7, Segment$`ID List`, q = qlogis, dparams = list(scale = sqrt(3/pi^2)))
grid.arrange(grobs = list(LogisECDF$Base, LogisECDF$Agg, LogisQQ), layout_matrix = rbind(c(1,2), c(3,3)), heights = c(2,1))

# Normality tests

for(Y in unique(PDLong.Transformed$Datum_tijd)){
  LWT <- subset(PDLong.Transformed, Position == 'Left' & Datum_tijd == Y)$value
  print(shapiro.test(LWT))
  print(ad.test(LWT))
  print(ks.test(LWT, 'pnorm'))
}


#### Chapter 8: Non-Parametric Approach ####

# Kernels
Rectangular <- function(x, xi = 0, h = 1) ifelse(abs((x-xi)/h) <= 1, (1/h)*1/2, 0)
Triangular <- function(x, xi = 0, h = 1) ifelse(abs((x-xi)/h) <= 1, (1/h)*(1-abs((x-xi)/h)), 0)
Epanechnikov <- function(x, xi = 0, h = 1) ifelse(abs((x-xi)/h) <= 1, (1/h)*(3*(1-((x-xi)/h)^2)/4), 0)
Biweight <- function(x, xi = 0, h = 1) ifelse(abs((x-xi)/h) <= 1, (1/h)*(15*(1-((x-xi)/h)^2)^2/16), 0)
Gaussian <- function(x, xi = 0, h = 1) (1/h)*dnorm((x-xi)/h)

k.R <- PlotKernel(Rectangular, col = 'black')
k.T <- PlotKernel(Triangular, col = 'black')
k.E <- PlotKernel(Epanechnikov, col = 'black')
k.B <- PlotKernel(Biweight, col = 'black')
k.G <- PlotKernel(Gaussian, col = 'black')

Kernels <-  c('Rectangular', 'Triangular', 'Epanechnikov', 'Biweight', 'Gaussian')
k <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun = Rectangular, n = 3000, aes(colour = 'Rectangular')) + 
  stat_function(fun = Triangular, n = 3000, aes(colour = 'Triangular')) + 
  stat_function(fun = Epanechnikov, n = 3000, aes(colour = 'Epanechnikov')) + 
  stat_function(fun = Biweight, n = 3000, aes(colour = 'Biweight')) + 
  stat_function(fun = Gaussian, n = 3000, aes(colour = 'Gaussian')) + 
  labs(title = 'Kernels', x = '', y = '') +
  theme(plot.title = element_text(face = 'bold')) +
  scale_colour_manual(name = TeX('\\textbf{Kernel}'),
                      breaks = Kernels,
                      values = hue_pal()(5)) +
  coord_cartesian(ylim = c(0,1)) +
  xlim(-5,5) +
  ThemeDef()

grid.arrange(k.R, k.T, k.E, k.B, k.G, k, layout_matrix = rbind(c(1, 6, 6, 6, 6),
                                                               c(2, 6, 6, 6, 6),
                                                               c(3, 6, 6, 6, 6),
                                                               c(4, 6, 6, 6, 6),
                                                               c(5, 6, 6, 6, 6))) + ThemeDef()

# Kernel Density
LWT_DF <- subset(PD_Long(DataTransform(PD, f = identity, s = s.MAD), 'All'), Position == 'Left')
  
d.rec <- Create_Density(LWT_DF, kernel = 'rectangular')
d.tri <- Create_Density(LWT_DF, kernel = 'triangular')
d.Epa <- Create_Density(LWT_DF, kernel = 'epanechnikov')
d.bi <- Create_Density(LWT_DF, kernel = 'biweight')
d.gaus <- Create_Density(LWT_DF, kernel = 'gaussian')
grid.arrange(d.rec, d.tri, d.Epa, d.bi, d.gaus)


klist.default <- list()
i <- 1
for(h in c(1,0.5,2)){
  for(k in Kernels){
    p <- PlotKernel(get(k), h = h) + ggtitle('') + coord_cartesian(ylim = c(0,2))
    if(h == 1) p <- p + ggtitle(k)
    klist.default[[i]] <- p
    i <- i + 1
  }
}

combine <- cbind(tableGrob(c('h = 1',  'h = 0.5', 'h = 2'), theme = ttheme_minimal(core = list(fg_params = list(hjust = -0.3, x = 0, fontface = 4L)))), 
                 arrangeGrob(grobs = klist.default, ncol = 5),  size = "last")
grid.arrange(combine)


#### Chapter 9: Extrapolation ####
# Wiggily Plot
WFunc <- function(x) x^2/3
x <- seq(0, 5, by = 0.2)
set.seed(10)
y <- WFunc(x) + runif(length(x), -2, 2)

ggplot(data = data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_point(alpha = 0.5) + 
  stat_function(fun = WFunc, n = 4000, aes(colour = 'True', linetype = 'True')) +
  geom_smooth(method = 'gam', aes(colour = 'Lambda1', linetype = 'Lambda1'),
              formula = y ~ s(x, sp = 0.00000001),
              se = F) +
  geom_smooth(method = 'gam', aes(colour = 'Lambda2', linetype = 'Lambda2'),
              formula = y ~ s(x, sp = 10), 
              se = F) +
  geom_smooth(method = 'gam', aes(colour = 'LambdaG', linetype = 'LambdaG'),
              formula = y ~ s(x, sp = 0.1), 
              se = F) +
  scale_linetype_manual(labels = c('True Function', '\u03BB = 10e-07', '\u03BB = 10', '\u03BB = 0.1'),
                        breaks = c('True', 'Lambda1', 'Lambda2', 'LambdaG'),
                        values = c('True' = 'solid', 'Lambda1' = 'dotted', 'Lambda2' = 'dotted', 'LambdaG' = 'dotted')) +
  scale_colour_manual(labels = c('True Function', '\u03BB = 10e-07', '\u03BB = 10', '\u03BB = 0.1'),
                      breaks = c('True', 'Lambda1', 'Lambda2','LambdaG'),
                      values = hue_pal()(4)) +
  labs(colour = 'Fits', linetype = 'Fits') + 
  ThemeDef() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

#### Chapter 10: Results and Discussion ####

# Fitting Lognormal distributions
Lognorm.Perc <- parPercentiles(PD, 'lnorm')
last(colnames(Lognorm.Perc$Left)) <- 'Log-normal'

# Fitting Exponential distributions
Exponential.Perc <- parPercentiles(PD, 'exp')
last(colnames(Exponential.Perc$Left)) <- 'Exponential'

# Fitting Logistic distribution to log-transformed data
PD.Transformed.log <- DataTransform(PD, f = log)
Logis.Perc <- parPercentiles(PD.Transformed.log, 'logis')
Logis.Perc$Left$percentile75 <- exp(Logis.Perc$Left$percentile75)
last(colnames(Logis.Perc$Left)) <- 'Logistic'

# Fitting Empirical quantiles to standardised aggregated data
Empirical.Perc <- nonparQuantiles(PD, method = 'empirical')
last(colnames(Empirical.Perc$Left)) <- 'Empirical'

# Fitting Kernel quantiles to standardised aggregated data
Kernel.Perc <- nonparQuantiles(PD, method = 'kernel')
last(colnames(Kernel.Perc$Left)) <- 'Kernel'


PrototypteDF <- list(Lognorm.Perc$Left, Logis.Perc$Left, Empirical.Perc$Left, Kernel.Perc$Left) %>% 
  reduce(left_join, by = c('Datum_tijd'))
PrototypeLong <- melt(data.table(PrototypteDF), id.vars = 'Datum_tijd')
PrototypeLong <- PrototypeLong[PrototypeLong$variable %in% c('Log-normal', 'Logistic', 'Empirical', 'Kernel'),]

# Quantile Estimates Plot
ggplot(data = PrototypeLong, aes(x = Datum_tijd, y = value, colour = variable, shape = variable)) +
  geom_line(size = 1.1) +
  ylim(0,10.5) +
  RWS_Thresh() +
  labs(title = TeX('\\textbf{Estimates of} $\\textit{q}_{0.75}$'),
       x = '',
       y = expression('75'^{'th'}~'Percentile'),
       shape = 'Methods',
       colour = 'Methods',
       linetype = 'Methods') +
  guides(fill = guide_legend(direction = 'horizontal')) +
  scale_x_date(breaks = '1 year', date_labels = '%Y') +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  theme(plot.title = element_text(face = 'bold'),
        legend.title = element_text(face = 'bold'),
        legend.text = element_text(size = 11),
        plot.margin = unit(c(0,0.1,0,0), 'cm'),
        legend.background = element_rect(fill = alpha('white', 0)))

######### Uncomment for modified values
PrototypeLong[PrototypeLong$variable == 'Logistic',]$value <- PrototypeLong[PrototypeLong$variable == 'Log-normal',]$value
PrototypeLong[PrototypeLong$variable == 'Log-normal',]$value <- c(1.904111, 2.032367, 2.500176, 2.648652, 3.691093, 6.503076, 7.677133)
PrototypeLong$variable <- gsub('Log-normal', 'Modified Log-normal', PrototypeLong$variable)
PrototypeLong$variable <- gsub('Logistic', 'Log-normal', PrototypeLong$variable)

Extrapolation_DF <- data.frame()
Extrapolation_DFPol <- data.frame()
Extrapolation_DFminOne <- data.frame()
Extrapolation_DFminOnePol <- data.frame()
Extrapolation_DFminTwo <- data.frame()
Extrapolation_DFminTwoPol <- data.frame()

for(name in unique(PrototypeLong$variable)){
  # # All Quantiles
  NormalLin <- Extrapolated_DF(PrototypeLong[PrototypeLong$variable == name, c(1,3)], Method = 'lin')
  Extrapolation_DF <- rbind(Extrapolation_DF,
                            cbind(NormalLin, 'variable' = rep(name, nrow(NormalLin))))
  
  NormalPoly <- Extrapolated_DF(PrototypeLong[PrototypeLong$variable == name, c(1,3)])
  Extrapolation_DFPol <- rbind(Extrapolation_DFPol,
                               cbind(NormalPoly, 'variable' = rep(name, nrow(NormalPoly))))
  
  # MinOne Quantile
  MinOneLin <- Extrapolated_DF(head(PrototypeLong[PrototypeLong$variable == name, c(1,3)], -1), Method = 'lin')
  Extrapolation_DFminOne <- rbind(Extrapolation_DFminOne,
                                  cbind(MinOneLin, 'variable' = rep(name, nrow(MinOneLin))))
  
  MinOnePoly <- Extrapolated_DF(head(PrototypeLong[PrototypeLong$variable == name, c(1,3)], -1))
  Extrapolation_DFminOnePol <- rbind(Extrapolation_DFminOnePol,
                                     cbind(MinOnePoly, 'variable' = rep(name, nrow(MinOnePoly))))
  
  MinTwoLin <- Extrapolated_DF(head(PrototypeLong[PrototypeLong$variable == name, c(1,3)], -2), Method = 'lin')
  Extrapolation_DFminTwo <- rbind(Extrapolation_DFminTwo,
                                  cbind(MinTwoLin, 'variable' = rep(name, nrow(MinTwoLin))))
  
  MinTwoPoly <- Extrapolated_DF(head(PrototypeLong[PrototypeLong$variable == name, c(1,3)], -2))
  Extrapolation_DFminTwoPol <- rbind(Extrapolation_DFminTwoPol,
                                     cbind(MinTwoPoly, 'variable' = rep(name, nrow(MinTwoPoly))))
  
  
  # print(last(Extrapolated_DF(head(PrototypeLong[PrototypeLong$variable == name, c(1,3)], -1)), 'variable'= c(name, name)))
  # print(PredictionDifferences(head(PrototypeLong[PrototypeLong$variable == name, c(1,3)], -1)))
  # print(PredictionDifferences(PrototypeLong[PrototypeLong$variable == name, c(1,3)]))
}

colnames(Extrapolation_DF) <- c('Datum_tijd', 'value', 'variable')
colnames(Extrapolation_DFPol) <- c('Datum_tijd', 'value', 'variable')

colnames(Extrapolation_DFminOne) <- c('Datum_tijd', 'value', 'variable')
colnames(Extrapolation_DFminOnePol) <- c('Datum_tijd', 'value', 'variable')

colnames(Extrapolation_DFminTwo) <- c('Datum_tijd', 'value', 'variable')
colnames(Extrapolation_DFminTwoPol) <- c('Datum_tijd', 'value', 'variable')

inPlot <- PlotExtrapolation(MainData = PrototypeLong,
                            ExtraData = Extrapolation_DF,
                            ExtraDataPol = Extrapolation_DFPol) + 
  scale_x_date(limits = c(as.Date('2019-12-01'), as.Date('2020-05-01'))) + 
  scale_y_continuous(limits = c(9.2, 10)) + theme(plot.title = element_blank(),
                                                  panel.grid = element_blank(),
                                                  axis.title = element_blank(),
                                                  axis.text = element_blank(),
                                                  axis.ticks = element_blank(),
                                                  legend.position = 'none')

PlotExtrapolation(MainData = PrototypeLong,
                  ExtraData = Extrapolation_DF,
                  ExtraDataPol = Extrapolation_DFPol) +
  annotation_custom(ggplotGrob(inPlot), 
                    xmin = min(PrototypeLong$Datum_tijd), xmax = as.Date('2016-01-01'), 
                    ymin = 5, ymax = 9.8)

PlotExtrapolation(MainData = subset(PrototypeLong, year(Datum_tijd) < 2019 & variable == 'Log-normal'),
                  ExtraData = subset(Extrapolation_DFminOne, variable == 'Log-normal'),
                  ExtraDataPol = subset(Extrapolation_DFminOnePol, variable == 'Log-normal')) + theme(legend.position = 'none')

PlotExtrapolation(MainData = subset(PrototypeLong, year(Datum_tijd) < 2018 & variable %in% c('Log-normal', 'Modified Log-normal')),
                  ExtraData = subset(Extrapolation_DFminTwo, variable %in% c('Log-normal', 'Modified Log-normal')),
                  ExtraDataPol = subset(Extrapolation_DFminTwoPol, variable %in% c('Log-normal', 'Modified Log-normal')))

# Mixed Density

PD.Long <- PD_Long(PD, 'All')
PD.Long$Position <- gsub('Left', 'Mixed', PD.Long$Position)
PD.Long$Position <- gsub('Right', 'Mixed', PD.Long$Position)
PD.Long <- subset(PD.Long, Position == 'Mixed')
d1 <- Create_Density(PD.Long, kernel = 'gaussian') + 
  facet_grid(Datum_tijd ~ Position, scales = 'free_y') +
  labs(title = Segment$`ID List`$Title) + 
  ThemeDef()

d2 <- Create_Density(subset(PD_Long(DataTransform(PD, s = s.MAD, mixed = TRUE), 'All'), Position == 'Mixed'), kernel = 'gaussian') +
  labs(title = 'Kernel Density Estimation of Standardised Mixed Data') +
  ThemeDef()
grid.arrange(d1, d2, ncol = 2)

PlotCurves.DF(head(nonparQuantiles(DataTransform(PD, mixed = TRUE))$Mixed[, c(1,4)], -2))


# A44 
A44.Stats <- WTL(R044, All_DF)
RoadStretch(A44.Stats, Var = 'Quantile', Lanes = c('1RR', '2RR', '1RL', '2RL'), points = T) +
  ggtitle(TeX('\\textbf{A44: Progression of} $\\textit{q}_{0.75}$ '))

A44_Predictions <- RoadPredictions(R044, All_DF)
grid.arrange(grobs = A44_Predictions$Plot$Mixed, nrow = 5)
print(xtable(A44_Predictions$DF[, !colnames(A44_Predictions$DF) %in% c('Position', 'TSD.Emp', 'PRL.Emp', 'dPRL.Emp')]),  
      include.rownames = FALSE)


# A50
A50.Stats <- WTL(R050, All_DF)
RoadStretch(subset(A50.Stats, Hm < 150), Var = 'Quantile', Lanes = c('1RR', '2RR', '1RL', '2RL'), points = TRUE) +
  ggtitle(TeX('\\textbf{A50: Progression of} $\\textit{q}_{0.75}$ '))
RoadStretch(subset(A50.Stats, Hm > 150), Var = 'Quantile', Lanes = c('1RL', '2RL'), points = TRUE) +
  ggtitle(TeX('\\textbf{A50: Progression of} $\\textit{q}_{0.75}$ '))

Segment <- Specified_Segment(Weg = R050, Baan = '1HRR', HmStart = 145.5, Strook = '1RR')
PD <- BVS_DF(R050, Segment$`Index List`)
PD <- DataTransform(PD, mixed = TRUE)
PlotCurves.DF(nonparQuantiles(PD, method = 'kernel')$Mixed[-c(1,2) ,c(1,4)]) + 
  scale_x_date(date_breaks = '10 years', date_labels = '%Y')


Segment <- Specified_Segment(Weg = R050, Baan = '1HRL', HmStart = 204.8, Strook = '1RL')
PD <- BVS_DF(R050, Segment$`Index List`)
Hectometer(PD) + 
  ggtitle(Segment$`ID List`$Title) +
  theme(plot.title = element_text(face = 'bold'),
        legend.background = element_rect(fill = alpha('white', 0)))


A50_Predictions <- RoadPredictions(R050, All_DF, Hm = 200)
grid.arrange(grobs = A50_Predictions$Plot$Mixed, nrow = 5)
print(xtable(A50_Predictions$DF[, !colnames(A50_Predictions$DF) %in% c('Position', 'TSD.Emp', 'PRL.Emp', 'dPRL.Emp')]),  
      include.rownames = FALSE)


# A6
A6.Stats <- WTL(R006, All_DF)
RoadStretch(A6.Stats, breaks = 1, points = TRUE, LaneLevels = c('1RR','2RR')) +
  ggtitle(TeX('\\textbf{A6: Progression of} $\\textit{q}_{0.75}$ '))

Segment <- Specified_Segment(Weg = R006, Baan = '1HRR', HmStart = 285.8, Strook = '1RR')
PD <- BVS_DF(R006, Segment$`Index List`)
Hectometer(PD) + 
  ggtitle(Segment$`ID List`$Title) +
  theme(plot.title = element_text(face = 'bold'),
        legend.background = element_rect(fill = alpha('white', 0)))

A6.Worst <- sort(unique((subset(A6.Stats, Quantile > 6, select = Hm))$Hm))
A6_Predictions <- RoadPredictions(R006, All_DF, Hm = A6.Worst)
grid.arrange(grobs = A6_Predictions$Plot$Mixed, nrow = 5)
print(xtable(A6_Predictions$DF[, !colnames(A6_Predictions$DF) %in% c('Position', 'TSD.Emp', 'PRL.Emp', 'dPRL.Emp')]),  
      include.rownames = FALSE)

# Convex increasing per met
A44_Predictions.Conv <- RoadPredictions(R044, All_DF, convex = TRUE)
A50_Predictions.Conv <- RoadPredictions(R050, All_DF, convex = TRUE, Hm = 200)
A6_Predictions.Conv <- RoadPredictions(R006, All_DF, convex = TRUE, Hm = A6.Worst)

#### Chapter 11: Conclusion ####
for(s in c(2.9, 2.8, 3)){
  Segment <- Specified_Segment(Weg = R044, Baan = '1HRR', HmStart = s, Strook = '1RR')
  PD <- BVS_DF(R044, Segment$`Index List`)
  grid.draw(Hectometer(PD) + 
    ggtitle(Segment$`ID List`$Title) +
    theme(plot.title = element_text(face = 'bold'),
          legend.background = element_rect(fill = alpha('white', 0))))
}


#### Random ####
testply <- lapply(testpredlist$Mixed[1:5], function(x) x + theme(axis.title = element_text(),
                                                                 axis.ticks = element_line(),
                                                                 plot.subtitle = element_text(face = 'bold', size = 10),
                                                                 axis.text = element_text(),
                                                                 legend.position = 'none'))
grid.arrange(grobs = testply, nrow = 2)





ADF <- Anomalies_List$R006
ADF$Date <- as.character(ADF$Date)
print(xtable(ADF[,c(3,2,4,5,6)]),  include.rownames = FALSE)