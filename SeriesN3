
# -- ----------------------------------------------------------------------------------------- -- #
# -- ITESO, Universidad Jesuita de Guadalajara ----------------------------------------------- -- #
# -- Ingeniería Financiera - Departamento de Matematicas y Fisica ---------------------------- -- #
# -- Licencia: GNU General Public License ---------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

SerieN3 <- function(Assets)  {

  MultiData <- data.frame(Assets[,1],Assets[,2]/max(Assets[,2]),Assets[,3]/max(Assets[,3]),
  Assets[,4]/max(Assets[,4]))
  colnames(MultiData) <- colnames(Assets)
  MultiData <- melt(MultiData, id="TimeStamp", variable.name="Assets", value.name="NormalizedPrice")

  ggsm  <- ggplot(MultiData, aes(x=TimeStamp,y=NormalizedPrice),group=Assets)    +
  geom_line(aes(colour = Assets), linetype = "longdash", size = 1.5)             +
  labs(title = "Stocks", x = "TimeStamp", y = "Normalized Price")                +
  scale_color_manual(values=c("blue","dark grey", "dark blue"))

  y_min <- round(min(MultiData[,3]),1)
  y_max <- round(max(MultiData[,3]),1)
  y_num <- (y_max-y_min)/10

  ggsm1 <<- ggsm + theme(panel.background = element_rect(fill="white
