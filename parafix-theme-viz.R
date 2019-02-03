install.packages("install.load")
library("install.load")
install_load("ggplot2","ggthemes","RColorBrewer","ggalt","ggExtra","ggcorrplot")

theme_parafix <- theme(
  plot.title = element_text(colour=c("#000000"),face="bold",size=rel(1.5),hjust = 0.95,vjust=-20),
  plot.subtitle = element_text(colour=c("#888888"),face="plain",size=rel(1.2),hjust = 0.95,vjust=-20),
  plot.caption = element_text(colour=c("#999999"),face="plain",size=rel(1),hjust = 0.95,vjust=165),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  axis.title.x = element_text(colour=c("#333333")),
  axis.title.y = element_text(colour=c("#333333")),
  axis.text.x = element_text(colour=c("#888888")),
  axis.text.y = element_text(colour=c("#888888")),
  axis.line = element_line(colour=c("#000000")),
  axis.ticks = element_line(colour=c("#000000")),
  legend.title = element_text(colour=c("#333333"),face="plain",size=rel(1.2),hjust = -0.65),
  legend.text = element_text(colour=c("#888888")),
  legend.position = "right",
  legend.box = "vertical"
)

options(scipen=999)  # turn-off scientific notation like 1e+48

# http://www.sthda.com/english/wiki/be-awesome-in-ggplot2-a-practical-guide-to-be-highly-effective-r-software-and-data-visualization
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Scatterplot

#SIMPLE SCATTERPLOT
ggplot(data = mtcars, aes(x = disp, y = mpg)) +
  geom_point(aes(colour = mpg, size=mpg)) +
  scale_fill_manual(values = brewer.pal(n = 9, name = "Blues")) +
  geom_smooth(method="loess", se=FALSE,colour=c("#000000")) +
  labs(title= "title",subtitle = "subtitle",caption = "Caption", y="Y-as", x = "X-as", colour="mile per gallon", size="per mile per gallon") +
  theme_parafix


#SIMPLE SCATTERPLOT + ENCIRCLING
ggplot(data = mtcars, aes(x = disp, y = mpg)) +
  geom_point(aes(colour = mpg, size=mpg)) +
  scale_fill_manual(values = brewer.pal(n = 9, name = "Blues")) +
  geom_smooth(method="loess", se=FALSE,colour=c("#000000")) +
  geom_encircle(aes(x=disp, y=mpg),
                data=mtcars,
                color=c("#FF0000"),
                size=2,
                expand=0.08) +
  labs(title= "title",subtitle = "subtitle",caption = "Caption", y="Y-as", x = "X-as", colour="mile per gallon", size="per mile per gallon") +
  theme_parafix

#SIMPLE JITTERPLOT
ggplot(data = mtcars, aes(x = disp, y = mpg)) +
  geom_jitter(width = .5, size=1, colour=c("#FF0000")) +
  geom_smooth(method="loess", se=FALSE,colour=c("#000000")) +
  labs(title= "title",subtitle = "subtitle",caption = "Caption", y="Y-as", x = "X-as", colour="mile per gallon", size="per mile per gallon") +
  theme_parafix

#COUNTS PLOT
ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_count(col=c("#FF0000"), show.legend = F) +
  labs(title= "title",subtitle = "subtitle",caption = "Caption", y="Y-as", x = "X-as", colour="mile per gallon", size="per mile per gallon") +
  theme_parafix

#MARGINAL HISTOGRAM
mpg_select <- mpg[mpg$hwy >= 35 & mpg$cty > 27, ]
g <- ggplot(mpg, aes(x=cty,y=hwy)) +
  geom_count(col=c("#FF0000"), show.legend = F) +
  geom_smooth(method="lm", se=FALSE,colour=c("#000000"))
ggMarginal(g, type = "histogram", fill="transparent")

#CORRELATION
corr <- round(cor(mtcars), 1)
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method="circle",
           colors = brewer.pal(n = 3, name = "Blues"),
           title="Correlogram of mtcars",
           ggtheme=theme_parafix)
