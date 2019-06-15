# install.packages("tidyverse", dependencies = TRUE)
install.packages ("car", dependencies = TRUE)

library(ggplot2)
library(tidyverse)
library(reshape)

attach(iris)

grafico1 = ggplot(data = iris, aes(x=Species, y=Petal.Width)) + geom_bar(stat = "identity", width = 0.4, fill="dark blue") + 
geom_text(aes(label=Petal.Width), hjust=-2, size=5, color = "black", nudge_x = 0, family = "Calibri") + text(x=Species, y=Petal.Width, labels = as.character(Petal.Width))+theme_minimal()

dados_mailing = readxl::read_excel(file.choose())

dados_mailing = dados_mailing %>% mutate(penetracao = (Mailing/total_elegivel)*100)

dados_mailing = dados_mailing %>% rename(total_elegivel = `Total Elegivel`)

grafico_penetracao = ggplot2::ggplot(data = dados_mailing, aes(x = Mailing, y = total_elegivel, fill=factor(mounth))) + geom_bar(stat = "identity") + 
geom_text(aes(label=total_elegivel), stat = "identity", position = position_stack())

grafico_penetracao = ggplot2::ggplot(data = dados_mailing, aes(x = Mailing, y = total_elegivel)) + geom_bar(stat = "identity", fill="dark blue", color="orange") + 
  geom_text(aes(label=total_elegivel), stat = "identity", position = position_stack())


# For large datasets with overplotting the alpha
# aesthetic will make the points more transparent
df <- data.frame(x = rnorm(5000), y = rnorm(5000))
h  <- ggplot(df, aes(x,y))
h + geom_point()
h + geom_point(alpha = 0.5)
h + geom_point(alpha = 1/10)

# Alpha can also be used to add shading
j <- h + geom_line()
windows()
grafico_penetracao + geom_line()

library(car)

windows()
grap = scatterplotMatrix(~Petal.Length + Petal.Width +  
                    
                    Sepal.Length + Sepal.Width | Species,                  
                  
                  data = iris, 
                  
                  main = paste("Scatterplot Matrix for Iris Data", 
                               
                               "Using the \"car\" Package"))
b <- ggplot(economics, aes(x = date, y = unemploy))
b
j <- b + geom_line()
j
yrng <- range(economics$unemploy)
j <- j + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = party),
                   ymin = yrng[1], ymax = yrng[2], data = presidential)
j
j + scale_fill_manual(values = alpha(c("blue", "red"), .3))


graph1 = base + geom_line()

meses = format(ISOdate(2004,1:12,1),"%B")

dados_mailing = dados_mailing %>% mutate(nome_meses = meses)

dados_mailing = dados_mailing %>% mutate(dados_mailing[1:12,4] = as.Date(dados_mailing$mounth, "%b %y"))

date_reference = dados_mailing[1:12,1] = as.Date(as.POSIXct(dados_mailing$mounth, 'GMT'))


windows()

dados_mailing = readxl::read_excel(file.choose())

dados_mailing = dados_mailing %>% mutate(date_reference = date_reference)


windows()

base  = ggplot(dados_mailing, aes(x=date_reference, y=Mailing)) + scale_x_date(date_labels = "%B", date_breaks = "1 month")

base + geom_line()

y_range = range(dados_mailing$Mailing)

start = start > dados_mailing$Mailing[1,1]

interv = subset(dados_mailing, start > dados_mailing$date_reference[1])

base + geom_rect(aes(NULL, NULL, xmin = -Inf, xmax = Inf, fill = date_reference), ymin = -Inf, ymax = Inf, data = dados_mailing)

base

yrng <- range(economics$unemploy)
j <- j + geom_rect(aes(NULL, NULL, xmin = , xmax = end, fill = party),
                   ymin = yrng[1], ymax = yrng[2], data = presidential)



teste = dados_mailing$date_reference


library(ggplot2)

qplot(mounth, Mailing, data = dados_mailing, colour = date_reference)


md = median(dados_mailing$`Total Elegivel`)
qt = quantile(as.numeric(dados_mailing$`Total Elegivel`), c(0.25, 0.75), na.rm = T)
names(qt) = c("ymin", "ymax")
qt
qt+stat_summary(fun.data = "qt", geom = "ribbon")

  
data_dna = read.csv(file.choose(), header = TRUE)

dados = mutate(data_dna, date_difference = start)


plot(iris$Sepal.Length~iris$Petal.Length, ylab = "Sepal Length", xlab = "Petal Length", main = "Sepal Length X Petal Length", col="red", pch=25)

histogram(iris$Sepal.Length, main = "Sepal Length", xlab = "Petal Length")

windows()
ggplot(iris, aes(x=Sepal.Length)) + geom_histogram() + scale_x_continuous(breaks = seq(4.3, 7.9, 0.3), lim = c(4, 7.9))

max(iris$Sepal.Length)



ggplot(data= iris, aes(x = iris$Petal.Width))


windows()
ggplot(data= iris) + 
  stat_bin(aes(x = iris$Petal.Width, color=iris$Species, group=iris$Species), geom="line") +
  geom_line(aes(x = iris$Petal.Width), stat ="count")
  
  
 # geom_line(data= iris, aes(x = iris$Petal.Width)))

windows()
ggplot(data= iris) + 
  stat_bin(aes(x = iris$Petal.Width, color=iris$Species, group=iris$Species), geom="line") +
  geom_line(aes(x = iris$Petal.Width), stat ="count")





