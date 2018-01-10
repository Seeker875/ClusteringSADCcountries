#loading packages
library(tidyverse)

#Reading data
sadc15<-read_csv("sadc15.csv")
sadc15<-sadc15 %>% select(-c(1))

sadc15$CountryName<-as.factor(sadc15$CountryName)
sadc15$`Adult literacy rate, population 15+ years, both sexes (%)`<-as.double(sadc15$`Adult literacy rate, population 15+ years, both sexes (%)`)


#visualizing data

#base themes
myTheme=theme(axis.text.x = element_text(angle = 75, hjust = 1))+theme_hc()+
  theme(plot.title = element_text( size=16, face="bold.italic"))

myPlot=geom_col(width=0.5)


sadc15 %>%
  ggplot(aes(x=CountryName ,y=`Adult literacy rate, population 15+ years, both sexes (%)`))+
  myPlot+myTheme+ggtitle("Adult literacy rate of sadc countries")+labs(x="Country",y="Litearcy Rate")


sadc15 %>%
  ggplot(aes(x=CountryName ,y=`Improved water source (% of population with access)`))+
  myPlot+myTheme+ggtitle("Water accessibility ")+labs(x="Country",y="Water Access")



sadc15 %>%
  ggplot(aes(x=CountryName ,y=`Improved sanitation facilities (% of population with access)`))+
  myPlot+myTheme+ggtitle("Sanitation Facility accessibility ")+labs(x="Country",y="Sanitation facility Access")



sadc15 %>%
  ggplot(aes(x=CountryName ,y=`Mortality rate, infant (per 1,000 live births)`))+
  myPlot+myTheme+ggtitle("Mortality rate, infant ")+labs(x="Country",y="Mortality rate, infant")





sadc15 %>%
  ggplot(aes(x=CountryName ,y=`Population growth (annual %)`))+
  myPlot+myTheme+ggtitle("Population growth of SADC ")+labs(x="Country",y="Pop growth")




# clustering sadc
NormData <-  sadc15 %>% select(c(-1)) %>% scale() %>%  as.data.frame()

NormData$CountryName<-sadc15$CountryName

# Set seed
set.seed(1)
x=NormData %>% select(-c(6))
row.names(x) <- NormData$CountryName

# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 6 cluster centers
for (i in 1 : 6) {
  km.out <- kmeans(x, centers = i, nstart=20,iter.max = 50)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:6, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

k=3

km.out <- kmeans(x, centers = k, nstart = 20, iter.max = 50)

# countries in clusters
km.out$cluster




plot(x[, c(1, 2)],
     col = km.out$cluster,
     main = paste("k-means clustering of Countries with", k, "clusters"),
     xlab = "Sanitation", ylab = "Water")

#hier
hclust.out<-hclust(dist(x))

# Inspect the result
summary(hclust.out)

plot(hclust.out)
cutree(hclust.out,k=3)
