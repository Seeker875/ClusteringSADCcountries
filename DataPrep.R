# 
# # importing data 
# # world development indiactors
inds<-read_csv("Indicators.csv")
glimpse(inds)
# 
# #vector for sadc countries
sadcNames<-c("Zimbabwe","South Africa" ,"Angola","Botswana", "Lesotho",  "Malawi"
             ,"Madagascar", "Mauritius" ,"Mozambique","Namibia", "Seychelles"
              ,"Swaziland" ,"Zambia","Tanzania","Congo, Dem. Rep.")
# 
# # filtering data for sadc countries for the year 2015
sadc1 <- inds %>% 
   filter(CountryName %in% sadcNames & Year==2015 ) 

# #spreading data
sadc<- sadc1%>%select(1,4,6) %>% spread(IndicatorCode,Value)
# 
# # checking names of indicators to be kept
inds %>% group_by(IndicatorCode,IndicatorName) %>% summarise(n())
# 
# 
 sadcVarNames<- sadc1%>%select(1,3,6) %>% spread(IndicatorName,Value)
# 
sadcInds<-sadcVarNames %>% select(1,6,9,12,15)
# 
# 
# 
# 
# # importing world population data
pop <- read_csv("PopEst.csv")
# # cleaning and filtering data
 sadcPop15 <- pop %>% select(1,3,`2015 [YR2015]`) %>% 
   filter(`Country Name` %in% sadcNames  ) %>% spread(2,3) 
# 
# names(sadcPop15)
# 
# #main data for sadc pop
 sadcPop15f<- sadcPop15 %>% select(1,99,162,164,167,173,174)
# 
# 
# # importing edu data
 edu<-read_csv("edu.csv")
# #cleaning and filtering
 edu1 <- edu %>% select(1,3,"2015") %>% 
   filter(`Country Name` %in% sadcNames ) %>% spread(2,3) 
# 
# #slecting only adult literacy rate
sadcEdu15 <- edu1 %>% select(1,22)
# 
 sadcEdu15$CountryName<-sadcEdu15$`Country Name`
 sadcPop15f$CountryName<-sadcPop15f$`Country Name`
# #joining dataframes to get final dataset
 sadc15<-sadcInds %>% left_join(sadcEdu15,by = "CountryName") %>% left_join(sadcPop15f,by = "CountryName")
# 
sadc15<-sadc15 %>% select(c(1,4,5,7,9,10))
write.csv(sadc15,"sadc15.csv")


