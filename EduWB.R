#loading required libraries
library(ggplot2)
library(tidyverse)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(sf)
library(dplyr)
library(leaflet)
library(htmltools)
library(rnaturalearth)
library(leaflet.extras)
library(tmap)
library(RColorBrewer)
library(cartography)
library(mapproj)
library(spatial)
library(gganimate)
library(viridis)
library(hrbrthemes)
library(plotly)
library(htmlwidgets)

#setting the directory
setwd<-("E:\\Symbiosis\\Semester 2\\Programming for Spatial Sciences\\Education_West_Bengal")
#loading the shapefile
wb<-readOGR(choose.files(caption="Slect Shapefile",multi=FALSE))
#loading the  datasets
datwb<-read.csv("E:\\Symbiosis\\Semester 2\\Programming for Spatial Sciences\\Education_West_Bengal\\Education_West_Bengal.csv")
edumalefemale<-read.csv("E:\\Symbiosis\\Semester 2\\Programming for Spatial Sciences\\Education_West_Bengal\\Education by sex.csv")

#viewing the data tables
View(datwb)
View(wb)
View(edumalefemale)
#changing the column name
colnames(datwb)[1]<-"District.Code"

#merging the csv files with the shapefile using the "District.Code" column
edu<-merge(wb,datwb,by.x="district_c",by.y="District.Code")
malefemale<-merge(wb,edumalefemale,by.x="district_c",by.y="District.Code")

#viewing the merged files
View(edu)
View(malefemale)

#map of total population using leaflet (choropleth)
mybins<-c(0,2,4,6,8,10,Inf)
mypalette<-colorBin( palette="viridis", domain=malefemale$Total.Population/1000000,
                     na.color="transparent", bins=mybins)
mytext<-paste("District: ", malefemale$District,"<br/>", "Population (in millions): ",
              round(malefemale$Total.Population/1000000, 2), sep="") %>% lapply(htmltools::HTML)
leaflet(malefemale) %>% addTiles()  %>%
  addPolygons(fillColor=~mypalette(Total.Population/1000000), stroke=TRUE, fillOpacity=0.9, color="white", weight=0.3,
              label=mytext, labelOptions=labelOptions(style=list("font-weight"="normal", padding="3px 8px"),
              textsize = "13px", direction = "auto")) %>%
  addLegend(pal=mypalette, values=~malefemale$Total.Population/1000000,
            opacity=0.9, title="Population (in  millions)", position="bottomright" )%>%
  addResetMapButton()

#1
#view total, male, female population on map
tmap_mode("view")
tm_shape(malefemale)+tm_fill("Total.Population", palette = "Blues", id="District",
                             popup.vars=c("Total.Population","Total.Males","Total.Females"),interactive=TRUE)+
  tm_legend(outside=TRUE)+tm_layout(frame=FALSE)+tm_borders()+tm_minimap()

#2
#view literates total, male, female
tm_shape(malefemale)+tm_fill("Total.Literate", palette = "Reds", id="District",
                             popup.vars=c("Total.Literate","Male.Literate","Female.Literate"),interactive=TRUE)+
  tm_legend(outside=TRUE)+tm_layout(frame=FALSE)+tm_borders()

#3
#view illiterates total, male, female
tm_shape(malefemale)+tm_fill("Total.Illiterate", palette = "Greens", id="District",
                             popup.vars=c("Total.Illiterate","Male.Illiterate","Female.Illiterate"),interactive=TRUE)+
  tm_legend(outside=TRUE)+tm_layout(frame=FALSE)+tm_borders()

#4
#comparing total, literate, illiterate by total, male and female
#finding sum of the values in required columns
totpop<-sum(malefemale$Total.Population)
totmale<-sum(malefemale$Total.Males)
totfemale<-sum(malefemale$Total.Females)
totilt<-sum(malefemale$Total.Illiterate)
totmaleilt<-sum(malefemale$Male.Illiterate)
totfemaleilt<-sum(malefemale$Female.Illiterate)
totlit<-sum(malefemale$Total.Literate)
totmalelit<-sum(malefemale$Male.Literate)
totfemalelit<-sum(malefemale$Female.Literate)
#combining all the values into one matrix
litlevel<-rbind(c(totpop/1000000, totlit/1000000, totilt/1000000),
                c( totmale/1000000, totmalelit/1000000, totmaleilt/1000000),
                c(totfemale/1000000, totfemalelit/1000000,  totfemaleilt/1000000))
#view the matrix
litlevel
#create barplot
barplot(litlevel, beside = TRUE, col = c("darkblue", "forestgreen","gold"),
        main="Literacy according to sex",
        names=c("Total","Literate","Illiterate"), ylab="Frequency in millions",
        cex.axis=0.75, cex.names=1,ylim=c(0,100), legend.text=c("Total Population","Male","Female"),
        args.legend=list(cex=0.6,x="topright",bty="o"))

#5
#show number of primary schools
tmap_mode("plot")
tm_shape(edu)+tm_fill("Number.of.Primary.Schools",palette="Reds")+tm_layout(legend.outside=0.5)+tm_borders()
#6
#show number of upper primary schools
tmap_mode("plot")
tm_shape(edu)+tm_fill("Number.of.Upper.Primary.Schools",palette="Purples")+tm_layout(legend.outside=0.5)+tm_borders()
#7
#comparing total number of upper primary and primary schools
#creating vectors for primary and upper primary schools per district
totalup<-c(Nadia=544, Dakshin_Dinajpur=163, Murshidabad =648, Kolkata=597, Maldah =343, South_Twenty_Four_Parganas=773, Darjiling =128, Jalpaiguri =367, Uttar_Dinajpur=270, Purba_Medinipur=694, Haora =587, North_Twenty_Four_Parganas=996, Birbhum=461, Barddhaman =895, Bankura =701, Puruliya=468, Koch_Bihar =324, Paschim_Medinipur=840, Hugli =664)
totalp<-c(Nadia=2808, Dakshin_Dinajpur=1213, Murshidabad =3189, Kolkata=1590, Maldah =1902, South_Twenty_Four_Parganas=3717, Darjiling =786, Jalpaiguri =2062, Uttar_Dinajpur=1458, Purba_Medinipur=3259, Haora =2234, North_Twenty_Four_Parganas=3646, Birbhum=2387, Barddhaman =4036, Bankura =3532, Puruliya=2995, Koch_Bihar =1925, Paschim_Medinipur=4705, Hugli =3131)
#merging the columns
priup<-rbind(totalp,totalup)
#view the matrix
priup
#create barplot
barplot(priup, beside = TRUE,
        col = c("darkturquoise", "darkslategray"),
        main="Total number of upper-primary schools comapred to total number of primary schools district-wise",
        xlab="Districts", ylab="Frequency",ylim=c(0,5000),
        rownames=barvalues, cex.axis=0.75, cex.names=0.4,
        legend.text=c("Primary Schools","Upper Primary Schools"), args.legend=list(cex=0.6,x="topleft",bty="n"))

#8
#types of buildings on pie chart
#finding sum of values in required columns
sumpucca<-sum(datwb$Pucca.Building)
sumpartpucca<-sum(datwb$Partially.Pucca.Building)
sumkuccha<-sum(datwb$Kuchha.Building)
sumtent<-sum(datwb$Tent)
summult<-sum(datwb$Multiple.Types.of.Buildings)
sumno<-sum(datwb$No.building)
#storing the values in a vector
pievalues <- c(sumpucca,sumpartpucca,sumkuccha,summult,sumno,sumtent)
#view the values
pievalues
#creating a palette
mypal<-brewer.pal(6,"Set1")
#creating pie chart
pie(pievalues,labels=paste(c("Pucca","Partially Pucca","Kuccha","Multiple Types","No Building","Tent"),"Buidings=",
                           c(sumpucca,sumpartpucca,sumkuccha,summult,sumno,sumtent)), border="black", col=mypal,
    cex=.7, radius=1, main="Pie Chart showing Types of School Buildings in West Bengal")

#9
#comparing DISE specified numbers and actual numbers
#creating vectors for necessary columns
dise34<-c(Nadia=10184, Dakshin_Dinajpur=1427, Murshidabad =16362, Kolkata=5214, Maldah =9681, South_Twenty_Four_Parganas=16222, Darjiling =3457, Jalpaiguri =7694, Uttar_Dinajpur=7660, Purba_Medinipur=8714, Haora =8477, North_Twenty_Four_Parganas=13601, Birbhum=7741, Barddhaman =14129, Bankura =8391, Puruliya=8309, Koch_Bihar =6798, Paschim_Medinipur=10739, Hugli =9716)
shortdise34<-c(Nadia=594, Dakshin_Dinajpur=-1102, Murshidabad=3110, Kolkata=114, Maldah=1861, South_Twenty_Four_Parganas=4018, Darjiling=-277, Jalpaiguri =399, Uttar_Dinajpur=2088, Purba_Medinipur=-395, Haora =692, North_Twenty_Four_Parganas=1240, Birbhum=99, Barddhaman =1197, Bankura =-594, Puruliya=1931, Koch_Bihar =999, Paschim_Medinipur=-2401, Hugli =-483)
#finding the current scenario from the required numbers and shortages
current<-dise34-shortdise34
#view the data
current
#creating a matrix using the data
barvalues<-rbind(dise34,current)
#view the data
barvalues
#creating barchart
barplot(barvalues, beside = TRUE,
        col = c("dodgerblue3", "skyblue1"),
        main="Total number of teachers required as per DISE PTR=34 compared to current number of teachers present district-wise",
        xlab="Districts", ylab="Frequency",ylim=c(0,20000),
        rownames=barvalues, cex.axis=0.75, cex.names=0.4,
        legend.text=c("Required","Current"), args.legend=list(cex=0.6,x="topright",bty="o"))

#10
#comparing total number of teachers as per RTE to present number of teachers
#creating vectors for necessary columns
rte<-c(Nadia=12990, Dakshin_Dinajpur=2322, Murshidabad =18990, Kolkata=7095, Maldah =11322, South_Twenty_Four_Parganas=18370, Darjiling =4684, Jalpaiguri =10190, Uttar_Dinajpur=8580, Purba_Medinipur=12684, Haora =10535, North_Twenty_Four_Parganas=18110, Birbhum=9488, Barddhaman =16004, Bankura =10389, Puruliya=11944, Koch_Bihar =9110, Paschim_Medinipur=14016, Hugli =11988)
shortrte<-c(Nadia=3400, Dakshin_Dinajpur=-207, Murshidabad =5728, Kolkata=1995, Maldah =3502, South_Twenty_Four_Parganas=6166, Darjiling =950, Jalpaiguri =2895, Uttar_Dinajpur=3008, Purba_Medinipur=3575, Haora =2750, North_Twenty_Four_Parganas=5749, Birbhum=1846, Barddhaman =3072, Bankura =1404, Puruliya=5566, Koch_Bihar =3311, Paschim_Medinipur=876, Hugli =1789)
#finding the current scenario from the required numbers and shortages
presentrte<-rte-shortrte
#creating matrix of the data
rtevalues<-rbind(rte,presentrte)
#view the data
rtevalues
#creating barchart
barplot(rtevalues, beside = TRUE,
        col = c("darkgreen", "green3"),
        main="Total number of teachers as per RTE as comapared to the numbers present district-wise",
        xlab="Districts", ylab="Frequency",ylim=c(0,20000),
        rownames=barvalues, cex.axis=0.75, cex.names=0.4,
        legend.text=c("Required","Present"), args.legend=list(cex=0.6,x="topright",bty="o"))

#11
#Total graduates in descending order
#setting a start point
set.seed(1000)
#creating a data frame to store the columns
data<-data.frame(x=malefemale$District, y=malefemale$Total.Graduate.and.above)
#creating a lollipop plot
data<-data %>% arrange(y) %>% mutate(x=factor(x,x))
ggplot(data, aes(x=x, y=y))+geom_segment(aes(x=x, xend=x, y=0, yend=y), color= "black", size= 0.7)+
  geom_point(color="orange", size=5)+theme_ipsum()+coord_flip()+theme(legend.position="none")+
  xlab("")+ylab("Number of Graduates")+ggtitle("Number of Graduates in each District in descending order")

#12
#Total male graduates in descending order
#setting a start point
set.seed(1000)
#creating a data frame to store the columns
datamale<-data.frame(x=malefemale$District, y=malefemale$Male.Graduate.and.above)
#creating lollipop plot
datamale<-datamale %>% arrange(y) %>% mutate(x=factor(x,x))
ggplot(datamale, aes(x=x, y=y))+geom_segment(aes(x=x, xend=x, y=0, yend=y), color= "black", size= 0.7)+
  geom_point(color="purple", size=5)+theme_ipsum()+coord_flip()+theme(legend.position="none")+
  xlab("")+ylab("Number of Male Graduates")+ggtitle("Number of Male Graduates in each District in descending order")

#13
#Total female graduates in descending order
#setting a start point
set.seed(1000)
#creating a data frame to store the columns
datafemale<-data.frame(x=malefemale$District, y=malefemale$Female.Graduate.and.above)
#creating lollipop plot
datafemale<-datafemale %>% arrange(y) %>% mutate(x=factor(x,x))
ggplot(datafemale, aes(x=x, y=y))+geom_segment(aes(x=x, xend=x, y=0, yend=y), color= "black", size= 0.7)+
  geom_point(color="cornflowerblue", size=5)+theme_ipsum()+coord_flip()+theme(legend.position="none")+
  xlab("")+ylab("Number of Female Graduates")+ggtitle("Number of Female Graduates in each District in descending order")

#14
#male,female literacy level below primary with points showing population in a bubble scatter graph
#size of bubble is the total population the district
fig<-plot_ly(edumalefemale, x = ~Female.Below.Primary/100000, y = ~Male.Below.Primary/100000,
             type = 'scatter', mode = 'markers', size = ~Total.Below.Primary/100000,
             color = ~District, colors = 'Paired', sizes = c(10, 100), text="(in millions)",
             marker = list(opacity = 0.5, sizemode = 'diameter'))
fig %>% layout(title = 'Number of people with literacy level Below Primary',
               xaxis = list(title="Female (in lakhs)",showgrid = TRUE),
               yaxis = list(title="Male (in lakhs)",showgrid = TRUE),
               showlegend = TRUE)

#15
#comparing different literacy levels as per sex
#finding sum of necessary columns
totbp<-sum(malefemale$Total.Below.Primary)
totmalebp<-sum(malefemale$Male.Below.Primary)
totfemalebp<-sum(malefemale$Female.Below.Primary)
totp<-sum(malefemale$Total.Primary)
totmalep<-sum(malefemale$Male.Primary)
totfemalep<-sum(malefemale$Female.Primary)
totm<-sum(malefemale$Total.Middle)
totmalem<-sum(malefemale$Male.Middle)
totfemalem<-sum(malefemale$Female.Middle)
totms<-sum(malefemale$Total.Matric.or.Secondary)
totmalems<-sum(malefemale$Male.Matric.or.Secondary)
totfemalems<-sum(malefemale$Female.Matric.or.Secondary)
toths<-sum(malefemale$Total.Higher.Secondary)
totmalehs<-sum(malefemale$Male.Higher.Secondary)
totfemalehs<-sum(malefemale$Female.Higher.Secondary)
totg<-sum(malefemale$Total.Graduate.and.above)
totmaleg<-sum(malefemale$Male.Graduate.and.above)
totfemaleg<-sum(malefemale$Female.Graduate.and.above)
#creating a matrix of the data
lev<-rbind(c(totbp/100000, totmalebp/100000, totfemalebp/100000),
           c(totp/100000, totmalep/100000,  totfemalep/100000),
           c(totm/100000, totmalem/100000, totfemalem/100000),
           c(totms/100000, totmalems/100000, totfemalems/100000),
           c(toths/100000, totmalehs/100000, totfemalehs/100000),
           c(totg/100000, totmaleg/100000, totfemaleg/100000))
#view the matrix
lev
#create a barplot
barplot(lev, beside = TRUE,
        col = c("darkgreen", "mediumseagreen","mediumorchid4","cornflowerblue","chocolate","slategray"),
        main="Different education levels as per sex", names=c("Total","Male","Female"), ylab="Frequency (in lakhs)",
        cex.axis=0.75, cex.names=1,ylim=c(0,200),
        legend.text=c("Below Primary","Primary","Middle","Matric / Secondary","Higher Secondary","Graduate and above"),
        args.legend=list(cex=0.7,x="topright",bty="o",title="Education Level"))
