# Shiny Web Application for Data on Healthcare Workers that died from Coronavirus
# Disclaimer: This code comes with no guarantees.
# Purpose: Track healthcare worker deaths related to coronavirus (COVID-19)
# Data sources: news, links in spreadsheet
# Website: https://jontheepi.shinyapps.io/hcwcoronavirus/
# e-mail:hcwcoronavirus@gmail.com
# Last updated on 4/12/2020

# Loads libraries. use install.packages() first.
library(shiny)
library(leaflet)
library(magrittr)
library(tidyverse)
library(dplyr)
library(DT) # For Data Tables.
library(shinyWidgets)
library(forecast)
library(ggplot2)
library(plotly)
library(summarytools)
library(xtable)

# Define User Interface
ui <- fluidPage(

    # Show Map and Table
    mainPanel(width="100%",
          
               
   navbarPage("Healthcare worker deaths from novel Coronavirus (COVID-19) in the US",
          
        tabPanel("Map",h4("62 reported deaths as of 4/12/2020 9 PM", style = "color:#ff0000"),
                 h4("From news reports. An underestimate. Mapped location is work/facility location. Unknown locations are placed at state capitol or nearest city."),
                 h4("In some cases, the source of transmission (work vs. community) may be unclear."),
                 leafletOutput("map",width="100%",height="500px"),
                 h4("Notes: Only reported deaths, does not include illnesses and hospitalizations. Does not include other frontline and critical workers. Also included are other types of staff that worked in an affected facility. Does not include non-working retirees."),
                 h4("US deaths:",a("Medpagetoday",href="https://www.medpagetoday.com/infectiousdisease/covid19/85867")),
                 h4("EMS deaths:: ",a("EMS1.com",href="https://www.ems1.com/coronavirus-covid-19/articles/covid-19-ems-deaths-jk5zWFziwYVYUaM4/")),
                 h4("UK NHS deaths:",a("The Telegraph",href="https://www.telegraph.co.uk/news/2020/04/07/nhs-staff-died-coronavirus-frontline-workers-victims/")),
                 h4("E-mail: ",a("hcwcoronavirus@gmail.com",href="mailto:hcwcoronavirus@gmail.com")),
                 h4("Code and data for this project is on GitHub",a("https://github.com/jontheepi/hcwcoronavirus",href="https://github.com/jontheepi/hcwcoronavirus"))
        ),
        tabPanel("Attribute Table",dataTableOutput("table"),style = "font-size:90%"),
        tabPanel("Time series",plotOutput("time")),
        #tabPanel("Age", plotOutput("age")),
        tabPanel("By Worker Type",plotOutput("type")),
        tabPanel("By Age",plotOutput("age")),
        tabPanel("By Sex",plotOutput("sex")),
        tabPanel("Summary Statistics",uiOutput("stats"))
        #"tabPanel("State")
            )
       
    
        #downloadButton("downloadData", "Download")
      
    )
   )
  


# Server
server <- function(input, output) {
  mapdata<-read.csv("HCWs Died from Coronavirus - Data.csv",header=TRUE,sep=",")
  map_label<-paste0("Report Date: ",mapdata$Report_Date,"<br>","Age: ",mapdata$Age,"<br>","Sex: ",mapdata$Sex,"<br>","Type: ",mapdata$Type,"<br>","Specifics: ",mapdata$Specifics,"<br>","Facility: ",mapdata$Facility,"<br>","City: ",mapdata$City,"<br>","State: ",mapdata$State,"<br>","Notes: ",mapdata$Notes,"<br>","Military: ",mapdata$Military,"<br>",paste0("<a href=",  mapdata$Source, " target='_blank'>Link to source</a>"))
  
  # Map output
   output$map <- renderLeaflet({
     leaflet(data=mapdata)%>%addTiles()%>%setView(lat=38,lng=-90,zoom=4)%>%
       addMarkers(lat=~Latitude,lng=~Longitude,popup=map_label,clusterOptions = markerClusterOptions(maxClusterRadius = 10))
       #addMiniMap()
   })
   
   # Attribute Table output
   output$table<-renderDataTable({
     tabledata<-mapdata[,c(-2,-3,-6,-9,-11,-17,-18,-20)]
     #mapdata<-mapdata[!is.na(mapdata$latitude) | !is.na(mapdata$longitude),]
     #mapdata<-mapdata[mapdata$Primary.Cause==input$select,]
     tabledata$Source<-paste0("<a href=", tabledata$Source, " target='_blank'>Link to article</a>")
     DT::datatable(tabledata,escape=FALSE,filter="top")
     })
   
   # Time series plot
   output$time<-renderPlot({
     timedata<-mapdata
     timedata$Report_Date<-as.Date(timedata$Report_Date,"%m/%d/%Y")
     timedata$cnt<-1
     timedata<-timedata[,c(1,22)]
     time_agg<-aggregate(timedata$cnt,by=list(timedata$Report_Date),sum)
     temporal<-ggplot(time_agg, aes(x = Group.1, y = x))+  geom_bar(stat="identity")+scale_x_date(date_labels = "%m/%d/%Y", date_breaks = "1 day") +
       scale_y_discrete(limits=c(0,1,2,3,4,5,6,7,8,9,10))+theme(text = element_text(size=18),axis.text.x = element_text(angle = 90))
     temporal+xlab("Day")+ylab("Number of Deaths")+ggtitle("Number of Healthcare Worker Deaths from COVID-19, by News Article Date")+
       geom_text(aes(label=x),vjust=-1,size=6)
   })
   
   # HCW Type
     output$type<-renderPlot({
       aged<-mapdata
       aged$cnt<-1
       aged<-aged[,c(7,22)]
       aged2<-aggregate(x=aged$cnt,FUN=sum,by=list(aged$Type))
       aged2<- aged2[order(-aged2$x),]
       ageplot<-ggplot(data=aged2,aes(x=reorder(Group.1,-x),y=x))+geom_bar(stat="identity")
       ageplot+xlab("Type")+ylab("Number of Deaths")+ggtitle("Deaths from COVID-19, by Healthcare Worker Type")+theme(text = element_text(size=18),axis.text.x = element_text(angle = 90))+ ylim(0,30)+
         geom_text(aes(label=x),vjust=-1,size=6)
     })
     
     # Age histogram
     age_data<-mapdata
     age_data<-subset(age_data,age_data$Age>=0)
     
     output$age<-renderPlot({
       qplot(age_data$Age, geom="histogram",binwidth=5,fill=age_data$Type)+xlab("Age")+
         ylab("Number of Deaths")+ggtitle("Histogram of HCW Deaths from COVID-19, by Age and Type")+
         theme(text = element_text(size=20))+ylim(0,15)+scale_fill_discrete(name = "Type of HCW")
         
     })
     
    # Sex barchart
     output$sex<-renderPlot({
       sex<-mapdata
       sex$cnt<-1
       sex_agg<-aggregate(sex$cnt,by=list(sex$Sex),FUN=sum)
       sex_agg$Group.1=as.character(sex_agg$Group.1)
       sex_agg$Group.1[sex_agg$Group.1==""]<-"Missing"
       ggplot(sex_agg,aes(x=Group.1,y=x))+geom_bar(stat="identity")+
         xlab("Sex")+ylab("Number of Deaths")+ggtitle("HCW Deaths from COVID-19, by Sex")+theme(text = element_text(size=20))+ylim(0,30)+scale_fill_discrete(name = "Sex")+
         geom_text(aes(label=x),vjust=-1,size=6)
     })
     

     # Summary statistics
     #state<-summarytools::freq(mapdata$State,order="freq")
     #age<-summarytools::describe(mapdata$Age)
     allvariables<- mapdata %>% dplyr::select(Age,Sex,Type,Facility, City,State,Zipcode)
     overview<-dfSummary(allvariables,max.distinct.values = 50)
     output$stats<-renderUI({
       view(overview,
            method = 'render',
            omit.headings = TRUE, # not FALSE
            bootstrap.css = FALSE,footnote=NA)
     #xtable(new)
      #dfSummary(mapdata,bootstrap.css=FALSE,plain.ascii = FALSE,method="render",g
      # freq(iris$Species, plain.ascii = FALSE, style = "rmarkdown",bootstrap.css=TRUE,display.labels = TRUE,raph.magnif = 0.8))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# End of Program