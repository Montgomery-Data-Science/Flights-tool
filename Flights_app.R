library(zoo)
library(shiny)
library(shinythemes)
library(dbplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(usmap)
library(stringr)
library(reshape2)
library(scales)

# Function for date slider by month
rm(list=ls())
monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

#UI Design 
ui <- fluidPage(
              tags$style('.container-fluid {background-color: #cce9f0;}'),
              titlePanel("USA Flights Viewer"),
              theme=shinytheme("cerulean"),
              fluidRow(
                column(12,splitLayout(cellWidths = c("30%", "45%","25%"),plotOutput("lineplot"),plotOutput("plot2"),plotOutput("pieplot")))
                ),
              br(),
              tags$hr(style="border-color: black;"),
              br(),
              
              fluidRow(
              column(3, sliderInput("slider", "Date Range", 
                            min = as.Date("2019-01-31"),
                            max =as.Date("2020-12-31"),
                            value=c(as.Date("2019-01-31")),
                            timeFormat="%b %Y"),
                            textOutput("SliderText")
                     ),
              column(3,selectInput("States", "Select State:",
                          append(c(unique(Summary_stats$State_Abrv)),"ALL"),
                          selected='ALL')
                     ),
              
              column(3,selectInput("AirLine", "Select Airline",
                          append(c(unique(Summary_stats$carrier_name)),"ALL"),
                          selected='ALL')
                     ),
              column(3,selectInput("FlightType", "Type",
                          c('All Flights','Delayed Flights','Cancelled flights','Diverted Flights'),
                          selected='ALL'))
                      ))
       
server <- shinyServer(function(input, output, session){
  Summary_stats<-read.csv("combined_df.csv")
  states_abrv=c(unique(Summary_stats$State_Abrv))
  append(states_abrv, "ALL")
  statesmap <-map_data("state")
  statesmap<-statesmap%>%
    mutate(region=str_to_title(region))%>%
    mutate(S_abbrv=state.abb[match(region,state.name)])
  
  airport_df<-Summary_stats%>%
    select(c("airport","State_Abrv"))%>%
    distinct()
  
  Summary_stats<-read.csv("combined_df.csv")
  states_abrv=c(unique(Summary_stats$State_Abrv))
  append(states_abrv, "ALL")
  statesmap <-map_data("state")
  statesmap<-statesmap%>%
    mutate(region=str_to_title(region))%>%
    mutate(S_abbrv=state.abb[match(region,state.name)])
  
  airport_df<-Summary_stats%>%
    select(c("airport","State_Abrv"))%>%
    distinct()
  sliderMonth <- reactiveValues()
  observe({
    full.date <- as.POSIXct(input$slider, tz="GMT")
    print(full.date)
    sliderMonth$Month <- as.character(monthStart(full.date))
  })
  output$SliderText <- renderText({sliderMonth$Month})
  
  Summary_stats<-Summary_stats%>%
    mutate(on_time_flights=arr_flights-arr_del15-arr_cancelled-arr_diverted)
  
  output$pieplot<-renderPlot({
    
    #filter summary_stats by month
    yeart=as.numeric(substr(sliderMonth$Month, 1, 4))
    montht=as.numeric(substr(sliderMonth$Month, 6, 7))
    Summary_stats=filter(Summary_stats,year==yeart & month==montht)
    date=as.Date(sliderMonth$Month)
    date=as.yearmon(date, format = "%b/%Y")
    title='Flight distribution: \n'
    title=paste(date,title,sep=' ')
    #filter summary_stats by air_carrier
    if ( input$AirLine == 'ALL') {
      Summary_stats=Summary_stats
    }
    else  {
      Summary_stats=filter(Summary_stats,carrier_name==input$AirLine)
      title=paste(title,input$AirLine,sep=' ')
    }
    
    #filter summary_stats by state
    if ( input$States == 'ALL') {
      Summary_stats=Summary_stats
    }
    else  {
      Summary_stats=filter(Summary_stats,State_Abrv == input$States)
      title=paste(title,input$States,sep=' ')
    }
    
    
    if (input$FlightType!='Delayed Flights'){
      
      agg_df<-Summary_stats%>%
        na.omit()%>%
        filter(V16>-125)%>%
        group_by(year,month)%>%
        summarise(Mean_flights=sum(on_time_flights),mean_cancel=sum(arr_cancelled),mean_delay=sum(arr_del15), 
                  mean_diverted=sum(arr_diverted))%>%
        #select(-c(year,month))%>%
        melt(id.var=c("year","month"),var.name='value')%>%
        mutate(value=value/sum(value))%>%
        mutate(labels = scales::percent(value))
      
      validate(need(nrow(agg_df) > 0, 'No data exists, please select a diffrent Category'))
      ggplot(agg_df, aes(x="", y=value, fill=variable)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        geom_text(aes(label = labels),
                  position = position_stack(vjust = 0.8))+
        scale_fill_manual(values=c("#66C2A5", "#FC8D62", "#8DA0CB","#E78AC3"), 
                          name="Flight Type Distribution",
                          labels=c("On Time Flights", "Canceled Flights", "Delayed Flights","Diverted Flights"))+
        labs(x='distribution',
             y='',
             title=title)+ 
        theme(legend.position = c(0.8, 0.2),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank())
    }
    else{
      title=paste("Delayed flight reasons in",title,sep=' ')
      agg_df<-Summary_stats%>%
        na.omit()%>%
        filter(V16>-125)%>%
        group_by(year,month)%>%
        summarise(carrier_ct=sum(carrier_ct),weather_ct=sum(weather_ct),nas_ct=sum(nas_ct),
                  secuirty_ct=sum(security_ct),late_aircraft_ct=sum(late_aircraft_ct))%>%
        #select(-c(year,month))%>%
        melt(id.var=c("year","month"),var.name='value')%>%
        mutate(value=value/sum(value))%>%
        mutate(labels = scales::percent(value))
      
      ggplot(agg_df, aes(x="", y=value, fill=variable)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0)+
        geom_text(aes(label = labels),
                  position = position_stack(vjust = 0.8))+
        scale_fill_manual(values=c("#8dd3c7", "#ffffb3", "#bebada","#fb8072","#80b1d3"), 
                          name="Flight Delay Reason",
                          labels=c("Carrier", "Weather", "FAA System outage","Secuirty","Late Aircraft"))+
        labs(x='distribution',
             y='',
             title=title)+ 
        theme(legend.position = c(0.8, 0.2),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank())
    }
  })
  
  #Draw Line graph
  output$lineplot<-renderPlot({
    title=paste(input$FlightType,' 2019 & 2020')
    #filter summary_stats by air_carrier
    if ( input$AirLine == 'ALL') {
      Summary_stats=Summary_stats
    }
    else  {
      Summary_stats=filter(Summary_stats,carrier_name==input$AirLine)
      title=paste(title,input$AirLine,sep=' ')
    }
    
    #filter summary_stats by state
    if ( input$States == 'ALL') {
      Summary_stats=Summary_stats
    }
    else  {
      Summary_stats=filter(Summary_stats,State_Abrv == input$States)
      title=paste(title,input$States,sep=' ')
    }
    
    agg_df<-Summary_stats%>%
      na.omit()%>%
      filter(V16>-125)%>%
      group_by(year,month)%>%
      summarise(Mean_flights=sum(arr_flights),mean_cancel=sum(arr_cancelled),mean_delay=sum(arr_del15),
                diverted=sum(arr_diverted),long=mean(V16),lat=mean(V15))
    
    #filter agg by flight type
    if ( input$FlightType == 'All Flights') {
      ftype=agg_df$Mean_flights
    } else if ( input$FlightType == 'Delayed Flights') {
      ftype=agg_df$mean_delay
    } else if ( input$FlightType == 'Cancelled flights') {
      ftype=agg_df$mean_cancel
    } else {
      ftype=agg_df$diverted
    }
    

    
    agg_df$Date <- (with(agg_df, sprintf("%d-%02d", year, month)))
    agg_df$Date <- paste0(agg_df$Date,"-01")
    agg_df$Date <- as.Date(agg_df$Date,"%Y-%m-%d")
    
    ggplot(data=agg_df,aes(x=Date,y=ftype))+
      geom_line()+ 
      scale_x_date(date_breaks = "2 month", date_labels = "%m/%y")+ 
      geom_vline(xintercept =as.Date(sliderMonth$Month) , linetype="dashed",color = "red", size=1.5)+
      labs(x='Date',
           y=input$FlightType,
           title=title)
    
  }, height = 200, width = 450)
  
  #draws USA Map
  output$plot2<-renderPlot({
    
    title=input$FlightType
    #filter summary_stats by month
    yeart=as.numeric(substr(sliderMonth$Month, 1, 4))
    montht=as.numeric(substr(sliderMonth$Month, 6, 7))
    Summary_stats=filter(Summary_stats,year==yeart & month==montht)
    date=as.Date(sliderMonth$Month)
    date=as.yearmon(date, format = "%b/%Y")
    
    title=paste(title,date,sep=' ')
    #filter summary_stats by air_carrier
    if ( input$AirLine == 'ALL') {
      Summary_stats=Summary_stats
    }
    else  {
      Summary_stats=filter(Summary_stats,carrier_name==input$AirLine)
      title=paste(title,'for',sep=' ')
      title=paste(title,input$AirLine,sep=' ')
    }
    
    
    #aggregate the summary_stats
    agg_df<-Summary_stats%>%
      na.omit()%>%
      filter(V16>-125)%>%
      group_by(airport)%>%
      summarise(Mean_flights=sum(arr_flights),mean_cancel=sum(arr_cancelled),mean_delay=sum(arr_del15),mean_diverted=sum(arr_diverted),
                long=mean(V16),lat=mean(V15))
    
    agg_df = merge(x=agg_df,y=airport_df,by="airport",all.x=TRUE)
    
    
    #filter agg by state
    if ( input$States == 'ALL') {
      statesmap=statesmap
    }
    else  {
      
      statesmap=filter(statesmap,S_abbrv %in% c(as.character(input$States)))
      agg_df=filter(agg_df,State_Abrv == input$States)
      title=paste(title,'in',sep=' ')
      title=paste(title,input$States,sep=' ')
    }
    
    #filter agg by flight type
    if ( input$FlightType == 'All Flights') {
      ftype=agg_df$Mean_flights
    } else if ( input$FlightType == 'Delayed Flights') {
      ftype=agg_df$mean_delay
    } else if ( input$FlightType == 'Cancelled flights') {
      ftype=agg_df$mean_cancel
    } else {
      ftype=agg_df$mean_diverted
    }
    if ( input$FlightType == 'All Flights') {
      color_type<- scale_color_gradient(low = '#f7fcb9', high = '#31a354')
      
    }
    else  {
      color_type<- scale_color_gradient(low = "#fb6a4a", high = "#a50f15")
    }
    
    ggplot()+
      geom_polygon(data=statesmap,
                   aes(x=long, y=lat, group=group),
                   colour='black',
                   fill=NA)+
      geom_point(data=agg_df,
                 aes(long,lat,size=ftype,color=ftype))+
      color_type+
       labs(x='',
           y='',
           title=title,
           color='Number of Flights')+ 
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            panel.grid  = element_blank(),
            panel.background = element_rect(fill = "lightblue"))+ 
      guides(size = FALSE)+ 
      theme(legend.position = c(0.90, 0.2))
    }, height = 400, width = 675)
    
  })
  
shinyApp(ui = ui, server = server)
