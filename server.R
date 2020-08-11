
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(leaflet)
library(chron)
library(data.table)
library(ggthemes)

shinyServer(function(input,output){
  
  ########################## REACTIVITY ############################################
  ########################## Enabling Dynamic selection of data ####################
  {
    
  adata_datefiltered <- reactive({
      filter(agentdaywise,Date>=as.Date(input$daterange[1])& Date<=as.Date(input$daterange[2]))})
  odata_datefiltered <- reactive({
      filter(ordersdaywise,CREATED.DATE>=as.Date(input$daterange[1])& CREATED.DATE<=as.Date(input$daterange[2]))
  })
  
  rush_stats_daywise_datefiltered <- reactive({
      filter(rush_stats_daywise,CREATED.DATE>=as.Date(input$daterange[1])& CREATED.DATE<=as.Date(input$daterange[2]))
  })
  
  xpress_stats_daywise_datefiltered <- reactive({
    filter(xpress_stats_daywise,CREATED.DATE>=as.Date(input$daterange[1])& CREATED.DATE<=as.Date(input$daterange[2]))
  })
  
  latelogin_agents_datefiltered <- reactive({
    filter(latelogin_agents,Date>=as.Date(input$daterange[1])& Date<=as.Date(input$daterange[2]))
  })
  
  earlylogoff_agents_datefiltered <- reactive({
    filter(earlylogoff_agents,Date>=as.Date(input$daterange[1])& Date<=as.Date(input$daterange[2]))
  })
  
  dailyadata <- reactive({
    filter(adata,Date==as.Date(input$dailydate))
  })
  
  Attendance_datefiltered <- reactive({
    filter(adata,Date>=as.Date(input$daterange[1])& Date<=as.Date(input$daterange[2]))
  })
  
  dailyodata <- reactive({
    filter(ordersdaywise,CREATED.DATE==as.Date(input$dailydate))
  })
  
  rush_data_datefiltered <- reactive({
    filter(rush_data,CREATED.DATE==as.Date(input$dailydate))
  })
  
  currentstat <- reactive({
    filter(odata,CREATED.DATE==as.Date(input$dailydate)) 
  })
  
  splitdata <- reactive({
    filter(merger,CREATED.DATE==as.Date(input$dailydate))
  })
  
  daydata <- reactive({
    filter(agentdaywise,Date==as.Date(input$dailydate))
  })
  
  dailyagents <- reactive({
    filter(agentslist,Date==as.Date(input$dailydate))
  })
  
  df <- reactive({
          d1[d1$Name!=input$agnets,]
  })
  
  
  
  }
  ##################################OUTPUTS################################
  
  output$orderspagent <- renderPlot({
    ggplot(adata_datefiltered(),aes(x=Date))+
      geom_line(aes(y=o_a),size=1.25,alpha=0.7)+
      ggtitle("ORDERS PER AGENT")+labs(x="DATE",y="ORDERS")+
      theme_few()+theme(plot.title = element_text(hjust = 0.5,face="bold"))
  })
  
  output$revenuepagent <- renderPlot({
    ggplot(adata_datefiltered(),aes(x=Date))+
      geom_line(aes(y=r_a),color="grey",size=1.25,alpha=1)+
      ggtitle("REVENUE PER AGENT")+labs(x="DATE",y="REVENUE")+
      theme_few()+theme(plot.title = element_text(hjust = 0.5,face="bold"))
  })
  
  output$revenue_order_trend <- renderPlot({
    ggplot(adata_datefiltered(),aes(x=Date))+
      geom_bar(aes(y=Revenue,color="Revenue"),fill="slategray4",size=1.5,stat = "identity")+
      geom_line(aes(y=TotalDrops*100,color="Deliveries"),size=1.5,alpha=0.75)+
      scale_y_continuous("Revenue",sec.axis =sec_axis(~(./100),name="Delivered") )+
        ggtitle("REVENUE Vs ORDERS")+theme_few()+
        theme(plot.title = element_text(hjust = 0.5,face="bold"))+
        scale_color_manual("",values=c("mediumvioletred","slategray4"))+theme(legend.position = "bottom")      
  })
  
  output$rush_express_trend <- renderPlot({
    ggplot(adata_datefiltered(),aes(x=Date))+
      geom_line(aes(y=Rush,color="Rush"),size=1.25,alpha=0.55)+
      geom_line(aes(y=Xpr,color="Xpress"),size=1.25,alpha=0.55)+
      ggtitle("RUSH-XPRESS")+ theme(plot.title = element_text(hjust = 0.5,face="bold"))+
      scale_color_manual("",values=c("green","orange"))+theme(legend.position = "bottom") 
  })
  
  output$standard_orders <- renderPlot({
    ggplot(adata_datefiltered(),aes(x=Date,y=Stand))+geom_bar(fill="grey",stat = "identity")+
      ggtitle("STANDARD TREND")+
      labs(x="DATE",y="ORDERS")+theme(plot.title = element_text(hjust = 0.5,face="bold"))
  })
  
  output$cancellations <- renderPlot({
    
    ggplot(odata_datefiltered(),aes(x=CREATED.DATE,y=CANCELLED,fill=ORDER.URGENCY.TYPE),alpha=0.65)+
      geom_bar(stat = "identity",position = "dodge",width=0.5)+ylim(0,90)+
      ggtitle("CANCELLATIONS")+theme_few()+theme(legend.position = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))
  })
  
  output$fleet_trend <- renderPlot({
    ggplot(adata_datefiltered(),aes(x=Date,y=FLEET))+
      geom_bar(stat="identity",width = 0.25,fill="wheat4")+
      ggtitle("FLEET")+theme(plot.title = element_text(hjust = 0.5,face="bold"))
  })
  
  output$rushbreach <- renderPlot({
    rush <- odata_datefiltered() %>% filter(ORDER.URGENCY.TYPE=="RUSH")
    ggplot(rush,aes(x=CREATED.DATE))+
      geom_bar(aes(y=BREACH),stat = 'identity',width = 0.5)+
      ggtitle("RUSH BREACH")+labs(x="Date",y="Breach")+
      theme(plot.title = element_text(hjust = 0,face="bold"))
  })
  
  output$rushreach <- renderPlot({
    rush <- odata_datefiltered() %>% filter(ORDER.URGENCY.TYPE=="RUSH") 
    ggplot(rush,aes(x=CREATED.DATE))+
      geom_line(aes(y=REACH),size=1.25,alpha=0.75,color="steelblue")+
      ggtitle("RUSH REACH")+labs(x="Date",y="Reach Time")+
      theme(plot.title = element_text(hjust = 0,face="bold"))
    
  })
  
  output$xpressbreach <- renderPlot({
    xpress <- odata_datefiltered() %>% filter(ORDER.URGENCY.TYPE=="XPRESS")
    ggplot(xpress,aes(x=CREATED.DATE))+
      geom_line(aes(y=BREACH),size = 1.5,alpha=0.65,color="rosybrown4")+
      ggtitle("XPRESS BREACH")+theme_few()+
      theme(plot.title = element_text(hjust = 0,face="bold"))
  })
  
  output$xpressreach <- renderPlot({
    xpress <- odata_datefiltered() %>% filter(ORDER.URGENCY.TYPE=="XPRESS")
    ggplot(xpress,aes(x=CREATED.DATE))+
      geom_line(aes(y=REACH),size = 1.5,alpha=0.75,color="tan")+
      ggtitle("XPRESS REACH")+theme_few()+
      theme(plot.title = element_text(hjust = 0,face="bold"))
  })
  
  output$Attendancesheet <- renderDataTable({
     Attendance_datefiltered() %>% group_by(Name) %>% 
            summarize(Present=sum(Present))
  })
  
  output$Lateloginagents <- renderDataTable({
    latelogin_agents_datefiltered()},
    options = list(autoWidth=T,columnDefs = list(list(width = '200px', targets = "_all"))
    )
    )
  
  output$earlylogoffagents <- renderDataTable({
    earlylogoff_agents_datefiltered()
  })
  
  output$agent_revenue_dist <-renderPlot({
    ggplot(dailyadata(),aes(Revenue))+
      geom_histogram(stat = "bin",fill="white",color="black")+
      ggtitle("AGENT REVENUE DISTRIBUTION")+xlim(0,2000)+
      theme(plot.title = element_text(hjust = 0,face="bold"))
  })
  
  output$rushreachscatter <- renderPlot({
    ggplot(rush_data_datefiltered(),aes(CREATED,REACH.TIME))+
      geom_point(color="turquoise4",aplha=2,size=3) +geom_hline(yintercept = 20)+ylim(c(0,150))+
      ggtitle("RUSH REACH")+labs(x="Reach Time",y="Time")+
      geom_hline(yintercept = mean(rush_data_datefiltered()$REACH.TIME,na.rm = T),color="red")+
      theme(plot.title = element_text(hjust = 0,face="bold"))
  })
  
  output$rushcyclescatter <- renderPlot({
    ggplot(rush_data_datefiltered(),aes(CREATED,FULFILLMENT.TIME))+
      geom_point(color="black",aplha=2,size=3)+ geom_hline(yintercept = 120)+
      ylim(c(0,240))+ ggtitle("RUSH CYCLE")+labs(x="Cycle Time",y="Time")+
      geom_hline(yintercept = mean(rush_data_datefiltered()$FULFILLMENT.TIME,na.rm = T),color="red")+
      theme(plot.title = element_text(hjust = 0,face="bold"))

  })
  
  output$fleet <- renderValueBox({
    valueBox(daydata()$FLEET,"FLEET",color="olive")
  })
  
  output$trev <- renderValueBox({
    valueBox(round(daydata()$Revenue),"REVENUE",color="olive")
  })
  
  output$opa <- renderValueBox({
    valueBox(round(daydata()$o_a),"ORDERS PER AGENT",color="olive")
  })
  
  output$rpa <- renderValueBox({
    valueBox(round(daydata()$r_a),"REVENUE PER AGENT",color="olive")
  })
  
  output$demand <- renderValueBox({
    valueBox(sum(dailyodata()$CREATED),"DEMAND",color="olive")
  })
  
  output$supply <- renderValueBox({
    valueBox(daydata()$TotalDrops,"SUPPLY",color="olive")
  })
  
  output$currentdaystatus <- renderDataTable({
    as.data.table(table(currentstat()$ORDER.STATUS))
    
  })
  
  output$ordersplit <- renderDataTable({
    dailyodata()
  })
  
  output$dailyagentslist <- renderDataTable({
    dailyagents()
  })
  
  output$dailybreak <- renderDataTable({
    adata_datefiltered()
  })
  
  output$list <- renderDataTable({
    df()
  })
  
})