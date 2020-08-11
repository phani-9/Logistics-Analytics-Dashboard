library(DT)
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
library(shinythemes)
library(shinyWidgets)


source('Dashboard backend.R') 

shinyUI(fluidPage(setBackgroundColor("grey"),
                 
    dashboardPage(skin='black',
        dashboardHeader(title="TELYPORT EYE"),
        dashboardSidebar(setBackgroundColor("black"),
            
## System Health : Overall performance analysis of the processes. 
## SLA Monitor   : Daily trends of SLA Adherence
## AGENTS        : Agents over all performance. Inc login/logoff punctuality,attendance
## Daily Report  : Generate a report for single day. Inc basic stats
                         
                         
            sidebarMenu(
                menuItem("SYSTEM HEALTH",tabName = "systemhealth"),
                menuItem("SLA MONITOR",tabName = "slamonitor"),
                menuItem("AGENTS",
                         menuSubItem("ATTENDANCE",tabName = "attendance"),
                         menuSubItem("LATE COMERS",tabName = "latecomers"),
                         menuSubItem("EARLY SKIPPERS",tabName = "earlyskippers")),
                menuItem("DAILY REPORT",tabName = "dailyreport"),
                menuItem("RAW DATA",tabName = "rawdata")
            ),
            dateRangeInput(
                inputId = "daterange",
                label ="Select the period",
                start=min(adata$Date),
                end=max(adata$Date),
                #min=min(adata$Date),
                #max=max(adata$Date),
                format = "yyyy-mm-dd",
                separator = "-"
            )
        ),
        dashboardBody(setBackgroundColor("green"),
            tabItems(
                tabItem(tabName = "systemhealth",
                        fluidRow(
                            box(plotOutput("orderspagent"),solidHeader = T),
                            box(plotOutput("revenuepagent"),solidHeader = T)
                            
                        ),
                        fluidRow(
                            box(plotOutput("standard_orders"),solidHeader = T),
                            box(plotOutput("rush_express_trend"),solidHeader = T)
                        ),
                        fluidRow(
                            box(plotOutput("revenue_order_trend"),solidHeader = T),
                            box(plotOutput("cancellations"),solidHeader = T)
                        ),
                        fluidRow(
                            box(plotOutput("fleet_trend"),solidHeader = T)
                        )
                    
                ),
                tabItem(tabName = "slamonitor",
                        fluidRow(
                            box(plotOutput("rushbreach"),solidHeader = T),
                            box(plotOutput("rushreach"),solidHeader = T)
                        ),
                        fluidRow(
                            box(plotOutput("xpressbreach"),solidHeader = T),
                            box(plotOutput("xpressreach"),solidHeader = T)
                        )
                ),
                tabItem(tabName = "attendance",
                            title ="ATTENDANCE",
                            box(dataTableOutput('Attendancesheet'))
                ),
                tabItem(tabName = "latecomers",
                        div(style = 'overflow-x: scroll',
                            dataTableOutput('Lateloginagents'))
                            
                ),
                tabItem(tabName = "earlyskippers",div(style = 'overflow-x: scroll'),
                            dataTableOutput('earlylogoffagents')
                ),
                tabItem(tabName = "dailyreport",
                        dateInput(
                            inputId = "dailydate",
                            label ="Generate DAILY REPORT for",
                            format = "yyyy-mm-dd"
                            ),
                        fluidRow(
                            valueBoxOutput("fleet",width = 3),
                            valueBoxOutput("trev",width = 3),
                            valueBoxOutput("opa",width = 3),
                            valueBoxOutput("rpa",width = 3)
                        ),
                        fluidRow(
                            valueBoxOutput("supply",width = 3),
                            valueBoxOutput("demand",width = 3)
                        ),
                        fluidRow(
                            title = "ORDER SPLIT",dataTableOutput("ordersplit"),solidHeader = T
                        ),
                        fluidRow(
                            box(plotOutput("rushreachscatter"),solidHeader = T),
                            box(plotOutput("rushcyclescatter"),solidHeader = T)
                        ),    
                        fluidRow(
                            box(plotOutput("agent_revenue_dist")),
                            box(title = "CURRENT STATUS" ,dataTableOutput('currentdaystatus'),solidHeader = T),
                            
                        ),
                    
                        
                           
                        fluidRow(
                           dataTableOutput("dailyagentslist")  
                        )
                                    
                
            ),
                tabItem(tabName = "rawdata",
                        
                        fluidRow(
                            box(dataTableOutput("dailybreak"),title = "DAY WISE BREAKUP",width = 10)
                        )
                        )
            
        )
))))
