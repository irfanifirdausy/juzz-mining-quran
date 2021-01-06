# created date: "28/11/2020"
# Text Mining  Al-Quran menggunakan R Shiny
# by : Irfani Firdausy (200605210001@student.uin-malang.ac.id) & Chilmiatus Shilfy (200605210010@student.uin-malang.ac.id)
#  ui

library(shiny)
library(shinythemes)
library(DT)
library(NLP)
library(ggplot2)
library(sqldf)
library(wordcloud)
library(dplyr)
library(stringr)
library(qdap)
library(tm)

# Define UI for application 
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  shinythemes::themeSelector(),  # <--- Theme Widget
                  #s: navbar
                  navbarPage("Quran Mining",
                             # s : menu 1 --
                             tabPanel(icon("home"),
                                      fluidRow(column(12,tags$img(src="home.jpg",width="960px",height="540px",class="img-responsive"),align="center")
                                      ),
                                      fluidRow(column(12,
                                               h2("Welcome"),
                                               hr(),
                                               uiOutput("homefile")
                                               ,align="left")
                                      )
                             ),
                             # e : menu 1 --
                             # s : menu 2 --
                             tabPanel("Juz In Quran",
                                      fluidRow(column(2,class="bg-primary",br(), 
                                        selectInput("selchoice", "Juz Choices:", 'all'),
                                        br()
                                        ),
                                          column(10, tabsetPanel(id = 'tabs1',
                                                  tabPanel("Info",  uiOutput("info"),
                                                           br(),
                                                           DT::dataTableOutput("QuranData")
                                                           ),
                                                  tabPanel("Sura & Aya", 
                                                           uiOutput("info_1"),
                                                           br(),
                                                           DT::dataTableOutput("SuraData"),
                                                           br(),
                                                           plotOutput("graphsura_rank",height = "700px"),
                                                           br(),
                                                           plotOutput("graphsura",height = "700px")),
                                                  tabPanel("Word", 
                                                           uiOutput("info_2"),
                                                           plotOutput("graphphrase"),
                                                           br(),
                                                           h3("Visualization with Word Cloud"),
                                                           
                                                           fluidRow(column(plotOutput("graphword"),width=5),
                                                                    column(plotOutput("graphword_bahasa"),width=7) 
                                                                    ),                
                                                           h3("Word Length"),
                                                           plotOutput("graphword_length")
                                                           ),
                                                  tabPanel("Letter",   
                                                           h3("Letter frequencies (in % )"),
                                                           plotOutput("graphletter")
                                                           )
                                                  ))
                                      )   

                             ),
                             # e : menu 2 --
                             # s : menu 3 --
                             tabPanel("About",
                             h3("About Us"),   
                             hr(),         
                             uiOutput("aboutfile")
                             ),
                             # e : menu 3 --
                             tags$script(HTML("var header = $('.navbar > .container-fluid');
                       header.append('<div style=\"float:right;\"><h4>Text Mining Juz In Quran</h4></div>');
                       console.log(header)"))
                  )        
                  # e: navbar
))