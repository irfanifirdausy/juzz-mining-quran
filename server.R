# created date: "28/11/2020"
# Text Mining  Al-Quran menggunakan R Shiny
# by : Irfani Firdausy (200605210001@student.uin-malang.ac.id) & Chilmiatus Shilfy (200605210010@student.uin-malang.ac.id)
#  define function and serve 

library(shiny)
library(shinythemes)
library(DT)
library(NLP)
library(ggplot2)
library(sqldf)
library(wordcloud2)
library(dplyr)
library(stringr)
library(qdap)
library(tm)

#set lang to arabic
#Sys.setlocale("LC_CTYPE","arabic")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

#read data from txt
q = read.csv("www/quran-simple-clean.txt", header=F, stringsAsFactor=F, encoding="UTF-8", sep="|")
colnames(q) = c("sura_no", "aya_no", "text")

q1 = read.csv("www/quran_surah_name.txt", header=F, stringsAsFactor=F, encoding="UTF-8", sep="|")
colnames(q1) = c("sura_no", "sura_name","sura_bahasa", "sumof_aya")


q2 = read.csv("www/quran_juzz.txt", header=F, stringsAsFactor=F, encoding="UTF-8", sep="|")
colnames(q2) = c("juzz_no", "sura_no_start", "aya_no_start", "sura_no_end", "aya_no_end")

q3 = read.csv("www/quran-Bahasa-Indonesia.txt", quote = "", 
              row.names = NULL, header=F, stringsAsFactor=F, encoding="UTF-8", sep="|")
colnames(q3) = c("juzz_no","sura_no", "aya_no", "text_bahasa")

v_juzz_no = 1
v_start_sura = 1
v_start_aya = 1
v_end_sura = 114
v_end_aya = 6
v_start_sura_name ='Al-Fatihah'
v_end_sura_name ='An-Nas'


shinyServer(function(input, output, session) {
    values <- reactiveValues(start = "all")
    choice <- reactive({
      choice_selected <- input$selchoice
      choice_selected
    })
    
    setfreq <- reactive({
      if (choice()=='all') {
        freq = 300
      } else {
        freq = 10
      }
      freq
    })
     
    #s: select form
    observe({
       x <- list('Juz 1 - 30'='all')
       for(i in 1:30) {
         loop <- i  
         x[[paste0("Juz ", i)]] <- loop
       }
        choiceList <- x
        updateSelectInput(session, "selchoice", choices = choiceList, selected ="all")
    })
    #e: select form
    
    # s: create info content
    info_text <- reactive({
      choice <- choice()
      if (choice()=="all") {
        qdd = sqldf("select max(juzz_no) maxjuz,max(sura_no) maxsura,count(aya_no) countaya from q3")
        dsp_text = sprintf("%s Juz , %s Sura , %s Aya  ",qdd$maxjuz,qdd$maxsura,qdd$countaya)
      } else {
        qdd = fn$sqldf("select count(sura_no) count_sura,sum(sumof_aya) sum_aya from (select distinct(sura_no),count(aya_no) sumof_aya  from q3 where juzz_no='$choice' group by sura_no)")
        dsp_text = sprintf(" %s Sura, %s Aya ",qdd$count_sura,qdd$sum_aya)
      }
      dsp_text
    })
    
    info_start_end <- reactive({
      choice <- choice()
      if (choice()=="all") {
      
        dsp_text = sprintf("Start : Sura %s.%s Aya %s, End : Sura %s.%s Aya %s", v_start_sura, v_start_sura_name, v_start_aya,v_end_sura,v_end_sura_name,v_end_aya)
      } else {
        qdd = fn$sqldf("select a.sura_no_start,b.sura_name sura_name_start,a.aya_no_start,a.sura_no_end,c.sura_name sura_name_end,a.aya_no_end from q2 a 
                       left join q1 b on a.sura_no_start = b.sura_no
                       left join q1 c on a.sura_no_end = c.sura_no
                       where a.juzz_no='$choice'") 
       
        dsp_text = sprintf("Start : Sura %s.%s Aya %s, End : Sura %s.%s Aya %s",qdd$sura_no_start,qdd$sura_name_start,qdd$aya_no_start,qdd$sura_no_end,qdd$sura_name_end,qdd$aya_no_end)
      }
      dsp_text
    })
    
    output$info <- renderUI({
      choice <- choice()
      if (choice=="all") {
        dsp_info = sprintf("<h3>Al-Quran consists of : </h3> <strong>%s<strong>",info_text())
        dsp_info1 =  sprintf("<br><strong> %s <strong>",info_start_end());
      } else {
        dsp_info =  sprintf("<h3>Juz  %s consists of :</h3> <strong>%s</strong>",choice,info_text())
        dsp_info1 = sprintf("<br><strong> %s <strong>",info_start_end());
      }
     HTML(dsp_info,dsp_info1)
    })
    
    output$info_1 <- renderUI({
      choice <- choice()
      if (choice=="all") {
        dsp_info = sprintf("<h3>Al-Quran consists of : </h3> <strong>%s<strong>",info_text())
        dsp_info1 = sprintf("<br><strong> %s <strong>",info_start_end());
      } else {
        dsp_info =  sprintf("<h3>Juz  %s consists of :</h3> <strong>%s</strong>",choice,info_text())
        dsp_info1 = sprintf("<br><strong> %s <strong>",info_start_end());
      }
      HTML(dsp_info,dsp_info1)
      
    })
    
    output$info_2 <- renderUI({
      choice <- choice()
      if (choice=="all") {
        dsp_info = sprintf("<h3>Juz 1-30 : </h3> <h3>Data Visualization Most Frequent Word ( Freq > %s )</h3>",setfreq())
      } else {
        dsp_info = sprintf("<h3>Juz %s : </h3> <h3>Data Visualization Most Frequent Word ( Freq > %s )</h3>",choice,setfreq())
      } 
      HTML(dsp_info)
    })  
    # e: create info content
    
    # s: dsp all data
    qdspdata <- reactive({
      choice <- choice()
      if (choice=="all") {
        qdatas = sqldf(paste0("select c.juzz_no as Juz,a.sura_no,b.sura_name,a.aya_no,a.text,c.text_bahasa from q a 
LEFT JOIN q1 b on a.sura_no=b.sura_no
LEFT JOIN q3 c on (a.sura_no=c.sura_no AND a.aya_no=c.aya_no)

where (a.sura_no ='", v_start_sura,"' and a.aya_no>='", v_start_aya,"') 
                      UNION
                      SELECT c.juzz_no as Juz,a.sura_no,b.sura_name,a.aya_no,a.text,c.text_bahasa FROM q a 
                      LEFT JOIN q1 b on a.sura_no=b.sura_no
                      LEFT JOIN q3 c on (a.sura_no=c.sura_no AND a.aya_no=c.aya_no)
                      where  (a.sura_no >'", v_start_sura,"' and a.sura_no <'", v_end_sura,"')
                      UNION
                      SELECT c.juzz_no as Juz,a.sura_no,b.sura_name,a.aya_no,a.text,c.text_bahasa FROM q a 
                      LEFT JOIN q1 b on a.sura_no=b.sura_no
                      LEFT JOIN q3 c on (a.sura_no=c.sura_no AND a.aya_no=c.aya_no)
                      where  a.sura_no='", v_end_sura,"' and a.aya_no<='", v_end_aya,"'
                      "))      
        
        
      } else {
        qgetjuzz = fn$sqldf("select juzz_no,sura_no_start,aya_no_start,sura_no_end,aya_no_end from q2 where juzz_no='$choice'") 
        v_juzz_no = qgetjuzz$`juzz_no`
        v_start_sura = qgetjuzz$`sura_no_start`
        v_start_aya = qgetjuzz$`aya_no_start`
        v_end_sura = qgetjuzz$`sura_no_end`
        v_end_aya = qgetjuzz$`aya_no_end`
        
        if (v_start_sura!=v_end_sura) {
        qdatas = sqldf(paste0("select '", v_juzz_no,"' as Juz,a.sura_no,b.sura_name,a.aya_no,a.text,c.text_bahasa from q a 
LEFT JOIN q1 b on a.sura_no=b.sura_no
LEFT JOIN q3 c on (a.sura_no=c.sura_no AND a.aya_no=c.aya_no)
where (a.sura_no ='", v_start_sura,"' and a.aya_no>='", v_start_aya,"') 
                      UNION
                      SELECT '", v_juzz_no,"' as Juz,a.sura_no,b.sura_name,a.aya_no,a.text,c.text_bahasa FROM q a 
                      LEFT  JOIN q1 b on a.sura_no=b.sura_no
                      LEFT JOIN q3 c on (a.sura_no=c.sura_no AND a.aya_no=c.aya_no)
                      where  (a.sura_no >'", v_start_sura,"' and a.sura_no <'", v_end_sura,"')
                      UNION
                      SELECT '", v_juzz_no,"' as Juz,a.sura_no,b.sura_name,a.aya_no,a.text,c.text_bahasa FROM q a 
                      LEFT JOIN q1 b on a.sura_no=b.sura_no
                      LEFT JOIN q3 c on (a.sura_no=c.sura_no AND a.aya_no=c.aya_no)
                      where  a.sura_no='", v_end_sura,"' and a.aya_no<='", v_end_aya,"'
                      "))   
        } else {
          qdatas = sqldf(paste0("select '", v_juzz_no,"' as Juz,a.sura_no,b.sura_name,a.aya_no,a.text,c.text_bahasa from q a 
LEFT JOIN q1 b on a.sura_no=b.sura_no
LEFT JOIN q3 c on (a.sura_no=c.sura_no AND a.aya_no=c.aya_no)
where (a.sura_no ='", v_start_sura,"') and (a.aya_no>='", v_start_aya,"'and a.aya_no<='", v_end_aya,"')")) 
        }
        
      }
      qdatas    
    }) 
    
    output$QuranData <- DT::renderDataTable(
      DT::datatable({
        qdspdata() 
      },
      options = list(lengthMenu=list(c(10,20,50),c('10','20','50')),pageLength=10,searchHighlight = TRUE,order = list(1, 'asc'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'lightblue', 'color': '1c1b1b'});",
                       "}"),
                     autowidth = TRUE,
                     #columnDefs=list(list(className='dt-center',targets="_all")),
                     columnDefs = list(list(targets=c(0), visible=TRUE, width='5%'),
                                       list(targets=c(1), visible=TRUE, width='5%'),
                                       list(targets=c(2), visible=TRUE, width='10%'),
                                       list(targets=c(3), visible=TRUE, width='5%'),
                                       list(targets=c(4), visible=TRUE, width='35%'),
                                       list(targets=c(5), visible=TRUE, width='40%'),
                                       list(className='dt-center',targets="_all")
                     )
      ),      
      filter = "top",
      selection = 'multiple',
      style = 'bootstrap',
      class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c("Juz No","Sura No","Sura Name","Aya No","Text","Text Bahasa")
      ))
    # e: dsp all data
    
   #-- s: create sura & aya info 
    #define query
    query_juz <- reactive({
      choice <- choice()
      if (choice=="all") {
     
        qjuz = sqldf(paste0("select a.sura_no,count(a.aya_no) sumof_aya from q a 
where (a.sura_no ='", v_start_sura,"' and a.aya_no>='", v_start_aya,"')  GROUP BY a.sura_no
                      UNION
                      SELECT a.sura_no,count(a.aya_no) sumof_aya FROM q a 
                      where  (a.sura_no >'", v_start_sura,"' and a.sura_no <'", v_end_sura,"') GROUP BY a.sura_no
                      UNION
                      SELECT a.sura_no,count(a.aya_no) sumof_aya FROM q a 
                      where  a.sura_no='", v_end_sura,"' and a.aya_no<='", v_end_aya,"' GROUP BY a.sura_no
                      "))      
        
      } else {
        qgetjuzz = fn$sqldf("select juzz_no,sura_no_start,aya_no_start,sura_no_end,aya_no_end from q2 where juzz_no='$choice'") 
        v_juzz_no = qgetjuzz$`juzz_no`
        v_start_sura = qgetjuzz$`sura_no_start`
        v_start_aya = qgetjuzz$`aya_no_start`
        v_end_sura = qgetjuzz$`sura_no_end`
        v_end_aya = qgetjuzz$`aya_no_end`
        
        if (v_start_sura!=v_end_sura) {
          qjuz = sqldf(paste0("select a.sura_no,count(a.aya_no) sumof_aya from q a 
where (a.sura_no ='", v_start_sura,"' and a.aya_no>='", v_start_aya,"') GROUP BY a.sura_no
                      UNION
                      SELECT a.sura_no,count(a.aya_no) sumof_aya FROM q a 
                      where  (a.sura_no >'", v_start_sura,"' and a.sura_no <'", v_end_sura,"') GROUP BY a.sura_no
                      UNION
                      SELECT a.sura_no,count(a.aya_no) sumof_aya FROM q a 
                      where  a.sura_no='", v_end_sura,"' and a.aya_no<='", v_end_aya,"' GROUP BY a.sura_no
                      "))   
        } else {
          qjuz = sqldf(paste0("select a.sura_no,count(a.aya_no) sumof_aya from q a 
          where (a.sura_no ='", v_start_sura,"') and (a.aya_no>='", v_start_aya,"'and a.aya_no<='", v_end_aya,"') GROUP BY a.sura_no")) 
        }
        
      }
      qjuz       
    })
    
    #define query with name
    query_juz_name <- reactive({
      choice <- choice()
      if (choice()=="all") {
      
        qj_name = sqldf(paste0("SELECT a.sura_no,b.sura_name,b.sumof_aya sumof_aya FROM q a 
LEFT JOIN q1 b on a.sura_no=b.sura_no
where (a.sura_no ='", v_start_sura,"' and a.aya_no>='", v_start_aya,"')  GROUP BY a.sura_no
                      UNION
                 SELECT a.sura_no,b.sura_name,b.sumof_aya sumof_aya FROM q a 
LEFT JOIN q1 b on a.sura_no=b.sura_no
 where  (a.sura_no >'", v_start_sura,"' and a.sura_no <'", v_end_sura,"') GROUP BY a.sura_no
                     UNION
                  SELECT a.sura_no,b.sura_name,b.sumof_aya sumof_aya FROM q a 
                  LEFT JOIN q1 b on a.sura_no=b.sura_no
                      where a.sura_no='", v_end_sura,"' and a.aya_no<='", v_end_aya,"' GROUP BY a.sura_no
                      "))
      } else {
        qgetjuzz = fn$sqldf("select juzz_no,sura_no_start,aya_no_start,sura_no_end,aya_no_end from q2 where juzz_no='$choice'") 
        v_juzz_no = qgetjuzz$`juzz_no`
        v_start_sura = qgetjuzz$`sura_no_start`
        v_start_aya = qgetjuzz$`aya_no_start`
        v_end_sura = qgetjuzz$`sura_no_end`
        v_end_aya = qgetjuzz$`aya_no_end`
        if (v_start_sura!=v_end_sura) {
        qj_name = sqldf(paste0("SELECT a.sura_no,b.sura_name,count(a.aya_no) sumof_aya FROM q a 
LEFT JOIN q1 b on a.sura_no=b.sura_no
where (a.sura_no ='", v_start_sura,"' and a.aya_no>='", v_start_aya,"')  GROUP BY a.sura_no
                      UNION
                 SELECT a.sura_no,b.sura_name,count(a.aya_no) sumof_aya FROM q a 
LEFT JOIN q1 b on a.sura_no=b.sura_no
 where  (a.sura_no >'", v_start_sura,"' and a.sura_no <'", v_end_sura,"') GROUP BY a.sura_no
                     UNION
                  SELECT a.sura_no,b.sura_name,count(a.aya_no) sumof_aya FROM q a 
                  LEFT JOIN q1 b on a.sura_no=b.sura_no
                      where a.sura_no='", v_end_sura,"' and a.aya_no<='", v_end_aya,"' GROUP BY a.sura_no
                      "))
        } else {
          qj_name = sqldf(paste0("select a.sura_no,b.sura_name,count(a.aya_no) sumof_aya from q a 
          LEFT JOIN q1 b on a.sura_no=b.sura_no
          where (a.sura_no ='", v_start_sura,"') and (a.aya_no>='", v_start_aya,"'and a.aya_no<='", v_end_aya,"') GROUP BY a.sura_no"))  
        }
      }
      qj_name       
    })
    
    output$SuraData <- DT::renderDataTable(
      DT::datatable({
        query_juz_name() 
      },
      options = list(lengthMenu=list(c(10,20,50),c('10','20','50')),pageLength=10,searchHighlight = TRUE,order = list(0, 'asc'),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'lightblue', 'color': '1c1b1b'});",
                       "}"),
                     columnDefs=list(list(className='dt-center',targets="_all"))
      ),
      filter = "top",
      selection = 'multiple',
      style = 'bootstrap',
      class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c("Sura No","Sura Name","Sum of Aya")
      ))
    
   # s: histogram plot      
    output$graphsura_rank <- renderPlot({
     
       theme_set(theme_grey(base_size=10))
       g = ggplot(query_juz(), aes(x = reorder(sura_no,sumof_aya), y= sumof_aya, fill = sumof_aya))
       g + geom_bar(stat = "identity") + coord_flip() + ggtitle("Histogram") + xlab("Sura No") + ylab("Sum Of Aya") +
       theme(plot.title = element_text(size = 22, face = "bold", family = "Helvetica"), axis.title=element_text(face="bold", size=18, color="black")) 
    })
    
    #s: lolipop plot 
    output$graphsura <- renderPlot({
        qj_name <-  query_juz_name() 
        ggplot(qj_name, aes(x=sura_name, y=sumof_aya)) +
           
            ggtitle("Lollipop Bar") + xlab("Sura No") + ylab("Sum Of Aya") + 
            geom_segment( aes(x=reorder(sura_name,sumof_aya), xend=sura_name, y=0, yend=sumof_aya), color="skyblue") +
            geom_point( color="blue", size=4, alpha=0.6) +
            
            theme_light() +
            coord_flip() +
            theme(
                #panel.grid.major.y = element_blank(),
                #panel.border = element_blank(),
                #axis.ticks.y = element_blank()
              plot.title = element_text(size = 22, face = "bold", family = "Helvetica"), 
              axis.title=element_text(face="bold", size=18, color="black")
            ) 
    })
    
    #-- e: create sura & aya info 
    
    #-- s: create phrase
    qphrase <- reactive({
      choice <- choice()
      if (choice=="all") {
        qjuz_text = sqldf(paste0("SELECT a.text FROM q a where (a.sura_no ='", v_start_sura,"' and a.aya_no>='", v_start_aya,"')  
                      UNION
                 SELECT a.text FROM q a where  (a.sura_no >'", v_start_sura,"' and a.sura_no <'", v_end_sura,"') 
                     UNION
                  SELECT a.text FROM q a where a.sura_no='", v_end_sura,"' and a.aya_no<='", v_end_aya,"'"))  
      } else {
        qgetjuzz = fn$sqldf("select juzz_no,sura_no_start,aya_no_start,sura_no_end,aya_no_end from q2 where juzz_no='$choice'") 
        v_juzz_no = qgetjuzz$`juzz_no`
        v_start_sura = qgetjuzz$`sura_no_start`
        v_start_aya = qgetjuzz$`aya_no_start`
        v_end_sura = qgetjuzz$`sura_no_end`
        v_end_aya = qgetjuzz$`aya_no_end`
        if (v_start_sura!=v_end_sura) { 
      qjuz_text = sqldf(paste0("SELECT a.text FROM q a where (a.sura_no ='", v_start_sura,"' and a.aya_no>='", v_start_aya,"')  
                      UNION
                 SELECT a.text FROM q a where  (a.sura_no >'", v_start_sura,"' and a.sura_no <'", v_end_sura,"') 
                     UNION
                  SELECT a.text FROM q a where a.sura_no='", v_end_sura,"' and a.aya_no<='", v_end_aya,"'"))  
        } else {
          qjuz_text = sqldf(paste0("select a.text from q a 
          where (a.sura_no ='", v_start_sura,"') and (a.aya_no>='", v_start_aya,"'and a.aya_no<='", v_end_aya,"')"))   
        }
      }
     
      qCorpus = Corpus(VectorSource(qjuz_text))
      qTerms = DocumentTermMatrix(qCorpus)
     
     qTerms
    })
    
    qphrase_bahasa <- reactive({
      choice <- choice()
      if (choice=="all") {
        
        qjuz_text = sqldf(paste0("SELECT a.text_bahasa FROM q3 a where (a.sura_no ='", v_start_sura,"' and a.aya_no>='", v_start_aya,"')  
                      UNION
                 SELECT a.text_bahasa FROM q3 a where  (a.sura_no >'", v_start_sura,"' and a.sura_no <'", v_end_sura,"') 
                     UNION
                  SELECT a.text_bahasa FROM q3 a where a.sura_no='", v_end_sura,"' and a.aya_no<='", v_end_aya,"'"))  
      } else {
        qgetjuzz = fn$sqldf("select juzz_no,sura_no_start,aya_no_start,sura_no_end,aya_no_end from q2 where juzz_no='$choice'") 
        v_juzz_no = qgetjuzz$`juzz_no`
        v_start_sura = qgetjuzz$`sura_no_start`
        v_start_aya = qgetjuzz$`aya_no_start`
        v_end_sura = qgetjuzz$`sura_no_end`
        v_end_aya = qgetjuzz$`aya_no_end`
        if (v_start_sura!=v_end_sura) { 
          qjuz_text = sqldf(paste0("SELECT a.text_bahasa FROM q3 a where (a.sura_no ='", v_start_sura,"' and a.aya_no>='", v_start_aya,"')  
                      UNION
                 SELECT a.text_bahasa FROM q3 a where  (a.sura_no >'", v_start_sura,"' and a.sura_no <'", v_end_sura,"') 
                     UNION
                  SELECT a.text_bahasa FROM q3 a where a.sura_no='", v_end_sura,"' and a.aya_no<='", v_end_aya,"'"))  
        } else {
          qjuz_text = sqldf(paste0("select a.text_bahasa from q3 a 
          where (a.sura_no ='", v_start_sura,"') and (a.aya_no>='", v_start_aya,"'and a.aya_no<='", v_end_aya,"')"))   
        }
      }
      
      qCorpus = Corpus(VectorSource(qjuz_text))
      qTerms = DocumentTermMatrix(qCorpus)
      qTerms
    })
    
    output$graphphrase<- renderPlot({
      qTerms <-  qphrase()
      findFreqTerms(qTerms,10)
      freq = sort(colSums(as.matrix(qTerms)),decreasing = T)
     
      wf = data.frame(word=names(freq), freq=freq)
      # mengambil kata2 yg muncul lebih dari 10
      wfplot = subset(wf,freq>setfreq())
      theme_set(theme_grey(base_size=10))
      ggplot(wfplot, aes(word, freq)) +
        geom_bar(stat="identity")+
        theme(axis.text.x=element_text(angle=45, hjust = 1)) +
      theme(plot.title = element_text(size = 22, face = "bold", family = "Helvetica"), axis.title=element_text(face="bold", size=18, color="black")) 
    })
    
    output$graphword<- renderPlot({
     
      qTerms <-  qphrase()
      findFreqTerms(qTerms,10)
      
      freq = sort(colSums(as.matrix(qTerms)),decreasing = T)
      wordcloud(words = names(freq), freq = freq, min.freq = 1,
                max.words=100, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    })
    
    output$graphword_bahasa<- renderPlot({
      
      qTerms <- qphrase_bahasa()
      findFreqTerms(qTerms,10)
      
      freq = sort(colSums(as.matrix(qTerms)),decreasing = T)
      wordcloud(words = names(freq), freq = freq, min.freq = 1,
                max.words=100, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    })
    
    output$graphword_length<- renderPlot({
      qTerms <-  qphrase()
      words = as.matrix(colnames(qTerms))
      wLen = data.frame(nletters=nchar(words))
      theme_set(theme_grey(base_size=10))
      ggplot(wLen, aes(x=nletters))+
        geom_histogram(binwidth=1) +
        geom_vline(xintercept=mean(nchar(words)),
                   colour="green", size=1, alpha=.5)+
        labs(x="Number of Letters", y="Number of Words")
    })
    #-- e: create phrase
    
    #-- s: create letter
    qletter <- reactive({
      qTerms <-  qphrase()
      words = as.matrix(colnames(qTerms))
      letter = str_split(words,"")
      letter=sapply(letter, function(x) x[-1])
      letter = unlist(letter)
      letter = dist_tab(letter)
      letterMutate = mutate(letter,Letter=factor(letter$interval, levels=letter$interval[order(letter$freq)]))
      letterMutate
    })
    
    output$graphletter<- renderPlot({
      letterMutate <-qletter()
      theme_set(theme_grey(base_size=10)) 
      ggplot(letterMutate, aes(letterMutate$Letter, weight=percent)) +
        geom_bar()+
        coord_flip()+
        ylab("Proportion")+
        xlab("Letter")+
        scale_y_continuous(breaks=seq(0,12,2),
                           label=function(x) paste0(x,"%"),
                           expand=c(0,0), limits=c(0,12))+
        theme(plot.title = element_text(size = 22, face = "bold", family = "Helvetica"), axis.title=element_text(face="bold", size=18, color="black")) 
    })   
    #-- e: create letter
    
    output$homefile <- renderUI({
      includeHTML("www/home.html")
     })
    
    output$aboutfile <- renderUI({
      includeHTML("www/about.html")
    })
})