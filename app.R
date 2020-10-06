rm(list=ls())

library(shiny)
library(shinyjs)
library(shinythemes)
library(DT)
library(shinyBS)

library(shinyIncubator)
library(stringr)
library(RMySQL)
library(knitr)
library(xtable)
library(log4r)

loggerServer <- create.logger()
# logfile(loggerServer) <- 'logs/server.log'
level(loggerServer) <- 'INFO'

options(stringsAsFactors=F)
host<-'127.0.0.1'
version<-'v2_20200505'
lapply(dbListConnections(MySQL()), dbDisconnect)
# Create a little question mark link that shows a help popup on hover
## Function from Joe Cheng
## https://gist.github.com/jcheng5/5913297

helpPopup <- function(title, content,
                      placement = c('right', 'top', 'left', 'bottom'),
                      trigger = c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover()})"),
        tags$style(type = "text/css", ".popover{max-width:500px; position: fixed;}")
      )
    ),
    tags$a(
      href = "#", class = "popover-link",
      `data-toggle` = "popover", `data-html` = "true",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok = TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok = TRUE)[1],
      icon("question-circle")
    )
  )
}

# this is for front page loading message
appCSS <- "
#loading-content {
position: absolute;
padding: 10% 0 0 0;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

css <- HTML("a:hover { background-color: transparent !important; color: #02b6e6 !important; }
            a { /*this is tab and link color CANKUT */ color: #02b6e6; text-decoration: none; }
            li a:hover {background-color: #555;  color: white; }
            ul {list-style-type: none; margin: 0; padding: 0; width: auto; background-color: #e80a89; }")
# usage: tags$head(tags$style(css))

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: myuser  Password: mypass"),
                     br(),
                     tags$code("Username: myuser1  Password: mypass1")
                   ))
)

credentials = data.frame(
  username_id = c("myuser", "myuser1"),
  passod   = c("mypass", "mypass1"),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

###### keep logged in using cookies #####
# https://gist.github.com/calligross/e779281b500eb93ee9e42e4d72448189
if (!dir.exists('www/')) {
  dir.create('www')
}

if(!file.exists("www/js.cookie.js")){
  download.file(
    url = 'https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js',
    destfile = 'www/js.cookie.js'
  )
}

addResourcePath("js", "www")
# This would usually come from your user database. But we want to keep it simple.
password_hash <- bcrypt::hashpw('secret123') # Never store passwords as clear text
sessionid <- "OQGYIrpOvV3KnOpBSPgOhqGxz2dE5A9IpKhP6Dy2kd7xIQhLjwYzskn9mIhRAVHo" # Our random sessionid

jsCode <- '
shinyjs.getcookie = function(params) {
var cookie = Cookies.get("id");
if (typeof cookie !== "undefined") {
Shiny.onInputChange("jscookie", cookie);
} else {
var cookie = "";
Shiny.onInputChange("jscookie", cookie);
}
}
shinyjs.setcookie = function(params) {
Cookies.set("id", escape(params), { expires: 0.5 });  
Shiny.onInputChange("jscookie", params);
}
shinyjs.rmcookie = function(params) {
Cookies.remove("id");
Shiny.onInputChange("jscookie", "");
}
'


ui <- 
  tagList(
    tags$head(
      # used for cookies
      # you must copy https://raw.githubusercontent.com/js-cookie/js-cookie/master/src/js.cookie.js to www/
      tags$script(src = "js/js.cookie.js")
    ),
    useShinyjs(),
    # this is used for cookies
    extendShinyjs(text = jsCode, functions = c("getcookie","setcookie","rmcookie")),
    inlineCSS(appCSS), 
    # Loading message
    # div(
    #   id = "loading-content",
    #   h2("Welcome to CanVar-UK"),
    #   h1("LOADING ...")
    # ),
    
    # align tabs to left/right
    tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }

                           .navbar-nav > li:nth-child(3) {
                           float: right;
                           /* right: 100px; */
                           }
                           .navbar-nav > li:nth-child(4) {
                           float: right;
                           /* right: 120px; */
                           }
                           .navbar-nav > li:nth-child(5) {
                           float: right;
                           }

                          .navbar .navbar-nav {float: right; 
                           color: #ff3368; 
                           font-size: 20px; 
                          } 

                        .navbar .navbar-header {float: left; 
                           color: #ff3368; 
                           font-size: 18px; 
                          } 

                           "))),
    
    navbarPage(title="CanVar-UK", id="navbartab", theme = shinytheme("flatly"),
               collapsible = TRUE,  inverse = T, fluid=F,  windowTitle = "CanVAr-UK", 
               
               
               tabPanel(title = "About", helpText("The Cancer Variant Database is a repository of annotations for variants in cancer predisposition genes 
Classifications, both automated and curated, are derived from integration of the variant level data according to objective criteria.
This resource is currently under development and is for research use only."),
                        fluidRow(column(3, tags$a(href='https://www.icr.ac.uk/',target="_blank", tags$div(img(src = "icr_logo_round.png", class="img-responsive", width = "200px", height = "200px"))), p(" "),
                                        tags$a(href='https://www.cancerresearchuk.org/',target="_blank", tags$div(img(src = "cancerres_logo_round.png", class="img-responsive", width = "200px", height = "200px"))),p(" "),
                                        align="center"),
                                 column(6,
                                        fluidRow(column(12,tags$a(href='http://cangene-canvaruk.org/',target="_blank", tags$div(img(src = "LOGO_RGB_noStroke.png", class="img-responsive", width = "500px", height = "500px")))),align="center"),
                                        #fluidRow(column(12, offset = 0, align="center", HTML(paste("<span style=\"color:#332f80; font-weight:bold; font-size: 60px \">Can</span><span style=\"color:#e80a89;
                                        #font-weight:bold; font-size: 60px\">Gene</span><span style=\"color:black; font-weight:bold;
                                        #font-size:60px\">&#8208;</span><span style=\"color:#332f80; font-weight:bold;
                                        #font-size: 60px\">Can</span><span style=\"color:#02b6e6; font-weight:bold; font-size:60px\">Var</span>")))),
                                        fluidRow(column(12, offset = 0, align="center", p("Data Resources, Clinical and Educational Tools", style = "font-family: 'Arial'; font-size:30px; color:#02b6e6"))),
                                        fluidRow(column(12, offset = 0, align="center", p("to leverage Cancer Susceptibility Genetics for", style = "font-family: 'Arial'; font-size:30px; color:#e80a89"))),
                                        fluidRow(column(12, offset = 0, align="center", p("Early Detection and Prevention of Cancer", style = "font-family: 'Arial'; font-size:30px; color:#332f80;")))),
                                 
                                 column(3, fluidRow(align="center",
                                                    tags$a(href="https://healthdatainsight.org.uk/", target="_blank", tags$div(img(src = "HDI_Logo_round.png", class="img-responsive", width = "200px", height = "200px"))),p(" "),
                                                    tags$a(href='https://www.cam.ac.uk/',target="_blank", tags$div(img(src = "cambridge_logo_round.png", class="img-responsive", width = "200px", height = "200px"))),p(" "),
                                                    tags$a(href='https://www.ox.ac.uk/',target="_blank", tags$div(img(src = "oxford_logo_round.png", class="img-responsive", width = "200px", height = "200px"))),p(" "),
                                                    tags$a(href='https://www.manchester.ac.uk/',target="_blank", tags$div(img(src = "manchester_logo_round.png", class="img-responsive", width = "200px", height = "200px"))),p(" "),
                                                    tags$a(href="https://www.southampton.ac.uk/",target="_blank", tags$div(img(src = "southampton_logo_round.png", class="img-responsive", width = "200px", height = "200px"))),p(" "),
                                                    # tags$a(href='https://www.leeds.ac.uk/',target="_blank", tags$div(img(src = "leeds_logo_round.png", class="img-responsive", width = "200px", height = "200px"))),p(" "),
                                                    tags$a(href='https://www.sgul.ac.uk/',target="_blank", tags$div(img(src = "stgeorges_logo_round.png", class="img-responsive", width = "200px", height = "200px"))),p(" ")
                                 ))
                                 
                        )
               ),  
               
               
               tabPanel(title="Explore",  
                        
                        tags$head(tags$style(
                          HTML('
                               #searchpanel {background-color: #18bc9c;;}
                               #curation {background-color: #18bc9c;}
                               #curation {background-color: #18bc9c;}')
                        )),
                        
                        
                        absolutePanel(
                          id = "examples", class = "panel panel-default", fixed = F,
                          draggable = TRUE, top = 80, left = "auto", right = 10, bottom = "auto",
                          width = 150, height = "auto",
                          HTML('<button data-toggle="collapse" data-target="#demo">Expand/collapse examples</button>'),
                          tags$div(id = 'demo',  class="collapse",  
                                   checkboxInput(inputId = 'useExample', 
                                                 label = div("Check to load an example",helpPopup(strong("Additional Information"), "You will load \n \"BRCA1 c.5207T>C\"",
                                                                                                  placement = "left", trigger = "click")),  value = FALSE ),
                                   helpText('BRCA1 c.181T>G'),
                                   helpText('BRCA1 c.557C>A'),
                                   helpText('BRCA2 c.7994A>G'),
                                   helpText('BRCA1 p.Arg1203Ter'),
                                   helpText('BRCA1 c.5207T>C'),
                                   helpText('MLH1 c.131C>T'),
                                   helpText('MSH2 c.2500G>A')
                                   
                          )),
                        
                        
                        absolutePanel(id = "searchpanel", class = "panel panel-default",
                                      top = 80, left = 20, width = 250, fixed=F,
                                      draggable = TRUE, height = "auto",
                                      selectInput(inputId = "genetrans", label="", choices = NULL, selectize=TRUE), 
                                      uiOutput("ui1"),
                                      uiOutput("ui2"),
                                      actionButton("go", "Search",width = "100%")),
                        
                        absolutePanel(id = "curation", class = "panel panel-default",
                                      top = 400, left = 20, width = 250, fixed=F,
                                      draggable = TRUE, height = "auto",
                                      HTML('<button data-toggle="collapse" data-target="#collapseCurrator">Expand/collapse variant curation panel</button>'), 
                                      tags$div(id = 'collapseCurrator',  class="collapse",
                                               fluidRow(column(width = 12, selectInput(inputId = "curatedclass", label="", choices = c("1-Non-pathogenic",
                                                                                                                                       "2-Likely non-pathogenic",
                                                                                                                                       "3-Uncertain/review",
                                                                                                                                       "3*-Uncertain-high suspicion",
                                                                                                                                       "4-Likely pathogenic",
                                                                                                                                       "5-Pathogenic"), selectize=TRUE))),
                                               
                                               fluidRow(column(width = 12, tags$textarea(id="newNote2", rows=7, cols=90,
                                                                                         style="margin:  10px 10px 10px 10px; width: 90%;
                                                                               height: 100%; resize: both; border-radius: 5px;", "",
                                                                                         placeholder="Maximum 200 characters", maxlength="200"))),
                                               fluidRow(column(width = 12, actionButton("goSave","SAVE",width = "100%")))
                                      )),
                        
                        
                        
                        absolutePanel(id = "logo", class = "card", bottom = 20, right = 60, width = 50, fixed=TRUE, draggable = FALSE, height = 50,
                                      tags$a(href="https://www.icr.ac.uk", target="_blank",tags$div(img(src='/images/ICR.jpeg', class="img-responsive")))),
                        
                        absolutePanel(id = "logo", class = "card", bottom = 20, right = 20, width = 50, fixed=TRUE, draggable = FALSE, height = 50,
                                      actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                   onclick = sprintf("window.open('%s')",
                                                                     "https://twitter.com/intent/tweet?text=%20@CangeneCanvar%20CanVar-UK%20data%20resources&url=http://www.canvaruk.org"))),
                        
                        # helpText("-----------------------------------------------------------------------------------------------------------------------------------------------------------"),
                        fluidRow(align="left", column(width=1),column(width = 10,offset = 2,
                                                                      uiOutput('genelevel'),
                                                                      uiOutput("variantlevel")),column(width=1))
                        
                        
               ),
               
               tabPanel(uiOutput("Access"), uiOutput("body"),uiOutput("logoutbtn")),
               
               navbarMenu("Documentation",
                          tabPanel("CanVar-UK examples"),
                          tabPanel("Installation"))
               
               # tabPanel("Batch", tabsetPanel(
               #   tabPanel("Plot", plotOutput("plot")),
               #   tabPanel("Summary", verbatimTextOutput("summary")),
               #   tabPanel("Table", tableOutput("table"))
               # )
               # )
               
    )
  )


server <- function(input, output, session){
  
  # update tab browser
  # observeEvent(input$navbartab, {
  #   updateQueryString(paste0("#", input$navbartab), mode = "push")
  # })
  
  
  #   observeEvent(getUrlHash(), {
  #   tab <- gsub("^#", "", getUrlHash())
  #   print(tab)
  #   if (tab != input$navbartab & !grepl("Explore", tab)){
  #     updateTabsetPanel(session, "navbartab", tab)
  #   }else if(tab != input$navbartab & grepl("Explore", tab)){
  #     updateQueryString(queryString = paste0("?gene=",isolate(input$genetrans),"&variant=",isolate(input$variationText)), mode = "push")
  #   }
  # })
  
  
  values <- reactiveValues()
  
  values$batch <- NULL
  values$sessionid <- paste(format(Sys.time(), "%d%b%Y_%H%M%S"),sep='_')
  values$iname <- ''
  values$inputFileType <- 'simple'
  # can be 'simple' (sample_id gene variation), 
  #        'clinical' (Worksheet  InvID	DNA No	Gene	Nomenclature	Het-hom	Class	Exon)
  #     or 'VCF'- to be implemented
  values$sessionschanged <- 0
  values$filesRead <- NULL
  values$filesLoaded <- NULL
  values$lastExploreButton <- 0
  values$lastBatchButton <- 0
  values$lastParseButton <- 0
  values$lastLoadButton <- 0
  values$lastSaveButton <- 0
  values$lastReturnButton <- 0
  values$lastMarkButton <- 0
  values$lastAddNoteButton <- 0
  values$lastSaveNotesButton <- 0
  values$lastExcludeDeep <- TRUE
  values$lastExcludeExtra <- TRUE
  values$lastExcludeCommon <- FALSE
  values$lastExcludeIndels <- FALSE
  values$selectedRow <- NULL
  values$contentType <- 'front'
  values$exploreTable <- NULL
  values$selectedVar <- NULL
  values$selectedVarBaseline <- NULL
  values$selectedVarClinical <- NULL
  values$selectedVarClinicalCurr <- NULL
  values$exploreTableiDL <- 10
  values$exploreTableiDS <- 0
  values$showSaveNotes <- FALSE
  values$showNewNote <- FALSE
  values$newNoteCurr <- ''
  values$offsetLimit <- 10
  values$variantReport <- NULL
  
  ####>> addtional values
  values$showNewNoteExp <- TRUE
  values$showSaveNotesExp <- TRUE
  values$newNoteCurr2 <- ''
  values$lastloginButton <- 0
  values$lastsaveNotesButton <- 0
  values$logged <- TRUE 
  values$curator <- "unknown"
  values$lastsignButton <- 0
  values$lastcreateAccountButton <- 0
  values$addFileName <- ""
  values$dirNotes <- ""
  values$notesupdate <- ""
  values$initVar <- ""
  
  values$sources <- c('ALAMUT','EVS','TGP','TGP_PHASE3','ICR','HGMD','IARC','DMUDB','BIC','LOVD',
                      'EASTON','LINDOR','HOUDAYER','GUIDUGLI','WALKER','MUTTASTER',
                      'POLYPHEN2','DROST','BOUWMAN','UMD','CADD', 'FINDLAY', 'GUIDUGLI2018','WESSEX','REVEL','TP53_SGE', 'GAVIN','LOVD3')
  # Output for the Summary panel
  
  cigmainfo <- reactive({
    
    fixempty <- function(x) {return(ifelse(is.null(x)||is.na(x)||x==0,'',x))}
    freqprint <- function(x){return(sprintf('%5.4f',as.numeric(x)))}
    
    gene <- input$genetrans  #"BRCA1"
    variation <- input$variationText #"c.5207T>C"
    transcript <- if(gene=="BRCA1"){"ENST00000357654"}
    transcript <- if(gene=="BRCA2"){"ENST00000380152"}
    transcript <- if(gene=="MLH1"){"ENST00000231790"}
    
    info(loggerServer,paste("in cigmainfo, gene=",gene," variation=",variation,sep=""))
    i<-NULL
    i$gene <- gene
    i$transcript <- transcript
    i$variation <- variation
    dataset<-NULL
    if (nchar(gene) && nchar(variation)) {
      mydb<-dbConnect(MySQL(), user='test',password='newpassword',dbname='cigma2',host=host)
      rs<-dbSendQuery(mydb,paste(sep="",collapse="",
                                 "select m.source,m.gene,m.transcript,rtranscript,hgvs_cdna,hgvs_prot,hgvs_prot_code1,
                                 m.altname,varLocation,r.name,varType,codingEffect,hg19_chr,hg19_pos,rsID,
                                 m.cdna_pos,offset,ntwt,ntmut,codon,aawt,aamut,c.firstorlast3,m.flags,pubmed,google_search_new,google_search_old
                                 from main_stable m 
                                 left join cdna2genomic c on m.gene=c.gene and m.cdna_pos=c.cdna_pos and m.varLocation='exon'
                                 left join capparegions r on m.gene=r.gene and m.hg19_chr=r.chr and m.hg19_pos between r.exonstart and r.exonend
                                 where m.gene='",gene,"' and m.hgvs_cdna='",variation,"'"))
      dataset<-fetch(rs,-1)
      if (!is.null(dataset) && dim(dataset)[1]>0) {
        
        revelDat <- dataset[,c("hg19_chr","hg19_pos","hgvs_prot_code1")]
        
        i$rsid<-dataset[,'rsID']
        i$google_search_old<-dataset[,dim(dataset)[2]]
        dataset<-dataset[,-dim(dataset)[2]]
        i$google_search_new<-dataset[,dim(dataset)[2]]
        dataset<-dataset[,-dim(dataset)[2]]
        i$pubmed<-dataset[,dim(dataset)[2]]
        dataset<-dataset[,-dim(dataset)[2]]
        i$summary<-as.data.frame(t(dataset),stringsAsFactors=F)
        
        varsources <-strsplit(i$summary['source',],' ')[[1]]
        for (source in values$sources) {
          table <- tolower(source)
          if (source=='EVS') table <- 'esp_cappa'
          if (source=='ICR') table <- 'icr_controls'
          if (source=='HGMD') table <- 'hgmd_cappa'
          prefix <- table
          prefix <- sub('_cappa$','',prefix)
          prefix <- paste0(prefix,'_')
          
          if(source=='FINDLAY'){
            query <- paste(sep="", collapse="","select score,class,probability from Findlay where gene='",gene,
                           "' AND  hgvs_cdna='",variation,"'")
            rs <- dbSendQuery(mydb,query)
            Findlay <- fetch(rs,-1)
            if(is.na(Findlay[1,1])){ Findlay <- NULL }
            i[[tolower(source)]] <- Findlay
          }
          
          if(source=='GUIDUGLI2018'){
            
            query <- paste(sep="", collapse="","select probability,classification from guidugli2018 where gene='",gene,
                           "' AND  hgvs_cdna='",variation,"'")
            rs <- dbSendQuery(mydb,query)
            guidugli2018 <- fetch(rs,-1)
            if(is.na(guidugli2018[1,1])){ guidugli2018 <- NULL }
            i[[tolower(source)]] <- guidugli2018
          }
          
          if(source=='WESSEX'){
            query <- paste(sep = "",collapse = "", "select specimen, spliceabnormality, sampletype from WESSEX where gene='",gene,"' and hgvs_cdna='",variation,"'")
            rs<-dbSendQuery(mydb, query)
            wessex <- fetch(rs,-1)
            if(nrow(wessex)==0){ wessex <- NULL }
            i[[tolower(source)]] <- wessex
          }
          
          if(source=='REVEL'){
            query <- paste(sep = "",collapse = "", "select REVEL from REVEL where chr='",revelDat$hg19_chr,"' and hg19_pos='",revelDat$hg19_pos,
                           "' and aachange='",gsub("p.|[0-9]+","",revelDat$hgvs_prot_code1),"'")
            rs<-dbSendQuery(mydb, query)
            revel <- fetch(rs,-1)
            if(nrow(revel)==0){ revel <- NULL }
            i[[tolower(source)]] <- revel
          }
          
          if(source=='GAVIN'){
            query <- paste(sep = "",collapse = "", "select gavinClass from GAVIN  where gene='",gene,"' and hgvs_cdna='",variation,"'")
            rs<-dbSendQuery(mydb, query)
            gavin <- fetch(rs,-1)
            if(nrow(gavin)==0){ gavin <- NULL }
            i[[tolower(source)]] <- gavin
          }
          
          if(source=='LOVD3'){
            query <- paste(sep = "",collapse = "", "select times_reported from LOVD3  where gene='",gene,"' and hgvs_cdna='",variation,"'")
            rs<-dbSendQuery(mydb, query)
            LOVD3 <- fetch(rs,-1)
            if(nrow(LOVD3)==0){ LOVD3 <- NULL }
            i[[tolower(source)]] <- LOVD3
          }
          
          if(source=='TP53_SGE' && gene=="TP53"){
            query <- paste(sep = "",collapse = "", "select Combined_Model from TP53_SGE where hgvs_cdna='",variation,"'")           
            rs<-dbSendQuery(mydb, query)
            tp53sge <- fetch(rs,-1)
            if(nrow(tp53sge)==0){ tp53sge <- NULL }
            i[[tolower(source)]] <- tp53sge$Combined_Model
          }
          
          if(source %in% varsources) {
            dataset<-NULL
            rs<-dbSendQuery(mydb,paste(sep="",collapse="",
                                       "select * from ",table," where gene='",gene,"' and hgvs_cdna='",variation,"'"))
            dataset<-fetch(rs,-1)
            dataset<-as.data.frame(t(dataset),stringsAsFactors=F)
            selectedrows<-grep(prefix,rownames(dataset))
            i[[tolower(source)]]<-as.data.frame(row.names=sub(paste0('^',prefix),'',rownames(dataset)[selectedrows]),
                                                dataset[selectedrows,],stringsAsFactors=F)
            
            if (source == 'TGP') {
              rs<-dbSendQuery(mydb,paste(sep="",collapse="",
                                         "select tgp_pop,tgp_AF,tgp_alleleCount,tgp_alleleTotal,tgp_genotypeCount,tgp_genotypePopSize from tgp_pop where tgp_rsID='",i$tgp["rsID",1],"'"))
              dataset<-fetch(rs,-1)
              i$tgp_pop<-as.data.frame(dataset,stringsAsFactors=F)
              colnames(i$tgp_pop)<-sub('^tgp_','',colnames(dataset))
              i$tgp_pop<-t(i$tgp_pop[order(i$tgp_pop$genotypePopSize,decreasing=T),])
            }
            
            if (source == 'TGP_PHASE3') {
              rs<-dbSendQuery(mydb,paste(sep="",collapse="",
                                         "select tgp_phase3_pop,tgp_phase3_AF,tgp_phase3_alleleCount,tgp_phase3_alleleTotal,tgp_phase3_genotypeCount,tgp_phase3_genotypePopSize from tgp_phase3_pop where tgp_phase3_id='",i$tgp_phase3["id",1],"'"))
              dataset<-fetch(rs,-1)
              i$tgp_phase3_pop<-as.data.frame(dataset,stringsAsFactors=F)
              colnames(i$tgp_phase3_pop)<-sub('^tgp_phase3_','',colnames(dataset))
              i$tgp_phase3_pop<-t(i$tgp_phase3_pop[order(i$tgp_phase3_pop$genotypePopSize,decreasing=T),])
            }
          }
        }
      }
      rs<-dbSendQuery(mydb,paste(sep="",collapse="",
                                 "select investigator_name,
                                 clinical_cigma_class,
                                 concat(date_format(creat_date,'%d/%m/%Y'),' (',
                                 if(round(datediff(curdate(),creat_date)/7)<9,
                                 concat(round(datediff(curdate(),creat_date)/7),' weeks ago'),
                                 concat(round(datediff(curdate(),creat_date)/30.4166),' months ago')
                                 ),')') date,
                                 notes
                                 from dogma_batchlog b 
                                 where b.gene='",gene,"' and b.variation='",variation,"'
                                 order by creat_date desc"));
      dataset<-fetch(rs,-1)
      if (!is.null(dataset) && dim(dataset)[1]>0) {
        i$notes<-as.data.frame(dataset,stringsAsFactors=F)
      }
      rs<-dbSendQuery(mydb,paste(sep="",collapse="",
                                 "select cigma_class, classification_justification
                                 from dogma_classification
                                 where gene='",gene,"' and hgvs_cdna='",variation,"' and version='",version,"'"));
      dataset<-fetch(rs,-1)
      if (!is.null(dataset) && dim(dataset)[1]>0) {
        i$baseline_class<-dataset[1,1]
        i$justification<-dataset[1,2]
      }
      dbDisconnect(mydb)
    }
    return(i)
  })
  
  
  output$summaryPanel <- renderText ({
    
    fixempty <- function(x) {return(ifelse(is.null(x)||is.na(x)||x==0,'',x))}
    freqprint <- function(x){return(sprintf('%5.4f',as.numeric(x)))}
    mydb<-dbConnect(MySQL(), user='test',password='newpassword',dbname='cigma2',host=host)
    rs<-dbSendQuery(mydb,"select table_name,field_name,field_label,variant_report,variant_report_order,analysis_output,analysis_output_order from fields where online_report='Y'")
    f<-fetch(rs,-1)
    dbDisconnect(mydb)  
    fn<-split(f$field_name,f$table_name)
    fl<-split(f$field_label,f$table_name)
    # fa is a structure with the fields for the analysis output
    # the fields from the main classification and dogma_batchlog tables have to be entered manually
    fa<-f[which(f$analysis_output=='Y' & !(f$table_name %in% c('main','classification','dogma_batchlog'))),]
    rownames(fa)<-paste(gsub('tgp_phase3_pop','tgp_phase3',gsub('tgp_pop','tgp',fa[,1])),fa[,2],sep='_')
    fv<-f[which(f$variant_report=='Y' & !(f$table_name %in% c('classification','dogma_batchlog'))),]
    rownames(fv)<-paste(gsub('tgp_phase3_pop','tgp_phase3',gsub('tgp_pop','tgp',fv[,1])),fv[,2],sep='_')
    fan<-split(fa$field_name,fa$table_name)
    fvn<-split(fv$field_name,fv$table_name)
    
    # fields is a structure for the display panels
    # create list of tables with named lists of fields descriptions:
    fields<-setNames(lapply(names(fl),function(x){return(setNames(paste0(fl[[x]],':'),fn[[x]]))}),names(fl))
    
    fixempty <- function(x) {return(ifelse(is.null(x)||is.na(x)||x==0,'',x))}
    freqprint <- function(x){return(sprintf('%5.4f',as.numeric(x)))}
    
    info<-cigmainfo()
    info(loggerServer,"in summaryPanel")
    scholarlink1<-a(class="btn btn-primary",href=info$google_search_new,target="_blank","Google Scholar")
    scholarlink2<-a(class="btn btn-primary",href=info$google_search_old,target="_blank","Google Scholar (old nomen.)")
    search_new<-gsub('/scholar','/search',gsub('http://scholar.','http://www.',info$google_search_new))
    searchlink<-a(class="btn btn-primary",href=search_new,target="_blank","Google Search")
    pubmedlink <- ''
    if (!is.null(info) && !is.null(info$pubmed) && info$pubmed!=''){
      pubmedlink <- a(class="btn btn-primary",href=info$pubmed,target="_blank","HGMD Pubmed IDs")
    }
    dbsnplink <- ''
    clinvarlink <- ''
    clinvarReview <- NULL
    if (!is.null(info) && !is.na(info$rsid) && info$rsid!=''){
      dbsnplink <- a(class="btn btn-primary",href=paste('http://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs=',substr(info$rsid,3,nchar(info$rsid)),'#Diversity',sep=''),target="_blank","dbSNP")
    }
    
    mydb<-dbConnect(MySQL(), user='test',password='newpassword',dbname='cigma2',host=host)
    rs<-dbSendQuery(mydb,paste(sep="",collapse="", "select * from CLINVAR where gene='",info$gene,"' and rsID='",info$rsid,"' and hgvs_cdna='",info$variation,"'"))
    dataset<-fetch(rs,-1)
    dbDisconnect(mydb)      
    
    if(nrow(dataset)>0){
      clinvarlink <- a(class="btn btn-primary",href=paste('https://www.ncbi.nlm.nih.gov/clinvar/variation/',dataset$VariationID,sep=''),target="_blank","Clinvar")
      cStarEmpty <- HTML("<span  style=\"color:lightgrey\" <i class=\"fa fa-star\" aria-hidden=\"false\"></i> </span>")
      cStarFull <- HTML("<span  style=\"color:black\" <i class=\"fa fa-star\" aria-hidden=\"false\"></i> </span>")
      clinvarDate <- trimws(strsplit(dataset$Stars[1], split="[(]")[[1]][1], which="right")
      # cStarFull <- icon("star", lib = "font-awesome")
      
      if(length(grep("100",dataset$Stars))>0){ 
        clinvarReview <- list(p(em(paste0("Clinvar status (", clinvarDate,"): ")),strong(dataset$Class)," (",cStarFull,cStarFull,cStarFull,cStarFull,")"))
      }else if(length(grep("75",dataset$Stars))>0){ 
        clinvarReview <- list(p(em(paste0("Clinvar status (", clinvarDate,"): ")),strong(dataset$Class)," (",cStarFull,cStarFull,cStarFull, cStarEmpty,")"))
      }else if(length(grep("50",dataset$Stars))>0){
        clinvarReview <- list(p(em(paste0("Clinvar status (", clinvarDate,"): ")),strong(dataset$Class), " (",cStarFull,cStarFull,cStarEmpty,cStarEmpty,")"))
      }else if(length(grep("25",dataset$Stars))>0){
        clinvarReview <- list(p(em(paste0("Clinvar status (", clinvarDate,"): ")),strong(dataset$Class), " (",cStarFull,cStarEmpty,cStarEmpty,cStarEmpty,")"))
      }else{
        clinvarReview <- list(p(em(paste0("Clinvar status (", clinvarDate,"): ")),strong(dataset$Class), " (",cStarEmpty,cStarEmpty,cStarEmpty,cStarEmpty,")"))
      }
      
    }else{
      clinvarlink <- ""
    }
    
    info.main<-as.list(info$summary[,1])
    names(info.main)<-rownames(info$summary)
    info.main[['name']] = gsub(info.main[['gene']],'',info.main[['name']])
    info.main[['name']] = gsub('on','on ',info.main[['name']])
    info.main[['name']] = gsub('of',' of ',info.main[['name']])
    firstorlast3<-fixempty(info.main[["firstorlast3"]])
    if (firstorlast3>0) firstorlast3<-paste0('+',as.character(firstorlast3))
    rmhclass<-''
    if (!is.null(info) && !is.null(info$rmh)) rmhclass<- info$rmh['class',1]
    sources<-gsub('ALAMUT', 'INSILICO', info.main[["source"]])
    sources<-gsub(' POLYPHEN2| MUTTASTER| CADD| SUSPECT','',sources)
    l<-list(tags$table(border="0",cellspacing="5",
                       tags$tr(tags$td(em(fields[["main"]][["gene"]]),             align="right"),tags$td(strong(info.main[["gene"]])),
                               tags$td(em(fields[["main"]][["hgvs_cdna"]]),        align="right"),tags$td(strong(info.main[["hgvs_cdna"]])),
                               tags$td(em(fields[["main"]][["varLocation"]]),      align="right"),tags$td(strong(info.main[["varLocation"]]))),
                       tags$tr(tags$td(em(fields[["main"]][["transcript"]]),       align="right"),tags$td(strong(info.main[["transcript"]])),
                               tags$td(em(fields[["main"]][["hgvs_prot"]]),        align="right"),tags$td(strong(fixempty(info.main[["hgvs_prot"]]))),
                               tags$td(em(fields[["main"]][["varType"]]),          align="right"),tags$td(strong(info.main[["varType"]]))),
                       tags$tr(tags$td(em(fields[["main"]][["rtranscript"]]),      align="right"),tags$td(strong(info.main[["rtranscript"]])),
                               tags$td(em(fields[["main"]][["hgvs_prot_code1"]]),  align="right"),tags$td(strong(fixempty(info.main[["hgvs_prot_code1"]]))),
                               tags$td(em(fields[["main"]][["codingEffect"]]),     align="right"),tags$td(strong(fixempty(info.main[["codingEffect"]])))),
                       tags$tr(tags$td(em(fields[["main"]][["hg19_chr"]]),         align="right"),tags$td(strong(info.main[["hg19_chr"]])),
                               tags$td(em(fields[["main"]][["altname"]]),          align="right"),tags$td(strong(fixempty(info.main[["altname"]]))),
                               tags$td(em(fields[["main"]][["cdna_pos"]]),         align="right"),tags$td(strong(info.main[["cdna_pos"]]))),
                       tags$tr(tags$td(em(fields[["main"]][["hg19_pos"]]),         align="right"),tags$td(strong(info.main[["hg19_pos"]])),
                               tags$td(em(fields[["main"]][["rsID"]]),             align="right"),tags$td(strong(fixempty(info.main[["rsID"]]))),
                               tags$td(em(fields[["main"]][["offset"]]),           align="right"),tags$td(strong(fixempty(info.main[["offset"]])))),
                       tags$tr(tags$td(em(fields[["main"]][["ntwt"]]),             align="right"),tags$td(strong(info.main[["ntwt"]])),
                               tags$td(em("Exon/intron:"),                         align="right"),tags$td(strong(info.main[["name"]])),
                               tags$td(em(fields[["main"]][["codon"]]),            align="right"),tags$td(strong(fixempty(info.main[["codon"]])))),
                       tags$tr(tags$td(em(fields[["main"]][["ntmut"]]),            align="right"),tags$td(strong(info.main[["ntmut"]])),
                               tags$td(),tags$td(),
                               tags$td(em(fields[["main"]][["firstorlast3"]]),     align="right"),tags$td(strong(firstorlast3)))
    ),
    br(),
    p(em('Data sources:'),strong(sources)),
    br(),
    p(em('Literature links:'),scholarlink1,scholarlink2,searchlink,pubmedlink),
    p(em('Database links:'),dbsnplink,clinvarlink),
    # p(em('Flags:'),strong(fixempty(info.main[["flags"]]))),
    # p(em('RMH class:'),strong(rmhclass)),
    br()
    )
    l1<-list()
    if(!is.null(clinvarReview)){ l1<-c(l1,clinvarReview) }
    if (!is.null(info$hgmd) && info$hgmd['tag',]!='') {l1<-c(l1,list(p(em('HGMD classification:'),strong(info$hgmd['tag',]))))}
    if (!is.null(info$dmudb) && info$dmudb['interpretation',]!='') {l1<-c(l1,list(p(em('DMuDB classification:'),strong(info$dmudb['interpretation',]))))}
    if (!is.null(info$umd) && info$umd['significance',]!='') {l1<-c(l1,list(p(em('UMD classification:'),strong(info$umd['significance',]))))}
    l<-c(l,l1)
    
    return(as.character(tagList(l)))
  })
  
  # Output for the Frequency panel
  output$frequencyPanel <- renderText ({
    
    fixempty <- function(x) {return(ifelse(is.null(x)||is.na(x)||x==0,'',x))}
    freqprint <- function(x){return(sprintf('%5.4f',as.numeric(x)))}
    mydb<-dbConnect(MySQL(), user='test',password='newpassword',dbname='cigma2',host=host)
    rs<-dbSendQuery(mydb,"select table_name,field_name,field_label,variant_report,variant_report_order,analysis_output,analysis_output_order from fields where online_report='Y'")
    f<-fetch(rs,-1)
    fn<-split(f$field_name,f$table_name)
    fl<-split(f$field_label,f$table_name)
    # fa is a structure with the fields for the analysis output
    # the fields from the main classification and dogma_batchlog tables have to be entered manually
    fa<-f[which(f$analysis_output=='Y' & !(f$table_name %in% c('main','classification','dogma_batchlog'))),]
    rownames(fa)<-paste(gsub('tgp_phase3_pop','tgp_phase3',gsub('tgp_pop','tgp',fa[,1])),fa[,2],sep='_')
    fv<-f[which(f$variant_report=='Y' & !(f$table_name %in% c('classification','dogma_batchlog'))),]
    rownames(fv)<-paste(gsub('tgp_phase3_pop','tgp_phase3',gsub('tgp_pop','tgp',fv[,1])),fv[,2],sep='_')
    fan<-split(fa$field_name,fa$table_name)
    fvn<-split(fv$field_name,fv$table_name)
    
    # fields is a structure for the display panels
    # create list of tables with named lists of fields descriptions:
    fields<-setNames(lapply(names(fl),function(x){return(setNames(paste0(fl[[x]],':'),fn[[x]]))}),names(fl))
    mydb<-dbConnect(MySQL(), user='test',password='newpassword',dbname='cigma2',host=host)
    
    info<-cigmainfo()
    # info(loggerServer,"in frequencyPanel")
    l<-vector(mode="list")
    
    
    if (!is.null(info) && !is.null(info$tgp)) {
      fields.tgp<-names(fields[["tgp"]])  
      info.tgp<-as.list(info$tgp[,1])
      names(info.tgp)<-rownames(info$tgp)
      fields.tgp_pop<-names(fields[["tgp_pop"]])
      l<-c(l,list(h5(a('1000 Genome project (TGP / 1KG)',
                       
                       #                       href=paste0('http://browser.1000genomes.org/Homo_sapiens/Variation/Population?db=core;v=',info$rsid,';vdb=variation'),
                       href=paste0("http://phase3browser.1000genomes.org/Homo_sapiens/Variation/Population?db=core;r=",info$summary["hg19_chr",],":",info$summary["hg19_pos",],"-",info$summary["hg19_pos",],";v=",info$summary["rsID",],";vdb=variation;vf=9043431"),
                       
                       
                       target='_blank')),
                  tags$table(border="0",cellspacing="5",
                             lapply(fields.tgp,function(x){
                               v<-info.tgp[[x]]
                               if (grepl('AF',x)) v<-freqprint(v)
                               return(tags$tr(tags$td(em(fields[["tgp"]][[x]]),align="right"),tags$td(strong(v), align="left")))
                             }
                             )
                  ),
                  tags$table(border="1",
                             lapply(fields.tgp_pop,function(x){
                               return(tags$tr(tags$td(em(fields[["tgp_pop"]][[x]]),align="right"),
                                              lapply(info$tgp_pop[x,],function(y,fld=x){
                                                v<-y
                                                if (grepl('AF',fld)) v<-freqprint(v)
                                                if (fld=='pop'){
                                                  return(tags$td(strong(v),align="left",title=tp[[v]]))
                                                }else{
                                                  return(tags$td(strong(v),align="left"))
                                                }
                                              }
                                              )
                               )
                               )
                             }
                             )
                  )
      )
      )
    }
    if (!is.null(info) && !is.null(info$tgp_phase3)) {
      fields.tgp_phase3<-names(fields[["tgp_phase3"]])  
      info.tgp_phase3<-as.list(info$tgp_phase3[,1])
      names(info.tgp_phase3)<-rownames(info$tgp_phase3)
      fields.tgp_phase3_pop<-names(fields[["tgp_phase3_pop"]])

      l<-c(l,list(h5(a('1000 Genome project - phase3 (TGP-phase3 / 2.5KG)',
                       href=paste0("http://phase3browser.1000genomes.org/Homo_sapiens/Variation/Population?db=core;r=",info$summary["hg19_chr",],":",info$summary["hg19_pos",],"-",info$summary["hg19_pos",],";v=",info$summary["rsID",],";vdb=variation;vf=9043431"),
                       target='_blank')),
                  
                  ####
                  tags$table(border="0",cellspacing="5",
                             lapply(fields.tgp_phase3,function(x){
                               v<-info.tgp_phase3[[x]]
                               if (grepl('AF',x)) v<-freqprint(v)
                               return(tags$tr(tags$td(em(fields[["tgp_phase3"]][[x]]),align="right"),tags$td(strong(v))))
                             }
                             )
                  ),
                  tags$table(border="1",
                             lapply(fields.tgp_phase3_pop,function(x){
                               return(tags$tr(tags$td(em(fields[["tgp_phase3_pop"]][[x]]),align="right"),
                                              lapply(info$tgp_phase3_pop[x,],function(y,fld=x){
                                                v<-y
                                                if (grepl('AF',fld)) v<-freqprint(v)
                                                if (fld=='pop'){
                                                  return(tags$td(strong(v),align="left",title=tp[[v]]))
                                                }else{
                                                  return(tags$td(strong(v),align="left"))
                                                }
                                              }
                                              )
                               )
                               )
                             }
                             )
                  )
      )
      )
    }
    
    if (!is.null(info) && !is.null(info$evs)) {
      fields.evs<-names(fields[["esp"]])
      info.evs<-as.list(info$evs[,1])
      names(info.evs)<-rownames(info$evs)
      info.main<-as.list(info$summary[,1])
      names(info.main)<-rownames(info$summary)
      l<-c(l,list(h5(a('Exome Sequencing Project (ESP) / Exome Variant Server (EVS)',
                       href=paste0('http://evs.gs.washington.edu/EVS/PopStatsServlet?searchBy=chromosome&chromosome=',
                                   info.main[["hg19_chr"]],'&chromoStart=',info.main[["hg19_pos"]],
                                   '&chromoEnd=',info.main[["hg19_pos"]],'&x=0&y=0'),target='_blank')),
                  tags$table(border="0",cellspacing="5",
                             lapply(fields.evs,function(x){
                               v<-info.evs[[x]]
                               if (grepl('MAF',x)) v<-freqprint(v)
                               return(tags$tr(tags$td(em(fields[["esp"]][[x]]),align="right"),tags$td(strong(v), align="left")))
                             }
                             )
                  )
      )
      )
    }
    
    if (!is.null(info) && !is.null(info$icr) && 1==0) {
      fields.icr<-names(fields[["icr"]])  
      info.icr<-as.list(info$icr[,1])
      names(info.icr)<-rownames(info$icr)
      l<-c(l,list(h5('1958 Birth Cohort (ICR)'),
                  tags$table(border="0",cellspacing="5",
                             lapply(fields.icr,function(x){
                               return(tags$tr(tags$td(em(fields[["icr"]][[x]]),align="right"),tags$td(strong(info.icr[[x]]))))
                             }
                             )
                  )
      )
      )
    }
    if (!is.null(info) && !is.null(info$bic)) {
      fields.bic<-names(fields[["bic"]])  
      info.bic<-as.list(info$bic[,1])
      names(info.bic)<-rownames(info$bic)

            l<-c(l,list(h5(a('BIC',href='https://research.nhgri.nih.gov/bic/',target='_blank')),
                  #
                  tags$table(border="0",cellspacing="5",
                             lapply(fields.bic,function(x){
                               return(tags$tr(tags$td(em(fields[["bic"]][[x]]),align="right"),tags$td(strong(info.bic[[x]]))))
                             }
                             )
                  )
      )
      )
    }
    
    if (!is.null(info) && !is.null(info$lovd3)) {
      # fields.lovd<-names(fields[["lovd"]])
      # fields.lovd<-fields.lovd[-which(fields.lovd=='insight_class')]
      # info.lovd<-as.list(info$lovd[,1])
      # names(info.lovd)<-rownames(info$lovd)
      gene<-info$summary['gene',1]
      variant<-info$summary['hgvs_cdna',1]
      lovd_link<-'LOVD'
      if (grepl('BRCA',gene)){
        lovd_link <- a(href=paste0('http://chromium.liacs.nl/LOVD2/cancer/variants.php?select_db=',gene,'&action=search_unique&search_Variant%2FDNA=',variant),'LOVD',target="_blank") 
      }
      if (gene %in% c('MLH1','MSH2','MSH6','PMS2')){
        lovd_link <- a(href=paste0('http://chromium.liacs.nl/LOVD2/colon_cancer/variants.php?select_db=',gene,'&action=search_unique&search_Variant%2FDNA=',variant),'LOVD',target="_blank")
      }
      
      lovd_link <- a(href=paste0('https://databases.lovd.nl/shared/variants/in_gene?search_geneid=%3D"',gene,'"&search_VariantOnTranscript/DNA=%3D"',variant,'"'),'LOVD3',target="_blank")
      
      l1lovd3 <-  list(h5(lovd_link),tags$tr(class='highlight',tags$td(em("Number of times reported in LOVD3 (July 2019): "),align="right"),tags$td(strong(info$lovd3[,1]))))
      l<-c(l,l1lovd3)
    }
    if (!is.null(info) && !is.null(info$dmudb)) {
      fields.dmudb<-names(fields[["dmudb"]])  
      info.dmudb<-as.list(info$dmudb[,1])
      names(info.dmudb)<-rownames(info$dmudb)
      l<-c(l,list(h5('DMuDB'),
                  tags$table(border="0",cellspacing="5",
                             lapply(fields.dmudb,function(x){
                               return(tags$tr(tags$td(em(fields[["dmudb"]][[x]]),align="right"),tags$td(strong(info.dmudb[[x]]))))
                             }
                             )
                  )
      )
      )
    }
    if (!is.null(info) && !is.null(info$umd)) {
      fields.umd<-names(fields[["umd"]])
      info.umd<-as.list(info$umd[,1])
      names(info.umd)<-rownames(info$umd)
      l<-c(l,list(h5('UMD'),
                  tags$table(border="0",cellspacing="5",
                             lapply(fields.umd,function(x){
                               return(tags$tr(tags$td(em(fields[["umd"]][[x]]),align="right"),tags$td(strong(info.umd[[x]]))))
                             }
                             )
                  )
      )
      )
    }

    
    dataset <- list()
    
    for(gp in c("","_count","_no","_homo")){
      toselect <- paste0("AFR",gp, ", AMR",gp, ", ASJ",gp, ", EAS",gp, ", FIN",gp, ", NFE",gp, ", OTH",gp, ", SAS",gp, ", Total", gp) 
      # myquery <- paste(sep = "",collapse = "", "select ",toselect  ," from gnomad where gene='",info$summary["gene",],"' and transcript='",info$summary["transcript",],"' and hgvs_cdna='", info$summary["hgvs_cdna",],"'")
      myquery <- paste(sep = "",collapse = "", "select ",toselect  ," ,link from gnomadtest where gene='",info$summary["gene",],"' and hgvs_cdna='",info$summary["hgvs_cdna",],"'")
      
      
      #rs<-dbSendQuery(mydb,myquery)
      #dtst <- dtst <- as.list(fetch(rs,-1))
      
      if(gp==""){
        rs<-dbSendQuery(mydb,myquery)
        dtst <- dtst <- as.list(fetch(rs,-1))
        
        
        dtst <- lapply(dtst, function(x) signif(as.numeric(x),2))
        dtst <- ifelse(dtst<1.0e-5 & dtst!=0, "<0.00001", dtst)
        dtst <- format(dtst,scientific = F)   
        dtst <- sapply(dtst, function(x) strtrim(x, width = 8))	
        dataset[["Allele Frequency"]] <- dtst
        
      }else if(gp=="_count"){
        
        rs<-dbSendQuery(mydb,myquery)
        dtst <- dtst <- as.list(fetch(rs,-1))
        dataset[["Allele Count"]] <- dtst
      }else if(gp=="_no"){
        
        rs<-dbSendQuery(mydb,myquery)
        dtst <- dtst <- as.list(fetch(rs,-1))
        dataset[["Allele Number"]] <- dtst
        
      }else if(gp=="_homo"){
        
        rs<-dbSendQuery(mydb,myquery)
        dtst <- dtst <- as.list(fetch(rs,-1))
        #dbDisconnect(mydb)
        dataset[["Number of Homozygotes"]] <- dtst
        
      }else{
        print("# there is an error")
        # dbDisconnect(mydb)
      }
      
    }
    
    dataset$"Number of Individuals" <- setNames(c("12487","17720","5185","9977","12562","64603","3614","15308","141456", "NA"), names(dataset[[1]]))
    info.gnomad <-  as.data.frame(t(do.call("cbind" ,dataset)))
    gnomadlink <- as.character(info.gnomad["Number of Homozygotes","link"])
    
    info.gnomad$o <- as.list(setNames(rownames(info.gnomad), rownames(info.gnomad)))
    info.gnomad <- info.gnomad[, c("o", "AFR","AMR","ASJ","EAS","FIN","NFE","OTH","SAS","Total")]
    fields.gnomad <- colnames(info.gnomad)

    info.gnomad[is.na(info.gnomad) | info.gnomad=="NA" | info.gnomad=="character(0)"] <- 0

    fields.gnomad2 <- list("AFR"="African","AMR"="Latino","ASJ"="Ashkenazi Jewish","EAS"="East Asian","FIN"="European (Finnish)","NFE"="European (Non-Finnish)","OTH"="Other","SAS"="South Asian","Total"="Total")
    
    lgnomad <- list(h5(a('gnomAD Population Frequencies',
                         href=paste0('http://gnomad.broadinstitute.org/variant/',gnomadlink),
                         #href=paste0('http://gnomad.broadinstitute.org/variant/',info$summary["hg19_chr",],'-',info$summary["hg19_pos",],'-',info$summary["ntwt",],'-',info$summary["ntmut",]),
                         target='_blank')),
                    tags$table(border="1",
                               lapply(fields.gnomad,function(x){
                                 v<-info.gnomad[[x]]
                                 #                                 v<-format(v, scientific=T)
                                 return(tags$tr(tags$td(em(ifelse(fields.gnomad2[[x]]!="", paste0(fields.gnomad2[[x]],":"), fields.gnomad2[[x]])),align="right"),
                                                tags$td(strong(v[["Allele Count"]]),align="center"), 
                                                tags$td(strong(v[["Allele Number"]]),align="center"), 
                                                tags$td(strong(v[["Number of Homozygotes"]]), align="center"), 
                                                tags$td(strong(v[["Allele Frequency"]]),align="center"),
                                                tags$td(v[["Number of Individuals"]]),align="center"))
                               }
                               )
                    ),helpText(em(h6("The GNOMAD dataset contains 125,748 exome sequences and 15,708 whole-genome sequences from unrelated individuals (141,456 individuals, 282,912 alleles)")))
                    
    )

      fields.phe2 <- list("BIRq3"="Birmingham", "BRIrvj"="Bristol", "CAMrgt"="Cambridge",
                          "GUYrj1"="Guy's", "LEEDrr8"="Leeds", "NEWrtd"="Newcastle", "MANrw3"="Manchester", "NOTrx1"="Nottingham",
                          "SALrnz"="Salisbury", "SHEFrcu"="Sheffield","scrCount"="Total probands", "totCount"="Total probands tested")
      
      toselect <- paste0(names(fields.phe2), collapse = ", ")
      myquery <- paste(sep = "",collapse = "", "select ",toselect  ," from PHE where gene='",info$summary["gene",],"' and transcript='",info$summary["transcript",],"' and hgvs_cdna='", info$summary["hgvs_cdna",],"'")
      
      rs<-dbSendQuery(mydb,myquery)
      phest <- as.list(fetch(rs,-1))
      
      info.phe <- phest
      info.phe <- rbind(fields.phe2[names(info.phe)], info.phe)
      colnames(info.phe) <- fields.phe <- as.character(fields.phe2)
      info.phe <- data.frame(info.phe, check.names=F)
      info.phe <- sapply(info.phe, function(x) setNames(x,c("Label","Count")))
      info.phe <- data.frame(info.phe, check.names=F)
      info.phe$"Total probands tested"[2] <- 16600
      
      if(!values$logged){
        fields.phe  <- c("Total probands", "Total probands tested")
      }
      
      lphe <- list(h5('UK diagnostic labs'),
                   tags$table(border="0", cellspacing="10",
                              lapply(fields.phe,function(x){
                                v<-info.phe[[x]]
                                return(tags$tr(
                                  tags$td(em(paste0(v[["Label"]],":", sep=" ")),align="right"),
                                  tags$td(strong(v[["Count"]]),align="center")))
                                
                              }
                              )
                   )
      )
      

    # 1st column is the "o" 
    # l <- c(lgnomad,l)
    l <- list()
    l <- c(lgnomad,l)
    lphe <- list()
    
    
    dbDisconnect(mydb)
    return(as.character(tagList(fluidRow(column(width=6,l), column(width=5, offset=1, lphe)))))
  })
  
  
  # Output for the Frequency panel
  output$frequencyPanel2 <- renderText ({
    
    fixempty <- function(x) {return(ifelse(is.null(x)||is.na(x)||x==0,'',x))}
    freqprint <- function(x){return(sprintf('%5.4f',as.numeric(x)))}
    mydb<-dbConnect(MySQL(), user='test',password='newpassword',dbname='cigma2',host=host)
    rs<-dbSendQuery(mydb,"select table_name,field_name,field_label,variant_report,variant_report_order,analysis_output,analysis_output_order from fields where online_report='Y'")
    f<-fetch(rs,-1)
    fn<-split(f$field_name,f$table_name)
    fl<-split(f$field_label,f$table_name)
    # fa is a structure with the fields for the analysis output
    # the fields from the main classification and dogma_batchlog tables have to be entered manually
    fa<-f[which(f$analysis_output=='Y' & !(f$table_name %in% c('main','classification','dogma_batchlog'))),]
    rownames(fa)<-paste(gsub('tgp_phase3_pop','tgp_phase3',gsub('tgp_pop','tgp',fa[,1])),fa[,2],sep='_')
    fv<-f[which(f$variant_report=='Y' & !(f$table_name %in% c('classification','dogma_batchlog'))),]
    rownames(fv)<-paste(gsub('tgp_phase3_pop','tgp_phase3',gsub('tgp_pop','tgp',fv[,1])),fv[,2],sep='_')
    fan<-split(fa$field_name,fa$table_name)
    fvn<-split(fv$field_name,fv$table_name)
    
    # fields is a structure for the display panels
    # create list of tables with named lists of fields descriptions:
    fields<-setNames(lapply(names(fl),function(x){return(setNames(paste0(fl[[x]],':'),fn[[x]]))}),names(fl))
    mydb<-dbConnect(MySQL(), user='test',password='newpassword',dbname='cigma2',host=host)
    
    info<-cigmainfo()
    # info(loggerServer,"in frequencyPanel")
    l<-vector(mode="list")
    
    if (!is.null(info) && !is.null(info$icr) && 1==0) {
      fields.icr<-names(fields[["icr"]])  
      info.icr<-as.list(info$icr[,1])
      names(info.icr)<-rownames(info$icr)
      l<-c(l,list(h5('1958 Birth Cohort (ICR)'),
                  tags$table(border="0",cellspacing="5",
                             lapply(fields.icr,function(x){
                               return(tags$tr(tags$td(em(fields[["icr"]][[x]]),align="right"),tags$td(strong(info.icr[[x]]))))
                             }
                             )
                  )
      )
      )
    }
    if (!is.null(info) && !is.null(info$bic)) {
      fields.bic<-names(fields[["bic"]])  
      info.bic<-as.list(info$bic[,1])
      names(info.bic)<-rownames(info$bic)
      l<-c(l,list(h5(a('BIC',href='https://research.nhgri.nih.gov/bic/',target='_blank')),
                  #
                  tags$table(border="0",cellspacing="5",
                             lapply(fields.bic,function(x){
                               return(tags$tr(tags$td(em(fields[["bic"]][[x]]),align="right"),tags$td(strong(info.bic[[x]]))))
                             }
                             )
                  )
      )
      )
    }
    
    if (!is.null(info) && !is.null(info$lovd3)) {
      
      gene<-info$summary['gene',1]
      variant<-info$summary['hgvs_cdna',1]
      lovd_link<-'LOVD'
      if (grepl('BRCA',gene)){
        lovd_link <- a(href=paste0('http://chromium.liacs.nl/LOVD2/cancer/variants.php?select_db=',gene,'&action=search_unique&search_Variant%2FDNA=',variant),'LOVD',target="_blank") 
      }
      if (gene %in% c('MLH1','MSH2','MSH6','PMS2')){
        lovd_link <- a(href=paste0('http://chromium.liacs.nl/LOVD2/colon_cancer/variants.php?select_db=',gene,'&action=search_unique&search_Variant%2FDNA=',variant),'LOVD',target="_blank")
      }
      
      lovd_link <- a(href=paste0('https://databases.lovd.nl/shared/variants/in_gene?search_geneid=%3D"',gene,'"&search_VariantOnTranscript/DNA=%3D"',variant,'"'),'LOVD3',target="_blank")
      
      l1lovd3 <-  list(h5(lovd_link),tags$tr(class='highlight',tags$td(em("Number of times reported in LOVD3 (July 2019): "),align="right"),tags$td(strong(info$lovd3[,1]))))
      l<-c(l,l1lovd3)
    }
    if (!is.null(info) && !is.null(info$dmudb)) {
      fields.dmudb<-names(fields[["dmudb"]])  
      info.dmudb<-as.list(info$dmudb[,1])
      names(info.dmudb)<-rownames(info$dmudb)
      l<-c(l,list(h5('DMuDB'),
                  tags$table(border="0",cellspacing="5",
                             lapply(fields.dmudb,function(x){
                               return(tags$tr(tags$td(em(fields[["dmudb"]][[x]]),align="right"),tags$td(strong(info.dmudb[[x]]))))
                             }
                             )
                  )
      )
      )
    }
    if (!is.null(info) && !is.null(info$umd)) {
      fields.umd<-names(fields[["umd"]])
      info.umd<-as.list(info$umd[,1])
      names(info.umd)<-rownames(info$umd)
      l<-c(l,list(h5('UMD'),
                  tags$table(border="0",cellspacing="5",
                             lapply(fields.umd,function(x){
                               return(tags$tr(tags$td(em(fields[["umd"]][[x]]),align="right"),tags$td(strong(info.umd[[x]]))))
                             }
                             )
                  )
      )
      )
    }
    
    
    fields.phe2 <- list("BIRq3"="Birmingham", "BRIrvj"="Bristol", "CAMrgt"="Cambridge",
                        "GUYrj1"="Guy's", "LEEDrr8"="Leeds", "NEWrtd"="Newcastle", "MANrw3"="Manchester", "NOTrx1"="Nottingham",
                        "SALrnz"="Salisbury", "SHEFrcu"="Sheffield","scrCount"="Total probands", "totCount"="Total probands tested")
    
    toselect <- paste0(names(fields.phe2), collapse = ", ")
    myquery <- paste(sep = "",collapse = "", "select ",toselect  ," from PHE where gene='",info$summary["gene",],"' and transcript='",info$summary["transcript",],"' and hgvs_cdna='", info$summary["hgvs_cdna",],"'")
    
    rs<-dbSendQuery(mydb,myquery)
    phest <- as.list(fetch(rs,-1))
    
    info.phe <- phest
    info.phe <- rbind(fields.phe2[names(info.phe)], info.phe)
    colnames(info.phe) <- fields.phe <- as.character(fields.phe2)
    info.phe <- data.frame(info.phe, check.names=F)
    info.phe <- sapply(info.phe, function(x) setNames(x,c("Label","Count")))
    info.phe <- data.frame(info.phe, check.names=F)
    info.phe$"Total probands tested"[2] <- 16600
    
    if(!values$logged){
      fields.phe  <- c("Total probands", "Total probands tested")
    }
    
    lphe <- list(h5('UK diagnostic labs'),
                 tags$table(border="0", cellspacing="10",
                            lapply(fields.phe,function(x){
                              v<-info.phe[[x]]
                              return(tags$tr(
                                tags$td(em(paste0(v[["Label"]],":", sep=" ")),align="right"),
                                tags$td(strong(v[["Count"]]),align="center")))
                              
                            }
                            )
                 )
    )
    
    
    # 1st column is the "o" 
    if(all(info.phe[2,]==0) | all(is.na(info.phe[2,]))){
      lphe <- list()
    }
    
    dbDisconnect(mydb)
    return(as.character(tagList(fluidRow(column(width=6,l), column(width=5, offset=1, lphe)))))
  })
  
  
  observeEvent(input$go,{
    updateQueryString(queryString = paste0("?gene=",isolate(input$genetrans),"&variant=",isolate(input$variationText),"&page=Exp"), mode = "push")
  })
  
  
  observeEvent(getQueryString(), {
    myurl <- getQueryString()
    print(myurl)
    
    observe({ if(length(myurl)==3){
      if(myurl$page=="Exp"){
        updateNavbarPage(session = session, inputId = "navbartab", selected = "Explore")
        
        observeEvent(input$navbartab,{
          updateSelectInput(session = session, inputId = "genetrans", selected = myurl$gene, label = myurl$gene)
          observeEvent(input$genetrans!="", {updateTextInput(session, "variationText", value = myurl$variant) })
          print(input$variationText)
          observe({ if(myurl$variant!=""){updateButton(session, inputId = "go", value = isolate(input$go)+1)} })
        })
        
      }else{
        return()
      }
    }else{
      return()
    }})
    print(isolate(input$go)+1)
  }, ignoreInit = F, ignoreNULL = T)
  
  
  observeEvent(input$useExample, {
    updateSelectInput(session, inputId = "genetrans", selected=if(input$useExample==T){"BRCA1"}else{""})
    observeEvent(input$genetrans!="", {updateTextInput(session, "variationText", value = if(input$useExample==T & input$genetrans!=""){"c.5207T>C"}else{""}) })
  }, ignoreInit = T, once = F, ignoreNULL = F)
  
  
  gl <- c("Select a gene" = "","BRCA1","BRCA2","MLH1")
  updateSelectInput(session, "genetrans", choices = gl)
  
  output$ui1 <- renderUI({
    if(input$genetrans!=""){
      fluidRow(textInput(inputId = "variationText", label="", placeholder="Variant starting with (e.g: c.181 )", width = "90%"), align = "center")
    }else{return()}
  })
  
  output$genelevel <- renderUI({
    if(input$genetrans!=""){
      tabs <- list()
      # tabs[[1]] <- tabPanel(title = paste0(input$genetrans," - Guideline"), br(),br(),      
      #                       h4(align="justify", "The American College of Medical Genetics and Genomics (ACMG) ...."))
      tabs[[1]] <- bsCollapse(id = "collapseExample", open = NULL, 
                              bsCollapsePanel(paste0(input$genetrans," - Guideline     (click to expand/collapse)"), style = "success",
                                              h5(align="justify", "The American College of Medical Genetics and Genomics (ACMG) previously developed guidance 
                                                                                                             for the interpretation of sequence variants.(1) In the past decade, sequencing technology has 
                                                                                                             evolved rapidly with the advent of high-throughput next-generation sequencing. By adopting and leveraging 
                                                                                                             next-generation sequencing, clinical laboratories are now performing an ever-increasing catalogue of genetic 
                                                                                                             testing spanning genotyping, single genes, gene panels, exomes, genomes, transcriptomes, and epigenetic 
                                                                                                              assays for genetic disorders. By virtue of increased complexity, this shift in genetic testing has been accompanied by 
                                                                                                              new challenges in sequence interpretation. In this context the ACMG convened a workgroup in 2013 comprising representatives 
                                                                                                              from the ACMG, the Association for Molecular Pathology (AMP), and the College of American Pathologists to revisit and revise 
                                                                                                              the standards and guidelines for the interpretation of sequence variants. The group consisted of clinical laboratory directors
                                                                                                              and clinicians. This report represents expert opinion of the workgroup with input from ACMG, AMP, and College of American Pathologists stakeholders. 
                                                                                                              These recommendations primarily apply to the breadth of genetic tests used in clinical laboratories, including genotyping, single genes,
                                                                                                              panels, exomes, and genomes. This report recommends the use of specific standard
                                                                                                              terminology-\"pathogenic,\" \"likely pathogenic,\" \"uncertain significance,\" \"likely benign,\" and \"benign\"-to describe
                                                                                                              variants identified in genes that cause Mendelian disorders. Moreover, this recommendation describes a process for classifying 
                                                                                                              variants into these five categories based on criteria using typical types of variant evidence (e.g., population data, computational data, 
                                                                                                              functional data, segregation data). Because of the increased complexity of analysis and interpretation of clinical genetic
                                                                                                              testing described in this report, the ACMG strongly recommends that clinical molecular genetic testing should be performed in a 
                                                                                                              Clinical Laboratory Improvement Amendments-approved laboratory, with results interpreted by a board-certified clinical molecular 
                                                                                                                    geneticist or molecular genetic pathologist or the equivalent.")
                              ))
      
      do.call(tabsetPanel, c(id="geneleveltabset",tabs))
    }else{return()}
  })
  
  observe({
    if (is.null(input$go) || is.na(input$go) || input$go == 0){
      return()
    }else{
      output$variantlevel <- renderUI({
        tabs <- list()
        # tabs[[1]] <- tabPanel(title = "Summary",  br(),br(), fluidRow(column(width=12,tableOutput('tb'))))
        tabs[[1]] <- tabPanel(title = "Summary",  br(),br(), fluidRow(column(width=12, uiOutput('summaryPanel'))))
        tabs[[2]] <- tabPanel(title = "In silico predictions", br(),br(), fluidRow(column(width=12, uiOutput('insilicoPanel'))))
        tabs[[3]] <- tabPanel(title = "Frequency",tabsetPanel(id = "freqtabID",
                                                              tabPanel(title = "Case", fluidRow(column(width=12, uiOutput('frequencyPanel2')))),
                                                              tabPanel(title = "Control", fluidRow(column(width=12, uiOutput('frequencyPanel'))))
        )
        )
        tabs[[4]] <- tabPanel(title = "Genetic Epidemiology", br(),br())
        tabs[[5]] <- tabPanel(title = "Splicing analysis", br(),br())
        tabs[[6]] <- tabPanel(title = "Functional analysis", br(),br())
        tabs[[7]] <- tabPanel(title = "CanVar-UK Classifications/notes", br(),br(), span(style = "color:green;font-weight:bold;font-size:75%;", DT::dataTableOutput("data"),uiOutput("hidden_downloads")))
        
        do.call(tabsetPanel, c(id="variantleveltabset", tabs))
      })
    }
  })
  
  
  observeEvent(input$freqtabID, {
    print(isolate(input$freqtabID))
    if(input$freqtabID == "Case"){
      session$sendCustomMessage("background-color", "grey")
    } else {
      session$sendCustomMessage("background-color", "lightblue")
    }
  })
  
  
  pName <- list.files("/home/cankut/Downloads/",pattern = "pdf", full.names = T)
  if(length(pName)>0){
    fClicks <- reactiveValues()
    for(i in seq_len(length(pName))){       
      fClicks[[paste0("firstClick_",i)]] <- F        
    }
    # Creating hidden Links
    output$hidden_downloads <- renderUI(
      lapply(seq_len(length(pName)), function(i) downloadLink(paste0("dButton_",i), label="")))
    
    # Creating Download handlers (one for each button)
    lapply(seq_len(length(pName)), function(i) {
      output[[paste0("dButton_",i)]] <- downloadHandler(
        filename = function() basename(pName)[i],
        content  = function(file) file.copy(pName[i], file))
    })
    
    # Function to generate the Action buttons (or actionLink)
    makeButtons <- function(len) {
      inputs <- character(len)
      for (i in seq_len(len))  inputs[i] <- as.character(  
        actionButton(inputId = paste0("aButton_", i),  
                     label   = "Download", 
                     onclick = 'Shiny.onInputChange(\"selected_button\", this.id, {priority: \"event\"})'))
      inputs
    }
    
    # Creating table with Action buttons
    df <- reactiveValues(data=data.frame(Name=basename(pName), 
                                         Classification="Pathogenic",
                                         Comment=" 200 words comment comment comment comment comment comment comment comment comment",
                                         Date=Sys.Date(),
                                         Curator="CanVIG-UK",
                                         Affilation="Institute Cancer Research",
                                         Department="Genetics and Epidemiology",
                                         Download=makeButtons(length(pName)), 
                                         row.names=seq_len(length(pName))))
    output$data <- DT::renderDataTable(df$data, server=F, escape=F, selection='none')
    
    # Triggered by the action button
    observeEvent(input$selected_button, {
      i <- as.numeric(strsplit(input$selected_button, "_")[[1]][2])
      shinyjs::runjs(paste0("document.getElementById('aButton_",i,"').addEventListener('click',function(){",
                            "setTimeout(function(){document.getElementById('dButton_",i,"').click();},0)});"))
      # Duplicating the first click
      if(!fClicks[[paste0("firstClick_",i)]])
      {
        click(paste0('aButton_', i))
        fClicks[[paste0("firstClick_",i)]] <- T
      }
    })
    #
  }
  
  output$ui2 <- renderUI({
    
    updateTextInput(session, "variationText",
                    value = trimws(input$variationText))
    
    if(!is.null(input$variationText)){
      if(input$variationText!=""){
        selectInput(inputId = "variationSel", label="", choices = c(''), selectize=TRUE)
      }
    }else{return()}
  })
  
  # dummy delay option for waiting screen
  output$tb <- renderTable({
    iris[1:5,]
  })
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            # pasverify <- password_verify(pasmatch, Password)
            pasverify <- pasmatch==Password
            
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 20px;")
  })
  
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      
      fluidRow(column(width = 6,h4(align="justify", color="red", "Succesfully LOGGEN IN")),
               br(),
               column(width =12, h4(align="justify", "You can use the logout button to close this session")))
    }
    else {
      loginpage
    }
  })
  
  output$Access <- renderUI({
    if (USER$login == TRUE ) {
      span(icon("sign-out"), HTML('&nbsp;'), strong("Logout"), style="color:#FE5815;")
    }
    else {
      span(icon("sign-in"), HTML('&nbsp;'), strong("Login"), style="color:white;")
    }
  })
  
  # Hide the loading message when the rest of the server function has executed
  # Simulate work being done for 1 second
  Sys.sleep(1)
  hide(id = "loading-content", anim = TRUE, animType = "fade")  
  
  
}

app <- shinyApp(ui, server)
runApp(app,launch.browser = T)
