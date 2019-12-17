library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(magrittr)
library(readxl)
library(tigris)
library(downloader)
library(stringr)
library(tidytext)
library(wordcloud)
library(reshape2)
library(gridExtra)
library(xml2)
library(rvest)

#Read in data and subset rows and columns that aren't NA's. Remove cases that are missing.
attack <- read_xls("GSAF5-2.xls")
attack <- attack[1:6483,1:16]
attack <- attack[-which(is.na(attack$`Case Number...1`)),]

#Change year to number. Create new variables to quantify aspects.
#Hurt is if there was actually an injury. GSAF is if it was reported by GSAF first.
#common is if it's Provoked or Unprovoked. Confirmed is if shark characteristics had been recorded.
attack$Year <- as.numeric(attack$Year)
attack$Hurt <- ifelse(grepl("No injury",attack$Injury)==T,0,1)
attack$GSAF <- ifelse(grepl("GSAF",attack$`Investigator or Source`)==T,1,0)
attack$common <- ifelse(attack$Type=="Unprovoked"|attack$Type=="Provoked",1,0)
attack$Confirmed <- ifelse(grepl("No shark",attack$Species)==T | is.na(attack$Species)==T|grepl("involvement",attack$Species)==T,0,1)
attack$`Fatal (Y/N)`[is.na(attack$`Fatal (Y/N)`)] <- 0
attack %<>% separate(Date,c("Day","Month","Yearr"),sep="-")

#Set year boundary past 2010. Change everything else to factors for analysis.
attack2010 <- subset(attack,Year>=2010 & Country=="USA")
attack2010$Hurt <- as.factor(attack2010$Hurt)
attack2010$common <- as.factor(attack2010$common)
attack2010$Confirmed <- as.factor(attack2010$Confirmed)

# Pull in states for leaflet.
states <- states(cb=T)

source("TextFunctions.R")

textdf <- readtext("https://www.trackingsharks.com/mans-inflatable-paddleboard-sunk-by-10ft-tiger-shark-shark-hits-kid/",
                   "https://www.trackingsharks.com/great-white-bites-kayak-in-california-leaves-2-massive-teeth/",
                   3,3); textdf$set <- "2019-Unharmed-TC"
textdf1 <- readtext("https://www.trackingsharks.com/shark-bite-snorkelers-arm-down-to-bone/",
                    "https://www.trackingsharks.com/shark-bites-surfer-near-cocoa-beach-pier/",
                    3,3); textdf1$set <- "2019-Harmed-TC"
textdf2 <- readtext("https://www.hawaiinewsnow.com/2019/06/15/teen-loses-leg-fingers-shark-attack/",
                    "https://www.hawaiinewsnow.com/2019/08/20/shark-attack-reported-napoopoo-beach-park-off-hawaii-island/",
                    1,2); textdf2$set <- "2019-Harmed-HNN"

textdf3 <- readtext("https://www.hawaiinewsnow.com/story/20010243/shark-attack-survivor-recounts-frightening-ordeal/",
                    "https://www.hawaiinewsnow.com/story/20230172/beaches-closed-after-man-by-shark-on-maui/",
                    1,2); textdf3$set <- "2010-Harmed-HNN"

alltext <- rbind(textdf,textdf1,textdf2,textdf3)

#Shinyapp
ui <- fluidPage(
  navbarPage("Tabs",
             tabPanel("About",
                      h2("Hello!"),
                      p("This Shiny App shows some of the basic data from the
                       Global Shark Atack File (GSAF). Specifically, it focuses on after 2010 and 
                        on the United States. This app was designed to easily look at and explore 
                        some of the simple, underlying patterns and visualize them."),
                      br(),
                      p("The 'Map of Attacks' page shows a map of attacks by state. 
                        Each state is colored based off of a selectable variable. 
                        You can select for the range of years."),
                      br(),
                      p("The 'Overall Data' and 'State and Year Data' show plots of data.
                        The former shows it for all states over nine years, the latter
                        shows it for one state in a given year."),
                      br(),
                      p("The last page, 'Sentiment Comparison', compares the sentiment between
                        two articles mentioned in the 'Sources' column in the GSAF dataset."),
                      br(),
                      p("You can explore the data and learn more by visiting http://www.sharkattackfile.net")),
             tabPanel("Map of Attacks",
                      mainPanel(sliderInput("range","Pick a range of years",2010,2019,value=range(attack2010$Year)),
                                selectInput("colors","Variable for Color",c("Attacks","Provoked","Uninjured","Fatalities")),
                                leafletOutput(outputId="mymap")),
                      strong("Key"),
                      p("Attacks is the number of recorded Unprovoked and Provoked shark attacks in that state."),
                      p("Provoked is the number of recorded Provoked shark attacks in that state."),
                      p("Uninjured is the number of incindents with a lack of injury from a shark. This is primarily chosen from analyzing text for 'no injury'."),
                      p("Fatalities is the number of recorded fatalities involving sharks in the state.")
                      ),
             tabPanel("Overall Data",
                      mainPanel(
                        selectInput("tot","Pick a Variable",c("Type","Hurt","Confirmed")),
                        plotOutput("totaldata")
                      ),
                      p("Type is the type of attack. Unprovoked and provoked refer to the shark. 
                        The rest involve strange occurences and are not really considered shark attacks."),
                      br(),
                      p("Hurt refers to if there was an injury (1 if injured, 0 if not). This was done with text analysis and may have missed a few."),
                      br(),
                       p("Confirmed refers to if any features of the shark were confirmed. If no species is recorded or a shark involvement was left unconfirmed, it was recorded as 0.")
                      ),
             tabPanel("State and Year Data",
                      mainPanel(
                        selectInput("year","Pick a Year",unique(attack2010$Year)),
                        selectInput("state","Pick a State",unique(attack2010$Area)),
                        selectInput("var","Pick a variable",c("Type","Activity","Hurt","Confirmed","Injury")),
                        plotOutput("statedata",width="100%")
                      ),
                      p("Activity refers to the activity during the shark attack. Injury refers to the type of injury.
                        Injury descriptions can be long and capture nuances, so it may appear scrunched on smaller windows.")
                      ),
             tabPanel("Text",
                      mainPanel(
                        selectInput("article","Pick one Article Pair",unique(alltext$set)),
                        selectInput("article2","Pick another Article pair",unique(alltext$set)),
                        plotOutput("sentorder")
                      ),
                      p("Each input refers to the code to an article. The first is the year,
                        the second is if there were harm from sharks, and the third is the source. 
                        Links will be provided below. There may be odd linebreaks depending on browser size,
                        so I apologize!"),
                      br(),
                      p("2019-Unharmed-TC: https://www.trackingsharks.com/mans-inflatable-paddleboard-sunk-by-10ft-tiger-shark-shark-hits-kid/ ,
                        https://www.trackingsharks.com/great-white-bites-kayak-in-california-leaves-2-massive-teeth/"),
                      br(),
                      p("2019-Harmed-TC: https://www.trackingsharks.com/shark-bite-snorkelers-arm-down-to-bone/ ,
                        https://www.trackingsharks.com/shark-bites-surfer-near-cocoa-beach-pier/"),
                      br(),
                      p("2019-Harmed-HNN: https://www.hawaiinewsnow.com/2019/06/15/teen-loses-leg-fingers-shark-attack/ ,
                        https://www.hawaiinewsnow.com/2019/08/20/shark-attack-reported-napoopoo-beach-park-off-hawaii-island/"),
                      br(),
                      
                      p("2010-Harmed-HNN: https://www.hawaiinewsnow.com/story/20010243/shark-attack-survivor-recounts-frightening-ordeal/ ,
                         https://www.hawaiinewsnow.com/story/20230172/beaches-closed-after-man-by-shark-on-maui/")
                      )
  )
)


server <- function(input, output) {
  
  output$totaldata <- renderPlot({
    ggplot(attack2010) +
      geom_bar(aes_string(x="Year",fill=colnames(attack2010)[colnames(attack2010)==input$tot])) +
      coord_flip() + ylab("Number of Occurrences") + theme_bw() +
      scale_x_continuous(breaks=seq(2010,2019,1))
  })

  newdata <- reactive({
    newdata <- attack2010 %>% subset(Year ==input$year & Area == input$state)
    return(newdata)
  })
    
  output$statedata <- renderPlot({
    ggplot(newdata()) + 
      geom_bar(aes_string(x=colnames(newdata())[colnames(newdata())==input$var],fill="Type")) +
      coord_flip() + ylab("Number of Occurences")
  })
  
  statesmerged <- reactive({
    start<- attack2010[attack2010$Year >= input$range[1] & attack2010$Year <= input$range[2],] %>% 
      subset(common==1) %>%
      group_by(Area) %>%
      summarize(Attacks=n(), Provoked=sum(Type=="Provoked"),Uninjured=sum(Hurt==0),Fatalities=sum(`Fatal (Y/N)`=="Y"))
    statesmerged <- geo_join(states,start,"NAME","Area")
    statesmerged <- subset(statesmerged, !is.na(Attacks))
    return(statesmerged)
  })
  

  colorpal <- reactive({
    colorNumeric("Reds",statesmerged()@data[,c(paste0(input$colors))])
  })
  

  output$mymap <- renderLeaflet({
    leaflet("mymap") %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-116.483330,38.712046,zoom=2)
  })

  
  observe({
    pal <- colorpal()
    
    leafletProxy("mymap", data=statesmerged()) %>%
      clearShapes() %>%
      addPolygons(data=statesmerged(),
                  fillColor=~pal(statesmerged()@data[,c(paste0(as.character(input$colors)))]),
                  fillOpacity=0.7,
                  weight=0.2,
                  smoothFactor=0.2,
                  label=~paste0("Total: ", as.character(statesmerged()$Attacks),
                                "\n Provoked: ", as.character(statesmerged()$Provoked),
                                "\n Uninjured: ", as.character(statesmerged()$Uninjured),
                                "\n Fatalities: ", as.character(statesmerged()$Fatalities)))
  })
  
  observe({
    pal <- colorpal()
    proxy <- leafletProxy("mymap",data=statesmerged())
     
    proxy %>% clearControls() %>%
      addLegend(pal=pal,
                values=statesmerged()@data[,c(paste0(as.character(input$colors)))],
                position="bottomright",
                title=paste(input$colors))
  })
  
  output$sentorder <- renderPlot({
    p1 <- sentorder(alltext[alltext$set==input$article,]) + theme(legend.position="none")
    p2 <- sentorder(alltext[alltext$set==input$article2,]) + theme(legend.position="none")
    
    grid.arrange(p1,p2,ncol=2)
  })
  
}

shinyApp(ui=ui,server=server)


