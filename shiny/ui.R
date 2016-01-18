plate <- c("AG","AR","AI","BL","BS","BE","FR","GE","GL","GR","JU","LU","NE","NW",
           "OW","SG","SH","SZ","SO","TG","TI","UR","VS","VD","ZG","ZH")

shinyUI(pageWithSidebar(
        headerPanel('Electric Vehicles in Switzerland'),
        sidebarPanel(
                selectInput('type', 'Graph Type', c("Map: percentage by cantons", "Map: total by cantons", "Plot: total vehicles"), selected = "Map: percentage by cantons"),
                conditionalPanel(
                        condition = "input.type == 'Map: total by cantons' || input.type == 'Map: percentage by cantons'",
                        selectInput('year', 'Year', 2005:2014, selected = 2014)
                        ),
                p("Electric vehicle in Switzerland. Data set comes from Swiss Federal Office"),
                a("http://www.bfs.admin.ch")
                ),
        mainPanel(
                plotOutput('plotEV'),
                tags$i("Davide Rivola - source: Bundesamt f??r Statistik (BFS)")
        )
)
)