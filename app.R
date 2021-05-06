library(shiny)
library(shinydashboard)
library(tidyverse)
library(crosstalk)
library(plotly)
library(DT)
library(StatsBombR)

# This imports some helper functions to plot soccer pitches
source("pitch_plots.R")

competitions <- FreeCompetitions() %>%
    filter(competition_name == "La Liga")

matches <- FreeMatches(competitions)

freekick_list <- list()

# Because the StatsBomb API is relatively slow I am only loading three matches
# The hosted shiny version works off a local sqlite file to not repeatedly hit the API.

for(i in c(1:3)){

    events <- get.matchFree(matches[i,]) %>%
        allclean() %>%
        filter(shot.type.name == "Free Kick") %>%
        mutate(Name = "Lionel Messi", 
               shot.statsbomb_xg = round(shot.statsbomb_xg, 2)) %>%
        mutate(shot.outcome.name = if_else(shot.outcome.name == "Post", "Off T", shot.outcome.name)) %>%
        mutate(shot.outcome.name = if_else(shot.outcome.name == "Saved Off Target", "Saved", shot.outcome.name)) %>%
        mutate(shot.outcome.name = if_else(shot.outcome.name == "Saved to Post", "Saved", shot.outcome.name)) %>%
        left_join(matches, by = "match_id") %>%
        mutate(opponent = if_else(home_team.home_team_name == "Barcelona", away_team.away_team_name, home_team.home_team_name)) %>%
        select(Name = Name,
               Type = shot.type.name,
               Foot = shot.body_part.name,
               Outcome = shot.outcome.name,
               xG = shot.statsbomb_xg,
               Opponent = opponent,
               Goalkeeper = player.name.GK,
               Season = season.season_name,
               Date = match_date,
               Minute = minute,
               Second = second,
               id = id,
               player.id = player.id,
               match_id = match_id,
               competition_id = competition_id,
               location_x = location.x,
               location_y = location.y,
               shot_end_x = shot.end_location.x,
               shot_end_y = shot.end_location.y,
               shot_end_z = shot.end_location.z,
               freeze_frame = shot.freeze_frame)

    freekick_list[[i]] <- events
}

freekicks <- bind_rows(freekick_list)

# Define UI for application that draws a histogram
ui <- fluidPage(

    dashboardBody(
        fluidRow(
            h2("Lionel Messi - La Liga Free Kicks - Seasons 06/07-18/19 (Powered By StatsBomb)", align = "center"),
            hr()
        ),
        fluidRow(
            column(width = 6,
                   h3(HTML("Shot Map: <span style='color:#67a9cf'>Goals</span> <span style='color:#8856a7'>Blocked</span> 
                           <span style='color:#ef8a62'>Saved</span> <span style='color:#e34a33'>Off Target</span>")),
                   h5("Select free kick locations to see goal impacts and filter table below (double-click to deselect)"),
                   plotlyOutput("shotMap", height = 600),
                   h3("Additional Information"),
                   h5("Select individual rows to highlight shot location and goal impact"),
                   div(DTOutput("eventTable", height = 300), style = "font-size:70%")
            ),
            column(width = 6,
                   h3("Shot Impact"),
                   h5("Select goal impacts to see free kick locations and filter table below (double-click to deselect)"),
                   plotlyOutput("GoalMap", height = 300),
                   h3("Summary and Freeze Map"),
                   h5("You can hover over the markers in the freeze frame to see player names"),
                   plotlyOutput("freezeMap", height = 600)
            ),
        ),
        fluidRow(
            h3("Powered by", align = "center"),
            HTML('<center><a href="https://github.com/statsbomb" target="_blank"><img src="stats-bomb-logo.png" height="100"></a> </center>')
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    sd <- highlight_key(freekicks, ~id)
    
    output$shotMap <- renderPlotly({
        q <- create_StatsBomb_ShotMap("#224C56", "#B3CED9", "#224C56", "#15393D") +
            geom_point(data = sd, aes(x = location_y, y = location_x, color = Outcome), size = 1) +
            scale_colour_manual(values = c("Goal" = "#67a9cf", "Blocked" = "#8856a7", "Saved" = "#ef8a62", "Off T" = "#e34a33")) +
            theme(legend.position = "none")
        
        p <- ggplotly(q, tooltip = NULL) %>%
            highlight("plotly_selected", opacityDim = getOption("opacityDim", 0.1)) %>%
            config(displayModeBar = F) %>%
            layout(dragmode = "select") %>%
            style(hoverinfo = 'none')
    })
    
    output$GoalMap <- renderPlotly({
        q <- create_StatsBomb_GoalMap("#775D6A", "#F4828C", "#775D6A", "#7E3C5D") +
            geom_point(data = sd, aes(x = shot_end_y, y = shot_end_z, color = Outcome), size = 1) +
            scale_colour_manual(values = c("Goal" = "#67a9cf", "Blocked" = "#8856a7", "Saved" = "#ef8a62", "Off T" = "#e34a33")) +
            theme(legend.position = "none")
        
        p <- ggplotly(q, tooltip = NULL) %>%
            highlight("plotly_selected", opacityDim = getOption("opacityDim", 0.1)) %>%
            config(displayModeBar = F) %>%
            layout(dragmode = "select") %>%
            style(hoverinfo = 'none')
    })
    
    output$freezeMap <- renderPlotly({
        df <- freekicks[sd$selection(),]
        # Below handles the case that only one event is brushed
        if(dim(df)[[1]] == 1){
            
            id_selected <- df$id

            freekicks_filtered <- freekicks %>%
                filter(id == id_selected)
            
            freeze_frame <- freekicks_filtered$freeze_frame[[1]] %>%
                rowwise() %>%
                mutate(freeze_x = location[[1]],
                       freeze_y = location[[2]],
                       teammate = ifelse(teammate, 0, 1)) %>%
                select(teammate, player.name, freeze_x, freeze_y)
            
            free_kick_location <- map_df(list(teammate = 2, 
                                              player.name = "Free Kick Location",
                                              freeze_x = df$location_x,
                                              freeze_y = df$location_y), ~.x)
            
            freeze_frame <- rbind(freeze_frame, free_kick_location)
            freeze_frame$teammate <- as.character(freeze_frame$teammate)
            
            q <- create_StatsBomb_ShotMap("#224C56", "#B3CED9", "#224C56", "#15393D") +
                geom_point(data = freeze_frame, aes(x = freeze_y, y = freeze_x, color = teammate, text = player.name)) +
                scale_colour_manual(values = c("0" = "#2c7fb8", "1"= "#31a354", "2" = "#addd8e")) +
                theme(legend.position = "none")
            
            p <- ggplotly(q, tooltip = c('text')) %>%
                highlight("plotly_selected") %>%
                config(displayModeBar = F) %>%
                layout(dragmode = "select")
        } else{
            df <- freekicks[sd$selection(),]
            
            if(nrow(df) == 0){
                df <- freekicks
            }
            
            count <- nrow(df)
            cum_xg <- sum(df$xG)
            goals <- nrow(subset(df, Outcome == "Goal"))
            saved <- nrow(subset(df, Outcome == "Saved"))
            blocked <- nrow(subset(df, Outcome == "Blocked"))
            off_target <- nrow(subset(df, Outcome == "Off T"))
            
            comment <- if_else(goals >= cum_xg,
                               "This seems to be a good area for Lionel. He has outperformed expected goals.",
                               "Not such a good spot. Lionel has scored less often than expected.")
            
            text = paste("\n   The selected <b>",
                         count,
                         "</b> free kicks have a cumulative xG of <b>",
                         cum_xg,
                         "</b>.\n\n",
                         "Lionel Messi has scored <b>",
                         goals, 
                         "</b> of these, <b>",
                         saved, 
                         "</b> were saved, \n <b>",
                         blocked,
                         "</b> were blocked and <b>",
                         off_target,
                         "</b> were off target. \n\n",
                         comment,
                         "\n\n",
                         "If you want to see Statsbomb's freeze map for a certain \n free kick please only select a single one.")
            
            p <- ggplot() + 
                annotate("text", x = 4, y = 25, size = 4, label = text) + 
                theme_void() +
                theme(axis.line=element_blank(),
                      panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank())
            
            
            ggplotly(p) %>%
                config(displayModeBar = F) %>%
                layout(dragmode = F) %>%
                style(hoverinfo = 'none')
        }
    })
    
    output$eventTable <- renderDT({
        
        not_visible <- c(11:20)
        datatable(sd, 
                  rownames = FALSE, 
                  options = list(
                      dom = 'ftp',
                      columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5,6,7,8,9,10)),
                                        list(visible = FALSE, targets = not_visible)),
                      scrollX = T
                  )
        )
    }, server = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
