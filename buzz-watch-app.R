library(shiny)
library(tidyverse)
library(googlesheets4)

library(plotly)
library(hms)
library(leaflet)
library(leaflet.extras)

library(bslib)
library(bsicons)
library(thematic)
library(fontawesome)
library(shinythemes)


### RESOURCES ----

# How to connect to googlesheets:
# https://debruine.github.io/shinyintro/data.html

gs4_auth(cache = ".secrets", email = "noam.schwarz@gmail.com")
sheet_url = readLines(".secrets/form_url.txt") # replace this with actual address
thematic_shiny(font = "auto")

ui <- page_navbar(
  title = "BuzzWatch",
  nav_panel("Dashboard", 
            # bg = "info",
            layout_columns(
              card(
                card_header("Welcome!"),
                p("BuzzWatch is a citizen science project dedicated to tracking mosquitoes. We collect data on captured mosquitoes, including the time and location of each catch, to understand their activity patterns throughout the year and during different times of the day."),
                p("The motivation behind this project is to explore how climate change influences our daily lives. Changing weather patterns are causing mosquitoes to be active for longer periods each year, enabling them to expand their range into new regions. This shift may have contributed to the West Nile fever outbreak in Israel following the winter of 2023-2024. For more details on our mosquito tracking methods, go to the 'Methods' section."),
                p("In this interface, we analyze our own data, which clearly shows a peak in mosquito activity during the early summer months. Interestingly, we also find that mosquitoes are active during the day and even in the morning. This information can help people stay informed and take precautions to avoid mosquito bites. To learn more about our data processing and the development of this interface, go to the 'Resources' section."),
              ),
              
              layout_column_wrap(
                
                value_box(title = "Total mosquitos",
                          value = textOutput("total_n"),
                          showcase = fa_i("mosquito-net"),
                          theme = "success",
                          p("recorded so far")
                ),
                
                value_box(title = "Daily average",
                          value = textOutput("daily_average"),
                          showcase = fa("chart-column"),
                          theme = "info",
                          # p("mosquitos per day"),
                          # showcase_layout = "top right"
                ),
                
                value_box(title = "Longest quiet",
                          value = textOutput("peace_duration"),
                          showcase = fa("peace"),
                          theme = "info",
                          # p(textOutput("peace_dates")),
                ),
                
                value_box(title = "Last entry",
                          value = textOutput("last"),
                          showcase = bs_icon("align-end"),
                          theme = "info"
                          
                ),
                
                width = 0.5
              ),
              col_widths = c(8,4)
            ),
            
            layout_column_wrap(
              width = 0.5,
              heights_equal = "row",
              
              card(
                leafletOutput("map", height = "600px"),
              ),
              navset_card_pill(
                # title = "look at the data",
                nav_panel("All data", 
                          layout_columns(
                            plotlyOutput("hist_date"),
                            layout_columns(
                              p("This graph tracks the number of mosquitos during different times. You can see that the peak is around June."),
                              p("You can adjust the resolution of the figure to see the data in a higher or lower resolution."),
                              
                              sliderInput("bins", 
                                          "Days per column:",
                                          min = 1, 
                                          max = 15, 
                                          value = 7),
                              col_widths = c(8,4)
                              
                            )
                          )
                ),
                
                nav_panel("By hour", 
                          layout_columns(
                            plotlyOutput("hist_time"),
                            p("This graph tracks the number of mosquitos per each hour of the day."),
                            
                            # )
                          ),
                ),
                nav_panel("By season", 
                          layout_columns(
                            plotlyOutput("seasonality"),
                            p("This graph tracks the number of mosquitos per each hour of the day."),
                            
                            # )
                          ),
                ),
              ),
              
              
              # col_widths = c(4, 4)
            )
  ),
  
  nav_panel("Methods",
            id = "methods",
            layout_columns(
              card(
                p("Counting all mosquitoes in an area is nearly impossible, so we developed a method to estimate their numbers without counting each one. While we could tally every time we hear a buzz, this would likely lead to counting the same mosquito multiple times. Instead, we only count when a mosquito is caught, ensuring each one is counted just once."),
                p("To make this easy, we created a custom iOS shortcut that collects key data, like the time and location of each catch, and sends it to a Google Form. This allows users to track data quickly with a single tap or even a Siri command."),
                p("The collected data is then processed using R, and the interface for visualization and analysis is built with Shiny. To protect privacy, we add noise to the data, so the locations shown on the map remain informative while keeping individual information private."),
                p("You can find the source files for this project on our ", 
                  a("GitHub", href = "https://github.com/your-repo-link", target = "_blank"), ".")
              ),
              layout_columns(
                # Add images for each tool/service
                card(
                  img(src = "path/to/ios_shortcut_image.png", height = "150px"),
                  p("Custom iOS Shortcut")
                ),
                card(
                  img(src = "path/to/google_form_image.png", height = "150px"),
                  p("Google Form")
                ),
                card(
                  img(src = "path/to/r_image.png", height = "150px"),
                  p("R Programming")
                ),
                card(
                  img(src = "path/to/shiny_image.png", height = "150px"),
                  p("Shiny App")
                ),
                col_widths = c(-2, 6, 2, -2)
              )
            )
  ),
  
  nav_panel("Data",
            titlePanel("Data Sources and Methodology"),
            h3("Data Collection"),
            p("We collect data through a network of IoT-enabled mosquito traps and manual field observations."),
            h3("Data Processing"),
            p("Raw data is cleaned, validated, and processed using advanced algorithms to ensure accuracy."),
            h3("Updates"),
            p("The dashboard is updated daily with the latest mosquito population data.")
  ),
  
  nav_spacer(),
  nav_item(
    # Include GitHub button HTML
    tags$head(
      tags$script(src = "https://buttons.github.io/buttons.js"),
      tags$link(rel = "stylesheet", href = "https://buttons.github.io/buttons.css")
    ),
    
    
    # Place the GitHub button
    tags$div(
      tags$a(
        class = "github-button",
        href = "https://github.com/schwarz-noam/buzz-watch",
        `data-color-scheme` = "no-preference: light; light: light; dark: dark;",
        `data-size` = "large",
        `aria-label` = "View source on GitHub",
        "View souce on GitHub"
      )
    )
  ),
  nav_item(input_dark_mode(id = "darkmode", mode = "dark"))
)


server <- function(input, output, session) {
  bs_theme <- bs_theme(version = 5, bootswatch = "flatly")
  session$setCurrentTheme(bs_theme)  # Set the theme for the session
  
  color_values <-  reactive({
    input$darkmode
    
    current_theme = bs_current_theme()
    
    if (input$darkmode == "dark") {
      list(
        success = bs_get_variables(current_theme, "success"),
        info = bs_get_variables(current_theme, "info"),
        
        plot_background = bs_get_variables(current_theme, "primary"),
        plot_text = bs_get_variables(current_theme, "light")
      )
    } else {
      list(
        success = bs_get_variables(current_theme, "success"),
        info = bs_get_variables(current_theme, "info"),
        
        plot_background = bs_get_variables(current_theme, "bg"),
        plot_text = bs_get_variables(current_theme, "primary")
      )
    }
  })
  
  # Read data from Google Sheet
  private_data <- reactive({
    data <- read_sheet(sheet_url)
    
    seasons <- list(
      spring_equinox = as_date("0001-03-20"),
      summer_solstice = as_date("0001-06-21"),
      autumn_equinox = as_date("0001-09-22"),
      winter_solstice = as_date("0001-12-21")
    )
    
    # Process the data
    data = data %>%
      mutate(
        date = as_date(date, format = "%d/%m/%Y"),
        time = as_hms(time),
        p_longitude = longitude + rnorm(n(), mean = 0, sd = 0.005),
        p_latitude = latitude + rnorm(n(), mean = 0, sd = 0.005),
        
        only_dm = make_date(year = 1, month = month(date), day = day(date)),
        season = factor(case_when(
          only_dm < seasons$spring_equinox ~ "Winter",
          between(only_dm, as_date("01-01-01"), seasons$spring_equinox) ~ "Winter",
          between(only_dm, seasons$spring_equinox, seasons$summer_solstice) ~ "Spring",
          between(only_dm, seasons$summer_solstice, seasons$autumn_equinox) ~ "Summer",
          between(only_dm, seasons$autumn_equinox, seasons$winter_solstice) ~ "Autumn",
          # between(only_dm, seasons$winter_solstice, as_date("31-12-01")) ~ "Winter",
          TRUE ~ "Undefined"))
        
      ) %>% 
      dplyr::arrange(date)
  })
  
  output$total_n = renderText({nrow(private_data())})
  output$daily_average = renderText({
    round(
      difftime(max(private_data()$date), min(private_data()$date))/nrow(private_data()),
      2)
    
  })
  output$first = renderText({format(min(private_data()$date), "%d/%m/%Y")})
  output$last = renderText({format(max(private_data()$date), "%d/%m/%Y")})
  
  longest_peace_info <- reactive({
    sorted_dates <- private_data() %>% 
      pull(date)
    
    date_diffs <- diff(sorted_dates)
    longest_peace_index <- which.max(date_diffs)
    
    start_date <- sorted_dates[longest_peace_index]
    end_date <- sorted_dates[longest_peace_index + 1]
    longest_peace_duration <- as.numeric(difftime(end_date, start_date, units = "days"))
    
    list(
      duration = longest_peace_duration,
      start = start_date,
      end = end_date
    )
  })
  
  output$peace_duration <- renderText({
    longest_peace_info()$duration
  })
  
  output$peace_dates <- renderText({
    # info <- longest_peace_info()
    
    if (month(longest_peace_info()$start) == month(longest_peace_info()$end) & year(longest_peace_info()$start) == year(longest_peace_info()$end)) {
      
      format(paste(
        "In", format(longest_peace_info()$start, "%b '%y")
      ))
    } else {
      tolower(paste(
        "from", format(longest_peace_info()$start, "%b '%y"),
        "to", format(longest_peace_info()$end, "%b '%y")
      ))
    }
    
    
  })
  
  output$map <- renderLeaflet({
    current_colors = color_values()
    
    emoji_icon <- makeIcon(
      iconUrl = "",  # Example emoji URL (Mosquito)
      iconWidth = 30,  # Set width
      iconHeight = 30  # Set height
    )
    
    awesome_icon <- makeAwesomeIcon(
      icon = "mosquito",
      iconColor = "black",
      markerColor = current_colors$success,
      library = "fa"
    )
    
    
    
    leaflet(private_data()) %>%
      addProviderTiles(if (input$darkmode == "dark") {"Stadia.AlidadeSmoothDark"} else {"Stadia.AlidadeSmooth"}) %>%
      addAwesomeMarkers(
        lng = ~p_longitude, 
        lat = ~p_latitude,
        icon = awesome_icon,  # Use the custom emoji icon
        popup = ~paste("<b>Date:</b>", format(date, "%b %Y"), "<br>",
                       "<b>Time:</b>", paste0(hour(time), ":", minute(time)), "<br>",
                       "<b>City:</b>", ifelse(is.na(city), "N/A", city)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto",
          opacity = 0.8,
          permanent = FALSE,
          sticky = FALSE,
          noHide = FALSE,
          offset = c(0, -30),
          interactive = TRUE,
          delay = 500
        )) %>% 
      setView(lng = median(private_data()$p_longitude), 
              lat = median(private_data()$p_latitude), 
              zoom = 13)
  })
  
  output$hist_date = renderPlotly({
    
    current_colors = color_values()
    
    fig = private_data() %>%
    # data %>%
      ggplot(aes(x = date)) +
      geom_histogram(
        aes(text = paste(..count.., "Mosquitos<br>between ", format(..x.., "%d/%m/%Y"), "-", format(..xmax.., "%d/%m/%Y"))),
        binwidth = input$bins,
        fill = current_colors$info,
        # show.legend = F
      ) +
      labs(x = "Date",
           y = "Count") +
      scale_x_date(date_labels = "%b '%y", date_breaks = "2 month") +
      theme_classic() +
      theme(plot.background = element_rect(fill = current_colors$plot_background),
            panel.background = element_rect(fill = current_colors$plot_background),
            text = element_text(color = current_colors$plot_text),
            axis.line = element_line(color = current_colors$plot_text),
            axis.ticks = element_line(color = current_colors$plot_text),
            axis.text = element_text(color = current_colors$plot_text),
            legend.position = "none")
    
    ggplotly(fig,
             tooltip = "text") %>% 
             # mapping = aes(text = format(date, "%d/%m/%Y")),
      layout(showlegend = F) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  output$hist_time = renderPlotly({
    current_colors = color_values()
    
    fig = private_data() %>% 
      ggplot(aes(x = hour(time))) +
      geom_histogram(aes(text = paste("Count:", ..count.., "<br>Hour:", round(..x.., 0))),
                     bins = 24,
                     fill = current_colors$info) +
      labs(x = "Time",
           y = "Count") +
      scale_x_continuous(breaks = seq(0, 24, 6),
                         minor_breaks = seq(0, 24, 3)) +
      theme_classic() +
      theme(plot.background = element_rect(fill = current_colors$plot_background),
            panel.background = element_rect(fill = current_colors$plot_background),
            text = element_text(color = current_colors$plot_text),
            axis.line = element_line(color = current_colors$plot_text),
            axis.ticks = element_line(color = current_colors$plot_text),
            axis.text = element_text(color = current_colors$plot_text))
    
    
    ggplotly(fig,
             tooltip = c("text")) %>% 
      
      plotly::config(displayModeBar = FALSE)
  })
  
  output$seasonality = renderPlotly({
    current_colors = color_values()
    
    season_colors <- c(
      Spring = "#FFB3BA",   # Spring (soft pink)
      Summer = "#FFDFBA",   # Summer (light peach)
      Autumn = "#BAE1FF",   # Autumn (light blue)
      Winter = "#B9FBC0"    # Winter (soft green)
    )
    
    outline_colors <- c(
      Spring = "#D50032",   # Spring (bold crimson red)
      Summer = "#FF8F00",   # Summer (vibrant orange)
      Autumn = "#2979FF",   # Autumn (strong blue)
      Winter = "#00BFAE"    # Winter (vivid teal)
    )
    
    
    fig = private_data() %>%
    # data %>% 
      ggplot(aes(x = season, fill = season, group = season, color = season)) +
      geom_bar(aes(text = paste("Count:", ..count..))) +
      scale_fill_manual(values = season_colors) +  # Use the defined colors
      scale_color_manual(values = outline_colors) +  # Use the defined colors
      theme_classic() +
      theme(plot.background = element_rect(fill = current_colors$plot_background),
            panel.background = element_rect(fill = current_colors$plot_background),
            text = element_text(color = current_colors$plot_text),
            axis.line = element_line(color = current_colors$plot_text),
            axis.ticks = element_line(color = current_colors$plot_text),
            axis.text = element_text(color = current_colors$plot_text))
    
    
    ggplotly(fig,
             tooltip = c("text")) %>% 
      layout(showlegend = F) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  
}

shinyApp(ui, server)
