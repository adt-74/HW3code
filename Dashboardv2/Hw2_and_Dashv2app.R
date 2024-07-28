# library(dplyr) # data wrangling
# library(readr) # reading data
# library(ggplot2) # data vizualization

# library(shiny) # main shiny app package
# library(bslib) # easier html construction
# library(plotly) # interactive visuals


# We don't have anything in particular to put in our global function at the minute
global = function(){
  library(dplyr) # data wrangling
  library(readr) # reading data
  library(ggplot2) # data vizualization
  library(tidygraph)
  library(shiny) # main shiny app package
  library(bslib) # easier html construction
  library(plotly) # interactive visuals
  library(sf)
}

read_data = function() {
  # Get any helper data you need to starts
  
    # 1) read county data
  read.csv("./County.csv") %>%
    saveRDS("county_names.rds")
  county_names=read_rds("./county_names.rds") %>% 
              filter(state == "Florida")
  county_names$county <- as.integer(county_names$county)
  
  countygeo = read_sf("./counties.geojson")
 
  
  from_county_names = county_names %>%
    filter(state == "Florida") %>% 
    rename(
      from_countyid = county,
      from_county_name = county_name,
      from_state = state
    )
    
  
  to_county_names = county_names %>%
    filter(state == "Florida") %>% 
    rename(
      to_countyid = county,
      to_county_name = county_name,
      to_state = state
)
  
  
  
  # # 2) read nodes data
  # read_rds("./dorian.rds") %>%
  #   # Focus on just the nodes of the network
  #   activate("nodes") %>%
  #   # Turn it into a tibble/data.frame
  #   as_tibble() %>%
  #   # Create a unique node id,
  #   # which we'll use to join in geoid to the from and to columns of the edges
  #   mutate(node = 1:n()) %>%
  #   # Reorder the variables; dropping geometry
  #   select(node, geoid, svi, pop, county) %>%
  #   # Save the nodes
  #   saveRDS("nodes.rds")
  # nodes=read_rds("./nodes.rds")
  # # Convert county column to integer
  # nodes$county <- as.integer(nodes$county)
  # 
  # # 3) read edges data
  # # Save the edges to file.
  # read_rds("./dorian.rds") %>%
  #   # Focus on just the edges of the network
  #   activate("edges") %>%
  #   # Turn it into a tibble/data.frame
  #   as_tibble() %>%
  #   # Drop the geometry 
  #   select(-geometry) %>%
  #   # Let's join the source (from) geoid in using the shared node id
  #   left_join(by = c("from" = "node"), y = nodes %>% select(node, from_geoid = geoid)) %>%
  #   # Let's join the destination (to) geoid in using the shared node id 
  #   left_join(by = c("to" = "node"), y = nodes %>% select(node, to_geoid = geoid)) %>%
  #   # Save this to file
  #   saveRDS("edges.rds")
  # edges=read_rds("./edges.rds")
  # 
  
  
  #######################################################################
  # read modified nodes data
  read_rds("dorian.rds") %>%
    # Focus on just the nodes of the network
    activate("nodes") %>%
    # Turn it into a tibble/data.frame
    as_tibble() %>%
    # Create a unique node id,
    # which we'll use to join in geoid to the from and to columns of the edges
    mutate(node = 1:n()) %>%
    # Reorder the variables; dropping geometry
    select(node, geoid, social_capital:evacuation_less, geometry) %>%
    # Save the nodes
    saveRDS("nodes.rds")
  
  # Load the nodes in
  nodes = read_rds("nodes.rds")
  
  
  # Save the edges to file.
  edges <- read_rds("dorian.rds") %>%
    # Focus on just the edges of the network
    activate("edges") %>%
    # Turn it into a tibble/data.frame
    as_tibble() %>%
    # Drop the geometry 
    # select(-geometry) %>%
    # Let's join the source (from) geoid in using the shared node id
    left_join(by = c("from" = "node"), y = nodes %>% select(node, from_geoid = geoid)) %>%
    # Let's join the destination (to) geoid in using the shared node id 
    left_join(by = c("to" = "node"), y = nodes %>% select(node, to_geoid = geoid)) %>%
    # Save this to file
    saveRDS("edges.rds")
  edges = read_rds("edges.rds")
  
  
  
  # nodes%>%
  #   left_join(by=c("county"="county"), y=county_names %>% select(county, county_name, state)) %>%
  #   saveRDS("nodes_n_county.rds")
  # nodes_n_county=read_rds("./nodes_n_county.rds")
  
  edges %>%
    mutate(from_countyid = as.integer(substr(from_geoid,1,5)), to_countyid = as.integer(substr(to_geoid,1,5))) %>%
    left_join(county_names, by=c("from_countyid" = "county")) %>%
    rename(from_county_name = county_name) %>%
    left_join(county_names, by =c("to_countyid" = "county")) %>% 
    #Joins the county names table tother to match with the and from county ids
    rename(to_county_name = county_name) %>%
    select(from:from_county_name, to_county_name, state = state.x) %>%
    mutate(evac_shel = if_else(evacuation > 0, "Evacuated", "Sheltered")) %>%
    left_join(
      by = c("from" = "node"),
      y = nodes %>% select(node, svi, pop)) %>% 
    # # join from node
    # left_join(by = c("from" = "node"), y = nodes %>% select(node, from_geoid = geoid, svi, pop)) %>%
    # # join to node
    # left_join(by = c("to" = "node"), y = nodes %>% select(node, to_geoid = geoid, svi, pop)) %>%
    saveRDS("nodes_n_edges.rds")
}

ui = function(){   
  read_data()
  # Load the nodes in
  nodes=read_rds("./nodes.rds")
  nodes_n_edges=read_rds("./nodes_n_edges.rds")
  
  
  nodes_n_edges %>%
    # Get the day of analysis.
    mutate(day = lubridate::day(date_time)) %>%
    # Get the month
    mutate(month = lubridate::month(date_time)) %>%
    # Get the hour
    mutate(hour = lubridate::hour(date_time)) 
  
  # Get unique counties
  from_counties <- unique(nodes_n_edges$from_county_name)
  to_counties <- unique(nodes_n_edges$to_county_name)
  ev_sh <- unique(nodes_n_edges$evac_shel)
  
  # Make a named vector, so you can select by Names but get back specific ids, eg. "Dec" = 12
  choices_from_county = setNames(object=from_counties, nm=from_counties)
  choices_min_date = min(nodes_n_edges$date_time)
  choices_max_date = max(nodes_n_edges$date_time)
  choices_ev_sh = setNames(object=ev_sh, nm=ev_sh)
  choices_to_county = setNames(object = to_counties, nm = to_counties)
  
  # choices_airlines = setNames(object = airlines$carrier, nm = airlines$name)
  # choices_origins = setNames(object = origins$origin, nm = origins$origin_abb)  
  
  # TITLE CARD ###################################
  c1 = card(
    # Make a card header whose background is the primary color (class = bg-primary)
    card_header(class = "bg-primary",
                # Add this title                
                card_title("Analysis of Hurricane Dorian Evacuation (2019)"))
  )
  
  # SELECTOR CARD #################################
  c2 = bslib::card(
    # Make a simple card header and title 
    card_header(card_title("FILTERS")),
    # Make a card body section
    card_body(
      sliderInput("dateRange",
                  "Select Date Range:",
                  min = as.Date(choices_min_date),
                  max = as.Date(choices_max_date),
                  value = c(
                    as.Date("2019-08-28"),
                    as.Date("2019-09-7")),
                  timeFormat = "%Y-%m-%d")
    ),
    card_body(
      selectInput(inputId = "evac_shel", label = "Evacuated or Sheltered", choices = choices_ev_sh, selected = "Evacuated")
    ),
    card_body(
      selectInput(inputId = "from_county_name", label = "From County", choices = choices_from_county, selected = sample(from_counties, 1))
    # card_body(
    #     selectInput(inputId = "to_county_name", label = "To County ", choices = choices_from_county, selected = sample(from_counties, 1))
    ),
    card_body(
      selectInput(inputId = "to_county_name", label = "To County", choices = choices_to_county, selected = sample(to_counties, 1), selectize = FALSE, size = 5)
  ))
  
  # PLOT CARD ##########################
  c3 = bslib::layout_column_wrap(
    card(plotlyOutput(outputId = "plot_one")),
    card(plotlyOutput(outputId = "plot_two")),
    card(plotlyOutput(outputId = "plot_three")),
    width = 0.5
  )
  # TEXT CARD ##############################  
  c4 = bslib::card(
    bslib::card_header("Spotlight", class = "bg-dark"),
    bslib::card_footer(textOutput("text_highlight")))
  
  ######################################################### added in new c5 and c6
  #Spacial Map
  c5 = bslib::layout_column_wrap(
    card(plotlyOutput(outputId = "map_one")),
    card(plotlyOutput(outputId = "plot_five")),
    card(plotlyOutput(outputId = "plot_six")),
    width = 0.5)
  
  
  
  c6 = bslib::card(
    bslib::card_header("Spotlight", class = "bg-dark"),
    bslib::card_footer(textOutput("topCounties")))
  
  # VALUE BOXES CARD ##########################
  # box1 = bslib::value_box(
  #   title = "Mean Delay", value = textOutput("text_mean"), "minutes",
  #   class = "bg-primary text-light",
  #   # add a fontawesome icon to showcase
  #   showcase = shiny::icon("clock"))
  # box2 = bslib::value_box(
  #   title = "Average Error (SE)", value = textOutput("text_se"), "minutes",
  #   class = "bg-warning text-light",
  #   # add a fontawesome icon to showcase
  #   showcase = shiny::icon("hashtag"))
  # box3 = bslib::value_box(
  #   title = "Sample Size", value = textOutput("text_n"), "flights",
  #   class = "bg-dark text-light", 
  #   # add a fontawesome icon to showcase
  #   showcase = shiny::icon("plane"))
  # Bundle them together with a header
  # c5 = card(
  #   # Add a header describing the Selections you made
  #   card_header(class = "bg-primary", card_title(textOutput("text_selection"))),
  #   # Bundle the value boxes together
  #   card_body(
  #     layout_column_wrap(box1,box2,box3, width = 1/3)      
  #   )
  # )
  
  # TABLE CARD ################################
  # c6 = card( tableOutput("table_one_month") )
  
  
  
  # Or add a sidebar-main split layout like this...  
  bslib::page(
    title = "Hurricane Dorian", 
    # add a bootstrap theme to the page
    theme = bslib::bs_theme(preset = "cerulean"),
    # Stack cards
    c1, # header
    # Put next cards in a sidebar-main panel split layout
    bslib::layout_sidebar(
      # Sidebar...
      sidebar = bslib::sidebar(c2), 
      # main panel
      # c5,
      # Make a series of panels we can click between
      bslib::navset_card_pill(
        selected = "plots",
        # Open plots
        bslib::nav_panel(title = "VISUALS", value = "plots", c3, c4),# plots
        bslib::nav_panel(title = "Spatial Data", value = "maps", c5, c6)
        # Or Open table
        # bslib::nav_panel(title = "TABLE", value = "tables", c6), # table
      )
    )
    
  )
  
}


server = function(input, output, session) {  
  # Load the nodes in
  nodes=read_rds("./nodes.rds")
  nodes_n_edges=read_rds("./nodes_n_edges.rds")
  countygeo = read_sf("./counties.geojson")
  
  stat = reactive({
    # Start by filtering the data based on the input county
    nodes_n_edges %>%
      mutate(date = as.Date(date_time), time = format(as.POSIXct(date_time),format = "%H:%M:%S")) %>%
      # Filter to just rows where county matches the user input
      # filter(county_name %in% input$county_name)
      filter(date > input$dateRange[1] & date < input$dateRange[2])
    # use side bar filter for date
    # Trigger whenever input$origin changes
  }) %>% bindEvent({input$dateRange[1];input$dateRange[2];input$to_county_name})
  
  #stat2 to generate highlighted text
  
  stat2 = reactive({
    # Start by filtering the data based on the input county
    nodes_n_edges %>%
      mutate(date = as.Date(date_time), time = format(as.POSIXct(date_time),format = "%H:%M:%S")) %>%
      # Filter to just rows where county matches the user input
      # filter(county_name %in% input$county_name)
      filter(date > input$dateRange[1] & date < input$dateRange[2]) %>%
      #sorts date range to examine hurricane
      filter(evac_shel == "Evacuated") %>%
      filter(from_county_name == input$from_county_name) %>%
      #group_by(from_county_names = input$from_county_name) %>%
      summarize(AVG_EVAC = mean(evacuation))
  }) %>% bindEvent({input$dateRange[1];input$dateRange[2];input$from_county_name})
  
  nodes_n_edges_stat = reactive({
    # Start by filtering the data based on the input county
    nodes_n_edges %>%
      mutate(date = as.Date(date_time),
              time = format(as.POSIXct(date_time), format = "%H:%M:%S")) %>%
        # Filter to just rows where county matches the user input
        # filter(county_name %in% input$county_name)
        filter(date > input$dateRange[1] &
                date < input$dateRange[2]) %>% # use side bar filter for date
      filter(to_county_name == input$to_county_name) # use side bar filter for to_county_name
    # Trigger whenever input$origin changes
  }) %>% bindEvent({
    list(input$dateRange, input$to_county_name)
  })
  
  
  ########################################################
  # Plot Outputs
  
  output$plot_one = renderPlotly({
    ## DateTime Vs. Mean Number of Evacuation
    plot_one_data=stat() %>%
      #sorts date range to examine hurricane
      group_by(date) %>%
      summarize(AVG_EVAC = mean(evacuation))
    
    gg_plot_one=ggplot(data = plot_one_data, mapping = aes( x = date, y = AVG_EVAC)) +
      geom_line(linewidth = 2, color = "salmon") +
      geom_hline(yintercept = 0, linetype = "solid", color = "black")+
      scale_x_date(date_breaks = "1 day", date_labels = "%m-%d") +
      theme_minimal()+
      theme(axis.title.y = element_text(size = 12, vjust = 1),
            plot.title = element_text(size = 12, hjust = 0.5, margin = margin(t = 20)),
            axis.text.x = element_text(angle = 0, hjust = 1),
            axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.position = "bottom")+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(x = "Date",
           y = "Evacuations",
           title = paste("Average Evacuations Over Time From", input$dateRange[1], " to ", input$dateRange[2]),
           color = "AVG_EVAC")
    
    
    # Make it plotly
    pp_plot_one = plotly::ggplotly(gg_plot_one, tooltip = c("Date", "Evacuations"))
    # return the visualization
    pp_plot_one
    # Trigger this plot to rerender when input$month changes
  }) %>% bindEvent({ stat(); input$dateRange })
  
  ## top 10 dest count
  output$plot_two = renderPlotly({
    plot_two_data=stat() %>% # use side bar filter for date
      filter(evac_shel == input$evac_shel) %>%  # use side bar filter EVAC OR SHELTERED
      group_by(from_county_name) %>%
      summarise(total_evacuations = sum(evacuation)) %>%
      arrange(desc(total_evacuations)) %>%
      head(10)
    
    gg_plot_two <- ggplot(data = plot_two_data, mapping = aes(
      x = reorder(from_county_name, total_evacuations), 
      y = total_evacuations, fill = total_evacuations)) +
      geom_col(size = 2) +
      coord_flip() +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      scale_fill_gradient(low = "skyblue", high = "blue") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))+
      labs(
        x = "Counties",
        y = "Evacuation Flow",
        fill = "Total Evacuations",
        title = paste("Top 10 Places Individuals Evacuated From", input$dateRange[1], "to", input$dateRange[2]),
        caption = "Positive values = Evacuations, Negative Values = Sheltered in Place")
    
    
    # Make it plotly
    pp_plot_two = plotly::ggplotly(gg_plot_two)
    # return the visualization
    pp_plot_two
    # Trigger this plot to rerender when input$month changes
  }) %>% bindEvent({ stat(); list(
    input$evac_shel, input$dateRange)
  })
  
  output$plot_three = renderPlotly({
    # for specific county, total evacuation
    plot_three_data = stat() %>%
      filter(evac_shel == input$evac_shel) %>%  # use side bar filter EVAC OR SHELTERED
      filter(from_county_name == input$from_county_name) %>%  # use side bar filter for county
      group_by(from_county_name, to_county_name) %>%
      summarise(total_evacuations = sum(evacuation), count = n()) %>%
      arrange(desc(total_evacuations))
    
    gg_plot_three = ggplot(data = plot_three_data, mapping = aes(x = to_county_name, y = total_evacuations)) +
      geom_col(size = 2) +
      theme_classic()+
      labs(x = "County",
           y = input$evac_shel,
           title = paste("Individuals", input$evac_shel, "from ", input$from_county_name," to:"))+ 
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")+
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    
    # Make it plotly
    pp_plot_three = plotly::ggplotly(gg_plot_three)
    # return the visualization
    pp_plot_three
    # Trigger this plot to rerender when input$month changes
  }) %>% bindEvent({ stat(); list(
    input$evac_shel, input$from_county_name_name )
  })
  
 
  
  ##################################################################################################################################### 
  output$map_one = renderPlotly({
    
    #data needed to crate the spacial map to show where the countys are. 
    
    data_sf <- stat() %>% 
      filter(evac_shel == "Evacuated", state == "Florida", to_county_name == input$to_county_name) %>% 
      select(from_county_name,to_county_name,from_countyid, to_countyid, from_geoid,to_geoid, geometry,km, evacuation, date) %>% 
      group_by(from_county_name,to_county_name, geometry) %>% 
      summarise(sum_evac= sum(evacuation), distance = mean(km)) %>% 
      arrange(desc(sum_evac)) %>%
      st_as_sf()
    
 
    counties_f <- countygeo %>% 
      filter(state == "FL") %>% 
      mutate(geoid = as.integer(geoid))
    
    
    
    #creates the top counties to be joined to crate county nodes
    countytotal <- stat() %>% 
      filter(evac_shel == "Evacuated", state == "Florida", to_county_name == input$to_county_name) %>% 
      select(from_countyid,, from_county_name, evacuation) %>%
      group_by(from_countyid, from_county_name) %>%
      summarise(sum_evac = sum(evacuation)) %>% 
      arrange(desc(sum_evac)) %>% 
      left_join(counties_f %>% select(geoid, geometry), by = c("from_countyid" = "geoid")) %>%
      st_as_sf()

    
    
    
    centroids <- st_centroid(countytotal)

    
    
    map_one <-ggplot() +
      geom_sf(data = counties_f, fill = "transparent", color = "black") +
      geom_sf(data = countytotal, aes(geometry = geometry, fill = sum_evac), color = "red") + # Fill based on sum_evac
      geom_sf(data = data_sf, aes(geometry = geometry, color = distance), size = 1.5) +
      geom_text(data = centroids, aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], label = from_county_name), size = 3, color = "black", nudge_y = 0.1) + 
      scale_fill_viridis_c(option = "rocket", direction = -1, name = "Total Evacuations") +
      scale_color_viridis_c(option = "plasma", direction = -1, name = "Distance Traveled in KM") +
      labs(title = paste("Evacuations to " ,input$to_county_name, " between ", input$dateRange[1], " and ", input$dateRange[2]), x = "Longitude", y = "Latitude") +
      theme_minimal() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
    
  }) %>% bindEvent({ stat(); list(
    input$to_county_name, input$dateRange )
  }) 
  output$plot_six = renderPlotly({
    plot_six_data = stat() %>% # use side bar filter for date
      filter(evac_shel == input$evac_shel) %>%  # use side bar filter EVAC OR SHELTERED
      filter(to_county_name == input$to_county_name) %>%  # use side bar filter for county
      group_by(from_county_name) %>%
      summarise(total_distance_traveled = mean(km),
                avg_svi = mean(svi)) %>%
      arrange(desc(total_distance_traveled)) %>%
      head(10)
    
    gg_plot_six = ggplot(
      data = plot_six_data,
      mapping = aes(
        x = reorder(from_county_name, total_distance_traveled),
        y = total_distance_traveled,
        fill = avg_svi
      )
    ) +
      geom_col(size = 2) +
      coord_flip() +
      theme_classic() +
      labs(
        x = "From Counties",
        y = "Distance Traveled (km)",
        fill = "Social Vulnerability Index",
        title = paste(
          "Average Distance Traveled",
          "to",
          input$to_county_name,
          "\n",
          "between",
          input$dateRange[1],
          "and",
          input$dateRange[2]
        )
      ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_log10(
        labels = function(x)
          format(x, scientific = FALSE)
      ) +
      
      scale_fill_gradient(low = "green", high = "red")
    
    # Make it plotly
    pp_plot_six = plotly::ggplotly(gg_plot_six)
    # return the visualization
    pp_plot_six
    # Trigger this plot to rerender when input$month changes
  }) %>% bindEvent({
    stat()
    list(input$evac_shel, input$dateRange, input$to_county_name)
  })
  
  
  
  output$plot_five = renderPlotly({
    data = read_rds("./dorian.rds")
    
    # Let's grab nodes from this network (cities / county-subdivisions)
    plot_five_nodes_data = data %>%
      # Focus on just the nodes of the network
      activate("nodes") %>%
      # Turn it into a tibble/data.frame
      as_tibble() %>%
      # Make it into a spatial data.frame
      st_as_sf()
    
    plot_five_edges_data = nodes_n_edges_stat() %>%
      # Make it into a spatial data.frame
      st_as_sf()
    
    # Visualize spatial data using the geometry column of an sf object using geom_sf()
    gg_plot_five = ggplot() +
      geom_sf(
        data = plot_five_nodes_data,
        mapping = aes(fill = svi),
        shape = 21,
        color = "white",
        size = 2
      ) +
      geom_sf(data = plot_five_edges_data, mapping = aes(alpha = nrow(plot_five_edges_data))) +
      scale_fill_gradient(low = "blueviolet", high = "chartreuse") +
      theme(axis.text.x = element_text(size = 9, margin = margin(t = 50, b = 50))) +
      theme(axis.text.y = element_text(size = 9, margin = margin(t = 50, b = 50)))
    
    # Make it plotly
    pp_plot_five = plotly::ggplotly(gg_plot_five)
    # return the visualization
    pp_plot_five
  })
  
  
  #####################################################################################################
  
  #Adds a highlighted text to show top 3 couinties where poeple evacuated to the selcetd to county

  
  
  top_counties <- reactive({
    stat() %>% 
      filter(evac_shel == "Evacuated", state == "Florida", to_county_name == input$to_county_name) %>% 
      select(from_countyid, from_county_name, evacuation) %>%
      group_by(from_countyid, from_county_name) %>%
      summarise(sum_evac = sum(evacuation)) %>% 
      arrange(desc(sum_evac)) %>% 
      head(3)
      })
  
  # Display the top 3 counties as text
  output$topCounties <- renderText({
    top_data <- top_counties()
    paste("Top 3 counties with the highest number of evacuations to: ",input$to_county_name, 
          paste(top_data$from_county_name, "(", top_data$sum_evac, ")", collapse = ", "))
  })
  
 ##################################################################################################### 
  
  
  stat_highlight = reactive({
    # Let's get some highlight stats for your carrier at one specific time
    stat2() %>%
      #summarize(highlight_mean = mean(AVG_EVAC, na.rm = TRUE)) %>%
      # Format a number for highlighting
      mutate(highlight = scales::number(AVG_EVAC, accuracy = 0.1) ) %>%
      # Summarize a label
      mutate(label = paste0(
        "Between ",input$dateRange[1], " and ",input$dateRange[2] ,
        " there was an avg of ", highlight, " number of evactuation in the selected county ",
        input$from_county_name, "."))
    #select(label)
    
    # When EITHER stat() or carrier or month changes, update this text.
  }) %>% bindEvent({ stat2()})
  
  
  
  
  ## Render to text output 'text_highlight'
  output$text_highlight = renderText({
    # Output a single text blob value. Must have just length 1.
    stat_highlight()$label
    # Trigger whenever stat_highlight() changes
  }) %>% bindEvent({ stat_highlight() })
  
  
  ##################################################################################### 
  
  
}

# Run app
shiny::shinyApp(ui = ui, server = server, onStart = global)









# gg_plot_two=ggplot(data = plot_two_data, mapping = aes(
#   x = reorder(from_county_name, total_evacuations), 
#   y = total_evacuations, fill = total_evacuations)
# ) +
#   geom_col(size = 2) +
#   coord_flip() +
#   scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
#   scale_fill_gradient(low = "skyblue", high = "blue")+
#   theme_classic()+
#   theme(plot.title = element_text(hjust = 0.5))+
#   theme(plot.caption = element_text(hjust=0.5)) +
#   labs(x = "Counties",
#        y = "Evacuation Flow",
#        fill = "Total Evacuations",
#        title = paste("Top 10 Places Individuals Evacuated From", input$dateRange[1], "and", input$dateRange[2]),
#        caption = "Postive values = Evacuations, Negative Values = Sheltered in Place")