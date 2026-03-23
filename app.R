library(shiny)
library(shinyMobile)
library(shinyTime)
library(RPostgreSQL)
library(DBI)
library(dplyr)
library(tidyr)
library(googleway)
library(exifr)
library(DT) # Ensure this is loaded for dataTableOutput to work correctly

# Source the database connection script
source("connectKennedy.R")

# Server logic to fetch angler names
fetchAnglerNames <- function() {
  con <- connectKennedy()
  if (!is.null(con)) {
    query <- "SELECT DISTINCT angler FROM fishdat ORDER BY angler"
    angler_data <- dbGetQuery(con, query)
    angler_data <- c("", angler_data$angler)
    dbDisconnect(con)
    return(angler_data)
  }
  return(character(0))
}

# Points system for species
species_points <- c(Bluegill = 1, Redear = 1, Green_Sunfish = 1, Largemouth_Bass = 2,
                    Warmouth = 3, Bullhead_Catfish = 3, Black_Crappie = 5, Chain_Pickerel = 10,
                    Channel_Catfish = 10, Grass_Carp = 12, Tiger_Muskie = 25)


# Custom CSS to fix the dropdown issue
customCSS <- "
.dropdown-menu {
z-index: 9999 !important;
}

.selectize-input input {
color: #000000 !important; /* Change text color to black */
}

.shiny-input-container {
  padding-left: 20px;  /* Adjust this value as needed */
}

#submit {
  display: block; /* Make sure the button is a block-level element */
  margin: 0 auto; /* Auto margins on the left and right */
  width: 50%; /* Adjust the width as needed */
  height: 60px; /* Adjust the height as needed */
}

/* Optional: Custom CSS to style the image */
.top-image {
  max-width: 350px;
  height: auto;
  display: block;
  margin: auto;
  padding: 10px; /* Adjust or remove padding as needed */
  border-radius: 15px; /* This creates rounded corners */
}
"

app_opts <- list(
    theme = "md",
    dark = FALSE,
    filled = TRUE,
    color = "#2e2e2e",
    touch = list(
      tapHold = TRUE,
      tapHoldDelay = 750,
      iosTouchRipple = FALSE
    ),
    iosTranslucentBars = FALSE,
    navbar = list(
      iosCenterTitle = TRUE,
      hideOnPageScroll = FALSE
    ),
    toolbar = list(
      hideOnPageScroll = FALSE
    ),
    pullToRefresh = FALSE
  )

Sys.setenv(TZ = "America/New_York")
current_time <- Sys.time()
date_str <- format(current_time, "%Y-%m-%d")
current_date <- as.Date(date_str)

# UI definition
ui <- f7Page(
  options = app_opts,
  title = "Kennedy Lakes Fishing Log",
  tags$head(
    tags$style(HTML(customCSS)) # Apply custom CSS
  ),
  f7SingleLayout(
    navbar = f7Navbar(title = "Kennedy Tree Farm Fishing App", hairline = FALSE, shadow = TRUE),
    f7Tabs(
      id = "main_tabs",
      f7Tab(
        title = "Data", # Using tabName for title
        tabName = "Data",
        icon = f7Icon("pencil_square"),
        tags$img(src = "DJI_0071.jpg", class = "top-image"),  # Add your image here
        f7Card(
          title = "New Catch",
          f7List(
            fileInput("file", "Upload an Image of your catch!",
                      accept = c("image/jpeg", "image/jpg", "image/png", "image/JPG",
                       "image/JPEG", "image/heic", "image/HEIC", "image/heif", "image/HEIF"))
          ),
          br(),
          br(),
          f7List(
            selectizeInput("angler_name", "Angler's Name", choices = fetchAnglerNames(), 
                              options = list(placeholder = 'Start typing...', create = TRUE)),
            f7ListItem(shiny::dateInput("catch_date", "Date", value = current_date)),
            f7ListItem(shiny::textInput("catch_time", "Time (HH:MM:SS)", value = format(Sys.time(), "%H:%M:%S", tz = "America/New_York"))),
            br(),
            div(id = "lake-label", shiny::selectInput("lake", "Lake", 
              choices = c("Big Lake", "Small Pond", "Nursery", "Hidden"))),
            br(),
            div(id = "species-label", shiny::selectInput("species", "Species", 
              choices = c('Largemouth Bass', 'Bluegill', 'Redear', 'Green Sunfish', 
                          'Black Crappie', 'Warmouth', 'Chain Pickerel', 'Bullhead Catfish', 
                          'Channel Catfish', 'Grass Carp', 'Tiger Muskie', 'Other'))),
            br(),
            div(id = "target-label", shiny::selectInput("target", "What species were you targeting?", 
              choices = c('', 'Largemouth Bass', 'Bluegill', 'Redear', 'Green Sunfish', 
                          'Black Crappie', 'Warmouth', 'Chain Pickerel', 'Bullhead Catfish', 
                          'Channel Catfish', 'Grass Carp', 'Tiger Muskie', 'Other'))),
            # f7ListItem(shiny::numericInput("length", "Length (cm)", value = NULL)),
            # f7ListItem(shiny::numericInput("weight", "Weight (g)", value = NULL)),
            br(),
            br(),
      f7ListItem(
  div(style = "
        width: 100%;
        font-size: 16px;
        padding: 8px;
        border: 1px solid #ccc;
        border-radius: 8px;
        background-color: #fff;
      ",
      shiny::numericInput("length", "Length (cm)", value = NULL)
  )
),      
f7ListItem(
  div(style = "
        width: 100%;
        font-size: 16px;
        padding: 8px;
        border: 1px solid #ccc;
        border-radius: 8px;
        background-color: #fff;
      ",
      shiny::numericInput("weight", "Weight (g)", value = NULL)
  )
),

            f7Card(
              title = "",
              f7Text("tagid", "Tag ID", value = "")
            ),
            shiny::selectInput("tag_color", "Tag Color", choices = c('', 'Orange', 'Green', 'Yellow')),
            f7Card(
              title = "",
              f7Text("bait_lure", "Bait or Lure", value = "")
            ),
            br(),
            tags$h3("How long since you caught your last fish (or started fishing)?", style = "text-align: center; margin-bottom: 20px;"),
            shinyWidgets::sliderTextInput(
                    inputId = "hours", 
                    label = "Hours:", 
                    choices = as.character(0:8), 
                    selected = "0",
                    grid = TRUE
                ),
            shinyWidgets::sliderTextInput(
                    inputId = "minutes", 
                    label = "Minutes:", 
                    choices = seq(0, 60, by = 10), 
                    selected = "0",
                    grid = TRUE
                ),
            br(),
            f7ListItem(shiny::textInput("notes", "Comments", value = "")),
            br()
          ),
          style = "font-size: 18px;"
        ),
        f7Button(inputId = "submit", label = "Submit")
        # f7Button(inputId = "submit", label = "Submit")
      ),
      f7Tab(
        title = "Leaderboard",
        tabName = "Leaderboard", 
        icon = f7Icon("line_horizontal_3_decrease_circle_fill"),
        fluidRow(
        column(6, dateInput("start_date", "Start Date", value = "2021-11-05")),
        column(6, dateInput("end_date", "End Date", value = current_date))
        ),  
        f7Card(
          title = "Leaderboard", 
          tags$h4("Max Fish Are Identifiable By Their Tag ID (Fish: tagID)"),
          div(DT::dataTableOutput("leaderboard_table"), style = "font-size:50%")
        ),
        style = "font-size: 18px;"
      )
    )
  )
)

server <- function(input, output, session) {

# Example reactive expression to fetch catches
  catches <- reactive({
    req(input$start_date, input$end_date)  # Ensure dates are selected
    
    con <- connectKennedy()  # Ensure you have this function to connect to your DB
    
    # Construct the query to fetch relevant data
    query <- sprintf("SELECT angler, date, common, weight_g, length_cm, tagid FROM fishdat WHERE date >= '%s' AND date <= '%s'",
                     input$start_date, input$end_date)
    
    data <- dbGetQuery(con, query)
    dbDisconnect(con)
    
  data <- data %>%
    filter(!is.na(angler) & angler != "", 
           !is.na(common) & common != "")

    data
  })

leaderboard_data <- reactive({
  data <- catches()
  if(nrow(data) == 0) return(data.frame())  # Early exit if no data

  # Convert weight and length to numeric
  data <- data %>%
    mutate(
      weight_g = as.numeric(weight_g),
      length_cm = as.numeric(length_cm),
      Date = as.Date(date),
      tagid = as.character(tagid)
    )
  
  # Identify the heaviest catch per angler
  max_catch_per_angler <- data %>%
    group_by(angler) %>%
    filter(length_cm == max(length_cm, na.rm = TRUE)) %>%
    slice(1) %>%  # In case of ties, pick the first
    ungroup() %>%
    select(angler, common, weight_g, length_cm, tagid)

  max_weight_per_angler <- data %>%
    group_by(angler) %>%
    filter(weight_g == max(weight_g, na.rm = TRUE)) %>%
    slice(1) %>%  # In case of ties, pick the first
    ungroup() %>%
    select(angler, common, weight_g, length_cm, tagid)

  # Concatenate species name with weight and length for MaxCatch
  max_catch_per_angler <- max_catch_per_angler %>%
    mutate(MaxLength = length_cm) %>%
    mutate(MaxLengthFish = common) %>%
    mutate(tagid_Length = tagid)
    
  max_weight_per_angler <- max_weight_per_angler %>%
    mutate(MaxWeight = weight_g) %>%
    mutate(MaxWeightFish = common) %>%
    mutate(tagid_Weight = tagid)

  # Count the total number of catches per angler
  total_catches <- data %>%
    group_by(angler) %>%
    summarize(NumCatches = n()) %>%
    ungroup()

  # Join the total catches with the max catch details
  leaderboard <- total_catches %>%
    left_join(max_catch_per_angler %>% select(angler, MaxLength, MaxLengthFish, tagid_Length), by = "angler") %>%
    left_join(max_weight_per_angler %>% select(angler, MaxWeight, MaxWeightFish, tagid_Weight), by = "angler") %>%
    arrange(desc(NumCatches))

  # Rename columns for clarity
  leaderboard <- leaderboard %>%
    mutate(
    "MaxLengthFish" = paste(MaxLengthFish, ": ", tagid_Length, sep=""), 
    "MaxWeightFish" = paste(MaxWeightFish, ": ", tagid_Weight, sep="")) %>%
    rename(Angler = angler, "Catches" = NumCatches, "MaxLength (cm)" = MaxLength, "MaxWeight (g)" = MaxWeight) %>%
    select(-c(tagid_Length, tagid_Weight))

leaderboard <- leaderboard[, c("Angler", "Catches", "MaxLengthFish", "MaxWeightFish", "MaxLength (cm)", "MaxWeight (g)")]

  return(leaderboard)
})


output$leaderboard_table <- DT::renderDataTable({
  DT::datatable(
    leaderboard_data(),
    options = list(
      autoWidth = TRUE,
      searching = FALSE, # Disable searching to simplify the view
      paging = FALSE,  # Disable pagination
      info = FALSE,  # Hide the DataTable info (Showing 1 to 3 of 3 entries)
      dom = 't',  # Simplify the table display; adjust as needed
            columnDefs = list(
        list(width = '45px', targets = "_all")  # Set all columns to 75px width
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'font-size': '16px'});",  # Decrease font size of header
        "$(this.api().table().body()).css({'font-size': '12px'});",    # Decrease font size of body
        "}"
      )
    ),
    rownames = FALSE
  )
})


  observeEvent(input$submit, {
    # shiny::showNotification("Submit button pressed.", type = "success")
    # session$reload()
    # isValidTime <- grepl("^([01]\\d|2[0-3]):([0-5]\\d):([0-5]\\d)$", input$catch_time)
    totalHours <- as.numeric(input$hours) + (as.numeric(input$minutes) / 60)
    imageName <- NA
    # angler_name <- ifelse(input$angler_name=="", NA, as.character(input$angler_name))
    if (!is.na(input$angler_name)) {
      # Check if a file has been uploaded
      # Initialize lat and lon with NA
      lat <- NA
      lon <- NA

      if (!is.null(input$file)) {
          uploadedFile <- input$file
          destPath <- paste0(getwd(), "/www/", uploadedFile$name)
          file.rename(uploadedFile$datapath, destPath)
          imageName <- as.character(uploadedFile$name)
          
          # Attempt to extract EXIF data
          exifData <- read_exif(destPath)
          
          # Check if GPSLatitude and GPSLongitude exist in the exifData
          if ("GPSLatitude" %in% names(exifData) && "GPSLongitude" %in% names(exifData)) {
            lat <- exifData$GPSLatitude
            lon <- exifData$GPSLongitude
          }
      }


    # Default values for numeric inputs

    length_cm <- ifelse(is.na(as.numeric(input$length)), NA, as.numeric(input$length))
    weight_g <- ifelse(is.na(as.numeric(input$weight)), NA, as.numeric(input$weight))
    # tagid <- ifelse(input$angler_name=="", NA, as.character(input$angler_name))
    totalHours <- ifelse(totalHours==0, NA, totalHours)
    # notes <- ifelse(is.na(as.character(input$notes)), NA, as.character(input$notes))
    
    test_df=data.frame(
        angler_name=input$angler_name,
        time_targeted = totalHours,
        length_cm = length_cm,
        weight_g = weight_g
                        )
    write.csv(test_df, "./test.csv", row.names=FALSE)

      new_entry <- data.frame(
        angler = input$angler_name,
        date = as.Date(input$catch_date),
        time = input$catch_time,
        lake = as.character(input$lake),
        common = as.character(input$species),
        target_species = as.character(input$target),
        length_cm = length_cm,
        weight_g = weight_g,
        time_targeted = totalHours,
        tagcolor = input$tag_color,
        tagid = input$tagid,
        bait_lure = input$bait_lure,
        notes = input$notes,
        image = imageName,  # This could be NA if no file was uploaded
        lat = lat,  # Include latitude
        lon = lon,  # Include longitude
        stringsAsFactors = FALSE
      )
      write.csv(new_entry, "./query.csv", row.names=FALSE)
      
    tryCatch({
        dbWriteTable(connectKennedy(), "fishdat", new_entry, append = TRUE, row.names = FALSE)
        }, error = function(e) {
            print(e)  # Print the error to the R console
            shiny::showNotification(as.character(e), type = "error")  # Optionally, show the error in the app
        })

      shiny::showNotification("Catch logged successfully!", type = "error")
    } else {
      shiny::showNotification("Please ensure all fields are filled out correctly.", type = "error")
    } 
    Sys.sleep(3)
    session$reload()
  })

}


shinyApp(ui, server)