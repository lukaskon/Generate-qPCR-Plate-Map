# ============================================================
# CFX Maestro Plate Map Generator
# Compatible target: Bio-Rad CFX Maestro 2.2
# Version: 5.2.008/0222 (2021)
# Author: Nikki Lukasko
# Date: 2026-09-16
# Creates a 96-well plate map CSV for import into CFX Maestro.
#
# Defaults:
#   - Duplicates
#   - Vertical replicates
#   - Unknown sample type for all samples except controls
#   - 96-well plate
#
# ============================================================


# ---------------------------
# 1. Packages
# ---------------------------

required_packages <- c("shiny", "DT", "readr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(shiny)
library(DT)
library(readr)


# ---------------------------
# 2. Helper functions
# ---------------------------

# Convert a well number (1-96) into Row / Column.
# The plate is filled A -> H, then column 2, etc.
well_coordinates <- function(well_number) {

  column <- ceiling(well_number / 8)

  row_number <- ((well_number - 1) %% 8) + 1

  row_letters <- LETTERS[1:8]

  data.frame(
    Row = row_letters[row_number],
    Column = column,
    stringsAsFactors = FALSE
  )
}


# Clean control input without changing capitalization.
#
# Controls can be entered:
#   Water
#
# or multiple controls:
#   Water, NTC1, Blank
#
parse_control_input <- function(x) {

  if (is.null(x) || !nzchar(trimws(x))) {
    return(character(0))
  }

  values <- unlist(strsplit(x, ",", fixed = TRUE))

  values <- trimws(values)

  values[values != ""]
}


# ---------------------------
# 3. User Interface
# ---------------------------

ui <- fluidPage(

  titlePanel(
    "CFX Maestro 96-Well Plate Map Generator"
  ),

  sidebarLayout(

    sidebarPanel(

      h4("1. Paste your sample list"),

      helpText(
        "Paste one sample/isolate per line. ",
        "Sample names will be preserved exactly as entered."
      ),

      textAreaInput(
        inputId = "sample_list",
        label = NULL,
        value = paste(
          c(
            "Water",
            "791",
            "792",
            "794",
            "827",
            "965",
            "974",
            "975",
            "1061",
            "1757"
          ),
          collapse = "\n"
        ),
        rows = 12,
        width = "100%",
        placeholder = "Paste one sample name per line..."
      ),

      hr(),

      h4("2. Controls"),

      helpText(
        "Enter sample names exactly as they appear in the list. ",
        "For multiple controls of the same type, separate names with commas."
      ),

      textInput(
        inputId = "ntc",
        label = "NTC / No Template Control",
        value = "Water",
        placeholder = "Example: Water"
      ),

      textInput(
        inputId = "positive",
        label = "Positive Control",
        value = "794",
        placeholder = "Example: 794"
      ),

      textInput(
        inputId = "negative",
        label = "Negative Control",
        value = "",
        placeholder = "Leave blank if none"
      ),

      helpText(
        "Control names are matched exactly, including capitalization, ",
        "spaces, and punctuation."
      ),

      hr(),

      h4("3. Plate settings"),

      selectInput(
        inputId = "replicates",
        label = "Technical replicates:",
        choices = c(
          "Singlets" = 1,
          "Duplicates" = 2,
          "Triplicates" = 3
        ),
        selected = 2
      ),

      selectInput(
        inputId = "orientation",
        label = "Replicate orientation:",
        choices = c(
          "Vertical" = "vertical",
          "Horizontal" = "horizontal"
        ),
        selected = "vertical"
      ),

      helpText(
        "Vertical = replicates are stacked down the same column. ",
        "Horizontal = replicates are placed across adjacent columns."
      ),

      hr(),

      h4("4. Target"),

      textInput(
        inputId = "target",
        label = "Target Name:",
        value = "Folac",
        placeholder = "Example: Folac"
      ),

      helpText(
        "Enter the target name used by your assay. ",
        "If your experiment has a second target, it can be added ",
        "easily in CFX Maestro after the plate map is uploaded."
      ),

      hr(),

      h4("Biological Group"),

      helpText(
        "Biological Group is intentionally left blank by this app. ",
        "It can be added or edited easily in CFX Maestro after the ",
        "plate map has been uploaded."
      ),

      hr(),

      actionButton(
        inputId = "generate",
        label = "Generate Plate Map",
        class = "btn-primary",
        width = "100%"
      ),

      br(),
      br(),

      downloadButton(
        outputId = "download_csv",
        label = "Download CFX Maestro CSV",
        class = "btn-success",
        width = "100%"
      ),

      br(),
      br(),

      downloadButton(
        outputId = "download_summary",
        label = "Download Plate Summary",
        width = "100%"
      )
    ),


    mainPanel(

      h3("Plate Map"),

      uiOutput("status_message"),

      DTOutput("plate_table"),

      hr(),

      h3("CFX Maestro Data"),

      DTOutput("data_table"),

      hr(),

      h4("Important"),

      tags$ul(

        tags$li(
          "The plate contains 96 wells (A1-H12)."
        ),

        tags$li(
          "Controls are placed before unknown samples."
        ),

        tags$li(
          "Unknown samples are assigned the CFX Sample Type 'Unkn'."
        ),

        tags$li(
          "Replicate # identifies technical replicate groups."
        ),

        tags$li(
          "Biological Group is left blank and can be added later in CFX Maestro."
        ),

        tags$li(
          "The exported file is a CSV intended for CFX Maestro spreadsheet import."
        )

      )
    )
  )
)


# ---------------------------
# 4. Server
# ---------------------------

server <- function(input, output, session) {


  # ----------------------------------------------------------
  # Generate plate map
  # ----------------------------------------------------------

  generated_data <- eventReactive(input$generate, {

    # -----------------------
    # Read samples
    # -----------------------

    raw_samples <- input$sample_list

    if (is.null(raw_samples) || !nzchar(trimws(raw_samples))) {

      showNotification(
        "Please paste at least one sample.",
        type = "error"
      )

      return(NULL)
    }


    # One sample per line
    samples <- unlist(
      strsplit(raw_samples, "\n", fixed = TRUE)
    )

    # Remove carriage returns from Windows input
    samples <- gsub("\r", "", samples, fixed = TRUE)

    # Remove only completely empty lines.
    # Do NOT otherwise modify sample names.
    samples <- samples[nzchar(trimws(samples))]


    if (length(samples) == 0) {

      showNotification(
        "No samples were found.",
        type = "error"
      )

      return(NULL)
    }


    # --------------------------------------------------------
    # Control lists
    # --------------------------------------------------------

    ntc_samples <- parse_control_input(input$ntc)

    positive_samples <- parse_control_input(input$positive)

    negative_samples <- parse_control_input(input$negative)


    # --------------------------------------------------------
    # Validate control names
    # --------------------------------------------------------

    all_controls <- c(
      ntc_samples,
      positive_samples,
      negative_samples
    )

    missing_controls <- setdiff(
      all_controls,
      samples
    )


    if (length(missing_controls) > 0) {

      showNotification(
        paste(
          "The following control name(s) were not found in your sample list:",
          paste(missing_controls, collapse = ", ")
        ),
        type = "error",
        duration = NULL
      )

      return(NULL)
    }


    # --------------------------------------------------------
    # Check for control conflicts
    # --------------------------------------------------------

    ntc_conflicts <- intersect(
      ntc_samples,
      c(positive_samples, negative_samples)
    )

    positive_conflicts <- intersect(
      positive_samples,
      negative_samples
    )

    conflicts <- unique(
      c(ntc_conflicts, positive_conflicts)
    )


    if (length(conflicts) > 0) {

      showNotification(
        paste(
          "The following sample(s) are assigned to more than one control type:",
          paste(conflicts, collapse = ", ")
        ),
        type = "error",
        duration = NULL
      )

      return(NULL)
    }


    # --------------------------------------------------------
    # Determine sample type
    # --------------------------------------------------------

    get_sample_type <- function(sample_name) {

      if (sample_name %in% ntc_samples) {

        return("NTC")

      } else if (sample_name %in% positive_samples) {

        return("Pos Ctrl")

      } else if (sample_name %in% negative_samples) {

        return("Neg Ctrl")

      } else {

        return("Unkn")
      }
    }


    # --------------------------------------------------------
    # Controls first
    # --------------------------------------------------------

    control_order <- c(
      ntc_samples,
      positive_samples,
      negative_samples
    )


    # Keep the order from the pasted list while moving
    # controls ahead of unknowns.
    controls_in_input_order <- samples[
      samples %in% control_order
    ]

    unknowns_in_input_order <- samples[
      !(samples %in% control_order)
    ]

    ordered_samples <- c(
      controls_in_input_order,
      unknowns_in_input_order
    )


    # --------------------------------------------------------
    # Remove accidental duplicate sample lines
    #
    # This is deliberate:
    # one isolate/name in the input represents one biological
    # sample, and the replicate setting determines how many
    # wells it receives.
    # --------------------------------------------------------

    ordered_samples <- unique(ordered_samples)


    # --------------------------------------------------------
    # Replicate settings
    # --------------------------------------------------------

    replicate_number <- as.integer(input$replicates)

    orientation <- input$orientation

    target_name <- input$target


    if (is.null(target_name) || !nzchar(target_name)) {

      showNotification(
        "Please enter a Target Name.",
        type = "error"
      )

      return(NULL)
    }


    # --------------------------------------------------------
    # Determine whether the samples fit
    # --------------------------------------------------------

    wells_required <- length(ordered_samples) * replicate_number

    plate_capacity <- 96

    if (wells_required > plate_capacity) {

      showNotification(
        paste0(
          "Plate is full: ",
          length(ordered_samples),
          " sample(s) × ",
          replicate_number,
          " replicate(s) = ",
          wells_required,
          " wells required. A 96-well plate has only 96 wells."
        ),
        type = "error",
        duration = NULL
      )

      return(NULL)
    }


    # --------------------------------------------------------
    # Build plate assignments
    # --------------------------------------------------------

    output_rows <- list()

    current_well <- 1

    replicate_id <- 1


    for (sample_name in ordered_samples) {

      sample_type <- get_sample_type(sample_name)


      # -----------------------------------------
      # Generate well numbers for this sample
      # -----------------------------------------

      if (orientation == "vertical") {

        # Vertical replicates:
        # A1, B1, C1...
        #
        # This is implemented by calculating each
        # replicate position separately.

        start_row <- ((current_well - 1) %% 8) + 1
        start_col <- ceiling(current_well / 8)

        replicate_wells <- c()

        for (r in 0:(replicate_number - 1)) {

          row_position <- start_row + r
          col_position <- start_col

          # If vertical replicates don't fit in the
          # remaining rows of this column, move to the
          # next column at row A.

          if (row_position > 8) {

            col_position <- col_position + 1
            row_position <- row_position - 8
          }

          well_number <- (
            (col_position - 1) * 8
          ) + row_position

          replicate_wells <- c(
            replicate_wells,
            well_number
          )
        }

      } else {

        # Horizontal replicates:
        # A1, A2, A3...

        start_row <- ((current_well - 1) %% 8) + 1
        start_col <- ceiling(current_well / 8)

        replicate_wells <- c()

        for (r in 0:(replicate_number - 1)) {

          row_position <- start_row
          col_position <- start_col + r

          # If horizontal replicates don't fit in
          # remaining columns, this will move the group
          # to the next row/available position.

          if (col_position > 12) {

            row_position <- row_position + 1
            col_position <- col_position - 12
          }

          well_number <- (
            (col_position - 1) * 8
          ) + row_position

          replicate_wells <- c(
            replicate_wells,
            well_number
          )
        }
      }


      # -----------------------------------------
      # Verify wells are valid
      # -----------------------------------------

      if (
        any(replicate_wells < 1) ||
        any(replicate_wells > 96) ||
        length(unique(replicate_wells)) != replicate_number
      ) {

        showNotification(
          paste(
            "Unable to place replicates for sample:",
            sample_name,
            ". Try a different replicate orientation."
          ),
          type = "error",
          duration = NULL
        )

        return(NULL)
      }


      # -----------------------------------------
      # Create CFX rows
      # -----------------------------------------

      for (well in replicate_wells) {

        coords <- well_coordinates(well)

        output_rows[[length(output_rows) + 1]] <- data.frame(

          Row = coords$Row,

          Column = coords$Column,

          `Sample Type` = sample_type,

          `Replicate #` = replicate_id,

          `*Target Name` = target_name,

          `*Sample Name` = sample_name,

          `*Biological Group` = "",

          `*Well Note` = "",

          `Starting Quantity` = "N/A",

          Units = "copy number",

          stringsAsFactors = FALSE,

          check.names = FALSE
        )
      }


      # -----------------------------------------
      # Next sample
      # -----------------------------------------

      # The next available well is determined from
      # the furthest well used by this sample.
      current_well <- max(replicate_wells) + 1

      replicate_id <- replicate_id + 1
    }


    # --------------------------------------------------------
    # Combine rows
    # --------------------------------------------------------

    final_data <- do.call(
      rbind,
      output_rows
    )


    # --------------------------------------------------------
    # Sort in physical plate order:
    # A1, B1, C1...H1, A2, B2...
    # --------------------------------------------------------

    final_data$well_order <- match(
      final_data$Row,
      LETTERS[1:8]
    ) +
      (final_data$Column - 1) * 8


    final_data <- final_data[
      order(final_data$well_order),
    ]

    final_data$well_order <- NULL


    rownames(final_data) <- NULL

    final_data
  })


  # ----------------------------------------------------------
  # Status message
  # ----------------------------------------------------------

  output$status_message <- renderUI({

    data <- generated_data()

    if (is.null(data)) {

      return(
        tags$div(
          class = "alert alert-info",
          "Enter your samples and settings, then click ",
          tags$b("Generate Plate Map.")
        )
      )
    }


    number_of_samples <- length(
      unique(data$`*Sample Name`)
    )

    wells_used <- nrow(data)

    wells_remaining <- 96 - wells_used


    tags$div(
      class = "alert alert-success",

      tags$b("Plate generated successfully. "),

      paste0(
        number_of_samples,
        " sample(s), ",
        wells_used,
        " well(s) used, ",
        wells_remaining,
        " well(s) remaining."
      )
    )
  })


  # ----------------------------------------------------------
  # Create plate visualization
  # ----------------------------------------------------------

  output$plate_table <- renderDT({

    data <- generated_data()

    if (is.null(data)) {
      return(NULL)
    }


    # Create complete 8 x 12 plate
    plate <- expand.grid(
      Column = 1:12,
      Row = LETTERS[1:8],
      stringsAsFactors = FALSE
    )


    # Ensure A1, B1...H12 ordering
    plate$well_order <-
      match(plate$Row, LETTERS[1:8]) +
      (plate$Column - 1) * 8


    plate <- plate[
      order(plate$well_order),
    ]


    # Add sample information
    plate$Sample <- ""

    plate$Type <- ""

    plate$Replicate <- ""


    for (i in seq_len(nrow(data))) {

      row_match <- which(
        plate$Row == data$Row[i] &
        plate$Column == data$Column[i]
      )

      plate$Sample[row_match] <-
        data$`*Sample Name`[i]

      plate$Type[row_match] <-
        data$`Sample Type`[i]

      plate$Replicate[row_match] <-
        as.character(data$`Replicate #`[i])
    }


    # Create compact display
    plate$Display <- ifelse(
      plate$Sample == "",
      "",
      paste0(
        plate$Sample,
        "<br><small>",
        plate$Type,
        " | Rep ",
        plate$Replicate,
        "</small>"
      )
    )


    # Convert to wide format
    wide <- data.frame(
      Row = LETTERS[1:8],
      stringsAsFactors = FALSE
    )


    for (col in 1:12) {

      values <- plate$Display[
        plate$Column == col
      ]

      wide[[paste0("C", col)]] <- values
    }


    names(wide) <- c(
      "Row",
      paste0(1:12)
    )


    datatable(
      wide,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        autoWidth = TRUE
      )
    )
  })


  # ----------------------------------------------------------
  # CFX data table
  # ----------------------------------------------------------

  output$data_table <- renderDT({

    data <- generated_data()

    if (is.null(data)) {
      return(NULL)
    }


    datatable(
      data,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })


  # ----------------------------------------------------------
  # CSV download
  # ----------------------------------------------------------

  output$download_csv <- downloadHandler(

    filename = function() {

      paste0(
        "CFX_Maestro_Plate_Map_",
        format(Sys.Date(), "%Y-%m-%d"),
        ".csv"
      )
    },

    content = function(file) {

      data <- generated_data()

      if (is.null(data)) {
        return(NULL)
      }


      # Ensure exact desired column order
      data <- data[
        c(
          "Row",
          "Column",
          "Sample Type",
          "Replicate #",
          "*Target Name",
          "*Sample Name",
          "*Biological Group",
          "*Well Note",
          "Starting Quantity",
          "Units"
        )
      ]


      # Write UTF-8 CSV.
      # readr handles quoting of names containing commas,
      # quotation marks, etc.
      write_csv(
        data,
        file,
        na = ""
      )
    }
  )


  # ----------------------------------------------------------
  # Human-readable summary download
  # ----------------------------------------------------------

  output$download_summary <- downloadHandler(

    filename = function() {

      paste0(
        "CFX_Maestro_Plate_Summary_",
        format(Sys.Date(), "%Y-%m-%d"),
        ".csv"
      )
    },

    content = function(file) {

      data <- generated_data()

      if (is.null(data)) {
        return(NULL)
      }


      summary_data <- data.frame(

        Well = paste0(
          data$Row,
          data$Column
        ),

        Sample = data$`*Sample Name`,

        Sample_Type = data$`Sample Type`,

        Replicate = data$`Replicate #`,

        Target = data$`*Target Name`,

        stringsAsFactors = FALSE
      )


      write_csv(
        summary_data,
        file
      )
    }
  )
}


# ---------------------------
# 5. Launch application
# ---------------------------

shinyApp(
  ui = ui,
  server = server
)
