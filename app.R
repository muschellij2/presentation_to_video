## app.R ##
library(shiny)
library(shinydashboard)
library(ari)
library(ariExtra)
library(googleAuthR)
library(googleLanguageR)
library(grid)
library(png)
library(shinyjs)

is_language_auth = function() {
    inherits(googleAuthR::Authentication$public_fields$token, "Token")
}
check_gl_auth = function() {
    if (!is_language_auth()) {
        needed <- c("https://www.googleapis.com/auth/cloud-language",
                    "https://www.googleapis.com/auth/cloud-platform")
        
        googleAuthR::gar_attach_auto_auth(needed,
                                          environment_var = "GL_AUTH")
    }
    is_language_auth()
}
check_gl_auth()

pptx_mime_type = function() {
    paste0(
        "application/",  
        "vnd.openxmlformats-officedocument", 
        ".presentationml.presentation")
}

ui <- dashboardPage(
    dashboardHeader(title = "Convert a Presentation to a Video"),
    ## Sidebar content
    dashboardSidebar(
        shinyjs::useShinyjs(),  # Set up shinyjs
        
        sidebarMenu(
            selectInput("service", label = "Voice Service", 
                        choices = c("google", "amazon", "microsoft"),
                        selected = "google"),
            textInput("voice", label = "Voice to use", value = 
                          "en-US-Standard-B"),
            menuItem("Google Slide", tabName = "gs", icon = icon("google-drive")),
            menuItem("PowerPoint", tabName = "pptx", icon = icon("file-powerpoint"))
        )
    ),
    
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(
                tabName = "gs",
                fluidRow(
                    box(plotOutput("gs_thumbnail", height = 250)),
                    box(
                        title = "Inputs",
                        textInput("gs_id", "Google Slide ID (e.g. 1Opt6lv7rRi7Kzb9bI0u3SWX1pSz1k7botaphTuFYgNs )")
                    ),
                    box(
                        title = "Render Video",
                        shinyjs::disabled(
                            actionButton("gs_render", "Render Video",
                                         icon = icon("video"))
                        ),
                        shinyjs::disabled(
                            downloadButton("gs_download", "Download Video",
                                           icon = icon("download"))
                        )
                    )
                    
                )
            ),
            
            # First tab content
            tabItem(
                tabName = "pptx",
                fluidRow(
                    box(plotOutput("pptx_thumbnail", height = 250)),
                    box(
                        title = "Inputs",
                        fileInput("pptx_input", "PowerPoint File",
                                  accept = pptx_mime_type(),
                                  multiple = FALSE)
                    ),
                    box(
                        title = "Render Video",
                        shinyjs::disabled(
                            actionButton("pptx_render", "Render Video",
                                         icon = icon("video"))
                        ),                        
                        shinyjs::disabled(
                            downloadButton("pptx_download", "Download Video",
                                           icon = icon("download"))
                        )
                    )                    
                )
            )
        )
    )
)

thumbnail_args = function(res, max_seq = 9) {
    images = res$images
    max_seq = min(length(images), max_seq)
    images = images[seq(max_seq)]
    args = lapply(res$images, function(img) {
        grid::rasterGrob(as.raster(
            png::readPNG(img)), interpolate = FALSE)
    })
    args$ncol = 3
    return(args)
}

run_ari = function(result, 
                   divisible_height = TRUE,
                   voice = "en-US-Standard-B", service = "google") {
    video = ari::ari_spin(
        images = result$images,
        paragraphs = result$script,
        divisible_height = divisible_height,
        service = service,
        voice = voice)
    return(video)
}
server <- function(input, output) {
    
    output$gs_thumbnail <- renderPlot({
        validate(
            need(input$gs_id, "Need Google Slide ID")
        )
        gs_id = input$gs_id
        # x = text2speech::tts_google_voices()
        
        gs_ari_result <<- gs_to_ari(
            gs_id, 
            open = FALSE)
        args = thumbnail_args(gs_ari_result)
        shinyjs::enable("gs_render")
        do.call(gridExtra::grid.arrange, args = args)
    })
    
    
    output$pptx_thumbnail <- renderPlot({
        validate(
            need(input$pptx_input, "Need PowerPoint file")
        )
        pptx_input = input$pptx_input
        # print(input$pptx_input)
        pptx_ari_result <<- pptx_to_ari(
            path = pptx_input$datapath, 
            open = FALSE)
        args = thumbnail_args(pptx_ari_result)
        shinyjs::enable("pptx_render")
        
        do.call(gridExtra::grid.arrange, args = args)  
    })
    
    observeEvent(input$pptx_render, {
        if (!exists("pptx_ari_result")) {
            validate(
                need(input$pptx_input, "Need PowerPoint file")
            )            
            pptx_ari_result <<- pptx_to_ari(
                path = pptx_input$datapath, 
                open = FALSE)
        }
        video = run_ari(pptx_ari_result, 
                        voice = input$voice,
                        service = input$service)
        shinyjs::enable("pptx_download")
        pptx_video <<- attr(video, "outfile")
    })
    
    observeEvent(input$gs_render, {
        if (!exists("gs_ari_result")) {
            validate(
                need(input$gs_id, "Need Google Slide ID")
            )
            gs_ari_result <<- gs_to_ari(
                input$gs_id, 
                open = FALSE)
        }
        video = run_ari(gs_ari_result, 
                        voice = input$voice,
                        service = input$service)
        shinyjs::enable("gs_download") 
        gs_video <<- attr(video, "outfile")
    })
    
    ################################
    # Rendering Video for Downlaod
    ################################
    output$gs_download <- downloadHandler(
        filename = function() {
            name = paste0(
                "gs_", 
                input$gs_id,
                "_",
                input$voice,
                "_",
                input$service,
                ".mp4")
            return(name)
        },
        content = function(output) {
            file.copy(gs_video, output, overwrite = TRUE)
        }
    )
    
    output$pptx_download <- downloadHandler(
        filename = function() {
            name = paste0(
                "pptx_", 
                input$pptx_input$name,
                "_",
                input$voice,
                "_",
                input$service,
                ".mp4")
            name
        },
        content = function(output) {
            file.copy(pptx_video, output, overwrite = TRUE)
        }
    )
    
    
}

shinyApp(ui, server)
