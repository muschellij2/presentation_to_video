## app.R ##
auth_file ="google_authorization.json"
Sys.setenv(GL_AUTH = auth_file)
library(animation) #need for ffmpeg
library(shiny)
library(shinyjs)
library(docxtractr)
library(shinydashboard)
library(text2speech)
library(ari)
library(ariExtra)
library(googleAuthR)
library(googleLanguageR)
library(grid)
library(gridExtra)
library(png)
library(googledrive)

source("libreoffice_checks.R")

stopifnot(text2speech::tts_auth("google"))

ari::ffmpeg_exec()
x = ari::ffmpeg_audio_codecs()
x = x[ x$encoding_supported, ]
# cat(file = stderr(), paste(x$codec, collapse = "\n"))
# cat(file = stderr(), paste(x$codec_name, collapse = "\n"))

ari::set_audio_codec("aac")

is_language_auth = function() {
    inherits(googleAuthR::Authentication$public_fields$token, "Token")
}
check_gl_auth = function() {
    if (!is_language_auth()) {
        needed <- c(
            "https://www.googleapis.com/auth/cloud-language",
            "https://www.googleapis.com/auth/cloud-platform")
        
        googleAuthR::gar_attach_auto_auth(needed,
                                          environment_var = "GL_AUTH")
    }
    is_language_auth()
}

pptx_mime_type = function() {
    paste0(
        "application/",  
        "vnd.openxmlformats-officedocument", 
        ".presentationml.presentation")
}

# need these for rechecks
gs_id = ""
pptx_input_name = ""


##############################
# User Interface
##############################
ui <- dashboardPage(
    dashboardHeader(title = "Convert a Presentation to a Video"),
    ## Sidebar content
    dashboardSidebar(
        shinyjs::useShinyjs(),  # Set up shinyjs
        
        sidebarMenu(
            selectInput("service", label = "Voice Service", 
                        choices = c("google", "amazon", "microsoft"),
                        selected = "google"),
            checkboxInput("show_thumbnails", label = "Show thumbnails of slides",
                          value = FALSE),
            textInput("voice", label = "Voice to use", value = 
                          "en-US-Standard-B"),
            menuItem("Google Slide", tabName = "gs", icon = icon("google-drive")),
            menuItem("PowerPoint", tabName = "pptx", icon = icon("file-powerpoint")),
            actionButton("clear_results", "Clear Previous Results",
                         icon = icon("trash"))
        )
    ),
    
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(
                tabName = "gs",
                fluidRow(
                    box(h2("Thumbnails"), plotOutput("gs_thumbnail", height = 250)),
                    box(
                        title = "Inputs",
                        textInput("gs_id", paste0(
                            "Google Slide ID ", "(e.g. ", 
                            "1Opt6lv7rRi7Kzb9bI0u3SWX1pSz1k7botaphTuFYgNs )")
                        )
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
                    ),
                    box(h2("Extracted Script"),
                        uiOutput("gs_script")),
                    box(
                        HTML(
                            paste0(
                                "If any errors occur, please see the JavaScript log",
                                " see "
                            ),
                            as.character(tags$a("how to use the log",
                                                href = "https://developers.google.com/web/tools/chrome-devtools/console/log"))
                        )
                    )     
                    
                )
            ),
            
            # First tab content
            tabItem(
                tabName = "pptx",
                fluidRow(
                    box(h2("Thumbnails"), plotOutput("pptx_thumbnail", height = 250)),
                    box(
                        title = "Inputs",
                        fileInput(
                            "pptx_input", 
                            HTML(paste0(
                                "PowerPoint File<br> There is a ", 
                                '<a href = "https://community.rstudio.com/t/shinyapps-io-lua-errors-with-pptx/36944">bug</a>',
                                " uploading PPTX files to shinyapps.io, ", 
                                "so please zip your file (extension ", 
                                ".zip) and upload")),
                            # accept = pptx_mime_type(),
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
                    ),
                    box(h2("Extracted Script"),
                        uiOutput("gs_script")),
                    box(
                        HTML(
                            paste0(
                                "If any errors occur, please see the JavaScript log"
                            )
                        )
                    )
                )
            )
        )
    )
)

##############################
# create thumbnails
##############################
thumbnail_args = function(res, max_seq = 4) {
    images = res$images
    max_seq = min(length(images), max_seq)
    images = images[seq(max_seq)]
    args = lapply(res$images, function(img) {
        grid::rasterGrob(as.raster(
            png::readPNG(img)), interpolate = FALSE)
    })
    args$ncol = floor(sqrt(max_seq))
    return(args)
}

##############################
# run ari
##############################
run_ari = function(result, 
                   divisible_height = TRUE,
                   voice = "en-US-Standard-B", service = "google") {
    video = ari::ari_spin(
        images = result$images,
        paragraphs = result$script,
        divisible_height = divisible_height,
        service = service,
        voice = voice,
        verbose = 2)
    if (!video) {
        warning(
            paste0(
                "Video doesn't seem to have generated correctly,",
                " look at logs"
            )
        )
    }
    return(video)
}

##############################
# full server
##############################
server <- function(input, output) {
    cat_and_log = function(msg) {
        shinyjs::logjs(msg)
        cat(file = stderr(), msg)
    }
    
    
    if (have_libreoffice()) {
        cat_and_log("libreoffice found!\n")
    }
    if (!have_libreoffice() && !is.null(token)) {
        cat_and_log(
            paste0("Using Google Slides workaround ",
                   "- no libreoffice\n"))
    }
    
    output$gs_thumbnail <- renderPlot({
        validate(
            need(input$gs_id, "Need Google Slide ID")
        )
        if (!is.null(input$gs_id)) {
            if (!exists("gs_ari_result") || trimws(input$gs_id) != gs_id) {
                gs_id <<- trimws(input$gs_id)
                # x = text2speech::tts_google_voices()
                cat_and_log("Running gs_to_ari")
                withCallingHandlers(
                    {            
                        gs_ari_result <<- gs_to_ari(
                            gs_id, 
                            open = FALSE, 
                            verbose = 2)
                    },
                    message = function(m) {
                        shinyjs::logjs(m$message)
                    }
                ) 
            }
            
            if (input$show_thumbnails) {
                cat_and_log("Creating Thumbnails\n")
                args = thumbnail_args(gs_ari_result)
            }
            cat_and_log("Render video should be enabled!\n")
            shinyjs::enable("gs_render")
            if (input$show_thumbnails) {            
                do.call(gridExtra::grid.arrange, args = args)
            } else {
                NULL
            }
        }
    })
    output$gs_script <- renderUI({
        validate(
            need(input$gs_id, "Need Google Slide ID")
        )
        if (!is.null(gs_ari_result$script)) {
            return(
                HTML(
                    paste(
                        paste0(
                            "(Slide ", 1:length(gs_ari_result$script), ") ",
                            gs_ari_result$script),
                        collapse = '<br/><br/>')
                )
            )
        } else {
            return(HTML(""))
        }
    })
    
    output$pptx_thumbnail <- renderPlot({
        validate(
            need(input$pptx_input, "Need PowerPoint file"),
            need(input$pptx_input$name, "")
        )
        if (!exists("pptx_ari_result") || 
            input$pptx_input$name != pptx_input_name) {
            
            pptx_input = input$pptx_input
            print(input$pptx_input)
            pptx_input_name <<- input$pptx_input$name
            msg = paste("input name: ", 
                        input$pptx_input$name, "\n")
            cat_and_log(msg)
            msg = paste("input path: ", 
                        input$pptx_input$datapath, "\n")
            cat_and_log(msg)
            msg = paste("input type: ", 
                        input$pptx_input$type, "\n")
            cat_and_log(msg)
            
            datapath = pptx_input$datapath
            if (tools::file_ext(tolower(pptx_input$name)) == "zip") {
                datapath = unzip(zipfile = datapath, exdir = tempdir())
                datapath = datapath[1]
            }
            cat_and_log("Running pptx_to_ari")
            withCallingHandlers(
                {            
                    pptx_ari_result <<- pptx_to_ari(
                        path = datapath,
                        open = FALSE, 
                        verbose = 2)
                },
                message = function(m) {
                    shinyjs::logjs(m$message)
                }
            )        
        }
        if (input$show_thumbnails) {
            cat_and_log("Creating Thumbnails\n")
            args = thumbnail_args(pptx_ari_result)
        }        
        cat_and_log("Render video should be enabled!\n")
        shinyjs::enable("pptx_render")
        if (input$show_thumbnails) { 
            do.call(gridExtra::grid.arrange, args = args)  
        } else {
            NULL
        }
    })
    
    output$pptx_script <- renderUI({
        validate(
            need(input$gs_id, "NeedP PPTX file")
        )
        if (!is.null(pptx_ari_result$script)) {
            return(
                HTML(
                    paste(
                        paste0(
                            "(Slide ", 1:length(pptx_ari_result$script), ") ",
                            pptx_ari_result$script),
                        collapse = '<br/><br/>')
                )
            )
        } else {
            return(HTML(""))
        }
    })
    
    observeEvent(input$pptx_render, {
        if (!exists("pptx_ari_result")) {
            validate(
                need(input$pptx_input, "Need PowerPoint file")
            )     
            datapath = input$pptx_input$datapath
            if (tools::file_ext(tolower(pptx_input$name)) == "zip") {
                datapath = unzip(zipfile = datapath, exdir = tempdir())
                datapath = datapath[1]
            }
            pptx_ari_result <<- pptx_to_ari(
                path = datapath,
                open = FALSE)            
        }
        cat_and_log("Running ari for PPTX")
        withCallingHandlers(
            {            
                video = run_ari(pptx_ari_result, 
                                voice = input$voice,
                                service = input$service)
            },
            message = function(m) {
                shinyjs::logjs(m$message)
            },
            warning = function(w) {
                shinyjs::logjs(w$message)
            }            
        )             
        shinyjs::enable("pptx_download")
        pptx_video <<- attr(video, "outfile")
    })
    
    observeEvent(input$clear_results, {
        rm(
            list = c("gs_ari_result", "pptx_ari_result",
                     "gs_video", "pptx_video")
        )   
        gs_id <<- ""
        pptx_input_name <<- ""
        shinyjs::disable("gs_download") 
        shinyjs::disable("gs_render")
        shinyjs::disable("pptx_download") 
        shinyjs::disable("pptx_render")        
        enable
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
        cat_and_log("Running ari for GS")
        withCallingHandlers(
            {            
                video = run_ari(gs_ari_result, 
                                voice = input$voice,
                                service = input$service)
            },
            message = function(m) {
                shinyjs::logjs(m$message)
            },
            warning = function(w) {
                shinyjs::logjs(w$message)
            }            
        )            
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
                id = get_slide_id(input$gs_id),
                "_",
                input$voice,
                "_",
                input$service,
                ".mp4")
            cat_and_log(paste0("output file is ", name))
            return(name)
        },
        content = function(output) {
            file.copy(gs_video, output, overwrite = TRUE)
        }
    )
    
    output$pptx_download <- downloadHandler(
        filename = function() {
            name = sub(".pptx.*", "", tolower(input$pptx_input$name))
            name = sub("[.]zip$", "", name)
            name = paste0(
                "pptx_", 
                name,
                "_",
                input$voice,
                "_",
                input$service,
                ".mp4")
            cat_and_log(paste0("output file is ", name))
            name
        },
        content = function(output) {
            file.copy(pptx_video, output, overwrite = TRUE)
        }
    )
    
    
}

shinyApp(ui, server)


