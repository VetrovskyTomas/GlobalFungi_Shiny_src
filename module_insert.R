# Function for module UI
insertUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # picture
    sidebarPanel(width = "100%", style = "background-color:#0c2b37;",
      fluidRow(
        column(1, style = "background-color:#0c2b37;",img(src='insert.png', height = 56)),
        column(11, h2(id="header_title", "Submit your study"))
      )
    ),
    # progress...
    sidebarPanel(id = ns("progress"), width = "100%", style = "background-color:white;",
      fluidRow(  
        column(12,verbatimTextOutput(ns('info_study'))),
        column(6,tableOutput(ns('info_table')))
      )
    ),
    # content tabls
    sidebarPanel(id = ns("panel"), width = "100%", style = "background-color:white;",
      tabsetPanel(id = ns("tabs"),
        tabPanel("Submission", value = "tab_intro",
          insertIntroUI(id = ns("insert_intro"))
        ),
        tabPanel("Basic info", value = "tab_basic",
          insertBasicUI(id = ns("insert_basic"))
        ),
        tabPanel("Email send", value = "tab_email",
          br(),
          verbatimTextOutput(ns('info_email'))
        ),
        tabPanel("Metadata", value = "tab_metadata",
          insertMetadataUI(id = ns("insert_metadata"))
        ),
        tabPanel("Data upload", value = "tab_upload",
          insertUploadUI(id = ns("insert_upload"))
        ),
        tabPanel("Overview", value = "tab_overview",
            fluidPage(
              br(),
              fluidRow(
                #column(6,actionButton(ns('filechoose'),label = "Pick a file")),
                column(12, "ALL SEEMS TO BE CORRECT - THANK YOU FOR SUBMITTING YOUR STUDY!")
              ),
              br(),
              fluidRow(  
                column(12,verbatimTextOutput(ns('info_over')))
              ),
              br()
          )
        )
      )
    )
  )
}

# server logic to read selected file ----
insertFunc <- function(input, output, session, variable) {
  
  ns <- session$ns
  
  # create storage for study info
  study <- reactiveValues()
  study$valid <- TRUE
  study$info <- NULL
  study$basic <- NULL
  study$metadata <- NULL
  study$correct <- FALSE
  study$start <- FALSE

  key <- isolate(variable$key)
  finished <- FALSE
  study$key <- key

  #################################
  if (!is.null(key)){
    query <- sprintf(paste0("SELECT `title` from ",options()$mysql$study," WHERE `hash` = '",key,"'"))
    info <- sqlQuery(query)
    if (nrow(info)==0){
      key <- NULL
    } else {
      # Check if finnished
      query <- sprintf(paste0("SELECT `submission_finished` from ",options()$mysql$study," WHERE `hash` = '",key,"'"))
      submitted <- sqlQuery(query)
      if (submitted =="1") {
        finished <- TRUE
        print("Study is done...")
      }
    }
  }
  #################################  
  
  observe({
    output$info_study <- renderText({
      if (!is.null(variable$key)){
        query <- sprintf(paste0("SELECT `title` from ",options()$mysql$study," WHERE `hash` = '",key,"'"))
        info <- sqlQuery(query)
        if (nrow(info)==0){
          "The study ID is not valid!"
        } else {
          #********************************
          # Check if finnished
          query <- sprintf(paste0("SELECT `submission_finished` from ",options()$mysql$study," WHERE `hash` = '",key,"'"))
          submitted <- sqlQuery(query)
          print(submitted)
          #********************************
          if (submitted =="1") {
            paste0(info, "\n\n"," The submission of study is finished.\n Thank you!")
          } else {
            paste(info, collapse='\n' )
          }
        }
      } 
    })
  })

  observe({  
  if (!is.null(key)){
    output$info_table <- renderTable({
      # update email confirmation
      query <- sprintf(paste0("UPDATE ",options()$mysql$study," SET `email_confirmed`=1  WHERE `hash` = '",key,"'"))
      sqlQuery(query)
  
      # show data
      query <- sprintf(paste0("SELECT `authors`,`year`,`journal`,`volume`,`pages`,`doi`,`affiliation_institute`,`affiliation_country`,`ORCID`,`contributor`,`repository`,`include`,`coauthor` from ",options()$mysql$study," WHERE `hash` = '",key,"'"))
      info <- sqlQuery(query)
      
      colnames(info) <- c("All authors",
                          "Year", "Journal","volume","Pages","DOI",
                          "Affiliation of institute","Affiliation of country","ORCID",
                          "Contributor","Repository","Include","Coauthor")
      clnames <- colnames(info)
      if (!is.null(study$metadata)){
        info$NumberOfSamples=study$metadata$num_of_samples
        colnames(info) <- c(clnames,"Number of samples")
      }
      
      data.frame(variable = rownames(t(info)), values = t(info))
    })
  }
  })
  # activate tables by steps...
  observe({
    if (finished){
        #
    } else {
      if (!is.null(key)){
        hideTab(inputId = "tabs", target = "tab_basic")
        hideTab(inputId = "tabs", target = "tab_email")
        ##
        print("Metadata form is shown...")
        showTab(inputId = "tabs", target = "tab_metadata")
        updateTabsetPanel(session, "tabs", selected = "tab_metadata")
        #*******************************************************************
        # upload form
        if (!is.null(study$metadata)){
          print("Upload form is shown...")
          showTab(inputId = "tabs", target = "tab_upload")
          updateTabsetPanel(session, "tabs", selected = "tab_upload")
          #---------------------------------------------
          if (!is.null(study$upload)){
            showTab(inputId = "tabs", target = "tab_overview")
            updateTabsetPanel(session, "tabs", selected = "tab_overview")
            #*****************************************
            # update submission confirmation
            query <- sprintf(paste0("UPDATE ",options()$mysql$study," SET `submission_finished`=1  WHERE `hash` = '",key,"'"))
            sqlQuery(query)
            #*****************************************
            alert(paste0("Submission has been finished successfully!"))
          }
          #---------------------------------------------
          callModule(module = insertUploadFunc, id = "insert_upload", study)
        } else {
          callModule(module = insertMetadataFunc, id = "insert_metadata", study)
          callModule(module = insertUploadFunc, id = "insert_upload", study)  
        }
        #*******************************************************************
        
      } else {
      # basic form
      if (study$start == TRUE){
        print("Basic form is shown...")
        showTab(inputId = "tabs", target = "tab_basic")
        hideTab(inputId = "tabs", target = "tab_email")
        updateTabsetPanel(session, "tabs", selected = "tab_basic")
        ###############################################################
        # metadata form
        if (!is.null(study$basic)){
          print("Email form is shown...")
          ######################
          output$info_email <- renderText({
            paste0("Thank you for your interest in ",global_info[,"name"]," Database!\n\n",
                  "Verification e-mail was sent to your address: ",study$basic$study_email,"\n\n",
                  "It may take few minutes.\n\n",
                  "Please continue with submission through recieved link.")
          })
          
          # send email...
          # # generate folder for user task...
          # outputDir <- paste0(global_out_path,"responses_", as.integer(Sys.time()),"/")
          # print(outputDir)
          # system(paste("mkdir ", outputDir, sep = ""))
          # # send email
          # x <- paste0("From: info@globalfungi.com\n",
          #   "Subject: Submit your study\n\n",
          #   "Dear User,\n\n",
          #   "Please follow this link to finish your submission:\n\n",
          #   "http://globalfungi.com/?study=",study$basic$study_hash,"\n\n",
          #   "Best\n",
          #   "Your ",global_info[,"name"],"Database Team\n")
          # print(x)
          # write.table(x, file = paste(outputDir,"email.txt", sep = ""), quote = F, col.names = F, row.names = F)
          # cmd <- paste0("sendmail ",study$basic$study_email,"  < ",outputDir,"email.txt")
          # print(cmd)
          # system(cmd)
          # # remove folder after use...
          # system(paste0("rm -rf ",outputDir))
          
          #################
          # EMAIL - START #
          #################
          bodytext <- paste0("Dear User,\n\n",
                             "Please follow this link to finish your submission:\n\n",
                             "http://globalfungi.com/?study=",study$basic$study_hash,"\n\n",
                             "Best\n",
                             "Your ",global_info[,"name"],"Database Team\n")
          
          sender <- "info@globalfungi.com"
          recipients <- c(study$basic$study_email)
          send.mail(from = sender,
                    to = recipients,
                    subject="Submit your study",
                    body = bodytext,
                    smtp = list(host.name = "email09.active24.com", port = 465, 
                                user.name="info@globalfungi.com", passwd="ea4XRNz0XT", ssl=TRUE),
                    authenticate = TRUE,
                    send = TRUE)
          ###############
          # EMAIL - END #
          ###############
          hideTab(inputId = "tabs", target = "tab_basic")
          showTab(inputId = "tabs", target = "tab_email")
          updateTabsetPanel(session, "tabs", selected = "tab_email")
        }
        ###############################################################
        } else {
        print("Forms are hidden...")
        hideTab(inputId = "tabs", target = "tab_basic")
        hideTab(inputId = "tabs", target = "tab_email")
        hideTab(inputId = "tabs", target = "tab_metadata")
        hideTab(inputId = "tabs", target = "tab_upload")
        hideTab(inputId = "tabs", target = "tab_overview")
        updateTabsetPanel(session, "tabs", selected = "tab_intro")
        }
      }
    }
  })
  
  observe({
    callModule(module = insertIntroFunc, id = "insert_intro", study)
    callModule(module = insertBasicFunc, id = "insert_basic", study)
  })
  
  output$info_over <- renderText(
    paste0(
      "------------------------------------------------------------------------------------\n",
      "Number of Samples: ", study$metadata$num_of_samples, "\n",
      "------------------------------------------------------------------------------------\n",
      "Number of Files: ", nrow(study$upload$data), "\n")
  ) 

}
