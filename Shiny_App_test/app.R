# ====================================================================================
# PACKAGES
# ====================================================================================

library(rsconnect)
library(shiny)
library(quarto)
library(bslib)
library(lubridate)
library(tinytex)

#rsconnect::deployApp()

# ====================================================================================
# DESIGN
# ====================================================================================

ui <- page_fluid(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2B6CB0",
    secondary = "#718096",
    success = "#38A169",
    base_font = font_google("Inter")
  ),
  
  tags$head(
    tags$style(HTML("
    
      body {
        background: #F4F7FB;
        color: #1F2937;
        font-family: 'Inter', sans-serif;
        padding: 24px;
      }
      
      .main-container {
        max-width: 1650px;
        margin: 0 auto;
      }
      
      /* HERO */
      .hero-section {
        background: linear-gradient(
          135deg,
          #1E3A5F 0%,
          #2B6CB0 50%,
          #4C8ED9 100%
        );
        
        border-radius: 30px;
        padding: 70px;
        color: white;
        margin-bottom: 38px;
        position: relative;
        overflow: hidden;
        box-shadow: 0 20px 60px rgba(43,108,176,0.18);
      }
      
      .hero-section::before {
        content: '';
        position: absolute;
        top: -120px;
        right: -120px;
        width: 340px;
        height: 340px;
        border-radius: 50%;
        background: rgba(255,255,255,0.08);
      }
      
      .hero-section::after {
        content: '';
        position: absolute;
        bottom: -80px;
        left: -80px;
        width: 240px;
        height: 240px;
        border-radius: 50%;
        background: rgba(255,255,255,0.06);
      }
      
      .hero-title {
        font-size: 52px;
        font-weight: 800;
        line-height: 1.1;
        margin-bottom: 22px;
        letter-spacing: -1px;
        position: relative;
        z-index: 2;
      }
      
      .hero-text {
        font-size: 18px;
        line-height: 1.9;
        max-width: 1100px;
        opacity: 0.95;
        position: relative;
        z-index: 2;
      }
      
      /* TABS */
      .nav-tabs {
          display: flex;
      
          flex-wrap: wrap !important;   
      
          overflow: visible;
      
          white-space: normal;
      
          gap: 12px;
      
          border: none !important;
      
          background: white;
      
          border-radius: 22px;
      
          padding: 16px;
      
          margin-bottom: 40px;
      
          box-shadow: 0 10px 35px rgba(15,23,42,0.06);
          
          
      }
      
      .nav-tabs .nav-item {
          flex: 0 0 auto;
      }
      
      .nav-tabs .nav-link {
      
          white-space: normal !important;
      
          text-align: center;
      
          min-width: 220px;   /* adjust width */
      
          padding: 15px 24px;
      }
      
      .nav-tabs::-webkit-scrollbar {
        height: 8px;
      }
      
      .nav-tabs::-webkit-scrollbar-thumb {
        background: #CBD5E1;
        border-radius: 999px;
      }
      
      .nav-tabs .nav-item {
        flex: 0 0 auto;
      }
      
      .nav-tabs .nav-link {
        border: none !important;
        border-radius: 16px !important;
        padding: 15px 24px;
        color: #475569;
        font-weight: 650;
        font-size: 15px;
        transition: all 0.25s ease;
        background: transparent;
        white-space: nowrap !important;
        min-width: max-content;
      }
      
      .nav-tabs .nav-link:hover {
        background: #EDF4FF;
        color: #2B6CB0;
        transform: translateY(-1px);
      }
      
      .nav-tabs .nav-link.active {
        background: linear-gradient(
          135deg,
          #2B6CB0 0%,
          #4C8ED9 100%
        ) !important;
        color: white !important;
        box-shadow: 0 8px 18px rgba(43,108,176,0.25);
      }
      
      /* CARD DESIGN */
      .card {
        border: none !important;
        border-radius: 28px !important;
        background: white;
        box-shadow: 0 14px 40px rgba(15,23,42,0.07);
        padding: 42px;
        margin-bottom: 32px;
        transition: all 0.25s ease;
      }
      
      .card:hover {
       transform: none !important;
      }
      
      .card-header {
        background: transparent !important;
        border: none !important;
        padding: 0 0 28px 0;
        margin-bottom: 28px;
        font-size: 30px;
        font-weight: 750;
        color: #111827;
      }
      
      /* FORM */
      .form-group,
      .shiny-input-container {
        width: 100% !important;
        max-width: 100% !important;
        margin-bottom: 34px !important;
      }
      
      .control-label {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        background: #EDF4FF;
        color: #1E3A5F;
        border-radius: 14px;
        padding: 12px 18px;
        font-size: 14px;
        font-weight: 700;
        margin-bottom: 14px;
        border: 1px solid #D7E7FF;
        line-height: 1.5;
        width: 100%;
      }
      
      .form-control,
      .form-select {
        border-radius: 16px !important;
        border: 1px solid #DCE3EC !important;
        background: #FAFCFE !important;
        min-height: 58px;
        padding: 16px 18px !important;
        font-size: 15px;
        color: #1F2937;
        transition: all 0.2s ease;
        box-shadow: none !important;
      }
      
      .form-control:hover,
      .form-select:hover {
        border-color: #B8C7DB !important;
      }
      
      .form-control:focus,
      .form-select:focus {
        background: white !important;
        border-color: #2B6CB0 !important;
        box-shadow: 0 0 0 0.18rem rgba(43,108,176,0.14) !important;
      }
      
      textarea.form-control {
        min-height: 180px !important;
        resize: vertical;
      }
      
      /* BUTTONS */
      .btn-primary {
        background: linear-gradient(
          135deg,
          #2B6CB0 0%,
          #4C8ED9 100%
        ) !important;
        border: none !important;
        border-radius: 16px !important;
        padding: 15px 34px !important;
        font-size: 15px;
        font-weight: 700;
        letter-spacing: 0.2px;
        box-shadow: 0 10px 22px rgba(43,108,176,0.22);
        transition: all 0.25s ease;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 14px 28px rgba(43,108,176,0.30);
      }
      
      /* TABLES */
      table {
        border-collapse: separate !important;
        border-spacing: 0 10px !important;
      }
      
      .table thead th {
        border: none !important;
        color: #64748B;
        font-weight: 700;
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }
      
      .table tbody tr {
        background: white;
        box-shadow: 0 4px 16px rgba(15,23,42,0.04);
      }
      
      .table tbody td {
        border-top: none !important;
        padding: 18px !important;
        vertical-align: middle;
      }
      
      .table tbody tr td:first-child {
        border-top-left-radius: 14px;
        border-bottom-left-radius: 14px;
      }
      
      .table tbody tr td:last-child {
        border-top-right-radius: 14px;
        border-bottom-right-radius: 14px;
      }
      
      /* SCROLLBAR */
      ::-webkit-scrollbar {
        height: 10px;
        width: 10px;
      }
      
      ::-webkit-scrollbar-thumb {
        background: #CBD5E1;
        border-radius: 999px;
      }
      
      /* MOBILE */
      @media (max-width: 992px) {
        
        body {
          padding: 14px;
        }
        
        .hero-section {
          padding: 40px 28px;
          border-radius: 24px;
        }
        
        .hero-title {
          font-size: 36px;
        }
        
        .card {
          padding: 28px;
          border-radius: 22px !important;
        }
      }
      
    "))
  ),
  
  # ======================================================================================
  # MAIN CONTENT
  # ======================================================================================
  
  div(
    class = "main-container",
    
    # ====================================================================================
    # HERO
    # ====================================================================================
    
    div(
      class = "hero-section",
      
      div(
        class = "hero-title",
        "Preregistration Template for Data Donation Studies"
      ),
      
      div(
        class = "hero-text",
        
        "This platform supports researchers in planning data donation studies by systematically working through the components essential for high-quality preregistrations. It provides a structured environment to make key methodological and ethical decisions explicit and ensure the completeness of study plans prior to data collection."
      )
    ),
    
    # ====================================================================================
    # NAVIGATION TABS
    # ====================================================================================
    
    navset_tab(
      
      # ==================================================================================
      # METADATA
      # ==================================================================================
      
      nav_panel(
        "Metadata",
        
        br(),
        
        card(
          
          card_header("Metadata"),
          
          textInput("title", "1. Title"),
          
          textInput("authors", "2. Authors & Affiliations"),
          
          dateInput(
            "date",
            "3. Date of Preregistration",
            value = NULL
          ),
          
          textInput(
            "version_identifier",
            "4. Version & Identifier (DOI or OSF link)"
          ),
          
          selectInput(
            "license",
            "5. License",
            choices = c(
              "No License" = "No License",
              "Academic Free License 3.0" = "Academic Free License 3.0",
              "Apache License 2.0" = "Apache License 2.0",
              "Artistic License 2.0" = "Artistic License 2.0",
              "BSD 2-Clause \"Simplified\" License" = "BSD 2-Clause \"Simplified\" License",
              "BSD 3-Clause \"New/Revised\" License" = "BSD 3-Clause \"New/Revised\" License",
              "CC-By Attribution 4.0 International" = "CC-By Attribution 4.0 International",
              "CC0 1.0 Universal" = "CC0 1.0 Universal",
              "CC-By Attribution-NonCommercial-NoDerivatives 4.0 International" = "CC-By Attribution-NonCommercial-NoDerivatives 4.0 International",
              "CC-By Attribution-ShareAlike 4.0 International" = "CC-By Attribution-ShareAlike 4.0 International",
              "Eclipse Public License 1.0" = "Eclipse Public License 1.0",
              "GNU General Public License (GPL) 2.0" = "GNU General Public License (GPL) 2.0",
              "GNU General Public License (GPL) 3.0" = "GNU General Public License (GPL) 3.0",
              "GNU Lesser General Public License (LGPL) 2.1" = "GNU Lesser General Public License (LGPL) 2.1",
              "GNU Lesser General Public License (LGPL) 3.0" = "GNU Lesser General Public License (LGPL) 3.0",
              "MIT License" = "MIT License",
              "Mozilla Public License 2.0" = "Mozilla Public License 2.0"
            )
          ),
          
          textInput(
            "keywords",
            "6. Keywords / Tags"
          ),
          
          textAreaInput(
            "funding",
            "7. Funding & Conflict of Interest Statement"
          ),
          
          textAreaInput(
            "ethics",
            "8. Ethics Approval / IRB Status"
          )
        )
      ),
      
      # ==================================================================================
      # STUDY OVERVIEW
      # ==================================================================================
      
      nav_panel(
        "Study Overview",
        
        br(),
        
        card(
          
          card_header("Study Overview"),
          
          textAreaInput(
            "background",
            "1. Background & Rationale (theoretical framework, prior work)"
          ),
          
          textAreaInput(
            "objectives",
            "2. Objectives & Research Questions"
          ),
          
          textAreaInput(
            "hypotheses",
            "3. Hypotheses (directional/non-directional; confirmatory/exploratory)"
          ),
          
          textAreaInput(
            "exploratory_questions",
            "4. Exploratory Research Questions (if applicable)"
          ),
          
          textAreaInput(
            "ddp_justification",
            "5. Theoretical Justification for DDP use (why data donation is needed over other data sources)"
          )
        )
      ),
      
      # ==================================================================================
      # DATA SOURCES & DESCRIPTION
      # ==================================================================================
      
      nav_panel(
        "Data Sources & Description",
        
        br(),
        
        card(
          
          card_header("Data Sources & Description"),
          
          textAreaInput(
            "platform_tool",
            "1. Platform & API / Export Tool Used"
          ),
          
          textAreaInput(
            "access_method",
            "2. Data Access Method"
          ),
          
          textAreaInput(
            "dataset_description",
            "3. Dataset Name / Description"
          ),
          
          textAreaInput(
            "download_dates",
            "4. Date(s) of Data Access or Download"
          ),
          
          textAreaInput(
            "availability",
            "5. Data Availability & Access Restrictions"
          ),
          
          textAreaInput(
            "prior_knowledge",
            "6. Prior Knowledge of Data"
          ),
          
          textAreaInput(
            "codebook",
            "7. Codebook & Documentation"
          ),
          
          textAreaInput(
            "collection_procedures",
            "8. Data Collection Procedures"
          ),
          
          textAreaInput(
            "privacy_security",
            "9. Privacy & Security Measures"
          )
        )
      ),
      
      # ==================================================================================
      # SAMPLING PLAN
      # ==================================================================================
      
      nav_panel(
        "Sampling Plan",
        
        br(),
        
        card(
          
          card_header("Sampling Plan"),
          
          textAreaInput(
            "population",
            "1. Target Population & Inclusion/Exclusion Criteria"
          ),
          
          textAreaInput(
            "recruitment",
            "2. Recruitment Methods"
          ),
          
          textAreaInput(
            "sample_size",
            "3. Sample Size Target (+ Rationale / Power Analysis)"
          ),
          
          textAreaInput(
            "stopping_rule",
            "4. Stopping Rule"
          ),
          
          textAreaInput(
            "representativeness",
            "5. Representativeness & Bias Considerations"
          )
        )
      ),
      
      # ==================================================================================
      # DATA STRUCTURE & PREPROCESSING
      # ==================================================================================
      
      nav_panel(
        "Data Structure & Preprocessing",
        
        br(),
        
        card(
          
          card_header("Data Structure & Preprocessing"),
          
          textAreaInput(
            "raw_data_structure",
            "1. Raw Data Structure (file formats, main tables, variable types)"
          ),
          
          textAreaInput(
            "data_cleaning",
            "2. Data Cleaning & Screening (duplicate removal, invalid entries, missing metadata)"
          ),
          
          textAreaInput(
            "feature_extraction",
            "3. Feature Extraction & Variable Operationalization"
          ),
          
          textAreaInput(
            "assumptions",
            "4. Assumptions about Digital Trace Measures"
          ),
          
          textAreaInput(
            "missing_data",
            "5. Missing Data Handling Plan"
          )
        )
      ),
      
      # ==================================================================================
      # STUDY DESIGN
      # ==================================================================================
      
      nav_panel(
        "Study Design",
        
        br(),
        
        card(
          
          card_header("Study Design"),
          
          textAreaInput(
            "study_type",
            "1. Study Type (observational, experimental, quasi-experimental, simulation-based)"
          ),
          
          textAreaInput(
            "blinding",
            "2. Blinding / Masking"
          ),
          
          textAreaInput(
            "design_description",
            "3. Design Description"
          ),
          
          textAreaInput(
            "conditions",
            "4. Conditions / Groups"
          ),
          
          textAreaInput(
            "randomization",
            "5. Randomization Strategy"
          )
        )
      ),
      
      # ==================================================================================
      # MEASUREMENTS
      # ==================================================================================
      
      nav_panel(
        "Measurements",
        
        br(),
        
        card(
          
          card_header("Measurements"),
          
          textAreaInput(
            "independent_variables",
            "1. Independent / Predictor Variables"
          ),
          
          textAreaInput(
            "dependent_variables",
            "2. Dependent / Outcome Variables"
          ),
          
          textAreaInput(
            "control_variables",
            "3. Control / Covariate Variables"
          ),
          
          textAreaInput(
            "derived_variables",
            "4. Derived / Composite Variables (calculation formulas)"
          ),
          
          textAreaInput(
            "platform_indicators",
            "5. Platform-specific Indicators"
          )
        )
      ),
      
      # ==================================================================================
      # ANALYSIS PLAN
      # ==================================================================================
      
      nav_panel(
        "Analysis Plan",
        
        br(),
        
        card(
          
          card_header("Analysis Plan"),
          
          textAreaInput(
            "primary_analyses",
            "1. Primary Analyses (methods, statistical models, outcome measures)"
          ),
          
          textAreaInput(
            "secondary_analyses",
            "2. Secondary Analyses"
          ),
          
          textAreaInput(
            "inference_criteria",
            "3. Inference Criteria (NHST, Bayesian thresholds, effect sizes)"
          ),
          
          textAreaInput(
            "modeling_parameters",
            "4. Modeling & Simulation Parameters"
          ),
          
          textAreaInput(
            "performance_measures",
            "5. Performance Measures (accuracy, bias, precision)"
          ),
          
          textAreaInput(
            "software_packages",
            "6. Software & Packages Used"
          ),
          
          textAreaInput(
            "reproducibility",
            "7. Reproducibility Measures (code sharing plan)"
          )
        )
      ),
      
      # ==================================================================================
      # RISKS & MITIGATION
      # ==================================================================================
      
      nav_panel(
        "Specific Risks & Mitigation",
        
        br(),
        
        card(
          
          card_header("Specific Risks & Mitigation"),
          
          textAreaInput(
            "participant_risks",
            "1. Potential Risks to Participants (privacy breaches, reidentification risk)"
          ),
          
          textAreaInput(
            "risk_mitigation",
            "2. Risk Mitigation Strategies (secure storage, encryption, controlled access)"
          ),
          
          textAreaInput(
            "ethical_justification",
            "3. Ethical Justification for Using DDP Data"
          )
        )
      ),
      
      # ==================================================================================
      # OPEN SCIENCE & REPLICABILITY
      # ==================================================================================
      
      nav_panel(
        "Open Science & Replicability",
        
        br(),
        
        card(
          
          card_header("Open Science & Replicability"),
          
          textAreaInput(
            "data_sharing",
            "1. Data Sharing Plan (raw, processed, or synthetic)"
          ),
          
          textAreaInput(
            "code_sharing",
            "2. Code Sharing Plan"
          ),
          
          textAreaInput(
            "pipeline_documentation",
            "3. Preprocessing Pipeline Documentation"
          ),
          
          textAreaInput(
            "update_policy",
            "4. Preregistration Updates Policy (if deviations occur)"
          )
        )
      ),
      
      # ==================================================================================
      # REFERENCES & SUPPORTING MATERIAL
      # ==================================================================================
      
      nav_panel(
        "References & Supporting Material",
        
        br(),
        
        card(
          
          card_header("References & Supporting Material"),
          
          textAreaInput(
            "citations",
            "1. Citations for prior studies, APIs, and tools"
          ),
          
          textAreaInput(
            "appendices",
            "2. Appendices (item lists, questionnaires, data schemas)"
          )
        )
      )
    ),
    
    br(),
    
    div(
      style = "display:flex; gap:12px; justify-content:right;",
      
      actionButton(
        "fill_test",
        "Fill with test data",
        class = "btn btn-secondary"
      ),
      
      downloadButton(
        "download_pdf",
        "Download PDF"
      )
    )
  )
)

# ==================================================================================
# SERVER
# ==================================================================================

server <- function(input, output, session) {
  
  # Autofill 
  fill_textareas <- function(ids, value = "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis, feugiat a, tellus. Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum. Aenean imperdiet. Etiam ultricies nisi vel augue. Curabitur ullamcorper ultricies nisi. Nam eget dui. Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio et ante tincidunt tempus. Donec vitae sapien ut libero venenatis faucibus. Nullam quis ante. Etiam sit amet orci eget eros faucibus tincidunt. Duis leo. Sed fringilla mauris sit amet nibh. Donec sodales sagittis magna. Sed consequat, leo eget bibendum sodales, augue velit cursus nunc,") {
    lapply(ids, function(id) {
      updateTextAreaInput(session, id, value = paste(value, "-", "TEST"))
    })
  }
  
  
  observeEvent(input$fill_test, {
    
    fill_textareas(c(
      # METADATA
      "title",
      "authors",
      "date",
      "version_identifier",
      "license",
      "keywords",
      "funding",
      "ethics",
      
      # STUDY OVERVIEW
      "background",
      "objectives",
      "hypotheses",
      "exploratory_questions",
      "ddp_justification",
      
      # DATA SOURCES
      "platform_tool",
      "access_method",
      "dataset_description",
      "download_dates",
      "availability",
      "prior_knowledge",
      "codebook",
      "collection_procedures",
      "privacy_security",
      
      # SAMPLING
      "population",
      "recruitment",
      "sample_size",
      "stopping_rule",
      "representativeness",
      
      # DATA STRUCTURE
      "raw_data_structure",
      "data_cleaning",
      "feature_extraction",
      "assumptions",
      "missing_data",
      
      # STUDY DESIGN
      "study_type",
      "blinding",
      "design_description",
      "conditions",
      "randomization",
      
      # MEASUREMENTS
      "independent_variables",
      "dependent_variables",
      "control_variables",
      "derived_variables",
      "platform_indicators",
      
      # ANALYSIS
      "primary_analyses",
      "secondary_analyses",
      "inference_criteria",
      "modeling_parameters",
      "performance_measures",
      "software_packages",
      "reproducibility",
      
      # RISKS
      "participant_risks",
      "risk_mitigation",
      "ethical_justification",
      
      # OPEN SCIENCE
      "data_sharing",
      "code_sharing",
      "pipeline_documentation",
      "update_policy",
      
      # REFERENCES
      "citations",
      "appendices"
    ))
    
  })
  
  output$download_pdf <- downloadHandler(
    
    filename = function() {
      "Preregistration.pdf"
    },
    
    content = function(file) {
      
      temp_dir <- tempdir()
      temp_qmd <- file.path(temp_dir, "Preregistration.qmd")
      
      file.copy("Preregistration.qmd", temp_qmd, overwrite = TRUE)
      
      params <- reactiveValuesToList(input)
      params$download_pdf <- NULL
      params$fill_test <- NULL
      
      withProgress(message = "Generating PDF...", value = 0, {
        
        incProgress(0.2, detail = "Preparing document")
        
        result <- try(
          quarto::quarto_render(
            input = temp_qmd,
            output_file = "Preregistration.pdf",
            execute_params = params
          ),
          silent = FALSE
        )
        
        incProgress(0.8, detail = "Finalizing PDF")
      })
      
      pdf_path <- file.path(temp_dir, "Preregistration.pdf")
      
      if (!file.exists(pdf_path)) {
        stop("PDF error")
      }
      
      file.copy(pdf_path, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)

