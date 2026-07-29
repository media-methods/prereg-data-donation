# ====================================================================================
# PACKAGES
# ====================================================================================

library(rsconnect)
library(shiny)
library(quarto)
library(bslib)
library(lubridate)

#rsconnect::deployApp()

# ====================================================================================
# DESIGN
# ====================================================================================

# Helper: render a bold title, a grey hint paragraph, then the input box.
# Pass the input with label = NULL so the title/hint above are the only labels.
field <- function(title, hint, input_tag) {
  div(
    class = "field-with-hint",
    tags$label(class = "field-title", title),
    tags$p(class = "field-hint", hint),
    input_tag
  )
}

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
      
      /* field wrapper: blue title box, grey hint, then the input box */
      .field-with-hint {
        margin-bottom: 16px;
      }
      /* the title styled as the blue rounded box */
      .field-with-hint .field-title {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        background: #EDF4FF;
        color: #1E3A5F;
        border-radius: 14px;
        padding: 12px 18px;
        font-size: 14px;
        font-weight: 700;
        line-height: 1.5;
        width: 100%;
        border: 1px solid #D7E7FF;
        margin-bottom: 8px;
      }
      .field-with-hint .field-hint {
        display: block;
        font-size: 0.82em;
        font-weight: 400;
        color: #8A94A6;
        margin: 0 0 8px 0;
        line-height: 1.35;
      }
      /* links inside a hint match the blue of the title box */
      .field-with-hint .field-hint a,
      .field-with-hint .field-hint a:hover,
      .field-with-hint .field-hint a:focus {
        color: #1E3A5F !important;
        text-decoration: underline;
      }
      /* the input's own (now empty) label must not render as a second blue box */
      .field-with-hint .form-group > .control-label,
      .field-with-hint .shiny-input-container > .control-label {
        display: none !important;
      }
      /* input's own form-group loses its default label spacing */
      .field-with-hint .form-group,
      .field-with-hint .shiny-input-container {
        margin-bottom: 0 !important;
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
          
          field("1. Title",
            "Use an informative title for the study.",
            textInput("title", label = NULL)),
          
          field("2. Authors & Affiliations",
            "List all authors and their institutional affiliations. If possible, add ORCID-numbers for identification.",
            textInput("authors", label = NULL)),
          
          field("3. Date of Preregistration",
            "Add the date this preregistration was created (auto-generated to be the current date).",
            dateInput("date", label = NULL, value = NULL)),
          
          field("4. License",
                tagList(
                  "Choose how others may reuse your materials. See ",
                  tags$a(
                    href = "https://help.osf.io/article/148-licensing#license",
                    target = "_blank",
                    rel = "noopener",
                    "OSF's licensing guide"
                  ),
                  " for descriptions of each option."
                ),
                selectInput(
              "license",
              label = NULL,
              choices = c(
                "No License" = "No License",
                "Academic Free License 3.0" = "Academic Free License 3.0",
                "Apache License 2.0" = "Apache License 2.0",
                "Artistic License 2.0" = "Artistic License 2.0",
                "BSD 2-Clause \"Simplified\" License" = "BSD 2-Clause \"Simplified\" License",
                "BSD 3-Clause \"New\"/\"Revised\" License" = "BSD 3-Clause \"New\"/\"Revised\" License",
                "CC-By-Attribution 4.0 International" = "CC-By-Attribution 4.0 International",
                "CC-By-Attribution-NonCommercial-NoDerivatives 4.0 International" = "CC-By-Attribution-NonCommercial-NoDerivatives 4.0 International",
                "CC-By Attribution-ShareAlike 4.0 International" = "CC-By Attribution-ShareAlike 4.0 International",
                "CC0 1.0 Universal" = "CC0 1.0 Universal",
                "Eclipse Public License 1.0" = "Eclipse Public License 1.0",
                "GNU General Public License (GPL) 2.0" = "GNU General Public License (GPL) 2.0",
                "GNU General Public License (GPL) 3.0" = "GNU General Public License (GPL) 3.0",
                "GNU Lesser General Public License (LGPL) 2.1" = "GNU Lesser General Public License (LGPL) 2.1",
                "GNU Lesser General Public License (LGPL) 3.0" = "GNU Lesser General Public License (LGPL) 3.0",
                "MIT License" = "MIT License",
                "Mozilla Public License 2.0" = "Mozilla Public License 2.0"
              )
            )),
          
                    field("5. Ethics Approval / IRB Status",
            "State the approving body and reference number (or why approval was not required).",
            textInput("ethics", label = NULL))
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
          
          field("1. Background & Rationale",
            "If needed, add information on what the study aims to test and the general study design.",
            textAreaInput("background", label = NULL)),
          
          field("2. Research Questions or Hypotheses",
            "What research questions or hypotheses are you planning to evaluate? List them separately.",
            textAreaInput("objectives", label = NULL)),
          
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
          
          field("1. Platform & API / Export Tool Used",
            "Name the platform and the API or export tool used to obtain the data.",
            textAreaInput("platform_tool", label = NULL)),
          
          field("2. Data Access Method",
            "Describe how participants exported and donated their data.",
            textAreaInput("access_method", label = NULL)),
          
          field("3. Dataset Name / Description",
            "Give the dataset a name and briefly describe its contents.",
            textAreaInput("dataset_description", label = NULL)),
          
          field("4. Date(s) of Data Access or Download",
            "State when the data were accessed or downloaded.",
            textAreaInput("download_dates", label = NULL)),
          
          field("5. Data Availability & Access Restrictions",
            "Describe who can access the data and any restrictions that apply.",
            textAreaInput("availability", label = NULL)),
          
          field("6. Prior Knowledge of Data",
            "Disclose any prior familiarity with the data before analysis.",
            textAreaInput("prior_knowledge", label = NULL)),
          
          field("7. Codebook & Documentation",
            "Indicate whether a codebook or documentation exists and where.",
            textAreaInput("codebook", label = NULL)),
          
          field("8. Data Collection Procedures",
            "Outline the step-by-step procedure for collecting the donated data.",
            textAreaInput("collection_procedures", label = NULL)),
          
          field("9. Privacy & Security Measures",
            "Describe measures taken to protect participant privacy and secure the data.",
            textAreaInput("privacy_security", label = NULL))
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
          
          field("1. Target Population & Inclusion/Exclusion Criteria",
            "Define the target population and inclusion/exclusion criteria.",
            textAreaInput("population", label = NULL)),
          
          field("2. Recruitment Methods",
            "Describe how participants will be recruited.",
            textAreaInput("recruitment", label = NULL)),
          
          field("3. Sample Size Target (+ Rationale / Power Analysis)",
            "State the target sample size and justify it (e.g. power analysis).",
            textAreaInput("sample_size", label = NULL)),
          
          field("4. Stopping Rule",
            "Specify the rule for when data collection stops.",
            textAreaInput("stopping_rule", label = NULL)),
          
          field("5. Representativeness & Bias Considerations",
            "Discuss how representative the sample is and possible sources of bias.",
            textAreaInput("representativeness", label = NULL))
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
          
          field("1. Raw Data Structure (file formats, main tables, variable types)",
            "Describe file formats, main tables, and variable types of the raw data.",
            textAreaInput("raw_data_structure", label = NULL)),
          
          field("2. Data Cleaning & Screening (duplicate removal, invalid entries, missing metadata)",
            "Explain how duplicates, invalid entries, and missing metadata are handled.",
            textAreaInput("data_cleaning", label = NULL)),
          
          field("3. Feature Extraction & Variable Operationalization",
            "Describe how raw data are turned into analysable variables.",
            textAreaInput("feature_extraction", label = NULL)),
          
          field("4. Assumptions about Digital Trace Measures",
            "State the assumptions you make about digital trace measures.",
            textAreaInput("assumptions", label = NULL)),
          
          field("5. Missing Data Handling Plan",
            "Describe your plan for handling missing data.",
            textAreaInput("missing_data", label = NULL))
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
          
          field("1. Study Type (observational, experimental, quasi-experimental, simulation-based)",
            "Specify the study type (observational, experimental, etc.).",
            textAreaInput("study_type", label = NULL)),
          
          field("2. Blinding / Masking",
            "State whether and how blinding or masking is applied.",
            textAreaInput("blinding", label = NULL)),
          
          field("3. Design Description",
            "Describe the overall design of the study.",
            textAreaInput("design_description", label = NULL)),
          
          field("4. Conditions / Groups",
            "List the conditions or groups compared, if any.",
            textAreaInput("conditions", label = NULL)),
          
          field("5. Randomization Strategy",
            "Describe how units are assigned to conditions, if applicable.",
            textAreaInput("randomization", label = NULL))
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
          
          field("1. Independent / Predictor Variables",
            "List the predictor or independent variables.",
            textAreaInput("independent_variables", label = NULL)),
          
          field("2. Dependent / Outcome Variables",
            "List the outcome or dependent variables.",
            textAreaInput("dependent_variables", label = NULL)),
          
          field("3. Control / Covariate Variables",
            "List control variables or covariates included.",
            textAreaInput("control_variables", label = NULL)),
          
          field("4. Derived / Composite Variables (calculation formulas)",
            "Describe any composite variables and how they are calculated.",
            textAreaInput("derived_variables", label = NULL)),
          
          field("5. Platform-specific Indicators",
            "Note any platform-specific metrics you use as measures.",
            textAreaInput("platform_indicators", label = NULL))
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
          
          field("1. Primary Analyses (methods, statistical models, outcome measures)",
            "Describe the main statistical models and outcome measures.",
            textAreaInput("primary_analyses", label = NULL)),
          
          field("2. Secondary Analyses",
            "Describe any additional planned analyses.",
            textAreaInput("secondary_analyses", label = NULL)),
          
          field("3. Inference Criteria (NHST, Bayesian thresholds, effect sizes)",
            "State thresholds for inference (p-values, Bayesian criteria, effect sizes).",
            textAreaInput("inference_criteria", label = NULL)),
          
          field("4. Modeling & Simulation Parameters",
            "Specify parameters for models or simulations, if used.",
            textAreaInput("modeling_parameters", label = NULL)),
          
          field("5. Performance Measures (accuracy, bias, precision)",
            "Describe how model performance is evaluated (accuracy, bias, precision).",
            textAreaInput("performance_measures", label = NULL)),
          
          field("6. Software & Packages Used",
            "List the software and packages used for analysis.",
            textAreaInput("software_packages", label = NULL)),
          
          field("7. Reproducibility Measures (code sharing plan)",
            "Describe how code and materials will be shared for reproducibility.",
            textAreaInput("reproducibility", label = NULL))
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
          
          field("1. Potential Risks to Participants (privacy breaches, reidentification risk)",
            "Identify potential risks to participants, such as reidentification.",
            textAreaInput("participant_risks", label = NULL)),
          
          field("2. Risk Mitigation Strategies (secure storage, encryption, controlled access)",
            "Describe strategies to reduce those risks (encryption, access control).",
            textAreaInput("risk_mitigation", label = NULL)),
          
          field("3. Ethical Justification for Using DDP Data",
            "Justify the ethical basis for using donated data.",
            textAreaInput("ethical_justification", label = NULL))
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
          
          field("1. Data Sharing Plan (raw, processed, or synthetic)",
            "State what data will be shared (raw, processed, or synthetic) and where.",
            textAreaInput("data_sharing", label = NULL)),
          
          field("2. Code Sharing Plan",
            "Describe your plan for sharing analysis code.",
            textAreaInput("code_sharing", label = NULL)),
          
          field("3. Preprocessing Pipeline Documentation",
            "Explain how the preprocessing pipeline is documented.",
            textAreaInput("pipeline_documentation", label = NULL)),
          
          field("4. Preregistration Updates Policy (if deviations occur)",
            "Describe how deviations from this preregistration will be recorded.",
            textAreaInput("update_policy", label = NULL))
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
          
          field("1. Citations for prior studies, APIs, and tools",
            "List citations for prior studies, APIs, and tools referenced.",
            textAreaInput("citations", label = NULL)),
          
          field("2. Appendices (item lists, questionnaires, data schemas)",
            "Attach or describe supporting material (item lists, schemas, questionnaires).",
            textAreaInput("appendices", label = NULL))
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
        "Download Word document"
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
      "license",
      "ethics",
      
      # STUDY OVERVIEW
      "background",
      "questions_hypotheses",
      
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
      "Preregistration.docx"
    },
    
    content = function(file) {
      
      temp_dir <- tempdir()
      temp_qmd <- file.path(temp_dir, "Preregistration.qmd")
      
      file.copy("Preregistration.qmd", temp_qmd, overwrite = TRUE)
      
      params <- reactiveValuesToList(input)
      params$download_pdf <- NULL
      params$fill_test <- NULL
      
      withProgress(message = "Generating document...", value = 0, {
        
        incProgress(0.2, detail = "Preparing document")
        
        result <- try(
          quarto::quarto_render(
            input = temp_qmd,
            output_file = "Preregistration.docx",
            execute_params = params
          ),
          silent = FALSE
        )
        
        incProgress(0.8, detail = "Finalizing document")
      })
      
      docx_path <- file.path(temp_dir, "Preregistration.docx")
      
      if (!file.exists(docx_path)) {
        stop("Document generation error")
      }
      
      file.copy(docx_path, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)

