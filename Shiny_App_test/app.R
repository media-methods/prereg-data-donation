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
    div(
      class = "field-box",
      tags$span(class = "field-title", title),
      tags$span(class = "field-hint", hint)
    ),
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
        padding: 44px 48px;
        color: white;
        margin-bottom: 38px;
        position: relative;
        overflow: hidden;
        box-shadow: 0 20px 60px rgba(43,108,176,0.18);
        cursor: pointer;
      }
      /* Home vs. form toggle: .home-mode on the root shows the landing view
         and hides the tabbed form; default shows the form and hides the view. */
      .home-view { display: none; }
      .home-mode .home-view { display: block; }
      .home-mode .form-view { display: none; }
      .home-start-row {
        margin-top: 6px;
        display: flex;
        justify-content: flex-start;
      }
      .home-start-btn {
        border: none;
        background: linear-gradient(135deg, #2B6CB0 0%, #4C8ED9 100%);
        color: #FFFFFF;
        font-size: 16px;
        font-weight: 700;
        padding: 12px 24px;
        border-radius: 14px;
        cursor: pointer;
        box-shadow: 0 6px 18px rgba(43,108,176,0.25);
      }
      .home-start-btn:hover {
        background: linear-gradient(135deg, #24578F 0%, #3E7FC7 100%);
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
        font-size: 38px;
        font-weight: 800;
        line-height: 1.15;
        margin-bottom: 14px;
        letter-spacing: -1px;
        position: relative;
        z-index: 2;
      }
      
      .hero-text {
        font-size: 16px;
        line-height: 1.6;
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
        padding: 0 0 14px 0;
        margin-bottom: 14px;
        font-size: 30px;
        font-weight: 750;
        color: #111827;
      }
      
      /* HOME landing tab */
      .home-text {
        font-size: 16px;
        line-height: 1.7;
        color: #374151;
        margin-bottom: 18px;
      }
      .home-subhead {
        font-size: 18px;
        font-weight: 700;
        color: #1E3A5F;
        margin: 8px 0 12px 0;
      }
      /* Collapsible existing-preregistrations section */
      .prereg-details {
        margin: 4px 0 4px 0;
        border: 1px solid #E5EAF1;
        border-radius: 12px;
        background: #FFFFFF;
      }
      .prereg-summary {
        font-size: 18px;
        font-weight: 700;
        color: #1E3A5F;
        padding: 12px 16px;
        cursor: pointer;
        list-style: none;
        display: flex;
        align-items: center;
        gap: 10px;
        user-select: none;
      }
      .prereg-summary::-webkit-details-marker { display: none; }
      .prereg-summary::before {
        content: '';
        display: inline-block;
        width: 0;
        height: 0;
        border-top: 5px solid transparent;
        border-bottom: 5px solid transparent;
        border-left: 7px solid #2B6CB0;
        transition: transform 0.15s ease;
      }
      .prereg-details[open] > .prereg-summary::before {
        transform: rotate(90deg);       /* points down when open */
      }
      .prereg-summary:hover { color: #2B6CB0; }
      .prereg-details[open] > .prereg-summary {
        border-bottom: 1px solid #EEF2F7;
      }
      .prereg-details > .home-text,
      .prereg-details > .prereg {
        padding-left: 16px;
        padding-right: 16px;
      }
      .prereg-details > .home-text { margin-top: 12px; }
      .prereg-details > .prereg { padding-bottom: 14px; }
      /* nested per-study-type sub-sections */
      .prereg-subdetails {
        margin: 0 0 12px 0;
        border: 1px solid #EEF2F7;
        border-radius: 10px;
        background: #FBFDFF;
      }
      .prereg-subdetails:last-child { margin-bottom: 4px; }
      .prereg-subsummary {
        font-size: 15px;
        font-weight: 650;
        color: #2B4A6F;
        padding: 9px 14px;
        cursor: pointer;
        list-style: none;
        display: flex;
        align-items: center;
        gap: 9px;
        user-select: none;
      }
      .prereg-subsummary::-webkit-details-marker { display: none; }
      .prereg-subsummary::before {
        content: '';
        display: inline-block;
        width: 0;
        height: 0;
        border-top: 4px solid transparent;
        border-bottom: 4px solid transparent;
        border-left: 6px solid #6B7688;
        transition: transform 0.15s ease;
      }
      .prereg-subdetails[open] > .prereg-subsummary::before {
        transform: rotate(90deg);
      }
      .prereg-subsummary:hover { color: #2B6CB0; }
      .prereg-subdetails > .prereg {
        padding: 0 14px 12px 14px;
      }
      /* Preregistration list: compact two-line entries */
      .prereg {
        display: grid;
        grid-template-columns: 1fr;
        gap: 10px;
        margin: 0 0 4px 0;
      }
      .prereg-item {
        padding: 10px 14px;
        border: 1px solid #E5EAF1;
        border-radius: 12px;
        background: #FBFDFF;
      }
      .prereg-item .prereg-title {
        font-size: 14px;
        line-height: 1.4;
        color: #1E3A5F;
      }
      .prereg-item .prereg-title .prereg-year {
        font-weight: 700;
      }
      .prereg-item .prereg-title a {
        color: #2B6CB0;
        text-decoration: none;
        font-weight: 650;
      }
      .prereg-item .prereg-title a:hover {
        text-decoration: underline;
      }
      .prereg-item .prereg-desc {
        font-size: 12.5px;
        line-height: 1.45;
        color: #6B7688;
        margin-top: 3px;
      }
      .home-divider {
        border: none;
        border-top: 1px solid #E5EAF1;
        margin: 18px 0 10px 0;
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
      /* the optional foreknowledge explanation is usually short */
      #foreknowledge_explanation {
        min-height: 80px !important;
      }
      /* the Other study-type description is usually short */
      #study_type_other {
        min-height: 80px !important;
      }
      /* General Study Design text boxes: about 3/4 of the default height */
      #integration,
      #integration_other,
      #experimental_conditions,
      #blinding_experiment,
      #randomization_general,
      #researcher_workflow,
      #user_workflow,
      #informed_consent,
      /* Sampling Plan text boxes */
      #population,
      #sample_size,
      #platform_population,
      #platform_sample_size,
      #observation_period,
      #representation_errors,
      /* Measurements: Data Donation text boxes (excludes Collected Variables) */
      #raw_data_donation,
      #new_variables,
      #quality_control,
      #measurement_error_reflections,
      #other_information,
      #qualitative_variables,
      #additional_other_information,
      /* References tab text boxes */
      #references {
        min-height: 135px !important;
      }
      /* Collected Variables sub-sections: 2/3 of the previous height */
      .var-sub textarea.form-control {
        min-height: 60px !important;
      }
      /* RQ text boxes and per-RQ analysis plans (direct children of a block) */
      .var-block > .shiny-input-container textarea.form-control {
        min-height: 110px !important;
      }
      /* Analysis Plan: Data Cleaning and Inference Criteria at half height */
      #data_cleaning,
      #inference_criteria {
        min-height: 68px !important;
      }
      /* Collected Variables name field: 2/3 of the default input height */
      .var-block input.form-control {
        min-height: 39px !important;
        height: 39px !important;
      }
      
      /* file upload: make the file-name box the same height as the browse button */
      .field-with-hint .shiny-input-container .input-group .form-control {
        height: 44px !important;
        min-height: 44px !important;
        display: flex;
        align-items: center;
      }
      .field-with-hint .shiny-input-container .input-group .btn-file,
      .field-with-hint .shiny-input-container .input-group .input-group-btn .btn {
        height: 44px !important;
        display: inline-flex;
        align-items: center;
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
          font-size: 30px;
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
      /* the blue rounded box now wraps BOTH the title and the hint */
      .field-with-hint .field-box {
        display: block;
        background: #EDF4FF;
        color: #1E3A5F;
        border-radius: 14px;
        padding: 12px 18px;
        width: 100%;
        border: 1px solid #D7E7FF;
        margin-bottom: 8px;
      }
      /* bold title line inside the box */
      .field-with-hint .field-title {
        display: block;
        font-size: 14px;
        font-weight: 700;
        line-height: 1.5;
      }
      /* grey hint line, below the title, inside the same box */
      .field-with-hint .field-hint {
        display: block;
        font-size: 0.82em;
        font-weight: 400;
        color: #5A6B82;
        margin: 2px 0 0 0;
        line-height: 1.35;
      }
      /* links inside a hint match the box text */
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
      /* ---- Collected Variables: repeatable variable blocks ---- */
      .var-block {
        border: 1px solid #DCE7F5;
        border-radius: 18px;
        background: #FBFDFF;
        padding: 22px 22px 8px 22px;
        margin-bottom: 18px;
        position: relative;
      }
      .var-block-head {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        margin-bottom: 14px;
      }
      .var-block-title {
        font-size: 16px;
        font-weight: 750;
        color: #1E3A5F;
      }
      .var-remove-btn.btn {
        border: none;
        background: #FCE8E8;
        color: #B02B2B;
        border-radius: 12px;
        padding: 6px 14px;
        font-size: 13px;
        font-weight: 650;
      }
      .var-remove-btn.btn:hover {
        background: #F7D4D4;
        color: #8F1F1F;
      }
      /* sub-section (preprocessing / transformation) inside a variable block */
      .var-sub {
        border-left: 3px solid #CBE0FF;
        padding-left: 14px;
        margin: 18px 0 14px 6px;
      }
      .var-sub-label {
        display: block;
        font-size: 13px;
        font-weight: 700;
        color: #2B6CB0;
        margin-bottom: 6px;
      }
      .var-sub-hint {
        display: block;
        font-size: 0.8em;
        font-weight: 400;
        color: #5A6B82;
        margin-bottom: 8px;
        line-height: 1.35;
      }
      .add-var-btn.btn {
        border: 1px dashed #A9C7EF;
        background: #F2F8FF;
        color: #2B6CB0;
        border-radius: 14px;
        padding: 12px 20px;
        font-weight: 650;
        width: 100%;
      }
      .add-var-btn.btn:hover {
        background: #E6F1FF;
        color: #1E3A5F;
        border-color: #2B6CB0;
      }
      /* foreknowledge dropdown options: bold headline + grey subtext below */
      .fk-opt {
        padding: 4px 0;
      }
      .fk-head {
        display: block;
        font-weight: 700;
        color: #1F2937;
        line-height: 1.3;
      }
      .fk-sub {
        display: block;
        font-size: 0.85em;
        font-weight: 400;
        color: #8A94A6;
        margin-top: 2px;
        line-height: 1.35;
        white-space: normal;
      }
      /* let the dropdown options wrap and be wide enough to read */
      .selectize-dropdown .option {
        white-space: normal;
      }
      /* highlight dropdown options in light blue (matching title boxes) on hover */
      .selectize-dropdown .selectize-dropdown-content .option.active,
      .selectize-dropdown .selectize-dropdown-content .option:hover,
      .selectize-dropdown [data-selectable].active,
      .selectize-dropdown [data-selectable]:hover {
        background-color: #EDF4FF !important;
        background: #EDF4FF !important;
        color: #1F2937 !important;
      }
      /* non-highlighted options stay white */
      .selectize-dropdown .selectize-dropdown-content .option {
        background-color: #FFFFFF;
        color: #1F2937;
      }
      /* make the open dropdown menu float above everything else */
      .selectize-dropdown {
        z-index: 3000 !important;
        box-shadow: 0 8px 24px rgba(15,23,42,0.15);
        border-radius: 10px;
      }
      /* the card must not clip a dropdown that extends past its bottom edge */
      .card,
      .card-body,
      .bslib-card,
      .tab-content,
      .tab-pane {
        overflow: visible !important;
      }
      /* cap the menu height so long lists scroll inside the menu instead of overflowing */
      .selectize-dropdown-content {
        max-height: 320px !important;
        overflow-y: auto !important;
        overflow-x: hidden !important;
      }
      /* the menu box itself: no sideways scroll, sit clear of the card edge */
      .selectize-dropdown {
        overflow-x: hidden !important;
        box-sizing: border-box !important;
      }
      /* keep the menu anchored to and as wide as the input container */
      .selectize-control {
        position: relative;
      }
      .selectize-control .selectize-dropdown {
        width: 100% !important;
        max-width: 100% !important;
        left: 0 !important;
        box-sizing: border-box !important;
      }
      /* option rows wrap their text instead of overflowing to the right */
      .selectize-dropdown .option,
      .selectize-dropdown .fk-opt,
      .selectize-dropdown .fk-head,
      .selectize-dropdown .fk-sub {
        white-space: normal !important;
        word-break: break-word;
        overflow-wrap: anywhere;
      }
      /* extra room at the bottom of a card so a dropdown near the end isn't cramped */
      .card {
        padding-bottom: 90px;
      }
      
    "))
  ),
  
  # ======================================================================================
  # MAIN CONTENT
  # ======================================================================================
  
  div(
    class = "main-container home-mode", id = "app_root",
    
    # ====================================================================================
    # HERO (clickable — returns to the Home landing view)
    # ====================================================================================
    
    div(
      class = "hero-section", id = "hero", onclick = "showHome()",
      title = "Back to overview",
      
      div(
        class = "hero-title",
        "Preregistration Template for Data Donation Studies"
      ),
      
      div(
        class = "hero-text",
        
        "This template supports researchers in planning and preregistering data donation studies."
      )
    ),
    
    # ====================================================================================
    # HOME / LANDING VIEW (shown first; hidden once the form is opened)
    # ====================================================================================
    
    div(
      class = "home-view",
      
      card(
        
        card_header("Overview"),
        
        # Intro / welcome block.
        p(class = "home-text",
          "This app guides researchers through the important decisions involved in planning and preregistering data donation studies. Its goal is to help you think through and document key methodological choices before data collection. At the same time, we are aware that such studies often involve many decisions that cannot be foreseen at the preregistration stage \u2014 for example, attrition rates that make power calculations infeasible, or platforms changing how they provide data access. The aim is therefore to support researchers, not to restrict them when they simply cannot anticipate every decision in advance."),
        
        # "Existing preregistrations" heading, followed by three collapsible
        # per-study-type sub-sections (native details/summary, no JS).
        div(
          class = "prereg-section",
          div(class = "home-subhead", "Existing preregistrations"),
          p(class = "home-text",
            "Below we list existing data donation preregistrations we are aware of, grouped by study type, as a resource and source of inspiration for your own registration. This list will be updated continuously."),
          local({
            # Helper: build a compact grid of prereg cards from a list of
            # (authors/year, title, url, description) tuples.
            prereg_grid <- function(entries) {
              div(
                class = "prereg",
                lapply(entries, function(p) {
                  div(
                    class = "prereg-item",
                    div(class = "prereg-title",
                      tags$span(class = "prereg-year", paste0(p[[1]], ": ")),
                      tags$a(href = p[[3]], target = "_blank", rel = "noopener", p[[2]])
                    ),
                    div(class = "prereg-desc", p[[4]])
                  )
                })
              )
            }
            # Helper: a nested collapsible sub-section for one study type.
            prereg_group <- function(label, entries) {
              tags$details(
                class = "prereg-subdetails",
                tags$summary(class = "prereg-subsummary", label),
                prereg_grid(entries)
              )
            }

            survey <- list(
              list("Pouwels et al. (2026)", "Digital Reflections of Social Experiences: A Snapchat Data Donation Study",
                   "https://osf.io/nxvcu/overview",
                   "Data donation from Snapchat plus survey / mobile experience sampling to understand adolescents' online social interactions."),
              list("Wirz et al. (2026)", "TikTok and Boredom",
                   "https://osf.io/w2exr/overview",
                   "Data donation of TikTok data to understand the relationship between platform use and boredom."),
              list("Quin et al. (2024)", "Mapping the Collective Wisdom of Online Rare Disease Communities",
                   "https://osf.io/mn9px/overview",
                   "Data donation of Facebook data plus survey on how caregivers of those affected by rare diseases use social media for information exchange and community support."),
              list("Quin et al. (2024)", "Social Perceptions Going Online: Social Media Food Content and Food Norms",
                   "https://osf.io/pnh2j/overview?view_only=c7802a2b0fa34deebf480dced1acf80e",
                   "Data donation of YouTube data plus survey on how platform food content affects perceived food norms."),
              list("Wald et al. (2024)", "The Google Family Home",
                   "https://osf.io/b5jwz/overview",
                   "Data donation of Google smart-speaker history plus survey on how families use these tools, including reported vs. observed use."),
              list("Quin et al. (2023)", "Mapping the Digital Food Environment",
                   "https://osf.io/fn39s/overview",
                   "Data donation of YouTube data plus survey on how platform behavior relates to food."),
              list("Flanagan & Brewer (2019)", "Cancer Loyalty Card Study (CLOCS)",
                   "https://www.isrctn.com/ISRCTN14897082",
                   "Data donation of high-street retailer loyalty-card data to predict changes in purchase behavior of ovarian cancer patients prior to diagnosis.")
            )
            experiment <- list(
              list("Stevkovics & Kmetty (2026)", "TikTok Data Donation Experiment",
                   "https://osf.io/7p8nu/overview",
                   "Data donation of TikTok data plus experiment on how AI-assisted support affects participation and burden."),
              list("Rodewald et al. (2026)", "Frame and Motivate",
                   "https://osf.io/vh9s3/overview?view_only=e99ac0c6f81c4f73b97ba741b57a275f",
                   "Data donation of Instagram, LinkedIn, and YouTube data plus experiment on how study framing and appeals affect participation."),
              list("Szafran et al. (2026)", "Instagram Abstinence and Body Image",
                   "https://osf.io/67hqf/overview",
                   "Data donation of Instagram data plus experiment on how abstaining from the platform affects body image perceptions."),
              list("Manzke & Hartl (2025)", "One Owl, Two Requests",
                   "https://osf.io/wp54d/overview",
                   "Data donation of Duolingo learning-analytics data plus experiment on how different request methods (providing data vs. usernames) affect participation."),
              list("Schmidbauer et al. (2025)", "Persuasive Messages for Data Donation",
                   "https://osf.io/abpnw/overview?view_only=d38f43e4cbd3403bb9c7144ce49b45e0",
                   "Vignette experiment varying the messages accompanying data donation requests to understand their effect on participation."),
              list("Hase & Haim (2024)", "Can We Get Rid of Bias? Mitigating Systematic Error through Survey Design",
                   "https://osf.io/vfazc/overview",
                   "Data donation of Instagram, Twitter, and YouTube data plus experiment on how support during donation, personalized incentives, and framing affect participation."),
              list("Silber et al. (2022)", "Willingness to Share Digital Trace Data",
                   "https://osf.io/dz8k6/overview",
                   "Vignette experiment varying features of data donation requests (e.g., data type, sharing method) to understand their effect on participation.")
            )
            interviews <- list(
              list("Marschlich (2023)", "User Engagement with Organizational Posts",
                   "https://osf.io/d5hsm/overview",
                   "Data donation of Twitter, Instagram, and Facebook data plus qualitative interviews on engagement with organizational posts.")
            )

            tagList(
              prereg_group("Data donation + survey", survey),
              prereg_group("Data donation + experiment", experiment),
              prereg_group("Data donation + qualitative interviews", interviews)
            )
          })
        ),
        
        tags$hr(class = "home-divider"),
        
        div(
          class = "home-start-row",
          tags$button(class = "home-start-btn", onclick = "showForm()",
                      "Start preregistration \u2192")
        )
      )
    ),
    
    # Toggle between the Home landing view and the tabbed form.
    tags$script(HTML(
      "function showForm(){var r=document.getElementById('app_root');if(r){r.classList.remove('home-mode');window.scrollTo(0,0);}}
       function showHome(){var r=document.getElementById('app_root');if(r){r.classList.add('home-mode');window.scrollTo(0,0);}}"
    )),
    
    # ====================================================================================
    # NAVIGATION TABS (the form)
    # ====================================================================================
    
    div(
      class = "form-view",
      
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
            dateInput("date", label = NULL, value = Sys.Date())),
          
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
            textInput("ethics", label = NULL)),
          
          field("6. Status of Study",
            tagList(
              "Preregistration distinguishes analyses planned before observing the data from those conducted after. ",
              "For pre-existing data, foreknowledge can introduce unintended influences on the analysis and conclusions. ",
              "Choose the situation that best describes your foreknowledge of the data and evidence for this study and analysis plan."
            ),
            selectizeInput(
              "foreknowledge",
              label = NULL,
              choices = c(
                "Please select..." = "",
                "Data does not yet exist. ||| No part of the data that will be used for this analysis plan exists, and no part will be generated until after this plan is registered." = "Data does not yet exist. No part of the data that will be used for this analysis plan exists, and no part will be generated until after this plan is registered.",
                "Data exists but the authors cannot observe it yet. ||| At least some of the data that will be used for this analysis plan exists but is inaccessible to the authors and will remain so until after this plan is registered." = "Data exists but the authors cannot observe it yet. At least some of the data that will be used for this analysis plan exists but is inaccessible to the authors and will remain so until after this plan is registered.",
                "Data exists but the authors have not observed it yet. ||| At least some of the data that will be used for this analysis plan exists and is possible for the authors to access. However, the authors certify that they have not accessed any of that data and will not do so until after this plan is registered." = "Data exists but the authors have not observed it yet. At least some of the data that will be used for this analysis plan exists and is possible for the authors to access. However, the authors certify that they have not accessed any of that data and will not do so until after this plan is registered.",
                "Only people other than the authors have observed the data. ||| At least some of the data that will be used for this analysis plan has been accessed by people other than the authors. However, the authors certify that they have not observed any of that data and will not do so until after this plan is registered." = "Only people other than the authors have observed the data. At least some of the data that will be used for this analysis plan has been accessed by people other than the authors. However, the authors certify that they have not observed any of that data and will not do so until after this plan is registered.",
                "Authors' limited observation of the data could not influence their analysis decisions. ||| At least some of the data that will be used for this analysis plan has been accessed and observed by the authors. However, the authors certify that they have not sufficiently observed relevant evidence to influence their analysis decisions for this analysis plan and will not do so until after this plan is registered." = "Authors' limited observation of the data could not influence their analysis decisions. At least some of the data that will be used for this analysis plan has been accessed and observed by the authors. However, the authors certify that they have not sufficiently observed relevant evidence to influence their analysis decisions for this analysis plan and will not do so until after this plan is registered.",
                "Authors have observed the data, but have not performed the proposed analyses. ||| At least some of the data that will be used for this analysis plan has been accessed and observed by the authors. The authors have sufficiently observed relevant evidence to influence their analysis decisions or conclusions. However, the authors have not yet performed any of the proposed analyses in this plan and will not do so until after this plan is registered." = "Authors have observed the data, but have not performed the proposed analyses. At least some of the data that will be used for this analysis plan has been accessed and observed by the authors. The authors have sufficiently observed relevant evidence to influence their analysis decisions or conclusions. However, the authors have not yet performed any of the proposed analyses in this plan and will not do so until after this plan is registered.",
                "Authors have observed the data. ||| The authors cannot certify meeting any of the levels above given prior access and observation of the data relevant to this analysis plan." = "Authors have observed the data. The authors cannot certify meeting any of the levels above given prior access and observation of the data relevant to this analysis plan.",
                "Analyses in this plan have been conducted already. ||| At least some of the analyses described in this analysis plan have been conducted by the authors making this a retrospective registration." = "Analyses in this plan have been conducted already. At least some of the analyses described in this analysis plan have been conducted by the authors making this a retrospective registration."
              ),
              options = list(
                render = I("{
                  option: function(item, escape) {
                    var p = item.label.split(' ||| ');
                    var sub = p.length > 1 ? p[1] : '';
                    return '<div class=\"fk-opt\">' +
                      '<span class=\"fk-head\">' + escape(p[0]) + '</span>' +
                      '<span class=\"fk-sub\">' + escape(sub) + '</span></div>';
                  },
                  item: function(item, escape) {
                    var p = item.label.split(' ||| ');
                    return '<div><span class=\"fk-head\">' + escape(p[0]) + '</span></div>';
                  }
                }")
              )
            )),
          
          conditionalPanel(
            condition = "input.foreknowledge != '' && input.foreknowledge.indexOf('Data does not yet exist.') !== 0",
            field("7. Explanation of Data Foreknowledge",
              tagList(
                "Only applicable if data for this study already exists: ",
                "report actions taken to reduce the risk of unintended influences on the analysis plan and conclusions."
              ),
              textAreaInput("foreknowledge_explanation", label = NULL))
          )
        )
      ),
      
      # ==================================================================================
      # STUDY OVERVIEW
      # ==================================================================================
      
      nav_panel(
        "Research Questions & Hypotheses",
        
        br(),
        
        card(
          
          card_header("Research Questions & Hypotheses"),
          
          field("1. Background & Rationale",
            "Add a brief overview on the main goals of the study",
            textAreaInput("background", label = NULL)),
          
          div(
            class = "field-with-hint",
            div(
              class = "field-box",
              tags$span(class = "field-title", "2. Research Questions or Hypotheses"),
              tags$span(class = "field-hint",
                "What research questions or hypotheses are you planning to evaluate? Add each one separately. You will be able to define an analysis plan for each of them in the Analysis Plan tab.")
            ),
            uiOutput("rqs_ui"),
            actionButton("add_rq", "+ Add new research question / hypothesis", class = "add-var-btn")
          ),
          
        )
      ),
      
      # ==================================================================================
      # GENERAL STUDY DESIGN
      # ==================================================================================
      
      nav_panel(
        "General Study Design",
        
        br(),
        
        card(
          
          card_header("General Study Design"),
          
          field("1. Study Type",
            "What type of data donation study are you planning to run? This includes the data donation as well as its potential integration in other frameworks, like surveys, experiments, or interviews.",
            selectizeInput(
              "study_type_general",
              label = NULL,
              choices = c(
                "Please select..." = "",
                "Data Donation ||| A standalone data donation study in which participants share exported platform data." = "Data Donation",
                "Data Donation & Survey ||| Data donation combined with a survey to collect self-reported measures alongside the donated data." = "Data Donation & Survey",
                "Data Donation & Experiment ||| Data donation combined with a (survey) experiment." = "Data Donation & Experiment",
                "Data Donation & Qualitative Interviews ||| Data donation combined with qualitative interviews with donors." = "Data Donation & Qualitative Interviews",
                "Other ||| A different design or combination of methods; describe it in the following sections." = "Other"
              ),
              options = list(
                render = I("{
                  option: function(item, escape) {
                    var p = item.label.split(' ||| ');
                    var sub = p.length > 1 ? p[1] : '';
                    return '<div class=\"fk-opt\">' +
                      '<span class=\"fk-head\">' + escape(p[0]) + '</span>' +
                      '<span class=\"fk-sub\">' + escape(sub) + '</span></div>';
                  },
                  item: function(item, escape) {
                    var p = item.label.split(' ||| ');
                    return '<div><span class=\"fk-head\">' + escape(p[0]) + '</span></div>';
                  }
                }")
              )
            )),
          
          # Everything after Study Type is rendered reactively so the field
          # numbers stay sequential across study types (Description appears only
          # for "Other"; Experimental Conditions and Blinding only for the
          # experiment path). See server: output$randomization_field.
          uiOutput("randomization_field"),
          
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
          
          field("1. Participants - Target Population & Sample",
            "Describe the population you aim to study and the sample you may draw. This includes information on how you will recruit participants, potential sampling strategies (including quota or screen-out criteria for sociodemographics or platforms for donation), or incentives.",
            textAreaInput("population", label = NULL)),
          
          field("2. Participants - Sample Size",
            "Describe how many participants you aim to reach and the rationale for this. Importantly, explain targeted sample sizes across different stages of the study (e.g., participation in the survey vs. the actual data donation) and when you will stop recruitment. Include descriptions of power analyses, if possible and adequate.",
            textAreaInput("sample_size", label = NULL)),
          
          field("3. Platforms/Devices - Target Population & Sample",
            "Describe what type of data participants are asked to donate (e.g., specific platforms, specific chat logs, specific device logs). This includes information on what type of data participants are asked to donate, whether they can choose between different options (e.g., different platforms), and whether participants can donate data from several platforms/devices. Note that specifications on variables extracted from the data donations are described elsewhere.",
            textAreaInput("platform_population", label = NULL)),
          
          field("4. Platforms/Devices - Sample Size",
            "Describe how many data donation submissions (e.g., data donation packages, other data points) you aim to reach in total as well as per participant and the rationale for this.",
            textAreaInput("platform_sample_size", label = NULL)),
          
          field("5. Observation Period",
            "Explain when you plan to start the data collection and how long you plan to be in the field.",
            textAreaInput("observation_period", label = NULL)),
          
          field("6. Reflections on Errors in Representation (Platforms, Participants, Drop-Out)",
            tagList(
              "Here, we invite you to reflect on errors in representations your study may carry (for further information, see ",
              HTML(paste0(
                as.character(tags$a(
                  href = "https://doi.org/10.5117/CCR2022.2.002.BOES",
                  target = "_blank",
                  rel = "noopener",
                  "this article"
                )),
                ")."
              )),
              " Do you expect coverage bias in who is prevalent on specific platforms/uses specific devices? Do you expect non-participation bias in who is willing to or actually sharing their data or not? Reflect on potential challenges you may expect and mitigation strategies, if possible."
            ),
            textAreaInput("representation_errors", label = NULL))
        )
      ),
      
      # ==================================================================================
      # MEASUREMENTS: DATA DONATION
      # ==================================================================================
      
      nav_panel(
        "Measures: Data Donation",
        
        br(),
        
        card(
          
          card_header("Measures: Data Donation"),
          
          field("1. Raw Data Structure",
            tagList(
              "Describe the raw data structure of data donations before preprocessing \u2014 that is, how the data will look like immediately after being downloaded or otherwise generated by the participants. What data type will it be and what folder structure will it have? If possible, upload a PDF of the current data documentation by platforms (e.g., see LinkedIn example ",
              HTML(paste0(
                as.character(tags$a(
                  href = "https://www.linkedin.com/help/linkedin/answer/a1339364/?lang=en-US",
                  target = "_blank", "here"
                )),
                ")."
              ))
            ),
            textAreaInput("raw_data_donation", label = NULL)),
          
          field("Upload additional material",
            "Upload a single PDF of the current data documentation by platforms, if available.",
            fileInput("data_documentation_file", label = NULL, accept = ".pdf",
                      buttonLabel = "Browse...", placeholder = "No file selected")),
          
          div(
            class = "field-with-hint",
            div(
              class = "field-box",
              tags$span(class = "field-title", "2. Collected Variables"),
              tags$span(class = "field-hint",
                "Add each variable you will collect, giving it a clear name. For every variable, describe how it will be preprocessed (filtering, aggregation, annotation) and how it will be transformed (e.g., re-coding, indexing). Use \"Add new variable\" to add as many as you need.")
            ),
            uiOutput("collected_variables_ui"),
            actionButton("add_variable", "+ Add new variable", class = "add-var-btn")
          ),
          
          field("3. Any Other Variables Derived from the Data Donations",
            "Describe any other variables you will derive from the data donation (e.g., if you combine several collected variables to an index you would rather describe here, etc.).",
            textAreaInput("new_variables", label = NULL)),
          
          field("4. Quality Control Procedures",
            "Describe any quality-control procedures you may implement for the data donation (e.g., handling of missing data or empty files, duplicate data donations, large file sizes).",
            textAreaInput("quality_control", label = NULL)),
          
          field("5. Reflections on Measurement Errors (Missing Data, Preprocessing, Transformation)",
            tagList(
              "Here, we invite you to reflect on measurement errors your study may carry (for further information, see ",
              tags$a(href = "https://doi.org/10.5117/CCR2022.2.002.BOES", target = "_blank", rel = "noopener", "this article"),
              " and ",
              HTML(paste0(
                as.character(tags$a(
                  href = "https://doi.org/10.14763/2024.3.1793",
                  target = "_blank", rel = "noopener", "this article"
                )),
                ")."
              )),
              " Do you expect platforms (or users) to not provide specific data points? Could errors arise from preprocessing or transformation? Reflect on potential challenges you may expect and mitigation strategies, if possible."
            ),
            textAreaInput("measurement_error_reflections", label = NULL)),
          
          field("6. Other Information",
            "Add any other information related to the data donation that you would like to document here.",
            textAreaInput("other_information", label = NULL)),
          
          field("Upload additional material",
            "Upload a single PDF with any additional material related to the data donation, if necessary.",
            fileInput("other_information_file", label = NULL, accept = ".pdf",
                      buttonLabel = "Browse...", placeholder = "No file selected"))
        )
      ),
      
      # ==================================================================================
      # MEASUREMENTS: ADDITIONAL DATA COLLECTION
      # ==================================================================================
      
      nav_panel(
        "Measures: Other Data",
        
        br(),
        
        card(
          
          card_header("Measures: Other Data"),
          
          div(
            class = "field-with-hint",
            div(
              class = "field-box",
              tags$span(class = "field-title", "1. Variables Collected from Other Quantitative Data Collection"),
              tags$span(class = "field-hint",
                "Add each variable you will collect using a different quantitative method (e.g., survey). You can add additional variables one by one. Simply leave blank if none are collected.")
            ),
            uiOutput("qvars_ui"),
            actionButton("add_qvar", "+ Add new variable", class = "add-var-btn")
          ),
          
          field("2. Variables Collected from Other Qualitative Data Collection",
            "Add additional data or variables you will collect using a different qualitative method (e.g., interviews). Descriptions can include, for example, interview guides and approaches for extracting themes from these. Simply leave blank if this does not apply to your study.",
            textAreaInput("qualitative_variables", label = NULL)),
          
          field("3. Other Information",
            "Add any other information related to the additional data collection that you would like to document here. Examples include survey items or interview guidelines.",
            textAreaInput("additional_other_information", label = NULL)),
          
          field("Upload additional material",
            "Upload a single PDF with any additional material related to the additional data collection, if necessary.",
            fileInput("additional_other_information_file", label = NULL, accept = ".pdf",
                      buttonLabel = "Browse...", placeholder = "No file selected"))
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
          
          field("1. Data Cleaning",
            "Describe how you will clean data. This includes removing observations (e.g., participants failing data quality checks or with missing data, outliers) and data points for observations (e.g., empty data donations, duplicate data donations).",
            textAreaInput("data_cleaning", label = NULL)),
          
          div(
            class = "field-with-hint",
            div(
              class = "field-box",
              tags$span(class = "field-title", "2. Analysis"),
              tags$span(class = "field-hint",
                "Describe the analysis plan for each hypothesis or research question added in the Research Questions & Hypotheses tab. Make sure to define which (sub-)sample the analysis will be conducted with (e.g., all participants, only subset of participants sharing data). If applicable, define the type of model you will use (e.g., ANOVA, regression, etc.) and model specifications (e.g., dependent variables, independent variables, controls, etc.). The section can also include planned robustness tests.")
            ),
            uiOutput("rq_analysis_ui")
          ),
          
          field("3. Inference Criteria",
            "If applicable, describe the criteria used to make inferences (e.g., p-values, Bayesian approach) and cut-off values.",
            textAreaInput("inference_criteria", label = NULL))
        )
      ),
      
      # ==================================================================================
      # REFERENCES
      # ==================================================================================
      
      nav_panel(
        "References",
        
        br(),
        
        card(
          
          card_header("References"),
          
          field("1. References",
            "Please copy in a list of references used in this preregistration.",
            textAreaInput("references", label = NULL)),
          
          field("2. Any Additional Information",
            "Here, you can upload any type of additional material accompanying your preregistration.",
            fileInput("additional_material_file", label = NULL, accept = ".pdf",
                      buttonLabel = "Browse...", placeholder = "No file selected"))
        )
      )
    ),
    
    br(),
    
    div(
      style = "display:flex; gap:12px; justify-content:right;",
      
      downloadButton(
        "download_pdf",
        "Download Word document"
      )
    )
    )
  )
)

# ==================================================================================
# SERVER
# ==================================================================================

server <- function(input, output, session) {

  # ---- Collected Variables: repeatable variable blocks ----
  # Each block has a stable integer id used to build unique input ids
  # (var_name_<id>, var_preprocess_<id>, var_transform_<id>). Ids are never
  # reused, so removing a block and adding another can't collide with stale
  # values. var_ids holds the ids currently displayed, in order.
  var_ids <- reactiveVal(1L)          # start with one block
  var_counter <- reactiveVal(1L)      # highest id issued so far
  var_removers <- reactiveVal(integer(0))  # ids that already have a remove-observer

  # Preserve a typed value across re-renders (returns "" if never set).
  keep_var <- function(id) {
    v <- isolate(input[[id]])
    if (is.null(v)) "" else v
  }

  output$collected_variables_ui <- renderUI({
    ids <- var_ids()
    blocks <- lapply(seq_along(ids), function(k) {
      id <- ids[[k]]
      name_id       <- paste0("var_name_", id)
      preprocess_id <- paste0("var_preprocess_", id)
      transform_id  <- paste0("var_transform_", id)
      remove_id     <- paste0("remove_var_", id)

      div(
        class = "var-block",
        div(
          class = "var-block-head",
          tags$span(class = "var-block-title", paste0("Variable ", k)),
          # Only offer removal when more than one block exists.
          if (length(ids) > 1) {
            actionButton(remove_id, "Remove", class = "var-remove-btn")
          }
        ),
        textInput(name_id, label = "Variable name",
                  value = keep_var(name_id),
                  placeholder = "e.g., daily_posting_frequency"),
        div(
          class = "var-sub",
          tags$span(class = "var-sub-label", "Preprocessing"),
          tags$span(class = "var-sub-hint",
            "How will this variable be preprocessed before researchers can access it? (e.g., local extraction, filtering of observations, aggregation, annotation by participants)"),
          textAreaInput(preprocess_id, label = NULL, value = keep_var(preprocess_id))
        ),
        div(
          class = "var-sub",
          tags$span(class = "var-sub-label", "Transformation"),
          tags$span(class = "var-sub-hint",
            "How will this variable be transformed for analysis after researchers can access it? (e.g., re-coding, aggregation, index-building)"),
          textAreaInput(transform_id, label = NULL, value = keep_var(transform_id))
        )
      )
    })
    do.call(tagList, blocks)
  })

  # Add a new block: issue the next id and append it.
  observeEvent(input$add_variable, {
    new_id <- var_counter() + 1L
    var_counter(new_id)
    var_ids(c(var_ids(), new_id))
  })

  # Register remove-observers lazily for any id that doesn't have one yet.
  # Each observer drops its own id from var_ids when the matching button is clicked.
  observe({
    ids <- var_ids()
    have <- var_removers()
    todo <- setdiff(ids, have)
    for (id in todo) {
      local({
        this_id <- id
        observeEvent(input[[paste0("remove_var_", this_id)]], {
          var_ids(setdiff(var_ids(), this_id))
        }, ignoreInit = TRUE)
      })
    }
    if (length(todo)) var_removers(union(have, todo))
  })

  # ---- Additional Data Collection: repeatable quantitative-variable blocks ----
  # Same machinery as Collected Variables, with a distinct "qvar" prefix so the
  # two sets never collide. Sub-sections are Operationalization and Transformation.
  qvar_ids <- reactiveVal(1L)
  qvar_counter <- reactiveVal(1L)
  qvar_removers <- reactiveVal(integer(0))

  output$qvars_ui <- renderUI({
    ids <- qvar_ids()
    blocks <- lapply(seq_along(ids), function(k) {
      id <- ids[[k]]
      name_id       <- paste0("qvar_name_", id)
      operation_id  <- paste0("qvar_operation_", id)
      transform_id  <- paste0("qvar_transform_", id)
      remove_id     <- paste0("remove_qvar_", id)

      div(
        class = "var-block",
        div(
          class = "var-block-head",
          tags$span(class = "var-block-title", paste0("Variable ", k)),
          if (length(ids) > 1) {
            actionButton(remove_id, "Remove", class = "var-remove-btn")
          }
        ),
        textInput(name_id, label = "Variable name",
                  value = keep_var(name_id),
                  placeholder = "e.g., political_interest"),
        div(
          class = "var-sub",
          tags$span(class = "var-sub-label", "Operationalization"),
          tags$span(class = "var-sub-hint",
            "How will this variable be operationalized? (e.g., survey items)"),
          textAreaInput(operation_id, label = NULL, value = keep_var(operation_id))
        ),
        div(
          class = "var-sub",
          tags$span(class = "var-sub-label", "Transformation"),
          tags$span(class = "var-sub-hint",
            "How will this variable be transformed for analysis after researchers can access it? (e.g., re-coding, aggregation, index-building)"),
          textAreaInput(transform_id, label = NULL, value = keep_var(transform_id))
        )
      )
    })
    do.call(tagList, blocks)
  })

  observeEvent(input$add_qvar, {
    new_id <- qvar_counter() + 1L
    qvar_counter(new_id)
    qvar_ids(c(qvar_ids(), new_id))
  })

  observe({
    ids <- qvar_ids()
    have <- qvar_removers()
    todo <- setdiff(ids, have)
    for (id in todo) {
      local({
        this_id <- id
        observeEvent(input[[paste0("remove_qvar_", this_id)]], {
          qvar_ids(setdiff(qvar_ids(), this_id))
        }, ignoreInit = TRUE)
      })
    }
    if (length(todo)) qvar_removers(union(have, todo))
  })

  # ---- Research Questions / Hypotheses: repeatable blocks ----
  # Each RQ has a stable id used for rq_text_<id> (the RQ text, in the Research
  # Questions tab) and rqplan_<id> (its analysis plan, in the Analysis Plan tab).
  # Both tabs read the same rq_ids(), so adding/removing an RQ keeps the two in
  # sync automatically.
  rq_ids <- reactiveVal(1L)
  rq_counter <- reactiveVal(1L)
  rq_removers <- reactiveVal(integer(0))

  output$rqs_ui <- renderUI({
    ids <- rq_ids()
    blocks <- lapply(seq_along(ids), function(k) {
      id <- ids[[k]]
      text_id   <- paste0("rq_text_", id)
      remove_id <- paste0("remove_rq_", id)
      div(
        class = "var-block",
        div(
          class = "var-block-head",
          tags$span(class = "var-block-title", paste0("RQ / Hypothesis ", k)),
          if (length(ids) > 1) {
            actionButton(remove_id, "Remove", class = "var-remove-btn")
          }
        ),
        textAreaInput(text_id, label = NULL, value = keep_var(text_id),
                      placeholder = "State the research question or hypothesis.")
      )
    })
    do.call(tagList, blocks)
  })

  observeEvent(input$add_rq, {
    new_id <- rq_counter() + 1L
    rq_counter(new_id)
    rq_ids(c(rq_ids(), new_id))
  })

  observe({
    ids <- rq_ids()
    have <- rq_removers()
    todo <- setdiff(ids, have)
    for (id in todo) {
      local({
        this_id <- id
        observeEvent(input[[paste0("remove_rq_", this_id)]], {
          rq_ids(setdiff(rq_ids(), this_id))
        }, ignoreInit = TRUE)
      })
    }
    if (length(todo)) rq_removers(union(have, todo))
  })

  # Per-RQ analysis plan blocks in the Analysis Plan tab. One block per RQ,
  # showing that RQ's text (live) plus an analysis-plan text area (rqplan_<id>).
  output$rq_analysis_ui <- renderUI({
    ids <- rq_ids()
    blocks <- lapply(seq_along(ids), function(k) {
      id <- ids[[k]]
      rq_text <- input[[paste0("rq_text_", id)]]
      rq_text <- if (is.null(rq_text) || trimws(rq_text) == "") {
        "(no text entered yet — add it in the Research Questions & Hypotheses tab)"
      } else {
        rq_text
      }
      plan_id <- paste0("rqplan_", id)
      div(
        class = "var-block",
        div(
          class = "var-block-head",
          tags$span(class = "var-block-title", paste0("RQ / Hypothesis ", k))
        ),
        div(
          class = "var-sub",
          tags$span(class = "var-sub-label", "Research question / hypothesis"),
          tags$span(class = "var-sub-hint", rq_text)
        ),
        tags$span(class = "var-sub-label", "Analysis plan"),
        textAreaInput(plan_id, label = NULL, value = keep_var(plan_id))
      )
    })
    do.call(tagList, blocks)
  })

  # All fields after "1. Study Type" are rendered here so their numbers stay
  # sequential across study types: "Description of Study Type" appears only for
  # "Other"; "Experimental Conditions" and "Blinding" only for the experiment
  # path. A running counter assigns the numbers. Typed values are preserved
  # across renumbering.
  output$randomization_field <- renderUI({
    st <- input$study_type_general
    is_other <- !is.null(st) && st == "Other"
    is_experiment <- !is.null(st) && st == "Data Donation & Experiment"

    keep <- function(id) {
      v <- isolate(input[[id]])
      if (is.null(v)) "" else v
    }

    # Sequential field counter (Study Type is 1, rendered statically above).
    i <- 1L
    nxt <- function() {
      i <<- i + 1L
      i
    }

    items <- list()

    # Description of Study Type — only for "Other".
    if (is_other) {
      items <- c(items, list(
        field(
          paste0(nxt(), ". Description of Study Type"),
          "Briefly describe the design or combination of methods you are planning to use.",
          textAreaInput("study_type_other", label = NULL, value = keep("study_type_other"))
        )
      ))
    }

    # Researcher-Side Workflow & Tool for Data Donation — always.
    items <- c(items, list(
      field(
        paste0(nxt(), ". Researcher-Side Workflow & Tool for Data Donation"),
        tagList(
          "Specify how the data collection will look like from the side of the research team. How and when is the data donation integrated in other methods, if at all? Which tool(s) will you use to collect the data (for data donation and/or other methods)? Note that specifications on sampling (e.g., platforms) and operationalization (e.g., data extraction) are noted elsewhere. See ",
          tags$a(href = "https://doi.org/10.1007/s11135-024-01983-x", target = "_blank", "this primer"),
          " for more information."
        ),
        textAreaInput("researcher_workflow", label = NULL, value = keep("researcher_workflow"))
      )
    ))

    # User-Side Workflow & Data Collection Procedure — always, with upload.
    items <- c(items, list(
      field(
        paste0(nxt(), ". User-Side Workflow & Data Collection Procedure"),
        "Specify how the data collection will look like from the side of the participants. How will they be able to request or otherwise collect their data for data donation? How will they share their data (for data donation and/or other methods) with researchers? For several methods (e.g., survey and data donation) which data will they share when? If possible, upload an example of researcher instructions on how participants can get their data or provide a link to where those are stored.",
        textAreaInput("user_workflow", label = NULL, value = keep("user_workflow"))
      ),
      field(
        "Upload additional material",
        "Upload a single PDF containing example participant instructions, if necessary.",
        fileInput("user_instructions_file", label = NULL, accept = ".pdf",
                  buttonLabel = "Browse...", placeholder = "No file selected")
      )
    ))

    # Experimental Conditions (with upload) and Blinding — only for the experiment path.
    if (is_experiment) {
      items <- c(items, list(
        field(
          paste0(nxt(), ". Experimental Conditions"),
          "Describe experimental conditions: what are experimental factors and levels for each factor? If possible, upload an example of respective stimuli or provide a link to where those are stored.",
          textAreaInput("experimental_conditions", label = NULL, value = keep("experimental_conditions"))
        ),
        field(
          "Upload additional material",
          "Upload a single PDF containing example stimuli, if necessary",
          fileInput("stimuli_file", label = NULL, accept = ".pdf",
                    buttonLabel = "Browse...", placeholder = "No file selected")
        ),
        field(
          paste0(nxt(), ". Blinding"),
          "Explain who is aware of the experimental conditions. This can include participants or researchers.",
          textAreaInput("blinding_experiment", label = NULL, value = keep("blinding_experiment"))
        )
      ))
    }

    # Randomization — always.
    items <- c(items, list(
      field(
        paste0(nxt(), ". Randomization"),
        "If you include any form of randomization (e.g., into experimental conditions, for types of data donation participants are asked about), describe how you will randomize. Add \"NA\" if no randomization is used.",
        textAreaInput("randomization_general", label = NULL, value = keep("randomization_general"))
      )
    ))

    # Informed Consent — always.
    items <- c(items, list(
      field(
        paste0(nxt(), ". Informed Consent"),
        "Explain how participants are informed about what this study entails and asked about their informed consent. This can include different steps (e.g., at the beginning of the study, when inquiring users about their willingness to donate data, before actual data transmission).",
        textAreaInput("informed_consent", label = NULL, value = keep("informed_consent"))
      )
    ))

    do.call(tagList, items)
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
      
      # dateInput returns a Date object; Quarto params expect a plain string.
      if (!is.null(params$date)) {
        params$date <- as.character(params$date)
      } else {
        params$date <- ""
      }
      
      # fileInput returns a data frame (name/size/type/datapath), which Quarto
      # params can't accept. Replace it with just the uploaded file's name (a string),
      # or "" if nothing was uploaded.
      if (is.data.frame(params$stimuli_file)) {
        params$stimuli_file <- params$stimuli_file$name
      } else {
        params$stimuli_file <- ""
      }
      if (is.data.frame(params$user_instructions_file)) {
        params$user_instructions_file <- params$user_instructions_file$name
      } else {
        params$user_instructions_file <- ""
      }
      if (is.data.frame(params$data_documentation_file)) {
        params$data_documentation_file <- params$data_documentation_file$name
      } else {
        params$data_documentation_file <- ""
      }
      if (is.data.frame(params$other_information_file)) {
        params$other_information_file <- params$other_information_file$name
      } else {
        params$other_information_file <- ""
      }
      if (is.data.frame(params$additional_other_information_file)) {
        params$additional_other_information_file <- params$additional_other_information_file$name
      } else {
        params$additional_other_information_file <- ""
      }
      if (is.data.frame(params$additional_material_file)) {
        params$additional_material_file <- params$additional_material_file$name
      } else {
        params$additional_material_file <- ""
      }

      # Collected Variables: assemble the repeatable blocks into a single
      # formatted string passed as `collected_variables`, then drop the raw
      # per-block inputs and the add/remove buttons from the params list.
      var_val <- function(id) {
        v <- params[[id]]
        if (is.null(v)) "" else trimws(v)
      }
      var_chunks <- lapply(var_ids(), function(id) {
        nm <- var_val(paste0("var_name_", id))
        pp <- var_val(paste0("var_preprocess_", id))
        tr <- var_val(paste0("var_transform_", id))
        if (nm == "" && pp == "" && tr == "") return(NULL)
        paste0(
          "Variable: ", if (nm == "") "(unnamed)" else nm, "\n",
          "Preprocessing: ", pp, "\n",
          "Transformation: ", tr
        )
      })
      var_chunks <- Filter(Negate(is.null), var_chunks)
      params$collected_variables <- if (length(var_chunks)) {
        paste(unlist(var_chunks), collapse = "\n\n")
      } else {
        ""
      }
      # Remove the raw per-block inputs and the add/remove controls.
      raw_var_keys <- grep("^(var_name_|var_preprocess_|var_transform_|remove_var_)",
                           names(params), value = TRUE)
      params[c(raw_var_keys, "add_variable")] <- NULL

      # Additional Data Collection quantitative variables: same treatment,
      # assembled into `quantitative_variables_collected`.
      qvar_chunks <- lapply(qvar_ids(), function(id) {
        nm <- var_val(paste0("qvar_name_", id))
        op <- var_val(paste0("qvar_operation_", id))
        tr <- var_val(paste0("qvar_transform_", id))
        if (nm == "" && op == "" && tr == "") return(NULL)
        paste0(
          "Variable: ", if (nm == "") "(unnamed)" else nm, "\n",
          "Operationalization: ", op, "\n",
          "Transformation: ", tr
        )
      })
      qvar_chunks <- Filter(Negate(is.null), qvar_chunks)
      params$quantitative_variables_collected <- if (length(qvar_chunks)) {
        paste(unlist(qvar_chunks), collapse = "\n\n")
      } else {
        ""
      }
      raw_qvar_keys <- grep("^(qvar_name_|qvar_operation_|qvar_transform_|remove_qvar_)",
                            names(params), value = TRUE)
      params[c(raw_qvar_keys, "add_qvar")] <- NULL

      # Research Questions / Hypotheses and their per-RQ analysis plans.
      # `research_questions` lists the RQs; `analysis_plan` pairs each RQ with
      # its plan. Both follow the RQ ids currently displayed.
      rq_list <- lapply(seq_along(rq_ids()), function(k) {
        id <- rq_ids()[[k]]
        txt <- var_val(paste0("rq_text_", id))
        if (txt == "") return(NULL)
        paste0("RQ / Hypothesis ", k, ": ", txt)
      })
      rq_list <- Filter(Negate(is.null), rq_list)
      params$research_questions <- if (length(rq_list)) {
        paste(unlist(rq_list), collapse = "\n\n")
      } else {
        ""
      }
      rq_plan_chunks <- lapply(seq_along(rq_ids()), function(k) {
        id <- rq_ids()[[k]]
        txt <- var_val(paste0("rq_text_", id))
        pl  <- var_val(paste0("rqplan_", id))
        if (txt == "" && pl == "") return(NULL)
        paste0(
          "RQ / Hypothesis ", k, ": ", if (txt == "") "(unnamed)" else txt, "\n",
          "Analysis plan: ", pl
        )
      })
      rq_plan_chunks <- Filter(Negate(is.null), rq_plan_chunks)
      params$analysis_plan <- if (length(rq_plan_chunks)) {
        paste(unlist(rq_plan_chunks), collapse = "\n\n")
      } else {
        ""
      }
      raw_rq_keys <- grep("^(rq_text_|rqplan_|remove_rq_)", names(params), value = TRUE)
      params[c(raw_rq_keys, "add_rq")] <- NULL
      
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

