# ==============================================================================
# File: Documentation/MiraProt_doc_user.R
#
# Purpose:
#   End-user documentation for MiraProt.
#   Audience: scientific users with limited proteomics and command-line experience.
# ==============================================================================

sp_doc_code_panel <- function(code_text) {
  tags$div(
    class = "sp-code-panel",
    tags$pre(code_text)
  )
}

#' User Guide — Overview
#' @keywords internal
render_user_miraprot_overview_content <- function() {
  tags$div(
    class = "user-doc",
    tags$h2("MiraProt User Guide"),
    tags$p(
      "MiraProt supports two usage modes: a local R-based workflow and a portable desktop workflow.",
      "Both provide the same analysis modules and scientific outputs."
    ),
    div(
      class = "alert alert-info",
      tags$b("Recommended analysis sequence"),
      tags$ol(
        tags$li("Load and inspect data in Data Wizard."),
        tags$li("Define groups and quality-check samples (Abundances, Sample IDs, Dimensionality Reduction)."),
        tags$li("Run statistical and biological interpretation modules (Volcano, GO, GSEA, STRING)."),
        tags$li("Summarize results with Heatmap, Venn, Dot Plot, and Plot Grid."),
        tags$li("Export tables and figures from module-specific download actions and the global workbook export.")
      )
    ),
    tags$h3("Main user interface areas"),
    tags$ul(
      tags$li(tags$b("Main Analysis:"), " all analysis modules are available as tabs."),
      tags$li(tags$b("System Info:"), " diagnostic values such as window dimensions and module status."),
      tags$li(tags$b("Documentation:"), " in-app module documentation for user and technical audiences."),
      tags$li(tags$b("About:"), " application metadata and project contact information.")
    ),
    tags$h3("When to choose each mode"),
    tags$ul(
      tags$li(tags$b("Local R-based mode:"), " best when you already use R/RStudio and want direct access to source code and development workflows."),
      tags$li(tags$b("Portable mode:"), " best when you want to run MiraProt without manually installing R packages.")
    )
  )
}

#' User Guide — Local R installation and execution
#' @keywords internal
render_user_miraprot_local_content <- function() {
  tags$div(
    class = "user-doc",
    tags$h2("Run MiraProt with a local R installation"),
    tags$h3("Prerequisites"),
    tags$ul(
      tags$li("Install the supported R release listed in the project setup script/README."),
      tags$li("Download or clone the MiraProt project folder to your computer."),
      tags$li("Choose one workflow: RStudio (recommended for beginners) or terminal/PowerShell + R console."),
      tags$li("For terminal/PowerShell usage, make sure R is callable from the command line (PATH configured), or be ready to start R with a full executable path."),
      tags$li("Use ", tags$code("install.R"), " once on a new machine to install required packages.")
    ),
    tags$h3("RStudio workflow (recommended)"),
    tags$ol(
      tags$li("Open RStudio."),
      tags$li("Use File -> Open Project (or Open File) and select the MiraProt folder."),
      tags$li("In the Console pane, run ", tags$code("source('install.R')"), " once for first-time setup."),
      tags$li("Start the app with ", tags$code("shiny::runApp('.')"), "."),
      tags$li("Keep the Console open while you use the app. Closing the Console stops the app.")
    ),
    tags$h3("Terminal/PowerShell workflow"),
    tags$ol(
      tags$li("Open Terminal (macOS/Linux) or PowerShell (Windows)."),
      tags$li("Change to the MiraProt project directory with ", tags$code("cd"), "."),
      tags$li("Start the R console (do not skip this step): use ", tags$code("R.exe"), " in Windows PowerShell, or ", tags$code("R"), " in macOS/Linux terminals."),
      tags$li("Run setup/app commands inside the R console (shown below)."),
      tags$li("Keep that terminal window open while MiraProt is running.")
    ),
    div(
      class = "alert alert-info",
      tags$b("How R is started from a terminal"),
      tags$p(tags$b("What is R.exe?"), " ", tags$code("R.exe"), " is the command-line executable for R on Windows."),
      tags$p(tags$b("Typical location on Windows:"), " ", tags$code("C:\\Program Files\\R\\R-x.y.z\\bin\\R.exe"), " (x.y.z is your installed R release)."),
      tags$p(tags$b("Why PATH matters:"), " your terminal can run ", tags$code("R.exe"), " or ", tags$code("R"), " directly only if the folder that contains the executable is listed in the PATH environment variable."),
      tags$p(tags$b("If R.exe is not found:"), " PowerShell/Terminal shows a command-not-found error. In that case, add R to PATH or run R by full path.")
    ),
    tags$h3("Check whether R is available in PATH"),
    tags$h4("Windows PowerShell"),
    sp_doc_code_panel(paste(
      "Get-Command R.exe",
      "R.exe --version",
      sep = "\n"
    )),
    tags$p("If ", tags$code("Get-Command R.exe"), " returns a valid path and ", tags$code("R.exe --version"), " prints the R version, PATH is configured correctly."),
    tags$p("If not found, start R using a full path (example):"),
    sp_doc_code_panel("\"C:\\Program Files\\R\\R-x.y.z\\bin\\R.exe\""),
    tags$h4("macOS/Linux terminal"),
    sp_doc_code_panel(paste(
      "which R",
      "R --version",
      sep = "\n"
    )),
    tags$p("If ", tags$code("which R"), " returns a file path and ", tags$code("R --version"), " prints the R version, PATH is configured correctly."),
    tags$p("If not found, install R or add the R binary directory to PATH, then open a new terminal window and check again."),
    tags$h3("Command examples"),
    tags$h4("First-time setup on a machine"),
    tags$p("Use this sequence only on first setup for a machine, or after deleting the local R package library."),
    tags$p(tags$b("Step 1 — In your terminal / PowerShell:")),
    tags$p("Open the project folder and then start the R console."),
    tags$p(tags$b("Windows PowerShell example:")),
    sp_doc_code_panel(paste(
      "cd C:\\path\\to\\MiraProt",
      "R.exe",
      sep = "\n"
    )),
    tags$p(tags$b("macOS/Linux example:")),
    sp_doc_code_panel(paste(
      "cd /path/to/MiraProt",
      "R",
      sep = "\n"
    )),
    tags$p(tags$b("Step 2 — In the R console:")),
    sp_doc_code_panel(paste(
      "source('install.R')   # installs required packages",
      "shiny::runApp('.')",
      sep = "\n"
    )),
    tags$p(
      "After installation completes, you can start the app immediately in the same R console with ",
      tags$code("shiny::runApp('.')"),
      "."
    ),
    div(
      class = "alert alert-info",
      tags$b("First-install note (Windows and fresh systems)"),
      tags$p(
        "If your computer is missing required installation components, ",
        tags$code("install.R"),
        " now tries an automatic fallback for ",
        tags$code("shinyTree"),
        " instead of stopping immediately."
      )
    ),
    tags$h4("Subsequent app starts"),
    tags$p("For normal daily use, do not run ", tags$code("source('install.R')"), " again unless package installation must be repeated."),
    tags$p(tags$b("Step 1 — In your terminal / PowerShell:")),
    tags$p(tags$b("Windows PowerShell example:")),
    sp_doc_code_panel(paste(
      "cd C:\\path\\to\\MiraProt",
      "R.exe",
      sep = "\n"
    )),
    tags$p(tags$b("macOS/Linux example:")),
    sp_doc_code_panel(paste(
      "cd /path/to/MiraProt",
      "R",
      sep = "\n"
    )),
    tags$p(tags$b("Step 2 — In the R console:")),
    sp_doc_code_panel(paste(
      "shiny::runApp('.')",
      sep = "\n"
    )),
    tags$h4("After closing and reopening the console"),
    tags$ul(
      tags$li("Closing Terminal/PowerShell or RStudio Console stops MiraProt."),
      tags$li("When you reopen R later, run ", tags$code("shiny::runApp('.')"), " again to start the app."),
      tags$li("You do not need to rerun ", tags$code("source('install.R')"), " after every restart."),
      tags$li("Run ", tags$code("source('install.R')"), " again only when dependencies are missing, after a clean reinstall, or when project requirements change.")
    ),
    div(
      class = "alert alert-info",
      tags$b("What does cd mean?"),
      tags$p(tags$code("cd"), " means change directory: it moves your terminal to another folder."),
      tags$p("This matters because ", tags$code("shiny::runApp('.')"), " starts the app from the current folder. The dot means \"this folder\"."),
      tags$p("Before running R, verify your current folder:"),
      tags$ul(
        tags$li(tags$code("pwd"), " on macOS/Linux terminals."),
        tags$li(tags$code("Get-Location"), " in PowerShell.")
      )
    ),
    tags$h3("Startup output interpretation"),
    tags$ul(
      tags$li(tags$b("Listening on ..."), " means the Shiny web server is running and waiting for browser connections."),
      tags$li("If auto-open is enabled in your environment, a browser tab usually opens automatically."),
      tags$li("If no browser opens, copy the displayed URL and paste it into your browser manually."),
      tags$li("If startup reports missing packages (for example AnnotationHub), rerun ", tags$code("source('install.R')"), " and follow the message shown at the end of installation.")
    ),
    tags$h3("IP address meanings"),
    tags$ul(
      tags$li(tags$b("localhost"), " and ", tags$b("127.0.0.1"), " are loopback addresses: only your own machine can access them."),
      tags$li("A LAN address such as 192.168.x.x or 10.x.x.x can be reached by other devices on the same network when host binding, port choice, and firewall rules allow it.")
    ),
    tags$h3("What this workflow does"),
    tags$ul(
      tags$li("Shiny auto-loads files in R/ and app.R initializes all modules."),
      tags$li("Module server functions are loaded into a shared environment (modEnv)."),
      tags$li("The app stops when your browser session closes, depending on miraprot.stop_on_close.")
    ),
    div(
      class = "alert alert-info",
      tags$b("Internet-dependent features"),
      tags$p("GO, STRING, and annotation queries can require internet access. Core analysis and plotting modules run offline.")
    )
  )
}

#' User Guide — Portable usage
#' @keywords internal
render_user_miraprot_portable_content <- function() {
  tags$div(
    class = "user-doc",
    tags$h2("Use the portable MiraProt edition"),
    tags$p("Portable mode includes a launcher, a bundled R runtime, and preinstalled packages."),
    tags$h3("Launch on Windows (step by step)"),
    tags$ol(
      tags$li("Download and extract the portable archive."),
      tags$li("Open the extracted folder and find MiraProt-launcher.exe."),
      tags$li("Double-click MiraProt-launcher.exe."),
      tags$li("If SmartScreen appears, select More info and then Run anyway."),
      tags$li("Wait until your browser opens the local MiraProt URL.")
    ),
    tags$h3("Startup checks"),
    tags$ul(
      tags$li(tags$b("How to confirm a successful launch:"), " your browser opens a page such as http://127.0.0.1:3838 and shows the MiraProt interface."),
      tags$li(tags$b("Where to find logs:"), " open the MiraProt data folder on your computer and check the logs subfolder for startup details and errors."),
      tags$li(tags$b("If no browser opens automatically:"), " wait 20-30 seconds, then manually open your browser and enter http://127.0.0.1:3838.")
    ),
    tags$h3("Quick troubleshooting"),
    tags$ul(
      tags$li(tags$b("SmartScreen warning:"), " choose More info -> Run anyway if you trust the file source."),
      tags$li(tags$b("Port already in use (blocked port):"), " close any older MiraProt window and restart the launcher so it can choose a different free port."),
      tags$li(tags$b("Stale lock message (already running):"), " if MiraProt is not actually open, close leftover MiraProt processes in Task Manager and launch again."),
      tags$li(tags$b("Still not starting:"), " restart Windows once, then run MiraProt-launcher.exe again from the extracted folder.")
    )
  )
}

#' User Guide — Session tab (save, restore, debug level)
#' @keywords internal
render_user_miraprot_session_content <- function() {
  tags$div(
    class = "user-doc",
    tags$h2("Use the Session tab"),
    tags$p(
      "The Session tab helps you save your current work, restore it later, and control the amount of debugging information shown in the app."
    ),
    tags$h3("What this tab is for"),
    tags$ul(
      tags$li("Create a session file so you can continue analysis later."),
      tags$li("Restore a previously saved session file (.rds)."),
      tags$li("Set the debug level used for session logs and diagnostics.")
    ),
    tags$h3("Save current session (download)"),
    tags$p("In Save Current Session, choose one save level and click Download Session."),
    tags$ul(
      tags$li(
        tags$b("Data & Metadata"),
        ": saves processed data, metadata setup, filtering, and Data Wizard pipeline state. This option usually creates the smallest files."
      ),
      tags$li(
        tags$b("Data & Analysis Results"),
        ": includes Data & Metadata and also saves GO and GSEA results, so they usually do not need to be recomputed."
      ),
      tags$li(
        tags$b("Full Session State"),
        ": includes analysis and module settings for visual modules (for example Volcano, PCA, Heatmap, Dot Plot, STRING, Venn). This is usually the largest file."
      )
    ),
    div(
      class = "alert alert-info",
      tags$b("Practical recommendation"),
      tags$p(
        "Use Data & Analysis Results for most collaborative work. Use Full Session State when visual configuration details are important for exact continuation."
      )
    ),
    tags$h3("Restore previous session"),
    tags$ol(
      tags$li("Open Session > Restore Previous Session."),
      tags$li("Upload a valid MiraProt session file with the .rds extension."),
      tags$li("Wait for validation feedback in the status box."),
      tags$li("After validation, restoration starts automatically and finishes with a success or warning message.")
    ),
    tags$p(
      "After restoration, modules may briefly re-process data. This is expected and helps ensure that tables and figures are consistent with restored inputs."
    ),
    tags$h3("Debug level in the Session tab"),
    tags$p("Use Debug Level to control how much information is shown in the session log."),
    tags$ul(
      tags$li(tags$b("0 - Essential"), ": key reproducibility events and core actions."),
      tags$li(tags$b("1 - Debug"), ": high-level lifecycle events such as module load and session save/restore."),
      tags$li(tags$b("2 - Verbose"), ": full diagnostics including detailed module-level messages.")
    ),
    tags$p(
      "You can lower the level to reduce noise while working, then raise it when troubleshooting. Previously captured log entries can become visible again when increasing the level."
    ),
    tags$h3("Close app with deep cleanup"),
    tags$p(
      "In Session > Application Shutdown, use ",
      tags$b("Run Deep Cleanup and Close App"),
      " when you want a stricter shutdown than the default fast close."
    ),
    tags$ul(
      tags$li("This action closes extra file/text connections, runs a garbage-collection pass, and then stops the app."),
      tags$li("It can take longer than normal close, especially after large analyses."),
      tags$li("Use it before long RStudio work sessions if you want to reduce leftover memory pressure.")
    ),
    div(
      style = paste(
        "background-color: #18bc9c; border-color: #18bc9c; color: #fff;",
        "padding: 15px; border-radius: 4px; margin-bottom: 20px;"
      ),
      tags$h4("When to use this"),
      tags$p(
        "Use deep cleanup when you notice repeated start/stop cycles making the RStudio session slower. For everyday work, the normal close path is usually faster."
      )
    )
  )
}

#' User Guide — Build local and portable distributions
#' @keywords internal
render_user_miraprot_build_content <- function() {
  tags$div(
    class = "user-doc",
    tags$h2("Build your own local and portable distributions"),
    tags$p(
      "This section explains how to create a distributable MiraProt folder on each operating system.",
      "You can copy the resulting dist/ folder directly or turn it into an installer."
    ),

    tags$h3("Prerequisites"),
    div(
      class = "alert alert-info",
      tags$p("Before running build commands, make sure these tools are available:"),
      tags$ul(
        tags$li(tags$b("All platforms:"), " the supported R release listed in the project setup script/README, plus a shell/terminal where you can run scripts."),
        tags$li(tags$b("Windows:"), " PowerShell (recommended) and execution rights to run local .ps1 scripts."),
        tags$li(tags$b("Linux:"), " bash and common build utilities (tar, chmod)."),
        tags$li(tags$b("macOS:"), " bash or zsh and standard developer command-line tools."),
        tags$li(tags$b("Launcher build support:"), " Go toolchain installed if you need to rebuild the launcher binary."),
        tags$li(tags$b("Installer packaging (optional):"), " use scripts in portable/installers/ when creating platform installers.")
      )
    ),
    tags$p("Policy note: omit -RVersion/--r-version for normal builds so portable/R_VERSION supplies the maintained R runtime default."),
    tags$p("MiraProt application versioning comes from Git/build metadata and R/version_info.R. It is independent of R, launcher, installer, and session-schema versions."),
    tags$p("A bundle keeps the runtime in r-portable and packages in r-library. On Windows, R is validated in temporary staging before safe promotion; failed stages and installer/probe logs are retained by default, and the previous runtime is preserved until its replacement passes final validation."),
    tags$p("Windows validation does not use a VERSION file. Absolute portable R.exe --version and Rscript.exe --version calls are startup probes; absolute portable Rscript.exe running getRversion() supplies the authoritative version and is queried again after promotion. Inherited R configuration is isolated, so a local R on PATH cannot satisfy validation."),

    tags$h3("Platform-specific build commands"),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Windows",
        tags$p("Use PowerShell from the repository root so relative paths resolve correctly."),
        tags$h4("1) Navigate to the project folder"),
        sp_doc_code_panel(paste(
          "cd C:\\path\\to\\MiraProt",
          "Get-Location   # confirms current folder",
          sep = "\n"
        )),
        tags$p("The first command moves you into the MiraProt folder. The second prints your current location so you can confirm you are in the correct directory."),
        tags$h4("2) Run the bundling script"),
        sp_doc_code_panel(".\\portable\\scripts\\bundle-r-windows.ps1 -OutputDir \".\\dist\""),
        tags$p("Omit -RVersion for an ordinary build: portable/R_VERSION supplies the maintained R runtime. -RVersion selects R, not the MiraProt application version."),
        tags$p("This creates a portable distribution in dist/. If PowerShell blocks script execution, see the common pitfalls section below."),
        tags$h4("3) Expected dist/ output"),
        tags$ul(
          tags$li(tags$code("dist/MiraProt-launcher.exe"), " (startup executable)"),
          tags$li(tags$code("dist/shiny-app/"), " (MiraProt application files)"),
          tags$li(tags$code("dist/r-portable/"), " (bundled R runtime)"),
          tags$li(tags$code("dist/r-library/"), " (R packages used by the app)"),
          tags$li(tags$code("dist/go-cache/"), " (cached annotation resources, when included)")
        ),
        tags$h4("4) Optional installer creation"),
        tags$p("To package this distribution as an installer, start from scripts under ", tags$code("portable/installers/"), ".")
      ),
      tabPanel(
        "Linux",
        tags$p("Use a bash shell and run commands from the repository root."),
        tags$h4("1) Navigate to the project folder"),
        sp_doc_code_panel(paste(
          "cd /path/to/MiraProt",
          "pwd   # confirms current folder",
          sep = "\n"
        )),
        tags$p("The cd command selects the project folder. The pwd command prints where you are."),
        tags$h4("2) Run the bundling script"),
        sp_doc_code_panel("./portable/scripts/bundle-r.sh --output-dir ./dist"),
        tags$p("Omit --r-version for an ordinary build: portable/R_VERSION supplies the maintained R runtime. --r-version selects R, not the MiraProt application version."),
        tags$p("This script assembles the portable runtime and app content in dist/."),
        tags$h4("3) Expected dist/ output"),
        tags$ul(
          tags$li(tags$code("dist/MiraProt-launcher"), " (startup binary)"),
          tags$li(tags$code("dist/shiny-app/"), " (MiraProt application files)"),
          tags$li(tags$code("dist/r-portable/"), " (bundled R runtime)"),
          tags$li(tags$code("dist/r-library/"), " (R packages used by the app)"),
          tags$li(tags$code("dist/go-cache/"), " (cached annotation resources, when included)")
        ),
        tags$h4("4) Optional installer creation"),
        tags$p("If you need a packaged installer, use the entry scripts located in ", tags$code("portable/installers/"), ".")
      ),
      tabPanel(
        "macOS",
        tags$p("You can use either bash or zsh. Commands below work in both shells."),
        tags$h4("1) Navigate to the project folder"),
        sp_doc_code_panel(paste(
          "cd /path/to/MiraProt",
          "pwd   # confirms current folder",
          sep = "\n"
        )),
        tags$p("Always confirm your folder before running build scripts to avoid writing files to the wrong location."),
        tags$h4("2) Run the bundling script"),
        sp_doc_code_panel("./portable/scripts/bundle-r.sh --output-dir ./dist"),
        tags$p("Omit --r-version for an ordinary build: portable/R_VERSION supplies the maintained R runtime. --r-version selects R, not the MiraProt application version."),
        tags$p("This prepares a portable MiraProt distribution that can be shared with other users."),
        tags$h4("3) Expected dist/ output"),
        tags$ul(
          tags$li(tags$code("dist/MiraProt-launcher"), " (startup binary)"),
          tags$li(tags$code("dist/shiny-app/"), " (MiraProt application files)"),
          tags$li(tags$code("dist/r-portable/"), " (bundled R runtime)"),
          tags$li(tags$code("dist/r-library/"), " (R packages used by the app)"),
          tags$li(tags$code("dist/go-cache/"), " (cached annotation resources, when included)")
        ),
        tags$h4("4) Optional installer creation"),
        tags$p("For macOS-specific packaging steps, begin with scripts under ", tags$code("portable/installers/"), ".")
      )
    ),

    tags$h3("Common pitfalls"),
    tags$ul(
      tags$li(tags$b("Wrong working directory:"), " run build commands only from the MiraProt repository root, otherwise relative script paths fail."),
      tags$li(tags$b("PowerShell execution policy:"), " if Windows blocks .ps1 scripts, open PowerShell as a user with script rights or allow local script execution for your session."),
      tags$li(tags$b("Missing Go toolchain:"), " bundling can still work with prebuilt launcher binaries, but rebuilding or updating the launcher requires Go to be installed and on PATH.")
    )
  )
}
