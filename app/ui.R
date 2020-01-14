dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "TRACKIT+",
    tags$li(
      class = "dropdown",
      tags$li(class = "dropdown", "v0.9.0"),
      tags$li(class = "dropdown", googleAuthUI("gauth_login")),
      tags$head(tags$script(src = "js.cookie.js")),
      tags$head(tags$script(src = "open-meta.js")),
      tags$head(tags$script(js_redirect)),
      tags$head(tags$style(sass(sass_file("www/csssheet.sass"))))
      # tags$head(includeScript(system.file('www', 'jssheet.js', package = 'myPackage'))),
      # singleton(tags$head(tags$link(rel="stylesheet", type = "text/css", href = "csssheet.scss")))
      # tags$head(includeCSS(system.file('www', 'csssheet.scss', package = 'myPackage')))
      # tags$li(class = "dropdown", googleAuth_jsUI("js_login"))
    )
  ),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Login",
        fluidRow(
          box(
            h2("Wellcome to Tabeltop Rolplay Adventure Calculator + Kombat 
               Initiative Tracker!"),
            h3("Login with your google account to get the adveture going"),
            h6("The app uses cookies generated at login. If you don't agree
               don't use it :P")
          )
        )
      ),
      tabItem(
        tabName = "AppInfo",
        fluidRow(
          box(
            includeMarkdown("README.md")
          )
        )
      ),
      tabItem(
        tabName = "ChangeLog",
        fluidRow(
          box(
            width = 3,
            uiOutput('textWithHTML')
          )
        )
      ),
      tabItem(
        tabName = "Select",
        uiOutput("select")
      ),
      tabItem(
        tabName = "Create",
        fluidRow(
          h2("Wellcome to character creation!"),
          h4("pliz fill out ALL the fields and click Create!"),
          box(
            title = "Character stats.",
            width = 2,
            solidHeader = TRUE,
            collapsible = TRUE,
            numericInput("str",
              "Strenght:",
              value = 10,
              width = "60px",
              min = 0
            ),
            numericInput("dex",
              "Dexterity:",
              value = 10,
              width = "60px",
              min = 0
            ),
            numericInput("con",
              "Constitution:",
              value = 10,
              width = "60px",
              min = 0
            ),
            numericInput("int",
              "Inteligence:",
              value = 10,
              width = "60px",
              min = 0
            ),
            numericInput("wis",
              "Wisdom:",
              value = 10,
              width = "60px",
              min = 0
            ),
            numericInput("cha",
              "Charisma:",
              value = 10,
              width = "60px",
              min = 0
            )
          ),
          box(
            title = "Other stats.",
            width = 3,
            solidHeader = TRUE,
            collapsible = TRUE,
            textInput("name",
              "Name:",
              width = "180px"
            ),
            textInput("class",
              "Class:",
              width = "180px"
            ),
            textInput("race",
              "Race:",
              width = "180px"
            ),
            numericInput("lvl",
              "Level:",
              value = 1,
              width = "60px",
              min = 0
            ),
            numericInput("AC",
              "Armmor:",
              value = 10,
              width = "60px",
              min = 0
            ),
            numericInput("HP",
              "Hit Points:",
              value = 10,
              width = "80px",
              min = 0
            ),
            numericInput("speed",
              "Speed:",
              value = 30,
              width = "60px", min = 0
            ),
            radioButtons(
              inputId = "size",
              label = "What is your size.?",
              choices = c("small", "normal", "big")
            )
          ),
          box(
            title = "Startin resurces",
            width = 3,
            solidHeader = TRUE,
            collapsible = TRUE,
            numericInput("food_c", "Food in kg:",
              value = 10, width = "120px", min = 0
            ),
            numericInput("wskin_c", "Number of watherskins:",
              value = 2, width = "120px", min = 0
            ),
            numericInput("catch_c", "Number of raincatcher:",
              value = 1, width = "120px", min = 0
            ),
            numericInput("repel_c", "Number of insect repelants:",
              value = 0, width = "120px", min = 0
            ),
            numericInput("gold_c", "Amount of gold (in cp, gx100):",
              value = 0, width = "120px", min = 0
            )
          ),
          box(
            title = "Encumberment",
            width = 3,
            solidHeader = TRUE,
            collapsible = TRUE,
            radioButtons(
              inputId = "able_c",
              label = "Able to calculate from str.?", choices = c("yes", "no")
            ),
            numericInput("carry", "(If not) How much can you carry?",
              value = 0, width = "120px", min = 0
            )
          ),
          actionButton("create", "Create!"),
          textOutput("rgistration")
        )
      ),
      tabItem(
        tabName = "Import",
        fluidRow(
          box(
            title = "Shop resurces",
            width = 3,
            solidHeader = TRUE,
            collapsible = TRUE,
            uiOutput("s_f"),
            uiOutput("s_w"),
            uiOutput("s_c"),
            uiOutput("s_r"),
            uiOutput("s_g"),
            radioButtons(
              inputId = "what",
              label = "What do you wish to do?",
              choices = c("calculate", "buy")
            ),
            actionButton("add_s", "Get!"),
            textOutput("buy_mes")
          ),
          box(
            title = "Shop items",
            solidHeader = TRUE,
            collapsible = TRUE,
            numericInput("i_price",
              "Price in gold for all pices (in cp, gx100)",
              value = 1,
              width = "120px",
              min = 0
            ),
            numericInput("amount",
              "Amount:",
              value = 1,
              width = "60px",
              min = 0
            ),
            textInput("i_name",
              "Item name:",
              width = "180px"
            ),
            numericInput("i_weight",
              "Weight (of one):",
              value = 1,
              width = "120px",
              min = 0
            ),
            textInput("des", "Description:"),
            actionButton("item", "Add!"),
            textOutput("item_shop")
          ),
          uiOutput("tab")
        )
      ),
      tabItem(
        tabName = "Travel",
        fluidRow(
          box(
            title = "Resurces in stock:",
            progressBar(
              id = "pb_f",
              value = 0,
              total = 10,
              title = "Food in stock (lbs)", status = "success"
            ),
            tags$style(".progress-bar-custom_f {background-color: #EC7063}"),
            progressBar(
              id = "pb_w",
              value = 0,
              total = 10,
              title = "Wather in stock (L)", status = "custom_w"
            ),
            tags$style(".progress-bar-custom_w {background-color: #2567c4;}"),
            progressBar(
              id = "pb_c",
              value = 5,
              total = 20,
              title = "Carrying capacity (lbs)", status = "custom_c"
            ),
            tags$style(".progress-bar-custom_c {background-color: #7a551d;}"),
            textOutput("limit")
          ),
          box(
            title = "Time",
            solidHeader = TRUE,
            splitLayout(
              numericInput("days_to_count", "How many days to count?",
                value = 0, width = "120px", min = 0
              ),
              actionButton("day", "Day >>",
                style = "padding:6px; font-size:200%"
              )
            ),
            actionButton("cuontdown", "Countdown!",
              style = "padding:4px; font-size:80%"
            ),
            textOutput("d"),
            textOutput("state_message"),
            splitLayout(
              radioGroupButtons(
                inputId = "eat",
                label = "This day I shall eat",
                choices = c("normal", "half", "nothing"),
                direction = "vertical", status = "success"
              ),
              radioGroupButtons(
                inputId = "drink",
                label = "This day I shall drink",
                choices = c("normal", "half", "nothing"),
                direction = "vertical", status = "primary"
              )
            ),
            switchInput(
              inputId = "repel_if", label = "use insecticide?",
              onLabel = "Yes", offLabel = "No"
            ),
            textOutput("repel_mes")
          ),
          box(
            title = "Wether",
            width = 2,
            solidHeader = TRUE,
            actionButton("rain", "Rainfall"),
            radioButtons(
              inputId = "r_amount", label = "The rain is:",
              choices = c("Light", "Medium", "Heavy")
            )
          ),
          box(
            title = "Wather manegment",
            solidHeader = TRUE,
            actionButton("refill", "Refill/poor"),
            uiOutput("w_slider")
          ),
          box(
            title = "Foraging resurces",
            width = 2,
            solidHeader = TRUE,
            numericInput("food_f", "Food in kg:",
              value = 0, width = "120px", min = 0
            ),
            numericInput("wather_f", "Wather in L:",
              value = 0, width = "120px", min = 0
            ),
            actionButton("add_f", "Add resurces")
          )
        )
      ),
      tabItem(
        tabName = "Inventory",
        fluidRow(
          DT::dataTableOutput("mytable"),
          box(
            title = "Add item to inventory",
            solidHeader = TRUE,
            collapsible = TRUE,
            numericInput("amount_it", "Amount:",
              value = 1, width = "60px", min = 0
            ),
            textInput("i_name_it", "Item name:",
              width = "180px"
            ),
            numericInput("i_weight_it", "Weight (of one):",
              value = 1, width = "120px", min = 0
            ),
            textInput("des_it", "Description:"),
            actionButton("item_it", "Add!"),
            textOutput("invent")
          ),
          box(
            title = "Money",
            solidHeader = TRUE,
            textOutput("sep")
          ),
          box(
            title = "Manage items",
            solidHeader = TRUE,
            collapsible = TRUE,
            actionButton("drop", "Drop items!"),
            actionButton("consume", "Consume!(x1)")
          ),
          box(
            title = "Change travel items",
            width = 2,
            solidHeader = TRUE,
            collapsible = TRUE,
            uiOutput("a_f"),
            uiOutput("a_w"),
            uiOutput("a_c"),
            uiOutput("a_r"),
            uiOutput("a_g"),
            actionButton("add", "Add!"),
            actionButton("restar", "epty!"),
            textOutput("throwed")
          ),
          box(
            title = "Travel items",
            width = 2,
            solidHeader = TRUE,
            collapsible = TRUE,
            textOutput("i_travel"),
            tableOutput("t_tabel")
          )
        )
      ),
      tabItem(
        tabName = "Character",
        fluidRow(
          
            includeHTML("charsheet.html")

          # uiOutput("cha_stats"),
          # uiOutput("charecter"),
          # uiOutput("information"),
          # uiOutput("lvl_up_panel"),
          # uiOutput("EXUST")
        )
      ),
      tabItem(
        tabName = "Combat",
        fluidRow(
          uiOutput("Turn"),
          uiOutput("initiative_in"),
          uiOutput("DM_combat"),
          uiOutput("DM_action"),
          box(
            title = "Health",
            solidHeader = TRUE,
            progressBar(
              id = "HP_com", value = 10,
              total = 10, title = "Hit points", status = "custom_H"
            ),
            tags$style(".progress-bar-custom_H {background-color: #cc3300;}"),
            numericInput("damage", "Damage taken:",
              value = 0, width = "120px", min = 0
            ),
            numericInput("healing", "Healing received:",
              value = 0, width = "120px", min = 0
            ),
            actionButton("take", "Recive!")
          ),
          uiOutput("DM_health")
        )
      )
    )
  ),
  setBackgroundImage(
    src = "junglesofchult.jpg",
    shinydashboard = TRUE
  )
)
