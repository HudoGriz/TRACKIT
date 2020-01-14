shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)
  print("restart")
  is.DM <- reactiveVal(0)
  user_r <- reactiveVal()

  info <- reactiveValues()
  sta <- reactiveValues()
  ind <- reactiveValues()

  days_count <- reactiveVal(0)

  food_r <- reactiveVal(food)
  wather_r <- reactiveVal(wather)
  weight_fw_r <- reactiveVal(weight_fw)
  weight_i_r <- reactiveVal(0)
  space_r <- reactiveVal(space)
  carry_r <- reactiveVal(carry)
  skin_r <- reactiveVal(skin)
  raincatc_r <- reactiveVal(raincatc)
  wather_max_r <- reactiveVal(wather_max)
  gold_r <- reactiveVal(gold)
  days_to_elepse_r <- reactiveVal(-5)
  days_elepsed <- reactiveVal(0)
  days_countdown <- reactiveVal(0)
  consume <- reactiveVal()
  repel_r <- reactiveVal(0)

  old <- reactiveVal(0)
  combat <- reactiveValues()
  combat$turn <- turn
  combat$me <- 0
  combat$order <- data.frame(0, "0")
  combat$intv <- 0
  combat$round <- runda
  combat$redy <- 0
  combat$turn_old <- 0
  combat$monsters <- data.frame(0, "0")
  autoInvalidate <- reactiveTimer(3000)
  combat$HP <- 5
  combat$lock <- 0
  combat$re <- 0

  output$textWithHTML <- renderUI({
    
    rawText <- readLines('CHANGELOG')
    splitText <- stringi::stri_split(str = rawText, regex = '\\n')
    
    replacedText <- lapply(splitText, p)
    
    return(replacedText)
  })

  access_token <- callModule(googleAuth, "gauth_login",
    login_text = "Login via Google",
    logout_text = "Logout",
    login_class = "btn btn-primary",
    logout_class = "btn btn-danger"
  )

  # Wait until JavaScript files have finished loading
  observeEvent(session$clientData$url_port, {
    jso$getCookie("sessionID", session = session)
    # print("re-login")
    # print(is.null(user_r()))
    # print(!is.null(input$js.sessionID))
    print("JawaScript messege")
  })
  
  observeEvent(input$js.sessionID, {
    print("cookie test")
    # print(input$js.sessionID)
    # print(is.null(input$js.sessionID))
    if (!is.null(user_r())) {
      if (names(user_r()) == "error" & !is.null(input$js.sessionID)) {
        print("re_login_in")
        pbodyf <- paste0(
          "id_token=",
          input$js.sessionID,
          "&providerId=google.com"
        )
        user <- o_auth_login(
          project_api = API_KEY,
          post_body = pbodyf,
          return_idp_credential = TRUE
        )
        
        if (names(user) == "error"){
          print("Invalid tok")
          # access_token(NULL)
          # rv$login <- FALSE
          # shinyjs::runjs("window.location.href = 'https://yourdomain.shinyapps.io/appName';")
          session$sendCustomMessage("mymessage", "mymessage")
          
        } else {
          
          user_r(user)
          print(user$email)
          
        }
      }
    }
  })

  observe({
    user <- user_r()
    created <- created_r()
    recived_token <- access_token()
    if (is.null(user)) {
      if (is.null(recived_token)) {
        output$menu <- renderMenu({
          sidebarMenu(
            menuItem("Login",
              tabName = "Login",
              icon = icon("power-off")
            ),
            menuItem("AppInfo",
              tabName = "AppInfo",
              icon = icon("info-circle")
            ),
            menuItem("ChangeLog",
                     tabName = "ChangeLog",
                     icon = icon("align-left")
            )
          )
        })
      } else {
        
        pbodyf <- paste0(
          "id_token=",
          recived_token$credentials$id_token,
          "&providerId=google.com"
        )
        user <- o_auth_login(
          project_api = API_KEY,
          post_body = pbodyf,
          return_idp_credential = TRUE
        )
        user_r(user)
        print("show menu")
        output$menu <- renderMenu({
          sidebarMenu(
            menuItem("Select character",
              tabName = "Select",
              icon = icon("angle-right"),
              selected = TRUE
            ),
            menuItem("Create character",
              tabName = "Create",
              icon = icon("plus")
            )
          )
        })
        # print("depositing cookie")
        # print(!is.null(user_r()))
        # print(is.null(user_r()))
        if (!names(user) == "error") {
          # print("change cookie")
          jso$setCookie("sessionID",
            cookie = recived_token$credentials$id_token,
            session = session
          )
        }
      }
      
    } else {
      
      print("cha select")
      print(user$email)
      
      cha_list <- try(download(
        projectURL = DATABASE_URL,
        paste0("users/", user$localId, "/characters"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      ))
      
      output$select <- renderUI({
        fluidRow(
          box(
            solidHeader = TRUE,
            h2("Wellecome to Tabeltop Rolplay Adventure Calculator + 
                 Kombat Initiative Tracker!"),
            h4("Selecte your character and let the adventure continue!"),
            if (class(cha_list) == "NULL") {
              h5("You first need to create a character!")
            } else {
              df <- data.frame(matrix(unlist(cha_list),
                nrow = length(cha_list),
                byrow = T
              ))
              df <- na.omit(df)
              vec <- as.character(df[, 1])
              awesomeRadio("selected_cha",
                "",
                choices = vec,
                status = "success"
              )
            },
            actionButton("enter", "Enter!"),
  
            textOutput("nogo_t")
          )
        )
      })
    }
  })

  ### select
  char <- reactiveVal()
  observeEvent(input$enter, {
    user <- user_r()
    selected <- try(input$selected_cha)
    # print(selected)
    if (class(selected) == "NULL") {
      sendSweetAlert(session,
        title = "U dense?!",
        text = "First you need to create a character ...",
        type = "error",
        btn_labels = "Ok",
        html = FALSE,
        closeOnClickOutside = TRUE
      )
    } else {
      sendSweetAlert(session,
        title = "Wellcome!",
        text = "Your last state of character has been successfully
                     loaded! Come let the adventure continue",
        type = "success",
        btn_labels = "Ok",
        html = FALSE,
        closeOnClickOutside = TRUE
      )
      char(selected)
      stats_sc <- download(
        projectURL = DATABASE_URL,
        paste0("users/", user$localId, "/", selected, "/stats"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      )
      info_sc <- download(
        projectURL = DATABASE_URL,
        paste0("users/", user$localId, "/", selected, "/info"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      )
      resurces_sc <- download(
        projectURL = DATABASE_URL,
        paste0("users/", user$localId, "/", selected, "/resurces"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      )
      status <- download(
        projectURL = DATABASE_URL,
        paste0("users/", user$localId, "/", selected, "/status"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      )
      # status
      combat$me <- status$combat
      combat$key <- status$instance
      if (combat$me >= 1) {
        print("combat active!")
      }
      # resurces
      skin_r(resurces_sc$skin)
      raincatc_r(resurces_sc$raincatc)
      gold_r(resurces_sc$gold)
      food_r(resurces_sc$food)
      wather_r(resurces_sc$wather)
      repel_r(resurces_sc$repel)

      # stats
      sta$str <- stats_sc$Str
      sta$dex <- stats_sc$Dex
      sta$con <- stats_sc$Con
      sta$int <- stats_sc$Int
      sta$wis <- stats_sc$Wis
      sta$cha <- stats_sc$Cha

      # info
      info$AC <- info_sc$AC
      info$HP <- info_sc$HP
      info$HPmax <- info_sc$HPmax
      info$carry <- info_sc$carry
      info$class <- info_sc$class
      info$ime <- info_sc$ime
      info$lvl <- info_sc$lvl
      info$race <- info_sc$race
      info$speed <- info_sc$speed
      info$size <- info_sc$size
      info$exsus <- info_sc$exsus
      # print(paste("imported speed:", info_sc$speed))

      consume(if (info$size == "big") {
        usage_f_l
      } else if (info$size == "small") {
        usage_f_s
      } else {
        usage_f_n
      })

      carry_r(
        if (info$carry == "yes") {
          if (info$size == "big") {
            stats_sc$Str * 15 * 2
          } else {
            stats_sc$Str * 15
          }
        } else {
          info_sc$carry
        }
      )

      i_in_invent <- try(download(
        projectURL = DATABASE_URL,
        paste0("users/", user$localId, "/", selected, "/inventory"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      ))
      # print(i_in_invent)
      if (class(i_in_invent) != "NULL") {
        output$mytable <- DT::renderDataTable({
          i_in_invent[, c(1, 3, 4, 2)]
        })
        weight_gained <- 0
        # print(nrow(i_in_invent))
        for (i in c(1:nrow(i_in_invent))) {
          item <- i_in_invent[i, ]
          # print(item)
          gained <- item$Amount * item$Weight
          weight_gained <- weight_gained + gained
          # print(weight_gained)
        }
        weight_i_r(weight_gained)
      }
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Character",
            tabName = "Character",
            icon = icon("address-book")
          ),
          menuItem("Travel",
            tabName = "Travel",
            icon = icon("map")
          ),
          menuItem("Inventory",
            tabName = "Inventory",
            icon = icon("th")
          ),
          menuItem("Shop",
            tabName = "Import",
            icon = icon("balance-scale")
          ),
          menuItem("Combat",
            tabName = "Combat",
            icon = icon("skull")
          )
        )
      })
    }

    if (!is.null(user)) {
      if (sum(user$localId == DMs) == 1) {
        if (!is.null(selected)) {
          if (selected == "DM") {
            is.DM(1)
          }
        }
      }
    }
  })

  created_r <- reactiveVal(0)
  observeEvent(input$create, {
    output$rgistration <- renderText("your character is beeing created, plis wait")
    user <- user_r()

    Str <- input$str
    Dex <- input$dex
    Con <- input$con
    Int <- input$int
    Wis <- input$wis
    Cha <- input$cha

    skin <- input$wskin_c
    food <- input$food_c
    raincatc <- input$catch_c
    wather <- skin * ws_c
    repel <- input$repel_c
    gold <- input$gold_c

    ime <- as.character(input$name)
    class <- as.character(input$class)
    race <- as.character(input$race)
    lvl <- input$lvl
    AC <- input$AC
    HPmax <- input$HP
    speed <- input$speed
    size <- input$size

    HP <- HPmax
    exsus <- 0

    if (input$able_c == "no") {
      carry <- input$carry
    } else {
      carry <- "yes"
    }

    cha_list <- try(download(
      projectURL = DATABASE_URL,
      paste0("users/", user$localId, "/characters"),
      secretKey = "none",
      token = "none",
      isClass = FALSE
    ))
    # print(class(cha_list))
    if (class(cha_list) == "NULL") {
      cha_list <- c(ime)
      # print(cha_list)
      # print("cha_list not detected")
      upload(
        x = cha_list,
        projectURL = DATABASE_URL,
        directory = paste0("users/", user$localId, "/characters"),
        token = "none"
      )
    } else {
      cha_list <- c(cha_list, ime)
      # print("cha_list destected")
      # print(cha_list)
      put(
        x = cha_list,
        projectURL = DATABASE_URL,
        directory = paste0("users/", user$localId, "/characters"),
        token = "none"
      )
    }

    # print(skin)
    # print(food)
    # print(raincatc)
    # print(wather)
    # print(gold)
    # print(ime)
    # print(class)
    # print(race)
    # print(AC)
    # print(HP)
    # print(speed)
    # print(Str)
    # print(Dex)
    # print(Con)
    # print(Wis)
    # print(Int)
    # print(Cha)
    # print(carry)

    resurces_df <- data.frame(skin, food, raincatc, wather, repel, gold)
    char_stats <- data.frame(Str, Dex, Con, Wis, Int, Cha)
    char_info <- data.frame(
      ime, class, race, lvl, AC, HP, HPmax, speed,
      carry, size, exsus
    )

    resurces <- as.list(resurces_df[, 1:ncol(resurces_df)])
    char_stats <- as.list(char_stats[, 1:ncol(char_stats)])
    char_info <- as.list(char_info[, 1:ncol(char_info)])

    put(
      x = resurces,
      projectURL = DATABASE_URL,
      directory = paste0("users/", user$localId, "/", ime, "/resurces"),
      token = "none"
    )
    put(
      x = char_stats,
      projectURL = DATABASE_URL,
      directory = paste0("users/", user$localId, "/", ime, "/stats"),
      token = "none"
    )
    put(
      x = char_info,
      projectURL = DATABASE_URL,
      directory = paste0("users/", user$localId, "/", ime, "/info"),
      token = "none"
    )

    combat <- 0
    instance <- 0
    status <- data.frame(combat, instance)
    status <- as.list(status[, 1:ncol(status)])
    put(
      x = status,
      projectURL = DATABASE_URL,
      directory = paste0("users/", user$localId, "/", ime, "/status"),
      token = "none"
    )

    sendSweetAlert(session,
      title = "Succes!",
      text = "Your character has been created",
      type = "success",
      btn_labels = "Ok",
      html = FALSE,
      closeOnClickOutside = TRUE
    )
    created_r(1)
  })

  ### Character ################################################################

  observe({
    char <- char()
    carry <- carry_r()
    if (class(char) != "NULL") {
      print(paste(char, "sheet renderd"))

      # info
      info$AC
      info$HP
      info$HPmax
      info$carry
      info$class
      info$ime
      info$lvl
      info$race
      info$speed
      info$size

      output$cha_stats <- renderUI({
        box(
          title = paste("lvl.", info$lvl),
          width = 3,
          splitLayout(
            numericInput("str_c",
              "Str",
              value = sta$str,
              width = "70px",
              min = 0
            ),
            h1(textOutput("str_txt"))
          ),
          splitLayout(
            numericInput("dex_c",
              "Dex",
              value = sta$dex,
              width = "70px",
              min = 0
            ),
            h1(textOutput("dex_txt"))
          ),
          splitLayout(
            numericInput("con_c",
              "Con",
              value = sta$con,
              width = "70px",
              min = 0
            ),
            h1(textOutput("con_txt"))
          ),
          splitLayout(
            numericInput("int_c",
              "Int",
              value = sta$int,
              width = "70px",
              min = 0
            ),
            h1(textOutput("int_txt"))
          ),
          splitLayout(
            numericInput("wis_c",
              "Wis",
              value = sta$wis,
              width = "70px",
              min = 0
            ),
            h1(textOutput("wis_txt"))
          ),
          splitLayout(
            numericInput("cha_c",
              "Cha",
              value = sta$cha,
              width = "70px",
              min = 0
            ),
            h1(textOutput("cha_txt"))
          )
        )
      })

      output$charecter <- renderUI({
        box(
          title = paste(char),
          progressBar(
            id = "HP_c",
            value = info$HP,
            total = info$HPmax,
            title = "Hit points",
            status = "custom_H"
          ),
          tags$style(".progress-bar-custom_H {background-color: #cc3300;}"),
          splitLayout(
            numericInput("AC_c",
              "AC",
              value = info$AC,
              width = "60px",
              min = 0
            ),
            numericInput("speed_c",
              "Speed",
              value = info$speed,
              width = "60px",
              min = 0
            )
          )
        )
      })

      output$information <- renderUI({
        box(
          width = 3,
          h4(paste("Race:", info$race)),
          h4(paste("Class:", info$class)),
          h4(paste("Size:", info$size)),
          actionButton("lvl_up_but", "Lvl. up!"),
          actionButton("change", "Save stats"),
          textOutput("lvl_up_m")
        )
      })
      output$exhaus <- renderTable(exhustion)

      output$EXUST <- renderUI({
        box(
          title = "Exhaustion",
          numericInput("exh",
            "Exhaustion level:",
            value = info$exsus,
            width = "60px",
            min = 0,
            max = 6
          ),
          tableOutput("exhaus")
        )
      })

      info$rend <- 1
    } else {
      info$rend <- 0
    }
  })

  observe({
    char <- char()
    if (class(char) != "NULL") {
      ind$str <- input$str_c
      ind$dex <- input$dex_c
      ind$con <- input$con_c
      ind$int <- input$int_c
      ind$wis <- input$wis_c
      ind$cha <- input$cha_c

      err <- try(index(ind$str))
      # print(class(err))
      if (class(err) == "try-error") {
        str_mod <- index(sta$str)
        updateNumericInput(
          session = session,
          "str_c",
          "Str",
          value = sta$str,
          min = 0
        )
      } else {
        str_mod <- index(ind$str)
      }
      output$str_txt <- renderText(str_mod)


      err <- try(index(ind$dex))
      # print(class(err))
      if (class(err) == "try-error") {
        dex_mod <- index(sta$dex)
        updateNumericInput(
          session = session,
          "dex_c",
          "Dex",
          value = sta$dex,
          min = 0
        )
      } else {
        dex_mod <- index(ind$dex)
      }
      output$dex_txt <- renderText(dex_mod)


      err <- try(index(ind$con))
      # print(class(err))
      if (class(err) == "try-error") {
        con_mod <- index(sta$con)
        updateNumericInput(
          session = session,
          "con_c",
          "Con",
          value = sta$con,
          min = 0
        )
      } else {
        con_mod <- index(ind$con)
      }
      output$con_txt <- renderText(con_mod)


      err <- try(index(ind$int))
      # print(class(err))
      if (class(err) == "try-error") {
        int_mod <- index(sta$int)
        updateNumericInput(
          session = session,
          "int_c",
          "Int",
          value = sta$int,
          min = 0
        )
      } else {
        int_mod <- index(ind$int)
      }
      output$int_txt <- renderText(int_mod)


      err <- try(index(ind$wis))
      # print(class(err))
      if (class(err) == "try-error") {
        wis_mod <- index(sta$wis)
        updateNumericInput(
          session = session,
          "wis_c",
          "Wis",
          value = sta$wis,
          min = 0
        )
      } else {
        wis_mod <- index(ind$wis)
      }
      output$wis_txt <- renderText(wis_mod)


      err <- try(index(ind$cha))
      # print(class(err))
      if (class(err) == "try-error") {
        cha_mod <- index(sta$cha)
        updateNumericInput(
          session = session,
          "cha_c",
          "Cha",
          value = sta$cha,
          min = 0
        )
      } else {
        cha_mod <- index(ind$cha)
      }
      output$cha_txt <- renderText(cha_mod)
    }
  })

  observeEvent(input$change, {
    user <- user_r()
    ime <- char()

    Str <- input$str_c
    Dex <- input$dex_c
    Con <- input$con_c
    Int <- input$int_c
    Wis <- input$wis_c
    Cha <- input$cha_c

    char_stats <- data.frame(Str, Dex, Con, Wis, Int, Cha)
    char_stats <- as.list(char_stats[, 1:ncol(char_stats)])
    put(
      x = char_stats,
      projectURL = DATABASE_URL,
      directory = paste0("users/", user$localId, "/", ime, "/stats"),
      token = "none"
    )

    AC <- input$AC_c
    speed <- input$speed_c

    try(put(
      x = AC,
      projectURL = DATABASE_URL,
      directory = paste0("users/", user$localId, "/", ime, "/info/AC"),
      token = "none"
    ),
    silent = TRUE
    )
    try(put(
      x = speed,
      projectURL = DATABASE_URL,
      directory = paste0("users/", user$localId, "/", ime, "/info/speed"),
      token = "none"
    ),
    silent = TRUE
    )

    exsus <- input$exh
    try(put(
      x = exsus,
      projectURL = DATABASE_URL,
      directory = paste0("users/", user$localId, "/", ime, "/info/exsus"),
      token = "none"
    ),
    silent = TRUE
    )

    output$lvl_up_m <- renderText("Your curent stats have been saved!")
  })



  observeEvent(input$lvl_up_but, {
    output$lvl_up_m <- renderText("Leveling iniciated!")
    output$lvl_up_panel <- renderUI({
      box(
        title = "lvl. up panel",
        h4("Before leveling up please, enter all the changes of stats.
             or info and input the hit points increase."),
        numericInput("add_hp",
          "Add to HP max",
          value = 0,
          width = "100px",
          min = 0
        ),
        actionButton("accept", "Lvl. Up!"),
        actionButton("cancle", "Cancle!")
      )
    })
  })

  observeEvent(input$cancle, {
    output$lvl_up_panel <- renderUI({
      NULL
    })
    output$lvl_up_m <- renderText("Leveling process has been canceled!")
  })

  observeEvent(input$accept, {
    confirmSweetAlert(
      session = session,
      inputId = "shure",
      title = "Level up check",
      text = "Are you shure that this are all the changes you want to apply?",
      type = "warning",
      danger_mode = FALSE,
      btn_labels = c("No", "Yes"),
      closeOnClickOutside = TRUE,
      html = FALSE
    )
  })

  observeEvent(input$shure, {
    # print(input$shure)
    if (input$shure == TRUE) {
      user <- user_r()
      ime <- char()

      Str <- input$str_c
      Dex <- input$dex_c
      Con <- input$con_c
      Int <- input$int_c
      Wis <- input$wis_c
      Cha <- input$cha_c

      sta$str <- Str
      sta$dex <- Dex
      sta$con <- Con
      sta$int <- Int
      sta$wis <- Wis
      sta$cha <- Cha

      char_stats <- data.frame(Str, Dex, Con, Wis, Int, Cha)
      char_stats <- as.list(char_stats[, 1:ncol(char_stats)])
      put(
        x = char_stats,
        projectURL = DATABASE_URL,
        directory = paste0("users/", user$localId, "/", ime, "/stats"),
        token = "none"
      )

      AC <- input$AC_c
      speed <- input$speed_c

      try(put(
        x = AC,
        projectURL = DATABASE_URL,
        directory = paste0("users/", user$localId, "/", ime, "/info/AC"),
        token = "none"
      ),
      silent = TRUE
      )
      try(put(
        x = speed,
        projectURL = DATABASE_URL,
        directory = paste0("users/", user$localId, "/", ime, "/info/speed"),
        token = "none"
      ),
      silent = TRUE
      )
      info$AC <- AC
      info$speed <- speed

      info$HPmax <- info$HPmax + input$add_hp
      info$HP <- info$HP + input$add_hp

      HPmax <- info$HPmax
      HP <- info$HP
      try(put(
        x = HPmax,
        projectURL = DATABASE_URL,
        directory = paste0("users/", user$localId, "/", ime, "/info/HPmax"),
        token = "none"
      ),
      silent = TRUE
      )
      try(put(
        x = HP,
        projectURL = DATABASE_URL,
        directory = paste0("users/", user$localId, "/", ime, "/info/HP"),
        token = "none"
      ),
      silent = TRUE
      )

      info$lvl <- info$lvl + 1
      lvl <- info$lvl
      try(put(
        x = lvl,
        projectURL = DATABASE_URL,
        directory = paste0("users/", user$localId, "/", ime, "/info/lvl"),
        token = "none"
      ),
      silent = TRUE
      )

      output$lvl_up_m <- renderText("You have gained a level!")
      output$lvl_up_panel <- renderUI({
        NULL
      })
    }
  })


  ### shop #####################################################################
  output$s_f <- renderUI(numericInput("food_s",
    "Food in kg:",
    value = 0,
    width = "120px",
    min = 0
  ))
  output$s_w <- renderUI(numericInput("wskin_s",
    "Number of watherskins:",
    value = 0,
    width = "120px",
    min = 0
  ))
  output$s_c <- renderUI(numericInput("catch_s",
    "Number of raincatcher:",
    value = 0,
    width = "120px",
    min = 0
  ))
  output$s_r <- renderUI(numericInput("repel_s",
    "Number of repelents:",
    value = 0,
    width = "120px",
    min = 0
  ))

  observeEvent(input$add_s, {
    user <- user_r()
    char <- char()
    resurces <- download(
      projectURL = DATABASE_URL,
      paste0("users/", user$localId, "/", char, "/resurces"),
      secretKey = "none",
      token = "none",
      isClass = FALSE
    )

    skin <- input$wskin_s
    food <- input$food_s
    raincatc <- input$catch_s
    wather <- skin * ws_c
    wather_max <- (resurces$skin + skin) * ws_c
    repel <- input$repel_s

    skin_p <- input$wskin_s * sp
    food_p <- input$food_s * fp
    raincatc_p <- input$catch_s * rp
    repel_p <- input$repel_s * rep
    cost <- skin_p + food_p + raincatc_p + repel_p

    output$buy_mes <- renderText(paste(
      "it would cost you:", cost,
      "cp, and you have:", resurces$gold, "cp"
    ))
    if (input$what == "buy") {
      if (resurces$gold >= cost) {
        food_r(resurces$food + food)
        wather_r(resurces$wather + wather)
        skin_r(resurces$skin + skin)
        raincatc_r(resurces$raincatc + raincatc)
        gold_r(resurces$gold - cost)

        resurces$gold <- resurces$gold - cost
        resurces$food <- resurces$food + food
        resurces$raincatc <- resurces$raincatc + raincatc
        resurces$skin <- resurces$skin + skin
        resurces$wather <- resurces$wather + wather
        resurces$repel <- resurces$repel + repel
        put(
          x = resurces,
          projectURL = DATABASE_URL,
          directory = paste0("users/", user$localId, "/", char, "/resurces"),
          token = "none"
        )

        output$s_f <- renderUI(numericInput("food_s",
          "Food in kg:",
          value = 0,
          width = "120px",
          min = 0
        ))
        output$s_w <- renderUI(numericInput("wskin_s",
          "Number of watherskins:",
          value = 0,
          width = "120px",
          min = 0
        ))
        output$s_c <- renderUI(numericInput("catch_s",
          "Number of raincatcher:",
          value = 0,
          width = "120px",
          min = 0
        ))
        output$s_r <- renderUI(numericInput("repel_s",
          "Number of repelents:",
          value = 0,
          width = "120px",
          min = 0
        ))
      } else {
        output$buy_mes <- renderText("You don't have enough money!!!")
      }
    }
    # a = food_r()
    # b = wather_r()
    # c = weight_fw_r()
    # d = space_r()
    # e = weight_r()
    # f = raincatc_r()
    # print(a)
    # print(b)
    # print(c)
    # print(d)
    # print(e)
    # print(f)
  })

  # wather puring/reffiling
  observe({
    skin <- skin_r()
    wather_max <- skin * ws_c
    output$w_slider <- renderUI(sliderInput("w_reff",
      "How much wather do you wish to have:",
      min = 0,
      max = wather_max,
      value = wather_max
    ))
  })
  wather_max_r <- reactive({
    skin_r() * ws_c
  })

  weight_fw_r <- reactive({
    food_r() * fw + wather_r() * ww + raincatc_r() * 5
  })
  weight_r <- reactive({
    weight_fw_r() + weight_i_r()
  })
  space_r <- reactive({
    carry_r() - weight_r()
  })

  output$a_f <- renderUI(numericInput("food",
    "Food in kg:",
    value = 0,
    width = "120px",
    min = 0
  ))
  output$a_w <- renderUI(numericInput("wskin",
    "Number of watherskins:",
    value = 0,
    width = "120px",
    min = 0
  ))
  output$a_c <- renderUI(numericInput("catch",
    "Number of raincatcher:",
    value = 0,
    width = "120px",
    min = 0
  ))
  output$a_r <- renderUI(numericInput("repel",
    "Number of repelents:",
    value = 0,
    width = "120px"
  ))
  output$a_g <- renderUI(numericInput("gold",
    "Amount of gold (in cp, gx100):",
    value = 0,
    width = "120px",
    min = 0
  ))

  url <- a("Zitronija drive", href = drive_link)
  output$tab <- renderUI({
    box(
      title = "Item catalog",
      h6("Work in progress, but you can find the catalog on the drive"),
      tagList(url)
    )
  })

  observeEvent(input$item, {
    user <- user_r()
    char <- char()
    resurces <- download(
      projectURL = DATABASE_URL,
      paste0("users/", user$localId, "/", char, "/resurces"),
      secretKey = "none",
      token = "none",
      isClass = FALSE
    )
    cost <- input$i_price

    if (resurces$gold >= cost) {
      i_in_invent <- try(download(
        projectURL = DATABASE_URL,
        paste0("users/", user$localId, "/", char, "/inventory"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      ))

      Amount <- input$amount
      Name <- input$i_name
      Weight <- input$i_weight
      Description <- input$des

      if (Amount < 0 || Weight < 0) {
        output$item_shop <- renderText("The amount and weight needs to be positive")
      } else {
        if (class(i_in_invent) != "NULL") {
          new <- data.frame(Amount, Name, Weight, Description)
          i_in_invent <- rbind(i_in_invent, new)
          output$mytable <- DT::renderDataTable({
            i_in_invent[, c(1, 3, 4, 2)]
          })
        } else {
          i_in_invent <- data.frame(Amount, Name, Weight, Description)
          output$mytable <- DT::renderDataTable({
            i_in_invent
          })
          names(i_in_invent) <- c("Amount", "Name", "Weight", "Description")

          old <- weight_i_r()
          weight_i_r(old + Weight * Amount)

          put(
            x = i_in_invent,
            projectURL = DATABASE_URL,
            directory = paste0("users/", user$localId, "/", char, "/inventory"),
            token = "none"
          )
          output$item_shop <- renderText("The item has been bought")
        }
        resurces$gold <- resurces$gold - cost
        put(
          x = resurces,
          projectURL = DATABASE_URL,
          directory = paste0("users/", user$localId, "/", char, "/resurces"),
          token = "none"
        )
        output$item_shop <- renderText(paste(
          "You bouth it, and still have",
          resurces$gold, "cp"
        ))
      }
    } else {
      output$item_shop <- renderText("You don't have enough money!!!")
    }
  })

  #### tab travel ##############################################################

  ### observe encumberment ###
  observe({
    weight <- weight_r()
    carry <- carry_r()

    if (weight <= carry / 3) {
      a <- paste(carry / 3, "> You are OK")
    } else if ((weight > carry / 3) & (weight <= (carry * 2) / 3)) {
      a <- paste(carry / 3, "< you are encumbered (-10ft) <", ((carry * 2) / 3))
    } else {
      a <- paste(((carry * 2) / 3), "< you are ENCUMBERED (-20ft, dis on str, con, dex)")
    }

    output$limit <- renderText({
      a
    })
  })

  ### Observe food and wather consumption ###
  observe({
    user <- user_r()
    char <- char()

    food <- food_r()
    wather <- wather_r()
    weight <- weight_r()
    space <- space_r()
    carry <- carry_r()
    wather_max <- wather_max_r()
    repel <- repel_r()

    # print(wather)
    # print(wather_max)
    if (space <= 0) {
      space <- food
    }
    # print(food)
    # print(wather)
    food_total <- space + food

    updateProgressBar(
      session = session,
      id = "pb_f",
      value = food,
      total = food_total,
      title = "Food in stock (lbs)",
      status = "success"
    )
    updateProgressBar(
      session = session,
      id = "pb_w", value = wather,
      total = wather_max,
      title = "Wather in stock (L)",
      status = "custom_w"
    )

    if (weight <= carry / 3) {
      status <- "success"
    } else if ((weight > carry / 3) & (weight <= (carry * 2) / 3)) {
      status <- "warning"
    } else {
      status <- "danger"
    }

    updateProgressBar(
      session = session,
      id = "pb_c",
      value = weight,
      total = carry,
      title = "Carrying capacity (lbs)",
      status = status
    )

    if (class(user) != "NULL") {
      if (class(char) != "NULL") {
        resurces <- download(
          projectURL = DATABASE_URL,
          paste0("users/", user$localId, "/", char, "/resurces"),
          secretKey = "none",
          token = "none",
          isClass = FALSE
        )

        resurces$food <- food
        resurces$wather <- wather
        resurces$repel <- repel

        put(
          x = resurces,
          projectURL = DATABASE_URL,
          directory = paste0("users/", user$localId, "/", char, "/resurces"),
          token = "none"
        )
      }
    }
  })

  ### pass one day ###
  observeEvent(input$day, {
    food <- food_r()
    wather <- wather_r()
    days <- days_count()
    period <- days_to_elepse_r()
    days_gone <- days_elepsed()
    days_countdown_s <- days_countdown()
    consume_f <- consume()
    repel <- repel_r()
    consume_w <- usage_w

    if (input$eat == "half") {
      consume_f <- consume_f / 2
    }
    if (input$eat == "nothing") {
      consume_f <- 0
    }

    if (input$drink == "half") {
      consume_w <- consume_w / 2
    }

    if (input$drink == "nothing") {
      consume_w <- 0
    }

    days_count(days + 1)
    food <- food - consume_f
    wather <- wather - consume_w

    if (food <= 0) {
      food <- 0
    }

    if (wather <= 0) {
      wather <- 0
    }

    weight_fw <- food * fw + wather * ww

    output$d <- renderText(paste("Days gone by:", days_count()))

    if (days_gone < (period + 2)) {
      period <- days_to_elepse_r()
      days_gone <- days_elepsed()

      days_gone <- days_gone + 1
      remaining <- period - days_gone
      days_elepsed(days_gone)

      output$state_message <- renderText(days_gone)

      if (remaining > 0) {
        state <- paste("Days to the event:", remaining)
      } else {
        state <- paste("The time is up!")
      }

      output$state_message <- renderText(state)
    } else {
      if (days_countdown_s == 1) {
        output$state_message <- renderText(NULL)
        days_countdown(0)
      }
    }

    if (input$repel_if == TRUE) {
      repel <- repel - 1

      if (repel < 0) {
        repel <- 0
        output$repel_mes <- renderText("You are out of repelents!")
      }

      output$repel_mes <- renderText(paste("You still have:", repel))
    }

    food_r(food)
    wather_r(wather)
    repel_r(repel)
  })

  ### Set countdown ###
  observeEvent(input$cuontdown, {
    days_to_elepse_r(input$days_to_count)
    days_elepsed(0)
    days_countdown(1)
    output$state_message <- renderText("Countdown started")
  })

  ### Refil wather skin ###
  observeEvent(input$refill, {
    wather <- input$w_reff
    wather_r(wather)
  })

  ### Colect wather with wather catchers ###
  observeEvent(input$rain, {
    wather <- wather_r()
    raincatc <- raincatc_r()
    wather_max <- wather_max_r()

    if (input$r_amount == "Light") {
      dez <- 1 * 2 * raincatc
    } else if (input$r_amount == "Medium") {
      dez <- 5 * 2 * raincatc
    } else {
      dez <- 20 * 2 * raincatc
    }

    if (dez + wather <= wather_max) {
      wather <- dez + wather
    } else {
      wather <- wather_max
    }

    wather_r(wather)
  })

  ### go gathering ###
  observeEvent(input$add_f, {
    food <- food_r()
    wather <- wather_r()
    wather_max <- wather_max_r()

    food_r(food + input$food_f)
    wather_new <- wather + input$wather_f

    if (wather_new <= wather_max) {
      wather_r(wather_new)
    } else {
      wather_r(wather_max)
    }

    updateNumericInput(
      session = session,
      "food_f",
      "Food in kg:",
      value = 0,
      min = 0
    )
    updateNumericInput(
      session = session,
      "wather_f",
      "Wather in L:",
      value = 0,
      min = 0
    )
  })

  #### tab Inventory ###########################################################

  ### observe encumberment ###
  observe({
    if (space_r() < 0) {
      sendSweetAlert(session,
        title = "Inventory full!",
        text = "You are overburdened and can't move!",
        type = "error",
        btn_labels = "Ok",
        html = FALSE,
        closeOnClickOutside = TRUE
      )
    }
  })

  ### add item ###
  observeEvent(input$item_it, {
    user <- user_r()
    char <- char()

    i_in_invent <- try(download(
      projectURL = DATABASE_URL,
      paste0("users/", user$localId, "/", char, "/inventory"),
      secretKey = "none",
      token = "none",
      isClass = FALSE
    ))

    Amount <- input$amount_it
    Name <- input$i_name_it
    Weight <- input$i_weight_it
    Description <- input$des_it

    if (Amount < 0 || Weight < 0) {
      output$invent <- renderText("The amount and weight needs to be positive")
    } else {
      if (class(i_in_invent) != "NULL") {
        new <- data.frame(Amount, Name, Weight, Description)
        i_in_invent <- rbind(i_in_invent, new)
        output$mytable <- DT::renderDataTable({
          i_in_invent[, c(1, 3, 4, 2)]
        })
      } else {
        i_in_invent <- data.frame(Amount, Name, Weight, Description)
        output$mytable <- DT::renderDataTable({
          i_in_invent
        })
        names(i_in_invent) <- c("Amount", "Name", "Weight", "Description")
      }

      old <- weight_i_r()
      weight_i_r(old + Weight * Amount)

      put(
        x = i_in_invent,
        projectURL = DATABASE_URL,
        directory = paste0("users/", user$localId, "/", char, "/inventory"),
        token = "none"
      )

      updateNumericInput(
        session = session,
        "amount_it",
        "Amount:",
        value = 1,
        min = 0
      )
      updateTextInput(
        session = session,
        "i_name_it",
        label = "Item name:",
        value = ""
      )
      updateNumericInput(
        session = session,
        "i_weight_it",
        "Weight (of one):",
        value = 1, min = 0
      )
      updateTextInput(
        session = session,
        "des_it",
        label = "Description:",
        value = ""
      )
    }
  })

  ### drop item ###
  observeEvent(input$drop, {
    user <- user_r()
    char <- char()

    i_in_invent <- try(download(
      projectURL = DATABASE_URL,
      paste0("users/", user$localId, "/", char, "/inventory"),
      secretKey = "none",
      token = "none",
      isClass = FALSE
    ))

    selected <- input$mytable_rows_selected
    weight_lost <- 0

    if (class(selected) != "NULL") {
      for (i in selected) {
        item <- i_in_invent[i, ]
        lost <- item$Amount * item$Weight
        weight_lost <- weight_lost + lost
      }

      i_in_invent <- i_in_invent[-selected, ]
      output$mytable <- DT::renderDataTable({
        i_in_invent[, c(1, 3, 4, 2)]
      })

      old <- weight_i_r()
      weight_i_r(old - weight_lost)

      put(
        x = i_in_invent,
        projectURL = DATABASE_URL,
        directory = paste0("users/", user$localId, "/", char, "/inventory"),
        token = "none"
      )
    }
  })

  ### consume item ###
  observeEvent(input$consume, {
    user <- user_r()
    char <- char()

    i_in_invent <- try(download(
      projectURL = DATABASE_URL,
      paste0("users/", user$localId, "/", char, "/inventory"),
      secretKey = "none",
      token = "none",
      isClass = FALSE
    ))

    selected <- input$mytable_rows_selected
    weight_lost <- 0

    for (i in selected) {
      item <- i_in_invent[i, ]

      if (item$Amount == 1) {
        i_in_invent <- i_in_invent[-i, ]
        weight_lost <- weight_lost + item$Weight
      } else {
        item$Amount <- item$Amount - 1
        weight_lost <- weight_lost + item$Weight
        i_in_invent <- i_in_invent[-i, ]
        i_in_invent <- rbind(i_in_invent, item)
      }
    }

    output$mytable <- DT::renderDataTable({
      i_in_invent[, c(1, 3, 4, 2)]
    })
    old <- weight_i_r()
    weight_i_r(old - weight_lost)

    put(
      x = i_in_invent,
      projectURL = DATABASE_URL,
      directory = paste0("users/", user$localId, "/", char, "/inventory"),
      token = "none"
    )
  })

  ### travel items ###
  observe({
    user <- user_r()
    char <- char()

    food_r()
    wather_r()
    raincatc_r()
    skin_r()
    repel_r()
    gold_r()

    resurces <- download(
      projectURL = DATABASE_URL,
      paste0("users/", user$localId, "/", char, "/resurces"),
      secretKey = "none",
      token = "none",
      isClass = FALSE
    )

    df <- data.frame(unlist(resurces))

    output$t_tabel <- renderTable(df, rownames = TRUE, colnames = FALSE)
  })

  ### Money ###
  observe({
    gold <- gold_r()
    cp <- ceiling(gold %% 10)
    sp <- (gold %% 100) %/% 10
    gp <- gold %/% 100
    output$sep <- renderText(paste(
      "gp", gp,
      "| sp", sp,
      "| cp", cp,
      " -- ", gold
    ))
  })

  ### drop travel items ###
  observeEvent(input$restar, {
    user <- user_r()
    char <- char()
    resurces <- download(
      projectURL = DATABASE_URL,
      paste0("users/", user$localId, "/", char, "/resurces"),
      secretKey = "none",
      token = "none",
      isClass = FALSE
    )

    food_p <- resurces$food
    raincatc_p <- resurces$raincatc
    skin_p <- resurces$skin
    repel_p <- resurces$repel
    gold_p <- resurces$gold
    output$throwed <- renderText(paste(
      "food -", food_p,
      ", raincatcher -", raincatc_p,
      ", insect repelent -", repel_p,
      ", wather skin -", skin_p,
      ", gold -", gold_p
    ))

    skin <- 0
    food <- 0
    wather <- 0
    raincatc <- 0
    repel <- 0
    gold <- 0

    food_r(food)
    wather_r(wather)
    raincatc_r(raincatc)
    skin_r(skin)
    repel_r(repel)
    gold_r(gold)

    resurces$gold <- gold
    resurces$food <- food
    resurces$raincatc <- raincatc
    resurces$skin <- skin
    resurces$wather <- wather
    resurces$repel <- repel

    put(
      x = resurces,
      projectURL = DATABASE_URL,
      directory = paste0("users/", user$localId, "/", char, "/resurces"),
      token = "none"
    )
  })

  ### add resurces ###
  observeEvent(input$add, {
    user <- user_r()
    char <- char()

    resurces <- download(
      projectURL = DATABASE_URL,
      paste0("users/", user$localId, "/", char, "/resurces"),
      secretKey = "none",
      token = "none",
      isClass = FALSE
    )

    skin <- input$wskin
    food <- input$food
    raincatc <- input$catch
    wather <- skin * ws_c
    wather_max <- (resurces$skin + skin) * ws_c
    gold <- input$gold
    repel <- input$repel

    # print(resurces$repel)
    # print(class(resurces$repel))
    # print(resurces$gold)
    # print(class(resurces$gold))

    food_r(resurces$food + food)
    wather_r(resurces$wather + wather)
    skin_r(resurces$skin + skin)
    raincatc_r(resurces$raincatc + raincatc)
    gold_r(resurces$gold + gold)
    repel_r(resurces$repel + repel)

    resurces$gold <- resurces$gold + gold
    resurces$food <- resurces$food + food
    resurces$raincatc <- resurces$raincatc + raincatc
    resurces$skin <- resurces$skin + skin
    resurces$repel <- resurces$repel + repel

    put(
      x = resurces,
      projectURL = DATABASE_URL,
      directory = paste0("users/", user$localId, "/", char, "/resurces"),
      token = "none"
    )


    output$a_f <- renderUI(numericInput("food",
      "Food in kg:",
      value = 0,
      width = "120px",
      min = 0
    ))
    output$a_w <- renderUI(numericInput("wskin",
      "Number of watherskins:",
      value = 0,
      width = "120px",
      min = 0
    ))
    output$a_c <- renderUI(
      numericInput(
        "catch",
        "Number of raincatcher:",
        value = 0,
        width = "120px",
        min = 0
      )
    )
    output$a_r <- renderUI(
      numericInput(
        "repel",
        "Number of repelents:",
        value = 0,
        width = "120px",
        min = 0
      )
    )
    output$a_g <- renderUI(
      numericInput(
        "gold",
        "Amount of gold (in cp, gx100):",
        value = 0,
        width = "120px",
        min = 0
      )
    )

    # a = food_r()
    # b = wather_r()
    # c = weight_fw_r()
    # d = space_r()
    # e = weight_r()
    # f = raincatc_r()
    # print(a)
    # print(b)
    # print(c)
    # print(d)
    # print(e)
    # print(f)

    output$throwed <- renderText(
      paste(
        "food +", food,
        ", raincatcher +", raincatc,
        ", insect repelent +", repel,
        ", wather skin +", skin,
        ", gold +", gold
      )
    )
  })

  ### Combat ###################################################################

  ### enter combat instance ###
  observeEvent(input$fight, {
    key <- input$instance

    combat$lock <- try(
      download(
        projectURL = DATABASE_URL, paste0("combat/", key, "/locked"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      ),
      silent = TRUE
    )

    locked <- combat$lock

    print(locked)
    # print(combat$lock != 0)
    # print(class(combat$lock) != "NULL")

    if (combat$lock == 0 || class(combat$lock) == "NULL") {
      if (combat$me == 0) {
        print(paste("fight", "ok"))
        user <- user_r()
        Char <- char()
        maxHP <- info$HPmax
        HP <- info$HP
        # print(char)
        # char <- as.character(char)
        intv <- input$initial_val
        # print(intv)
        combat$intv <- intv
        # print(intv)
        # intv <<- intv
        combat$key <- key
        # names(intv) <- c("haha")
        # print(intv)

        battle <- try(
          download(
            projectURL = DATABASE_URL,
            paste0("combat/", key, "/battle"),
            secretKey = "none",
            token = "none",
            isClass = FALSE
          )
        )


        output$loged <- renderText("Your initiative has been loged")
        redy <- 1

        action <- 1
        entery <- data.frame(
          user$localId,
          Char,
          intv,
          action,
          maxHP,
          HP
        )
        names(entery) <- c(
          "player",
          "name",
          "iniciative",
          "action",
          "maxHP",
          "HP"
        )

        if (class(battle) == "NULL") {
          try(
            put(
              x = redy,
              projectURL = DATABASE_URL,
              directory = paste0("combat/", key, "/redy"),
              token = "none"
            ),
            silent = TRUE
          )
          try(
            put(
              x = 1,
              projectURL = DATABASE_URL,
              directory = paste0("combat/", key, "/turn"),
              token = "none"
            ),
            silent = TRUE
          )
          try(
            put(
              x = 1,
              projectURL = DATABASE_URL,
              directory = paste0("combat/", key, "/round"),
              token = "none"
            ),
            silent = TRUE
          )
          try(
            put(
              x = 0,
              projectURL = DATABASE_URL,
              directory = paste0("combat/", key, "/locked"),
              token = "none"
            ),
            silent = TRUE
          )

          print(entery)
          combat$order <- entery
        } else {
          redy_old <- download(
            projectURL = DATABASE_URL,
            paste0("combat/", key, "/redy"),
            secretKey = "none",
            token = "none",
            isClass = FALSE
          )

          redy <- redy_old + redy

          try(
            put(
              x = redy,
              projectURL = DATABASE_URL,
              directory = paste0("combat/", key, "/redy"),
              token = "none"
            ),
            silent = TRUE
          )
          combat$turn <- try(
            download(
              projectURL = DATABASE_URL,
              paste0("combat/", key, "/turn"),
              secretKey = "none",
              token = "none",
              isClass = FALSE
            )
          )
          combat$round <- try(
            download(
              projectURL = DATABASE_URL,
              paste0("combat/", key, "/round"),
              secretKey = "none",
              token = "none",
              isClass = FALSE
            ),
            silent = TRUE
          )

          entery <- rbind(battle, entery)
          entery <- entery[order(entery$iniciative, decreasing = TRUE), ]
          entery$action <- c(1:nrow(entery))

          y <- combat$turn_old
          print(paste("combat_old_enter_combat:", y))
        }

        put(
          x = entery,
          projectURL = DATABASE_URL,
          directory = paste0("combat/", key, "/battle"),
          token = "none"
        )

        combat$redy <- redy
        combat$me <- 1
        old(1)

        combat <- combat$me
        instance <- key
        status <- data.frame(combat, instance)
        status <- as.list(status[, 1:ncol(status)])

        put(
          x = status,
          projectURL = DATABASE_URL,
          directory = paste0("users/", user$localId, "/", Char, "/status"),
          token = "none"
        )

        combat$re <- 0
      }
    } else {
      output$loged <- renderText("You can not enter this battle at the moment!")
    }
  })

  ### Observe combat instance ###
  observe({
    autoInvalidate()
    # print("validate")

    if (combat$me >= 1) {

      # print("get data")
      combat$re <- 1
      key <- combat$key

      combat$redy <- download(
        projectURL = DATABASE_URL,
        paste0("combat/", key, "/redy"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      )
      turn <- try(
        download(
          projectURL = DATABASE_URL,
          paste0("combat/", key, "/turn"),
          secretKey = "none",
          token = "none",
          isClass = FALSE
        )
      )
      rund <- try(
        download(
          projectURL = DATABASE_URL,
          paste0("combat/", key, "/round"),
          secretKey = "none",
          token = "none",
          isClass = FALSE
        ),
        silent = TRUE
      )
      combat$order <- download(
        projectURL = DATABASE_URL,
        paste0("combat/", key, "/battle"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      )

      if (class(turn) == "NULL") {
        turn <- 1
      }
      if (class(rund) == "NULL") {
        rund <- 1
      }
      # print(paste("turn:",turn))
      combat$turn <- turn
      combat$round <- rund
      if (rund > combat$round) {
        combat$round <- rund
        time <- combat$round * 5 - 5
        sec <- ceiling(time %% 60)
        min <- time %/% 60
        output$time <- renderText(paste(
          "Time passed:",
          min, "min",
          sec, "sec"
        ))
      }
    }
  })

  ### React to changes in combat instance ###
  observe({
    key <- combat$key
    old <- old()
    user <- user_r()
    Char <- char()
    # print("observe")

    if (combat$me == 0) {
      # print("No_int_table")
      output$Turn <- renderUI({
        box(
          title = "Turn order",
          h5("You are not in combat")
        )
      })
    } else {
      status <- download(
        projectURL = DATABASE_URL,
        paste0("users/", user$localId, "/", Char, "/status"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      )

      # print(paste0("combat status: ", status$combat))
      combat$me <- status$combat

      if (!combat$me == 0) {
        if (combat$re > 0) {
          output$Turn <- renderUI({
            box(
              title = "Turn order",
              formattableOutput("order"),
              textOutput("time")
            )
          })

          battle <- combat$order
          turn <- combat$turn
          if (turn > 1) {
            top <- c(turn:nrow(battle))
            bot <- c(1:(turn - 1))
            battle <- battle[c(top, bot), ]
          }

          # battle$health <- battle$HP/battle$maxHP
          display <- data.frame(
            player = battle$name,
            iniciative = battle$iniciative,
            HP = battle$HP / battle$maxHP
          )
          maks <- data.frame(
            player = 1,
            iniciative = 8000,
            HP = 1
          )
          # display <- rbind(display, maks)
          # battle[,c(5,3,6)]

          subset_df <- function(m) {
            formattable(display[m, ], list(
              HP = x ~ color_bar("red", 0.2)(maks$HP)[m]
            ))
          }

          display$HP <- percent(display$HP)
          myFormattable <- formattable(display, list(
            HP = color_bar("red")
          ))

          output$order <- renderFormattable(myFormattable)
        }
      } else {
        combat$re <- 0
      }
    }
  })

  ### end my turn ###
  observeEvent(input$end, {
    key <- combat$key
    print("next_turn")
    # print(combat$turn)
    combat$re <- 0

    if (combat$redy > 1) {
      if ((combat$redy == combat$turn) & (combat$redy > 0)) {
        # print("time_updata")
        rund <- combat$round + 1
        combat$turn <- 1
        turn <- 1

        try(
          put(
            x = turn,
            projectURL = DATABASE_URL,
            directory = paste0("combat/", combat$key, "/turn"),
            token = "none"
          ),
          silent = TRUE
        )
        try(
          put(
            x = rund,
            projectURL = DATABASE_URL,
            directory = paste0("combat/", combat$key, "/round"),
            token = "none"
          ),
          silent = TRUE
        )
      } else {
        turn <- combat$turn + 1
        # print(turn)
        try(put(x = turn, projectURL = DATABASE_URL, directory = paste0("combat/", key, "/turn"), token = "none")) # , silent=TRUE)
      }
    } else {
      output$butl <- renderText("You are alone in combat ...")
    }
  })

  ### render the right UI ###
  observe({
    # print(paste("ui_int", "1"))
    Char <- char()
    user <- user_r()

    if (combat$me == 0) {
      # print(paste("ui_int", "pace"))
      output$initiative_in <- renderUI({
        box(
          title = "Iniciative",
          solidHeader = TRUE,
          textInput("instance",
            "Instance key:",
            width = "180px",
            placeholder = "NeUstrasniPustolovci"
          ),
          numericInput("initial_val",
            "Enter your intiative (+dex)",
            value = 10,
            width = "120px"
          ),
          actionButton("fight", "Fight!"),
          textOutput("loged")
        )
      })
    } else {
      # print(paste("ui_int", "combat"))

      e <- combat$order
      turn <- combat$turn
      re <- combat$re

      # print(re)
      # print(paste("clas_of_re:",class(re)))
      # print(Char)
      # print(e)

      e <- e[e$action == turn, ]

      # print(e)
      # print(paste("chack_if:",combat$re == 1))

      if (!is.null(e)) {
        if (re == 1) {
          # print("OK")
          # print(paste("chack_if:", user$localId == e$player))

          if (user$localId == e$player) {
            # print("ui_myturn")
            output$initiative_in <- renderUI({
              box(
                title = "Iniciative",
                solidHeader = TRUE,
                h3("It's my turn!"),
                actionButton("end", "End my turn"),
                actionButton("leave", "Leave combat"),
                textOutput("butl")
              )
            })
          } else {
            # print("ui_notmyturn")
            output$initiative_in <- renderUI({
              box(
                title = "Iniciative",
                solidHeader = TRUE,
                h3("Wating for your turn ...")
              )
            })
          }
        }
      }
    }
  })

  ### leave combat ###
  observeEvent(input$leave, {
    # print("Leave!")
    Char <- char()
    user <- user_r()
    key <- combat$key
    battle <- combat$order
    turn <- combat$turn

    if (combat$re == 1) {
      combat$re <- 0
      redy <- combat$redy - 1
      combat$me <- combat$me - 1

      try(put(
        x = redy,
        projectURL = DATABASE_URL,
        directory = paste0("combat/", key, "/redy"),
        token = "none"
      ),
      silent = TRUE
      )

      rund <- 0

      if (redy == 0) {
        try(
          delete(
            x = turn,
            projectURL = DATABASE_URL,
            directory = paste0("combat/", key, "/turn"),
            token = "none"
          ),
          silent = TRUE
        )
        try(
          delete(
            x = redy,
            projectURL = DATABASE_URL,
            directory = paste0("combat/", key, "/redy"),
            token = "none"
          ),
          silent = TRUE
        )
        try(
          delete(
            x = rund,
            projectURL = DATABASE_URL,
            directory = paste0("combat/", key, "/round"),
            token = "none"
          ),
          silent = TRUE
        )
        try(
          delete(
            x = rund,
            projectURL = DATABASE_URL,
            directory = paste0("combat/", key, "/battle"),
            token = "none"
          )
        )
        try(
          delete(
            x = 1,
            projectURL = DATABASE_URL,
            directory = paste0("combat/", key, "/locked"),
            token = "none"
          )
        )

        instance <- ""
      } else {
        battle_old <- battle
        q <- turn == battle$action

        # print(q)
        # print(battle)

        battle <- battle[!q, ]

        # print(battle)

        battle <- battle[order(battle$iniciative, decreasing = TRUE), ]
        battle$action <- c(1:nrow(battle))

        # print(battle)

        put(
          x = battle,
          projectURL = DATABASE_URL,
          directory = paste0("combat/", key, "/battle"),
          token = "none"
        )

        instance <- key

        if (combat$turn == nrow(battle_old)) {
          # print("turn_deleted")
          turn <- combat$turn - 1

          try(
            put(
              x = turn,
              projectURL = DATABASE_URL,
              directory = paste0("combat/", key, "/turn"),
              token = "none"
            ),
            silent = TRUE
          )
        }
      }


      combat <- combat$me

      # print(paste("combatMe", combat))
      # print(paste("instance", instance))

      status <- data.frame(combat, instance)
      status <- as.list(status[, 1:ncol(status)])

      put(
        x = status,
        projectURL = DATABASE_URL,
        directory = paste0("users/", user$localId, "/", Char, "/status"),
        token = "none"
      )

      combat$re <- 0
    }
  })

  #### the DM ##################################################################

  ### render UI for DM ###
  observe({
    user <- user_r()
    char <- char()
    DM <- is.DM()

    # print(user$localId)
    # print(char)

    if (DM == 1) {
      print("DM is online")
      output$DM_action <- renderUI({
        uiOutput("DM_next")
      })

      output$DM_combat <- renderUI({
        box(
          "Iniciatives",
          textInput("instance_DM",
            "Instance key:",
            width = "180px",
            placeholder = "NeUstrasniPustolovci"
          ),
          textInput("DM_monster",
            "Monster name",
            width = "180px",
            placeholder = "Troll"
          ),
          numericInput("DM_inic",
            "Enter your intiative (+dex)",
            value = 10,
            width = "120px"
          ),
          numericInput("monster_h",
            "enter monster HP",
            value = 40,
            width = "120px",
            min = 0
          ),
          actionButton("DM_fight", "Add monster!"),
          actionButton("lock", "Lock this combat grup!"),
          actionButton("unlock", "UnLock this combat grup!"),
          actionButton("d_com", "Dissolve combat group!"),
          textOutput("DM_add")
        )
      })
    }
  })

  ### Lock the combat group ###
  observeEvent(input$lock, {
    key <- input$instance_DM
    try(
      put(
        x = 1,
        projectURL = DATABASE_URL,
        directory = paste0("combat/", key, "/locked"),
        token = "none"
      ),
      silent = TRUE
    )

    output$DM_add <- renderText(
      paste("The combat group", key, "has been unlocked")
    )
  })

  ### Unlock the combat group ###
  observeEvent(input$unlock, {
    key <- input$instance_DM
    try(
      put(
        x = 0,
        projectURL = DATABASE_URL,
        directory = paste0("combat/", key, "/locked"),
        token = "none"
      ),
      silent = TRUE
    )
    output$DM_add <- renderText(
      paste("The combat group", key, "has been unlocked")
    )
  })

  ### Add monster to instance ###
  observeEvent(input$DM_fight, {
    key <- input$instance_DM

    combat$lock <- try(
      download(
        projectURL = DATABASE_URL,
        paste0("combat/", key, "/locked"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      ),
      silent = TRUE
    )
    locked <- combat$lock
    # print(locked)
    # print(combat$lock != 0)
    # print(class(combat$lock) != "NULL")

    if (combat$lock == 0 || class(combat$lock) == "NULL") {
      if (combat$me == 0 || combat$re == 1) {
        # print(paste("fight", "ok", "DM"))
        user <- user_r()
        char <- char()
        # print(user$localId)
        # print(char)
        intv <- input$DM_inic
        monster <- input$DM_monster
        helth <- input$monster_h

        battle <- try(
          download(
            projectURL = DATABASE_URL,
            paste0("combat/", key, "/battle"),
            secretKey = "none",
            token = "none",
            isClass = FALSE
          )
        )

        output$DM_add <- renderText("Your initiative has been loged")
        redy <- 1

        action <- 1
        entery <- data.frame(user$localId, monster, intv, action, helth, helth)
        names(entery) <- c("player", "name", "iniciative", "action", "maxHP", "HP")

        if (class(battle) == "NULL") {
          try(
            put(
              x = redy,
              projectURL = DATABASE_URL,
              directory = paste0("combat/", key, "/redy"),
              token = "none"
            ),
            silent = TRUE
          )
          try(
            put(
              x = 1,
              projectURL = DATABASE_URL,
              directory = paste0("combat/", key, "/turn"),
              token = "none"
            ),
            silent = TRUE
          )
          try(
            put(
              x = 1,
              projectURL = DATABASE_URL,
              directory = paste0("combat/", key, "/round"),
              token = "none"
            ),
            silent = TRUE
          )
          try(
            put(
              x = 0,
              projectURL = DATABASE_URL,
              directory = paste0("combat/", key, "/locked"),
              token = "none"
            ),
            silent = TRUE
          )

          # print(entery)
          combat$order <- entery
        } else {
          redy_old <- download(
            projectURL = DATABASE_URL,
            paste0("combat/", key, "/redy"),
            secretKey = "none",
            token = "none",
            isClass = FALSE
          )
          redy <- redy_old + redy
          try(
            put(
              x = redy,
              projectURL = DATABASE_URL,
              directory = paste0("combat/", key, "/redy"),
              token = "none"
            ),
            silent = TRUE
          )
          combat$turn <- try(
            download(
              projectURL = DATABASE_URL,
              paste0("combat/", key, "/turn"),
              secretKey = "none",
              token = "none",
              isClass = FALSE
            )
          )
          combat$round <- try(
            download(
              projectURL = DATABASE_URL,
              paste0("combat/", key, "/round"),
              secretKey = "none",
              token = "none",
              isClass = FALSE
            ),
            silent = TRUE
          )

          entery <- rbind(battle, entery)
          entery <- entery[order(entery$iniciative, decreasing = TRUE), ]
          entery$action <- c(1:nrow(entery))

          y <- combat$turn_old
          # print(paste("combat_old_enter_combat:", y))
        }

        put(
          x = entery,
          projectURL = DATABASE_URL,
          directory = paste0("combat/", key, "/battle"),
          token = "none"
        )

        combat$redy <- redy

        me <- combat$me
        # print(me)
        combat$me <- combat$me + 1
        old(1)

        combat <- combat$me
        # print(combat)
        instance <- key
        status <- data.frame(combat, instance)
        status <- as.list(status[, 1:ncol(status)])
        put(
          x = status,
          projectURL = DATABASE_URL,
          directory = paste0("users/", user$localId, "/", char, "/status"),
          token = "none"
        )

        combat$re <- 0
      }
    } else {
      output$DM_add <- renderText("Warning: the combat group is locket")
    }
  })

  ### DAMAGE ###
  observe({
    updateProgressBar(
      session = session,
      id = "HP_com",
      value = info$HP,
      total = info$HPmax,
      title = "Hit points",
      status = "custom_H"
    )
  })

  ### recive damage or healing ###
  observeEvent(input$take, {
    user <- user_r()
    char <- char()
    DM <- is.DM()
    damage <- input$damage
    healing <- input$healing

    if (DM == 1) {
      # print("DM_monster")

      if (combat$me > 0) {
        print("Monster is in combat")
        selected <- input$DMselect
        tabel <- combat$order

        mine <- tabel[selected == tabel$name, ]

        if (damage > 0) {
          if ((mine$HP - damage) < 0) {
            mine$HP <- 0
          } else {
            mine$HP <- mine$HP - damage
          }
        }

        if (healing > 0) {

          # print((mine$HP + healing))
          # print((mine$HP + healing)> mine$HPmax)

          if ((mine$HP + healing) > mine$maxHP) {
            max <- mine$maxHP
            mine$HP <- max
          } else {
            mine$HP <- mine$HP + healing
          }
        }

        HP <- mine$HP
        me <- which(tabel$name == selected) - 1

        try(
          put(
            x = HP,
            projectURL = DATABASE_URL,
            directory = paste0("combat/", combat$key, "/battle/", me, "/HP"),
            token = "none"
          ),
          silent = TRUE
        )

        updateNumericInput(
          session = session,
          "damage",
          "Damage taken:",
          value = 0,
          min = 0
        )
        updateNumericInput(
          session = session,
          "healing",
          "Healing received:",
          value = 0,
          min = 0
        )

        updateProgressBar(
          session = session,
          id = "HP_com",
          value = HP,
          total = mine$maxHP,
          title = mine$name,
          status = "custom_H"
        )
      }
    } else {
      if (damage > 0) {
        if ((info$HP - damage) < 0) {
          info$HP <- 0
        } else {
          info$HP <- info$HP - damage
        }
      }

      if (healing > 0) {
        if ((info$HP + healing) > info$HPmax) {
          max <- info$HPmax
          info$HP <- max
        } else {
          info$HP <- info$HP + healing
        }
      }

      HP <- info$HP
      print(paste(char, "has taken", damage, "damge!"))
      print(paste(char, "has recived", healing, "healing!"))
      # print(paste(char, "HP is now:", HP))

      if (combat$me > 0) {
        tabel <- combat$order
        me <- which(tabel$name == char) - 1

        try(
          put(
            x = HP,
            projectURL = DATABASE_URL,
            directory = paste0("combat/", combat$key, "/battle/", me, "/HP"),
            token = "none"
          ),
          silent = TRUE
        )
      }
      updateNumericInput(
        session = session,
        "damage",
        "Damage taken:",
        value = 0,
        min = 0
      )
      updateNumericInput(
        session = session,
        "healing",
        "Healing received:",
        value = 0,
        min = 0
      )

      try(
        put(
          x = HP,
          projectURL = DATABASE_URL,
          directory = paste0("users/", user$localId, "/", char, "/info/HP"),
          token = "none"
        ),
        silent = TRUE
      )
    }
  })

  ### DM damage ####
  observe({
    user <- user_r()
    char <- char()
    DM <- is.DM()
    # print(user$localId)
    # print(char)

    if (DM == 1) {
      if (combat$re == 1) {
        tabel <- combat$order
        output$DM_health <- renderUI({
          box(
            title = "DM select character",
            selectInput("DMselect", "", choices = tabel$name),
            # actionButton("DMref", "Refresh"),
            actionButton("DMgetData", "Get data")
          )
        })
      }
    }
  })

  observe({
    if (is.DM() == 1) {
      selected <- input$DMselect
      tabel <- combat$order
      mine <- tabel[selected == tabel$name, ]

      updateProgressBar(
        session = session,
        id = "HP_com",
        value = mine$HP,
        total = mine$maxHP,
        title = mine$name,
        status = "custom_H"
      )
    }
  })

  ### disolve combat group ###
  observeEvent(input$d_com, {
    user <- user_r()
    key <- combat$key

    battle_group <- try(
      download(
        projectURL = DATABASE_URL,
        paste0("combat/battle"),
        secretKey = "none",
        token = "none",
        isClass = FALSE
      )
    )

    print("battle group downloaded")

    for (i in c(1:nrow(battle_group))) {
      r <- battle_group[i, ]

      if (r$player %in% DMs) {
        print(paste0(r$player, " is a DM"))

        cha_list <- try(
          download(
            projectURL = DATABASE_URL,
            paste0("users/", user$localId, "/characters"),
            secretKey = "none",
            token = "none",
            isClass = FALSE
          )
        )

        df <- data.frame(matrix(unlist(cha_list), nrow = length(cha_list), byrow = T))
        df <- na.omit(df)
        vec <- as.character(df[, 1])

        if (r$name %in% vec) {
          print(paste0("... but plays a pc"))

          try(
            put(
              x = 0,
              projectURL = DATABASE_URL,
              directory = paste0("users/", r$player, "/", r$name, "/status/combat"),
              token = "none"
            ),
            silent = TRUE
          )
          try(
            put(
              x = "",
              projectURL = DATABASE_URL,
              directory = paste0("users/", r$player, "/", r$name, "/status/instance"),
              token = "none"
            ),
            silent = TRUE
          )
        } else {
          print(paste0("... and plays a monster"))

          try(
            put(
              x = 0,
              projectURL = DATABASE_URL,
              directory = paste0("users/", r$player, "/DM/status/combat"),
              token = "none"
            ),
            silent = TRUE
          )
          try(
            put(
              x = "",
              projectURL = DATABASE_URL,
              directory = paste0("users/", r$player, "/DM/status/instance"),
              token = "none"
            ),
            silent = TRUE
          )
        }
      } else {
        print(paste0(r$player, " is pc"))

        try(
          put(
            x = 0,
            projectURL = DATABASE_URL,
            directory = paste0("users/", r$player, "/", r$name, "/status/combat"),
            token = "none"
          ),
          silent = TRUE
        )
        try(
          put(
            x = "",
            projectURL = DATABASE_URL,
            directory = paste0("users/", r$player, "/", r$name, "/status/instance"),
            token = "none"
          ),
          silent = TRUE
        )
      }
      print(paste0(r$name, " is out!"))
    }
    try(
      delete(
        x = 1,
        projectURL = DATABASE_URL,
        directory = paste0("combat/", key, "/turn"),
        token = "none"
      ),
      silent = TRUE
    )
    try(
      delete(
        x = 1,
        projectURL = DATABASE_URL,
        directory = paste0("combat/", key, "/redy"),
        token = "none"
      ),
      silent = TRUE
    )
    try(
      delete(
        x = 1,
        projectURL = DATABASE_URL,
        directory = paste0("combat/", key, "/round"),
        token = "none"
      ),
      silent = TRUE
    )
    try(
      delete(
        x = 1,
        projectURL = DATABASE_URL,
        directory = paste0("combat/", key, "/battle"), token = "none"
      )
    )
    try(
      delete(
        x = 1,
        projectURL = DATABASE_URL,
        directory = paste0("combat/", key, "/locked"),
        token = "none"
      )
    )
  })
})
