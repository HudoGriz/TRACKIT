library(shiny)
library(shinyjs)
library(DT)
library(formattable)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(fireData)
library(googleAuthR)
library(httr)
library(gargle)
library(stringi)
library(sass)


#### LOGIN ####

getOption("googleAuthR.securitycode")

source("secret.R")
source("dm.R")

o_auth_login <- function(
                         project_api,
                         request_uri = DATABASE_URL,
                         post_body,
                         return_idp_credential) {
  o_auth_login_url <- paste0(
    "https://www.googleapis.com/identitytoolkit/v3/relyingparty/verifyAssertion?key=",
    project_api
  )
  o_auth_login_data <- httr::POST(
    url = o_auth_login_url,
    body = list(
      "requestUri" = request_uri,
      "postBody" = post_body,
      "returnSecureToken" = "True",
      "returnIdpCredential" = return_idp_credential
    ),
    encode = "json"
  )
  httr::content(o_auth_login_data)
}

google_login <- function(
                         project_api,
                         web_client_id = "prompt",
                         web_client_secret = "prompt",
                         request_uri,
                         redirect_uri = httr::oauth_callback(),
                         return_idp_credential = TRUE,
                         cache = FALSE) {
  if (web_client_id == "prompt" && web_client_secret == "prompt") {
    web_client_id <- readline(prompt = "Web Client ID: ")
    web_client_secret <- readline(prompt = "Web Client Secret: ")
    print(paste0("Connecting to", project_api, ":"))
  }

  myapp <- httr::oauth_app("google",
    key = web_client_id,
    secret = web_client_secret,
    redirect_uri = redirect_uri
  )

  google_token <- httr::oauth2.0_token(httr::oauth_endpoints("google"), myapp,
    scope = "https://www.googleapis.com/auth/userinfo.profile",
    cache = cache
  )

  pbody <- paste0("id_token=", google_token$credentials$id_token, "&providerId=google.com")

  o_auth_login(
    project_api = project_api,
    request_uri = request_uri,
    post_body = pbody,
    return_idp_credential = return_idp_credential
  )
}

gar_shiny_getToken <- function(code,
                               redirect.uri,
                               client.id = getOption("googleAuthR.webapp.client_id"),
                               client.secret = getOption("googleAuthR.webapp.client_secret")) {
  gar_app <- oauth_app("google", key = client.id, secret = client.secret)

  scope_list <- getOption("googleAuthR.scopes.selected")

  req <-
    POST("https://accounts.google.com/o/oauth2/token",
      body = list(
        code = code,
        client_id = client.id,
        client_secret = client.secret,
        redirect_uri = redirect.uri,
        grant_type = "authorization_code"
      )
    )

  stopifnot(identical(
    headers(req)$`content-type`,
    "application/json; charset=utf-8"
  ))
  # content of req will contain access_token, token_type, expires_in
  token <- content(req, type = "application/json")
  # if(!is.null(token$error)){
  #   stop("Authentication error: ", token$error, token$error_description, call. = FALSE)
  # }
  # Create a Token2.0 object consistent with the token obtained from gar_auth()
  Token2.0$new(
    app = gar_app,
    endpoint = oauth_endpoints("google"),
    credentials = list(
      access_token = token$access_token,
      token_type = token$token_type,
      expires_in = token$expires_in,
      refresh_token = token$refresh_token,
      id_token = token$id_token
    ),
    params = list(
      scope = scope_list, type = NULL,
      use_oob = FALSE, as_header = TRUE
    ),
    cache_path = FALSE
  )
}

gar_shiny_getAuthUrl <-
  function(redirect.uri,
           state = getOption("googleAuthR.securitycode"),
           client.id = getOption("googleAuthR.webapp.client_id"),
           client.secret = getOption("googleAuthR.webapp.client_secret"),
           scope = getOption("googleAuthR.scopes.selected"),
           access_type = c("online", "offline"),
           approval_prompt = c("auto", "force")) {
    access_type <- match.arg(access_type)
    approval_prompt <- match.arg(approval_prompt)

    scopeEnc <- paste(scope, sep = "", collapse = " ")

    ## httr friendly version
    url <- modify_url(
      oauth_endpoints("google")$authorize,
      query = list(
        response_type = "code",
        client_id = client.id,
        redirect_uri = redirect.uri,
        scope = scopeEnc,
        state = state,
        access_type = access_type,
        approval_prompt = approval_prompt
      )
    )
    myMessage("Auth Token URL: ", url, level = 2)
    url
  }

has_auth_code <- function(pars, securityCode = getOption("googleAuthR.securitycode")) {
  if (!is.null(pars$state)) {
    if (pars$state != securityCode) {
      warning(
        "securityCode check failed in Authentication! Code:",
        pars$state,
        " Expected:",
        securityCode
      )
      return(NULL)
    }
  }

  # NULL if it isn't there
  pars$code
}

authReturnCode <- function(session,
                           securityCode = getOption("googleAuthR.securitycode")) {
  # check_package_loaded("shiny")
  pars <- shiny::parseQueryString(session$clientData$url_search)

  # NULL if it isn't there
  has_auth_code(pars, securityCode = securityCode)
}
myMessage <- function(..., level = 2) {
  compare_level <- getOption("googleAuthR.verbose", 3)

  if (level >= compare_level) {
    message(Sys.time(), "> ", ...)
  }
}
gar_shiny_getUrl <- function(session) {
  if (!is.null(session)) {
    pathname <- shiny::isolate(session$clientData$url_pathname)
    hostname <- shiny::isolate(session$clientData$url_hostname)
    port <- shiny::isolate(session$clientData$url_port)

    if (hostname == "127.0.0.1") {
      hostname <- "localhost"
    }

    url <- paste0(
      shiny::isolate(session$clientData$url_protocol),
      "//",
      hostname,
      if (port != "") paste0(":", port),
      if (pathname != "/") pathname
    )

    myMessage("Shiny URL detected as: ", url, level = 1)
    url
  } else {
    NULL
  }
}
googleAuth <- function(input, output, session,
                       login_text = "Login via Google",
                       logout_text = "Logout",
                       login_class = "btn btn-primary",
                       logout_class = "btn btn-default",
                       access_type = c("online", "offline"),
                       approval_prompt = c("auto", "force"),
                       revoke = FALSE) {
  # check_package_loaded("shiny")

  access_type <- match.arg(access_type)
  approval_prompt <- match.arg(approval_prompt)
  ns <- session$ns

  accessToken <- shiny::reactive({

    ## gets all the parameters in the URL. The auth code should be one of them.
    if (!is.null(authReturnCode(session))) {
      ## extract the authorization token
      app_url <- gar_shiny_getUrl(session)
      access_token <- gar_shiny_getToken(authReturnCode(session), app_url)

      access_token
    } else {
      NULL
    }
  })

  output$googleAuthUi <- shiny::renderUI({
    if (is.null(shiny::isolate(accessToken()))) {
      shiny::actionLink(
        ns("signed_in"),
        shiny::a(login_text,
          href = gar_shiny_getAuthUrl(gar_shiny_getUrl(session),
            access_type = access_type,
            approval_prompt = approval_prompt
          ),
          class = login_class,
          role = "button"
        )
      )
    } else {
      if (revoke) {
        logout_button <- shiny::actionButton(ns("revoke"), "Revoke Access",
          href = gar_shiny_getUrl(session),
          class = logout_class,
          role = "button"
        )
      } else {
        logout_button <- shiny::a(logout_text,
          href = gar_shiny_getUrl(session),
          class = logout_class,
          role = "button"
        )
      }

      logout_button
    }
  })

  shiny::observeEvent(input[[ns("revoke")]], {

    ## GETS the revoke URL for this user's access_token
    httr::GET(httr::modify_url("https://accounts.google.com/o/oauth2/revoke",
      query =
        list(
          token =
            shiny::isolate(access_token)$credentials$access_token
        )
    ))
    myMessage("Revoked access", level = 2)
  })

  return(accessToken)
}

#### FACTS ####

skin <- 0
food <- 0
wather <- 0
raincatc <- 0
wather_max <- 0

carry <- 80
weight_i <- 0
weight_fw <- 0
weight <- 0
space <- 0

usage_f_s <- 0.5
usage_f_n <- 1
usage_f_l <- 2
usage_w <- 2

fw <- 1
ww <- 1
ws_c <- 4

sp <- 5
fp <- 3
rp <- 2
rep <- 2

days_count <- 0

gold <- 0

redy <- 0
turn <- 1
runda <- 1

stats <- c()
info <- c()
i_in_invent <- c()

exhustion <- data.frame(Level = c(1:6), Effect = c(
  "Disadvantage on ability checks",
  "Speed halved",
  "Disadvantage on attack rolls and saving throws",
  "Hit point maximum halved",
  "Speed reduced to 0",
  "Death"
))


#### functions ####

c2 <- function(...) {
  vals <- c(...)

  if (is.null(names(vals))) {
    missing_names <- rep(TRUE, length(vals))
  } else {
    missing_names <- names(vals) == ""
  }
  if (any(missing_names)) {
    names <- vapply(substitute(list(...))[-1], deparse, character(1))
    names(vals)[missing_names] <- names[missing_names]
  }

  vals
}

index <- function(x) {
  x <- ((x - 10) / 2) %/% 1
  if (x >= 0) {
    # print("+")
    te <- paste0("+", x)
  } else {
    # print("-")
    te <- paste0(x)
  }
  return(te)
}

textInputRow <- function(inputId, label, value = "") {
  div(
    style = "display:inline-block",
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value, class = "input-small")
  )
}


#### JSON cookie ####

addResourcePath("js", "www")

jso <- list()

jso$redirect <- function(url) {
  session$sendCustomMessage("redirect", url)
}

jso$setCookie <- function(cookieType, cookie, daysTillExpire = 1, session) {
  session$sendCustomMessage("setCookie", list(
    cookieType = cookieType,
    cookie = cookie,
    days = daysTillExpire
  ))
}

jso$getCookie <- function(cookieType, session) {
  session$sendCustomMessage("getCookie", list(
    cookieType = cookieType
  ))
}

jso$removeCookie <- function(cookieType) {
  session$sendCustomMessage("removeCookie", list(
    cookieType = cookieType
  ))
}

js_redirect <- "Shiny.addCustomMessageHandler('mymessage', function(message){window.location = 'http://localhost:5555';});"