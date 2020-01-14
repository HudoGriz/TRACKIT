# Firebase 
API_KEY <- "AIzfrewgrtegvrFSbgfdsVDFVSDvdfsVFSD"
AUTH_DOMAIN <- "appname.firebaseapp.com"
DATABASE_URL <- "https://appname.firebaseio.com"
PROJECT_ID <- "appname"

# Google
WEB_CLIENT_ID <- "32143214-fdsaf341FDSFdsfsad435435r.apps.googleusercontent.com"
WEB_CLIENT_SECRET <- "-dFDSAfdsafDSAFSD"
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))


options("googleAuthR.webapp.client_id" = WEB_CLIENT_ID)
options("googleAuthR.webapp.client_secret" = WEB_CLIENT_SECRET)
options(googleAuthR.verbose = 0)

endpoint <- paste0('https://identitytoolkit.googleapis.com/v1/accounts:signInWithCustomToken?key=',API_KEY)
drive_link <- "yourDrive_link"
