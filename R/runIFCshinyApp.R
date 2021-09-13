################################################################################
# This file is released under the GNU General Public License, Version 3, GPL-3 #
# Copyright (C) 2021 Yohann Demont                                             #
#                                                                              #
# It is part of IFCshiny package, please cite:                                 #
#  -IFCshiny: Interactive Open-source Application for the Analysis of Imaging  #
#             and Conventional Flow Cytometry                                  #
#  -YEAR: 2021                                                                 #
#  -COPYRIGHT HOLDERS: Yohann Demont, Jean-Pierre Marolleau, Loïc Garçon,      #
#                      CHU Amiens                                              #
#                                                                              #
# DISCLAIMER:                                                                  #
#  -You are using this package on your own risk!                               #
#  -We do not guarantee privacy nor confidentiality.                           #
#  -This program is distributed in the hope that it will be useful, but WITHOUT#
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        #
# FITNESS FOR A PARTICULAR PURPOSE. In no event shall the copyright holders or #
# contributors be liable for any direct, indirect, incidental, special,        #
# exemplary, or consequential damages (including, but not limited to,          #
# procurement of substitute goods or services; loss of use, data, or profits;  #
# or business interruption) however caused and on any theory of liability,     #
# whether in contract, strict liability, or tort (including negligence or      #
# otherwise) arising in any way out of the use of this software, even if       #
# advised of the possibility of such damage.                                   #
#                                                                              #
# You should have received a copy of the GNU General Public License            #
# along with IFCshiny. If not, see <http://www.gnu.org/licenses/>.             #
################################################################################

#' @title Run an IFCshiny Application
#' @description
#' Function to run IFCshiny application
#' @param app an app name from IFCshiny package. Default is "IFCshiny".
#' @param host IPv4 address that the application should listen on. Defaults is "127.0.0.1".
#' @param launch.browser whether to run the app and launch default system browser. Default is NULL.\cr
#' If set to NULL and in Rstudio, the app will be run using and displayed in Rstudio's viewer.\cr
#' If set to TRUE, the app will be run and the default system browser will be launched.\cr
#' If set to FALSE, the app will be run but no browser will be launched.\cr
#' @param local TRUE, FALSE or an environment, determining where the application files should be sourced. Default is TRUE.
#' @param ... Other arguments to be passed. 
#' Arguments will be first passed to runApp function.\cr
#' Then, remaining named arguments will be passed as global variable to 'app'.
#' @examples 
#' if(interactive()) runIFCshinyApp()
#' @details it will source application files in environment set by `local` and then use shiny::shinyApp
#' @return it invisibly returns an object representing the app. See ?shiny::shinyApp.
#' @export
runIFCshinyApp <- function(app="IFCshiny", host="127.0.0.1", launch.browser=NULL, local=TRUE,  ...) {
  dots <- list(...)
  
  tmp <- names(dots) %in% setdiff(methods::formalArgs(runIFCshinyApp), "appDir")
  extra <- dots[!tmp]
  runApp_args <- c(list(host=host, launch.browser=launch.browser), dots[tmp])

  # locate all the shiny app that exist
  alw_apps <- list.files(system.file(package = "IFCshiny", "Apps"))

  # if an invalid name is given, throw an error
  if((length(app) != 1) || !app %in% alw_apps) stop(paste0(app, " is not allowed.","\nAllowed apps are:\n\t-", paste0(alw_apps, collapse = "\n\t-")), call. = FALSE)

  files <- list.files(system.file(package = "IFCshiny", "Apps", app), full.names = TRUE)
  files <- files[basename(files) %in% c("ui.R", "server.R", "global.R")]

  # use ... to assign values in environment set by local
  if(typeof(local) == "environment") {
    if(local == FALSE) for(i in names(extra)) assign(i, value = dots[[i]], envir = local)
  } else {
    if(is.logical(local)) {
      if(local) {
        for(i in names(extra)) assign(i, value = dots[[i]])
      } else {
        for(i in names(extra)) assign(i, value = dots[[i]], envir = globalenv())
      }
    } else {
      stop("'local' should be either TRUE, FALSE or an environment")
    }
  }

  # source application files
  ui <- NULL
  server <- NULL
  for(i in files) source(file = i, local = local, chdir = TRUE, echo = FALSE, verbose = FALSE)

  tryCatch({
    if(length(launch.browser)==0) runApp_args = runApp_args[names(runApp_args) != "launch.browser"]
    app <- shinyApp(ui=ui, server=server, options = runApp_args)
    runApp(app)
  }, error = function(e) print(e$message))
}

#' @title Run Only One Instance of IFCshiny Application
#' @description
#' Function to run IFCshiny application
#' @param options options to pass to shiny::shinyApp.
#' @return it invisibly returns an object representing the app. See ?shiny::shinyApp.
#' @export
runIFCshinyAppOnce <- function(options = list()) {
  dots <- options
  
  tmp <- names(dots) %in% setdiff(methods::formalArgs(IFCshiny::runIFCshinyApp), "appDir")
  extra <- dots[tmp]
  runApp_args <- dots[tmp]
  app = "IFCshiny"
  
  # locate all the shiny app that exist
  alw_apps <- list.files(system.file(package = "IFCshiny", "Apps"))
  
  # if an invalid name is given, throw an error
  if((length(app) != 1) || !app %in% alw_apps) stop(paste0(app, " is not allowed.","\nAllowed apps are:\n\t-", paste0(alw_apps, collapse = "\n\t-")), call. = FALSE)
  
  files <- list.files(system.file(package = "IFCshiny", "Apps", app), full.names = TRUE)
  files <- files[basename(files) %in% c("ui.R", "server.R", "global.R")]
  
  for(i in names(extra)) assign(i, value = dots[[i]])
  .only_once = TRUE
  
  # source application files
  ui <- NULL
  server <- NULL
  for(i in files) source(file = i, local = TRUE, chdir = TRUE, echo = FALSE, verbose = FALSE)
  
  tryCatch({
    app <- shinyApp(ui=ui, server=server, options = runApp_args)
    runApp(app)
  }, error = function(e) print(e$message))
}
