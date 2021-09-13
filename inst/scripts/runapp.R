################################################################################
# This file is released under the GNU General Public License, Version 3, GPL-3 #
# Copyright (C) 2021 Yohann Demont                                             #
#                                                                              #
# It is part of IFCshiny package, please cite:                                 #
#  -IFCshiny: An R Interactive Shiny Application for the Analysis of Imaging   #
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

#' @title Run Application in Current Directory
#' @description
#' Function to run application
#' @param ... arguments to be passed. 
#' Arguments will be first passed to shiny::runApp function.\cr
#' Then, remaining named arguments will be passed as current evaluated environment variables to 'app'.
#' @examples 
#' if(interactive()) run_my_app()
#' @return it invisibly returns an object representing the app. See ?shiny::shinyApp.
#' @keywords internal
runapp <- function(...) {
  dots <- list(...)
  old <- getwd()
  on.exit(setwd(old))
  .rundir = old
  
  .runfiles <- lapply(sys.frames(), function(x) x$ofile)
  .runfiles <- .runfiles[sapply(.runfiles, length) != 0]

  if("appDir" %in% names(dots)) {
    .rundir = dots$appDir
  } else {
    if(length(.runfiles) != 0) {
      .rundir = dirname(.runfiles[[length(.runfiles)]])
    } else {
      if("file" %in% names(dots)) {
        .rundir = dirname(dots$file)
      } 
    }
  }
  setwd(.rundir)
  
  if("port" %in% names(dots)) { dots[["port"]] <- as.integer(dots[["port"]]) }
  if("quiet" %in% names(dots)) { dots[["quiet"]] <- as.logical(dots[["quiet"]]) }
  if("launch.browser" %in% names(dots)) { dots[["launch.browser"]] <- as.logical(dots[["launch.browser"]]) }
  if("test.mode" %in% names(dots)) { dots[["test.mode"]] <- as.logical(dots[["test.mode"]]) }
  
  runApp_ <- names(dots) %in% setdiff(methods::formalArgs(shiny::runApp), "appDir")
  commandArgs_ <- names(dots) %in% c("appDir", "", "log", methods::formalArgs(R.utils::commandArgs))
  extra <- dots[!(runApp_ | commandArgs_)]
  runApp_args <- dots[runApp_]
  
  files <- normalizePath(list.files(full.names = TRUE))
  files <- files[basename(files) %in% c("ui.R", "server.R", "global.R")]
  if(!all(c("ui.R", "server.R") %in% basename(files))) stop("can't find app")
  
  # create dots variables
  if(".path_to_db" %in% names(extra)) { extra[[".path_to_db"]] <- normalizePath(extra[[".path_to_db"]])}
  if(".path_to_python" %in% names(extra)) { extra[[".path_to_python"]] <- normalizePath(extra[[".path_to_python"]]) }
  if(".max_time" %in% names(extra)) { extra[[".max_time"]] <- as.integer(extra[[".max_time"]])}
  if(".max_users" %in% names(extra)) { extra[[".max_users"]] <- as.integer(extra[[".max_users"]]) }
  if(".max_cores" %in% names(extra)) { extra[[".max_cores"]] <- as.integer(extra[[".max_cores"]]) }
  if(".only_once" %in% names(extra)) { extra[[".only_once"]] <- as.logical(extra[[".only_once"]]) }
  for(i in names(extra)) assign(i, value = extra[[i]])

  # source application files
  ui <- NULL
  server <- NULL
  for(i in files) source(file = i, local = TRUE, chdir = TRUE, echo = FALSE, verbose = FALSE)
  
  # launch the app
  dolog = FALSE
  if((length(as.logical(dots$log)) == 1) && as.logical(dots$log)) {
	logfile = file.path(getwd(), paste0("LOG_", basename(getwd()), "_", format(Sys.time(), "[%Y-%m-%d %H-%M-%S]"),".txt"))
	dolog = file.create(logfile)
	if(dolog) sink(logfile, split = TRUE)
  }
  tryCatch({
    cat("\nStarting App\n")
    shiny::shinyApp(ui=ui, server=server, options = runApp_args)
  }, error = function(e) print(e), finally = { if(dolog) sink() } )
}

if(requireNamespace("R.utils", quietly = TRUE)) {
  do.call(what = runapp, args = R.utils::commandArgs(trailingOnly = FALSE, asValues = TRUE, excludeReserved=FALSE, unique = TRUE, excludeEnvVars = TRUE,
                                                     default = list(host="0.0.0.0", quiet=TRUE, launch.browser=FALSE)))
} else {
  cat("R.utils package is required to use this script. Please type install.package('R.utils')")
}