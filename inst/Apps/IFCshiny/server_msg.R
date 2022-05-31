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

# function to create logs
add_log = function(mess = ", ", dir = session_react$dir, id = session_react$id) {
  observe({
    msg = paste(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [",id,"]"), mess, sep = " ")
    # LOGS.txt
    cat(msg, sep = "\n", append = TRUE, file = file.path(dir, "LOGS.txt"))
    # stdout
    cat(msg, sep = "\n")
  })
}

# variation of shinyjs::showLog to store logs and show JS console output
showLog = function(dir = session_react$dir, id = session_react$id) {
  if(!is.null(attr(session, "shinyjs_showLog"))) return()
  attr(session, "shinyjs_showLog") <- TRUE
  insertUI("head", "beforeEnd", {
    singleton(tags$head(tags$script("(function(){if(window.ShinySenderQueue == null) return null; var oldLog=console.log;var queue=new ShinySenderQueue();console.log=function(message){if(/(discarded from buffer)/.test(message)) return null; try {queue.send(\"shinyjs-showLog\", message);} catch(err) {} oldLog.apply(console,arguments);};})();")))
  }, immediate = TRUE)
  observeEvent(session$input[["shinyjs-showLog"]], {
    # LOGS.txt
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [",session_react$id,"] JS: ",
        jsonlite::toJSON(session$input[["shinyjs-showLog"]], auto_unbox = TRUE), "\n", sep = "", append = TRUE, file = file.path(dir, "LOGS.txt"))
    # stdout
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [",session_react$id,"] JS: ",
        jsonlite::toJSON(session$input[["shinyjs-showLog"]], auto_unbox = TRUE), "\n", sep = "")
  })
}

# download logs on app error
observeEvent(once = TRUE, session_react$dir, {
  options(shiny.error = function(dir = session_react$dir, id = session_react$id) {
    uri = paste0("data:text/plain;base64,",
                 cpp_base64_encode(charToRaw(paste0(c(readLines(file.path(dir, "LOGS.txt")), geterrmessage()), collapse="\n"))))
    runjs(code = sprintf("$('#get_logs').attr('href', '%s')", uri))
    runjs(sprintf("Shiny.onInputChange('get_logs', %i)", ifelse(length(input$get_logs) == 0, 0, input$get_logs + 1L)))
    # shinyjs::click("get_logs")
  })
})

# display and log busy message
mess_busy <- function(id, ctn = paste0(id, "_ctn"), msg = "Please wait", type = "busy1", reset = FALSE, 
                      session = shiny::getDefaultReactiveDomain()) {
  if(reset) {
    session$output[[id]] <- html(id = id, add = FALSE) 
    hideElement(id = ctn, anim = FALSE, animType = "fade", time = 0)
  } else {
    add_log(sprintf("busy_msg: %s", msg))
    deco = switch(type,
                  "busy1" = c(icon = "fa fa-spinner fa-pulse fa-3x fa-fw", color = "black"),
                  "busy2" = c(icon = "fa fa-spinner fa-spin fa-3x fa-fw", color = "black"),
                  "circle" = c(icon = "fa fa-circle-o-notch fa-spin fa-3x fa-fw", color = "black"),
                  "refresh" = c(icon = "fa fa-refresh fa-spin fa-3x fa-fw", color = "black"),
                  "cog" = c(icon = "fa fa-cog fa-spin fa-3x fa-fw", color = "black"),
                  c(icon = "fa fa-spinner fa-pulse fa-3x fa-fw", color = "black")
    )
    session$output[[id]] <- html(id = id, add = FALSE,
                                 sprintf("<div style='position:absolute;top:50%%;left:0;bottom:0;right:0;display:block;align-items:center;justify-content:center;text-align:center;margin:auto;vertical-align:middle;'>
                                              <h2>%s</h2>
                                              <i class='fa %s' aria-hidden='true' style='display:inline-block;vertical-align:super;color:%s;'>
                                              </i>
                                           </div>", msg, deco[1], deco[2]))
    showElement(id = ctn, anim = FALSE, animType = "fade", time = 0)
  }
}

# display and log global message
mess_global <- function(title = NULL, msg = "", type = "warning", duration = 10, where = "toast-top-full-width") {
  # type = c("success", "error", "warning", "info")
  if(type %in% c("done", "complete" , "ok", "success")) type = "success"
  if(type %in% c("error", "stop" )) type = "error"
  if(type %in% c("info", "message", "comment", "busy")) type = "info"
  msg = sapply(msg, FUN =function(x) strsplit(x, split = "\n", fixed = TRUE)[[1]])
  add_log(sprintf("global_msg: [%s] %s", title, msg))
  showToast(type = type,
            message = paste("<br>", msg),
            title = title,
            .options = list("positionClass" = where,
                            "progressBar"= TRUE,
                            "timeOut" = duration * 1000,
                            "closeButton"= TRUE,
                            "newestOnTop"= TRUE,
                            "preventDuplicates"= TRUE,
                            # "onclick"= NULL,
                            "showDuration"= 300,
                            "hideDuration"= 300,
                            "extendedTimeOut"= duration * 1000,
                            "showEasing" = "swing",
                            "hideEasing"= "linear",
                            "showMethod"= "fadeIn",
                            "hideMethod"= "fadeOut"))
}

observeEvent(input$msg_once, ignoreInit = TRUE, {
  to_show = input$msg_once
  to_show = setdiff(to_show, msg_react$done)
  if(length(to_show) != 0) {
    # data example
    if("IFCdata" %in% to_show) mess_global(title = "package required", msg = c("'IFCdata' package is not installed", "To install 'IFCdata' package, run `install.packages('IFCdata', repos = 'https://gitdemont.github.io/IFCdata/', type = 'source')`"), type = "info", duration = 100)
    # extra feat
    if("IFCip" %in% to_show) mess_global(title = "package required", msg = c("'IFCip' (>= 0.0.6) package is required to compute extra features from images", "Features computation module has been disabled"), type = "info", duration = 100)
    # compensation
    if("comp_dev" %in% to_show) mess_global(title = "Compensation",  msg = c("Compensation is under development", "This tab is not fully functional yet"), type = "info", duration = 100)
    if("comp_img" %in% to_show) mess_global(title = "Compensation",  msg = c("A real IFC compensation will modify images and masks to create a new cif file", "This IS NOT DONE here !","Only intensities within daf will be recomputed"), type = "info", duration = 100)
    # representation
    if("viridisLite" %in% to_show) mess_global(title = "package required", msg = c("'viridisLite' package is required to get more color palettes"), type = "info", duration = 100)
    # ML
    if("bestNormalize" %in% to_show) mess_global(title = "package required", msg = c("'bestNormalize' package is required to use Yeo-Johnson transformation", "Haralick's Normalization in machine learning module has been disabled"), type = "info", duration = 100)
    if("som" %in% to_show) mess_global(title = "package required", msg = c("'EmbedSOM' package can't be found", "Please install 'EmbedSOM' to build Self Organizing Map"), type = "info", duration = 100)
    if("lda" %in% to_show) mess_global(title = "package required", msg = c("'MASS' package can't be found", "Please install 'MASS' to run Linear Discriminant Analysis"), type = "info", duration = 100)
    if("svm" %in% to_show) mess_global(title = "package required", msg = c("'e1071' package can't be found", "Please install 'e1071' to use Support Vector Machine"), type = "info", duration = 100)
    if("tsne" %in% to_show) mess_global(title = "package required", msg = c("'Rtsne' package can't be found", "Please install 'Rtsne' to perform t-Distributed Stochastic Neighbor Embedding"), type = "info", duration = 100)
    if("umap" %in% to_show) mess_global(title = "package required", msg = c("'umap' package can't be found", "Please install 'umap' to compute manifold approximation and projection"), type = "info", duration = 100)
    if("xgb" %in% to_show) mess_global(title = "package required", msg = c("'xgboost' package can't be found", "Please install 'xgboost' for eXtreme Gradient Boosting Training"), type = "info", duration = 100)
    if("em" %in% to_show) mess_global(title = "package required", msg = c("'mclust' package can't be found", "Please install 'mclsut' for Gaussian Mixture Modelling for Model-Based Clustering"), type = "info", duration = 100)
    # parallel
    if("parallel" %in% to_show) mess_global(title = "package required", msg = c("'parallel' package is required for parallelization", "Parallelization will not be available"), type = "info", duration = 100)
    if("doParallel" %in% to_show) mess_global(title = "package required", msg = c("'doParallel' package is required for parallelization", "Parallelization will not be available"), type = "info", duration = 100)
    if("foreach" %in% to_show) mess_global(title = "package required", msg = c("'foreach' package is required for parallelization", "Parallelization will not be available"), type = "info", duration = 100)
    if("shiny_multi_thread" %in% to_show) mess_global(title = "ShinyApps.io", msg = c("Using parallelization is not possible on shinyapp.io"), type = "info", duration = 100)
    msg_react$done = c(msg_react$done, input$msg_once)
    msg_react$queue = setdiff(msg_react$queue, msg_react$done)
  }
})