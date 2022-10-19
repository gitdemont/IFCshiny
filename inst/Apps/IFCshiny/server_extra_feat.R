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

# multi-thread is disabled on shinyapps.io
observeEvent(input$use_parallelization, ignoreInit = TRUE, {
  if(Sys.getenv('SHINY_PORT') != "") msg_react$queue = c(msg_react$queue, "shiny_multi_thread")
  updateSelectInput(session = session, inputId = "msg_once", choices = msg_react$queue, selected = msg_react$queue)
  if((.no_cores <= 1) || (Sys.getenv('SHINY_PORT') != "")) updateMaterialSwitch(session=session, inputId = "use_parallelization", value = FALSE)
})
# extra features computation from images with multi-thread capability
# It relies on IFCip package to extract Zernike, Hu and Haralick features
observeEvent(input$compute_go, {
  if(!requireNamespace(package = "IFCip", quietly = TRUE) ||
     packageVersion("IFCip") < "0.0.6") {
    disable(id = "compute_features")
    msg_react$queue = c(msg_react$queue, "IFCip")
    updateSelectInput(session = session, inputId = "msg_once", choices = msg_react$queue, selected = msg_react$queue)
    return(NULL)
  }
  if(input$extra_zernike) {
    zmax = na.omit(as.integer(isolate(input$zernike)))
  } else {
    zmax = -1L
  }
  if(input$extra_haralick) {
    granularity = na.omit(as.integer(isolate(input$haralick)))
  } else {
    granularity = -1L
  }
  if((length(granularity) == 0 )|| (length(zmax) == 0)) {
    mess_global(title = "computing extra features", 
                msg = c("bad value for Haralick / Zernike"), 
                type = "error", duration = 10)
    return(NULL)
  }
  add_log("compute_features")
  mess_global(title = "computing extra features", 
              msg = c("Be aware that you are creating extra features",
                      "IDEAS will not be able to use it for applying on other files"), 
              type = "warning", duration = 10)
  mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Computing extra features", reset = FALSE)
  tryCatch({
    do_par = (.no_cores > 1) && input$use_parallelization
    extra_feat <- IFCip::ExtractFeatures(fileName = obj_react$back$fileName,
                                         offsets = obj_react$back$offsets,
                                         display_progress = TRUE,
                                         parallel = (.no_cores > 1) && input$use_parallelization,
                                         zmax = zmax,
                                         granularity = granularity,
                                         session = session)
    attr(extra_feat, "channel_names") <- sapply(as.integer(attr(extra_feat, "channel_id")), FUN = function(i_chan) obj_react$obj$description$Images$name[obj_react$obj$description$Images$physicalChannel == i_chan])
    extra_feat <- IFCip::as_IFC_features(extra_feat)[[2]]
    if(getFileExt(obj_react$back$fileName) == "daf") {
      extra_feat <- lapply(extra_feat, FUN = function(x) {
        x$name = paste0(x$name, "_extra")
        return(x)
      })
    }
    # new features are added to obj_react$obj
    obj_react$obj <- data_add_features(obj_react$obj, features = extra_feat, session = session)
    # we sort the features by name
    obj_react$obj$features <- structure(obj_react$obj$features[, order(names(obj_react$obj$features))], class = c("data.frame", "IFC_features"))
    obj_react$obj$features_def <- structure(obj_react$obj$features_def[order(names(obj_react$obj$features_def))], class = c("list", "IFC_features_def"))
    # when extra features have been computed we can show new tabs (if they are not already displayed)
    showElement(selector = "#navbar [data-value='tab1']")
    showElement(selector = "#navbar [data-value='tab2']")
    showElement(selector = "#navbar [data-value='tab3']")
    showElement(selector = "#navbar [data-value='tab4']")
    showElement(selector = "#navbar [data-value='tab5']")
    # we reactualize some input
    feat_n = grep(input$pattern, names(obj_react$obj$features), perl=TRUE, ignore.case=FALSE, value=TRUE)
    if(length(feat_n) == 0) feat_n = list()
    updateSelectInput(session=session, inputId = "sel_left", choices = feat_n, selected = feat_n)
    hideElement(id = "compute_features")
    updateTabsetPanel(session = session, "navbar_ML", selected = "ML_inputs")
    runjs(code = "$('#navbar_ML [data-value=\"ML_inputs\"]').trigger('click');")
  }, error = function(e) {
    mess_global(title = "features extraction", msg = e$message, type = "stop")
  }, finally = {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "", reset = TRUE)
  })
})
