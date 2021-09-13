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

# compensation is under development
if(length(obj_react$obj$features_comp) == 0) { 
  hideElement("comp_type")
  if(length(obj_react$obj$description$FCS) == 0) {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Preparing compensation", reset = FALSE)
    mess_global(title = "Compensation", 
                msg = c("Compensation is under development",
                        "This tab is not fully functional yet",
                        "A real IFC compensation will modify images and masks to create a new cif file",
                        "This IS NOT DONE here !",
                        "only intensities within daf will be recomputed"), 
                type = "info", duration = 10)
    tryCatch({
      info = react_dat()$info
      is_intensity = unlist(sapply(obj_react$obj$features_def, FUN = function(i_feat) {
        if(i_feat$type == "single" && i_feat$userfeaturetype == "Mask and Image") {
          foo = strsplit(i_feat$def, split = "|", fixed = TRUE)[[1]]
          if((foo[1] == "Intensity") && (foo[2] == "MC")) {
            bar = info$Images$physicalChannel[foo[3] == info$Images$name]
            if(length(bar) == 0) {
              return(info$Images$physicalChannel[foo[3] == sprintf("Ch%02i", info$Images$physicalChannel)])
            } else {
              return(bar)
            }
          } else {
            return(-1)
          }
        } else {
          return(-1)
        }
      }))
      if(length(is_intensity) != 0) is_intensity = sort(is_intensity[is_intensity >= 0])
      if(length(is_intensity) != ncol(info$CrossTalkMatrix)) {
        if(requireNamespace("IFCip", quietly = TRUE)) {
          basic_feat = IFCip::ExtractBasic(info = info, offsets = obj_react$obj$offsets, removal = "MC", session = session)
          comp_feat = IFCip::as_IFC_features(basic_feat)
          obj_react$obj$basic$features = comp_feat$features
          obj_react$obj$basic$features_def = comp_feat$features_def
          obj_react$obj$features_comp = comp_feat$features[, grepl("^Intensity", names(comp_feat$features))]
          attr(basic_feat, "channel_names") <- sapply(as.integer(attr(basic_feat, "channel_id")), FUN = function(i_chan) obj_react$obj$description$Images$name[obj_react$obj$description$Images$physicalChannel == i_chan])
          extra_feat <- IFCip::as_IFC_features(basic_feat)[[2]]
          if(getFileExt(react_dat()$fileName) == "daf") {
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
          updateSelectInput(session=session, inputId = "sel_left", choices = feat_n, selected = feat_n)
          hideElement(id = "compute_features")
        }
      } else {
        obj_react$obj$features_comp = obj_react$obj$features[, names(is_intensity)]
      }
      spillover = info$CrossTalkMatrix[which(info$in_use), which(info$in_use)]
      colnames(spillover) = sprintf("Ch%02i",info$Images$physicalChannel) #info$Images$name
      rownames(spillover) = names(is_intensity)
      obj_react$obj$description$spillover = matrix(0, ncol = ncol(spillover), nrow = nrow(spillover))
      diag(obj_react$obj$description$spillover) <- 1
      colnames(obj_react$obj$description$spillover) = colnames(spillover) 
      rownames(obj_react$obj$description$spillover) = rownames(spillover)
      
      shinyjs::runjs(sprintf("Shiny.onInputChange('comp_plot_1', %i)", 0))
      shinyjs::runjs(sprintf("Shiny.onInputChange('comp_plot_2', %i)", 0))
      shinyjs::runjs(sprintf("$('#comp_table').width('%ipx')", ncol(spillover) * 50))
      comp_react$spillover = spillover
      comp_react$pre = spillover
      comp_react$back = spillover
      comp_react$last = spillover
    }, error = function(e) {
      mess_global(title = "Compensation", 
                  msg = c("Can't create features for compensation", e$message), 
                  type = "error", duration = 10)
      hideElement(selector = "#navbar [data-value='tab9']")
      runjs(code = "$('#navbar [data-value=\"tab0\"]').trigger('click');")
    }, finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "", reset = TRUE)
    })
  } else {
    spillover = obj_react$obj$description$FCS$spillover
    is_intensity = grep("^SS|^FS|^Object Number$|^HDR|Time", names(obj_react$obj$features), ignore.case = TRUE, value = TRUE, invert = TRUE)
    AHW_intensity = list(A = grep("\\b*-A\\b", is_intensity, perl = FALSE, value = TRUE),
                         H = grep("\\b*-H\\b", is_intensity, perl = FALSE, value = TRUE),
                         W = grep("\\b*-W\\b", is_intensity, perl = FALSE, value = TRUE))
    if(length(spillover) != 0) {
      is_intensity = AHW_intensity[ncol(spillover) == sapply(AHW_intensity, FUN = length)]
    }
    if(length(is_intensity) == 1) {
      is_intensity = is_intensity[[1]]
    } else {
      avl_type = c("A","H","W")[sapply(AHW_intensity, length) != 0]
      showElement("comp_type")
      showModal(modalDialog(title = "Please select type of intensity to compensate",
                            radioButtons(inputId = "comp_intensity_type",
                                         label = "type",
                                         choices = avl_type,
                                         selected = avl_type[1],
                                         inline = TRUE),
                            size = "s",
                            easyClose = FALSE,
                            footer = list(actionButton(inputId = "comp_modal_proceed",label = "Proceed"))),
                session = session)
      observeEvent(input$comp_modal_proceed, once = TRUE, ignoreNULL = TRUE, ignoreInit = TRUE, autoDestroy = TRUE, {
        
        is_intensity <- AHW_intensity[[input$comp_intensity_type]]
        spillover = matrix(0, ncol = length(is_intensity), nrow = length(is_intensity))
        diag(spillover) <- 1
        colnames(spillover) = gsub("^(.*) <.*>$", "\\1", is_intensity)
        rownames(spillover) = is_intensity
        
        obj_react$obj$description$spillover = matrix(0, ncol = ncol(spillover), nrow = nrow(spillover))
        diag(obj_react$obj$description$spillover) <- 1
        colnames(obj_react$obj$description$spillover) = colnames(spillover) 
        rownames(obj_react$obj$description$spillover) = rownames(spillover)
        
        obj_react$obj$features_comp = obj_react$obj$features[, is_intensity]
        param_react$param = list(channels = data.frame(name = is_intensity))
        
        shinyjs::runjs(sprintf("Shiny.onInputChange('comp_plot_1', %i)", 0))
        shinyjs::runjs(sprintf("Shiny.onInputChange('comp_plot_2', %i)", 0))
        shinyjs::runjs(sprintf("$('#comp_table').width('%ipx')", ncol(spillover) * 70))
        comp_react$spillover = spillover
        comp_react$pre = spillover
        comp_react$back = spillover
        comp_react$last = spillover
        
        showElement(selector = "#navbar [data-value='tab9']")
        lapply(obs_comp, FUN = function(x) x$resume())
        shinyjs::click("comp_resample")
        add_log("compensation")
        hideElement("population")
        showElement("comp_side_inputs")
        removeModal(session = session)
      })
    }
  }
} 
if(length(obj_react$obj$features_comp) != 0) {
  showElement(selector = "#navbar [data-value='tab9']")
  lapply(obs_comp, FUN = function(x) x$resume())
  shinyjs::click("comp_resample")
  add_log("compensation")
  hideElement("population")
  showElement("comp_side_inputs")
}