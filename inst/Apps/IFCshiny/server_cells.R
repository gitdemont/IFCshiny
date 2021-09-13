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

obs_cells <- list(
  observeEvent(input$cells_save_type, suspended = TRUE, {
    if(length(input$cells_save_type) == 0) return(NULL)
    if(input$cells_save_type == "npy") {
      if(!exists(".path_to_python")) {
        .path_to_python = Sys.getenv("PATH_TO_PYTHON", "")
      }
      if(.path_to_python == "") {
        reticulate::py_discover_config(required_module = "numpy")
      } else {
        reticulate::use_python(.path_to_python, required = TRUE)
      }
      .numpy_avl = try(reticulate::py_module_available("numpy"), silent = TRUE)
      if(inherits(what = "try-error", x = .numpy_avl)) {
        try(reticulate::py_install("numpy"), silent = TRUE)
        .numpy_avl = try(reticulate::py_module_available("numpy"), silent = TRUE)
        if(inherits(what = "try-error", x = .numpy_avl)) .numpy_avl = FALSE
      }
      if(!.numpy_avl) {
        disable(id = "cells_save_btn")
        mess_global("python loading", msg = "python is not available", type = "warning")
      } 
    } else {
      enable(id = "cells_save_btn")
    }
  }),
  observeEvent(input$cellclicked, suspended = TRUE,  {
    if(is.null(input$cellclicked)) return(NULL)
    # add_log(sprintf("selected object: %i", input$cellclicked))
    updateNumericInput(session = session ,inputId = "chan_cell_todisplay", value = as.numeric(input$cellclicked))
  }),
  observeEvent(input$chanclicked, suspended = TRUE, {
    if(is.null(input$chanclicked)) return(NULL)
    # add_log(sprintf("selected channel: %02i", input$chanclicked))
    param_react$param$channels$physicalChannel[param_react$param$channels$name == input$chanclicked]
    updateSelectInput(session = session ,inputId = "chan_sel", selected = sprintf("%02i", param_react$param$channels$physicalChannel[param_react$param$channels$name == input$chanclicked]))
  }),
  observeEvent(input$cells_settings ,suspended = TRUE, {
    runjs("Shiny.onInputChange('img_manager_visible', true)")
  })
)