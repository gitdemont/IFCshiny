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

# this observer is highly important since it controls operation allowed depending on the tab
# this is done be showing / hiding input + resuming / suspending observers
observeEvent(input$navbar,{
  reinit_managers(c("reg","pop","img","graph"))
  enable(id = "file_input_ctn")
  hideElement("local_file_ctn")
  hideElement("browser_file_ctn")
  hideElement("cells")
  hideElement("example")
  hideElement("network")
  hideElement("plot")
  hideElement("stats")
  hideElement("infos_save")
  hideElement("logs_save")
  hideElement("example_save")
  hideElement("network_save")
  hideElement("cells_save")
  hideElement("ML_side_inputs")
  hideElement("ML_side_training")
  hideElement("graph_save")
  hideElement("stats_save")
  hideElement("features_save")
  hideElement("report_side_inputs")
  hideElement("batch_side_inputs")
  # compensation is under development
  if(all(file.exists(file.path(.rundir, c("server_compensation.R", "server_navbar_compensation.R"))))) {
    hideElement("comp_side_inputs")
    lapply(obs_comp, FUN = function(x) x$suspend())
  }
  if(length(input$shape_selected) != 0) {
    mess_global(title = "Region Edition", msg = "current action has cancelled region edition", type = "warning", duration = 10)
    runjs(code = "Shiny.onInputChange('shape_selected', null)")
    set_tool("init")
  }
  if(plot_react$action == "drawing") {
    mess_global(title = "Region Creation", msg = "current action has cancelled region creation", type = "warning", duration = 10)
    runjs(code = "Shiny.onInputChange('shape_selected', null)")
    set_tool("init")
  }
  lapply(obs_plot, FUN = function(x) x$suspend())
  lapply(obs_report, FUN = function(x) x$suspend())
  lapply(obs_cells, FUN = function(x) x$suspend())
  lapply(obs_ML, FUN = function(x) x$suspend())
  lapply(obs_batch, FUN = function(x) x$suspend())
  lapply(obs_batch_report, FUN = function(x) x$suspend())
  updateTabsetPanel(session = session, "navbar_ML", selected = "ML_inputs")
  runjs(code = "$('#navbar_ML [data-value=\"ML_inputs\"]').trigger('click');")
  # updateTabsetPanel(session = session, "navbar_batch", selected = "Violin")
  # runjs(code = "$('#navbar_batch [data-value=\"Violin\"]').trigger('click');")
  switch(input$navbar,
         "tab0" = {
           add_log("infos")
           if(input$use_example) disable(id = "file_input_ctn")
           if(input$use_example) showElement("example_save")
           showElement("info")
           showElement("example")
           hideElement("population")
           if(.access_fs) {
             showElement("local_file_ctn")
           } else {
             showElement("browser_file_ctn")
           }
           if(length(obj_react$back$info$in_use) != 0) showElement("infos_save")
         } ,"tab1" = {
           add_log("cells")
           lapply(obs_cells, FUN = function(x) x$resume())
           showElement("population")
           showElement("cells")
           showElement("cells_save")
         }, "tab2" = {
           add_log("network")
           hideElement("population")
           showElement("network")
           showElement("network_save")
         }, "tab3" = {
           add_log("graphs")
           lapply(obs_plot, FUN = function(x) x$resume())
           hideElement("population")
           showElement("plot")
           showElement("graph_save")
           if(input$plot_type == "3D") runjs(code = "$('#plot_3D')[0].rglinstance.drawScene()")
         }, "tab4" = {
           add_log("stats")
           hideElement("population")
           showElement("stats")
           showElement("features_save")
           showElement("stats_save")
         }, "tab5" = {
           add_log("ML")
           if(!requireNamespace("bestNormalize", quietly = TRUE)) { 
             msg_react$queue = c(msg_react$queue, "bestNormalize")
           } else {
             enable("features_har")
           }
           if(!requireNamespace("Rtsne", quietly = TRUE)) msg_react$queue = c(msg_react$queue, "tsne")
           if(!requireNamespace("umap", quietly = TRUE)) msg_react$queue = c(msg_react$queue, "umap")
           if(!requireNamespace("EmbedSOM", quietly = TRUE)) msg_react$queue = c(msg_react$queue, "som")
           if(!requireNamespace("mclust", quietly = TRUE)) msg_react$queue = c(msg_react$queue, "em")
           if(!requireNamespace("e1071", quietly = TRUE)) msg_react$queue = c(msg_react$queue, "svm")
           if(!requireNamespace("xgboost", quietly = TRUE)) msg_react$queue = c(msg_react$queue, "xgb")
           if(!requireNamespace("MASS", quietly = TRUE)) msg_react$queue = c(msg_react$queue, "lda")
           updateSelectInput(session = session, inputId = "msg_once", choices  = msg_react$queue, selected = msg_react$queue)
           lapply(obs_ML, FUN = function(x) x$resume())
           hideElement("population")
           showElement("ML_side_inputs")
         }, "tab6" = {
           # disable(selector = "#navbar a")
           add_log("report")
           mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Loading Report", reset = FALSE)
           lapply(obs_report, FUN = function(x) x$resume())
           hideElement("population")
           showElement("report_side_inputs")
           runjs(sprintf("Shiny.onInputChange('report_draw', %i)", input$report_draw + 1L))
         }, "tab7" = {
           add_log("batch")
           if(.access_fs) {
             showElement("local_file_batch_ctn")
           } else {
             showElement("browser_file_batch_ctn")
           }
           lapply(obs_batch, FUN = function(x) x$resume())
           N = unname(sapply(obj_react$batch, FUN = function(b) basename(b$fileName)))
           if(length(N) == 0) N = list()
           sel = input$file_main;
           if((length(N) == 0) || !any(N %in% input$file_main)) {
             id1 = random_name(special = NULL)
             file_b = paste0(id1, " _ ", basename(obj_react$obj$fileName))
             # place files in batch_raw dir
             dir.create(file.path(session_dir, "batch_raw"), showWarnings = FALSE)
             # define new names
             new_names = file.path(session_dir, "batch_raw", file_b)
             # copy input$file_batch
             file.copy(from = obj_react$obj$fileName, to = new_names)
             
             obj_react$obj$fileName <- new_names
             obj_react$back <- obj_react$obj
             obj_react$curr = id1
             obj_react$batch = list(obj_react$obj)
             names(obj_react$batch) = id1
             N = file_b
             sel = N
             hideElement("batch_save")
           } else {
             showElement("batch_save")
           }
           showElement("batch_side_inputs")
           updateSelectInput(session = session, inputId = "file_main", choices = N, selected = sel)
         }, "tab8" = {
           showElement("logs_save")
         }, "tab9" = {
           # compensation is under development
           msg_react$queue = c(msg_react$queue, "comp_dev")
           updateSelectInput(session = session, inputId = "msg_once", choices = msg_react$queue, selected = msg_react$queue)
           if(all(file.exists(file.path(.rundir, c("server_compensation.R", "server_navbar_compensation.R"))))) source(file.path(.rundir, "server_navbar_compensation.R"), local = TRUE)$value
         })
})