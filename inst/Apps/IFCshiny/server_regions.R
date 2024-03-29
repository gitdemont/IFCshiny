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

obs_reg <- list(
  observeEvent(regions_react$pre, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = regions_react$pre$name
    if(!any(R %in% N)) return(NULL)
    reg_back = obj_react$obj$regions[[R]]
    reg_back$cx = signif(reg_back$cx, digits = 3)
    reg_back$cy = signif(reg_back$cy, digits = 3)
    reg_back$x = signif(reg_back$x, digits = 3)
    reg_back$y = signif(reg_back$y, digits = 3)
    reg_def(reg = regions_react$pre, reg_back = reg_back, all_names = N, check = "both")
  }),
  observeEvent(input$reg_def_label, suspended = TRUE,{
    regions_react$pre$label <- input$reg_def_label
  }),
  observeEvent(input$reg_def_label_reset, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = regions_react$pre$name
    if(!any(R %in% N)) return(NULL)
    regions_react$pre$label <- obj_react$obj$regions[[R]]$label
    updateTextInput(session = session, inputId = "reg_def_label", value = regions_react$pre$label)
  }),
  observeEvent(input$reg_color_light, suspended = TRUE,{
    regions_react$pre$lightcolor <- match_col(input$reg_color_light)
  }),
  observeEvent(input$reg_color_light_reset, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = regions_react$pre$name
    if(!any(R %in% N)) return(NULL)
    regions_react$pre$lightcolor <- obj_react$obj$regions[[R]]$lightcolor
    updateColourInput(session = session, inputId = "reg_color_light", value = tolower(regions_react$pre$lightcolor))
  }),
  observeEvent(input$reg_color_dark, suspended = TRUE,{
    regions_react$pre$color <- match_col(input$reg_color_dark)
  }),
  observeEvent(input$reg_color_dark_reset, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = regions_react$pre$name
    if(!any(R %in% N)) return(NULL)
    regions_react$pre$color <- obj_react$obj$regions[[R]]$color
    updateColourInput(session = session, inputId = "reg_color_dark", value = tolower(regions_react$pre$color))
  }),
  observeEvent(input$reg_def_cx, suspended = TRUE,{
    regions_react$pre$cx <- input$reg_def_cx
  }),
  observeEvent(input$reg_def_cx_reset, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = regions_react$pre$name
    if(!any(R %in% N)) return(NULL)
    regions_react$pre$cx <- obj_react$obj$regions[[R]]$cx
    updateNumericInput(session = session, inputId = "reg_def_cx", value = regions_react$pre$cx)
  }),
  observeEvent(input$reg_def_cy, suspended = TRUE,{
    regions_react$pre$cy <- input$reg_def_cy
  }),
  observeEvent(input$reg_def_cy_reset, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = regions_react$pre$name
    if(!any(R %in% N)) return(NULL)
    regions_react$pre$cy <- obj_react$obj$regions[[R]]$cy
    updateNumericInput(session = session, inputId = "reg_def_cy", value = regions_react$pre$cy)
  }),
  observeEvent(input$reg_def_table_cell_edit, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = regions_react$pre$name
    if(!any(R %in% N)) return(NULL)
    value = signif(as.numeric(input$reg_def_table_cell_edit$value), 3)
    what = c("x","y")[1+input$reg_def_table_cell_edit$col]
    regions_react$pre[[what]][input$reg_def_table_cell_edit$row] <- value
    if((regions_react$pre$type == "line") && (what == "y")) {
      regions_react$pre[[what]] <- rep(value, 2)
      output$reg_def_table <- DT::renderDataTable(data.frame(x = regions_react$pre$x , y = regions_react$pre$y, stringsAsFactors = FALSE), editable = TRUE, server = FALSE, # escape = FALSE, 
                                              rownames = FALSE, extensions = 'Buttons',
                                              selection = list(mode = 'none'), #style = "bootstrap",
                                              options = list(pageLength = -1,
                                                             buttons = c('csv', 'excel', 'pdf'),
                                                             dom = 'Btr',
                                                             autoWidth = FALSE,
                                                             columnDefs = list(list(orderable = FALSE, targets = "_all"))))
    }
  }),
  observeEvent(input$reg_def_table_reset, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = regions_react$pre$name
    if(!any(R %in% N)) return(NULL)
    regions_react$pre$x <- signif(obj_react$obj$regions[[R]]$x, 3)
    regions_react$pre$y <- signif(obj_react$obj$regions[[R]]$y, 3)
    output$reg_def_table <- DT::renderDataTable(data.frame(x = regions_react$pre$x , y = regions_react$pre$y, stringsAsFactors = FALSE), editable = TRUE, server = FALSE, # escape = FALSE, 
                                            rownames = FALSE, extensions = 'Buttons',
                                            selection = list(mode = 'none'), #style = "bootstrap",
                                            options = list(pageLength = -1,
                                                           buttons = c('csv', 'excel', 'pdf'),
                                                           dom = 'Btr',
                                                           autoWidth = FALSE,
                                                           columnDefs = list(list(orderable = FALSE, targets = "_all"))))
  }),
  observeEvent(input$reg_close, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = regions_react$pre$name
    if(!any(R %in% N)) {
      runjs("Shiny.onInputChange('reg_manager_visible', false)")
      return(NULL)
    }
    reg_back = obj_react$obj$regions[[R]]
    reg_back$cx = signif(reg_back$cx, digits = 3)
    reg_back$cy = signif(reg_back$cy, digits = 3)
    reg_back$x = signif(reg_back$x, digits = 3)
    reg_back$y = signif(reg_back$y, digits = 3)
    if(!all(reg_def(reg = regions_react$pre, reg_back = reg_back, all_names = names(obj_react$obj$regions), check = "both"))) {
      showModal(modalDialog(tags$p("Are you sure you want to discard applied changes in region '", tags$b(R), "' ?"),
                            size = "s",
                            easyClose = FALSE,
                            footer = list(actionButton(inputId = "reg_close_nok",label = "No"),
                                          actionButton(inputId = "reg_close_ok",label = "Yes"))),
                session = session)
    } else {
      runjs("Shiny.onInputChange('reg_manager_visible', false)")
    }
  }),
  remove = observeEvent(input$reg_remove, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = regions_react$pre$name
    if(!any(R %in% N)) {
      mess_global(title = "Region Removal", msg = c("Can't find region:", R), type = "error", duration = 10)
      return(NULL)
    }
    obs_reg[["confirm"]]$resume()
    to_remove = data_rm_regions(obj = obj_react$obj, regions = R, list_only = TRUE)
    if((length(to_remove$regions) > 1 ) || (length(to_remove$pops) > 0)) {
      to_remove_msg = list(tags$p("Removing region '", tags$b(R), "' will also induce the removal of"))
      if(length(to_remove$regions) > 1) to_remove_msg = c(to_remove_msg, list(tags$p("- region(s):"), tags$ul(lapply(to_remove$regions[-1], FUN = function(x) tags$li(x)))))
      if(length(to_remove$pops) > 0) to_remove_msg = c(to_remove_msg, list(tags$p("- populations(s):"), tags$ul(lapply(to_remove$pops, FUN = function(x) tags$li(x)))))
      showModal(modalDialog(to_remove_msg, 
                            size = "s",
                            easyClose = FALSE,
                            footer = list(actionButton(inputId = "reg_confirm_removal", label = "Proceed"),
                                          modalButton(label = "Abort"))),
                session = session) 
    } else {
      showModal(modalDialog(tags$p("Are you sure to remove '", tags$b(R), "' region ?"),
                            size = "s",
                            easyClose = FALSE,
                            footer = list(actionButton(inputId = "reg_confirm_removal", label = "Yes"),
                                          modalButton(label = "Abort"))),
                session = session) 
    }
  }),
  confirm = observeEvent(input$reg_confirm_removal, suspended = TRUE,{
    obj_back = obj_react$obj
    tryCatch({
      N = names(obj_react$obj$regions)
      R = regions_react$pre$name
      if(!any(R %in% N)) stop("")
      toredraw = data_rm_regions(obj = obj_react$obj, regions = R, list_only = TRUE)
      obj_react$obj = data_rm_regions(obj = obj_react$obj, regions = R, list_only = FALSE)
      if(length(toredraw$graph) > 0) obj_react$obj = reinit_layout(obj_react$obj)
      plot_react$param_ready = FALSE
      N =  names(obj_react$obj$regions)
      sel = input$plot_regions
      sel = sel[sel %in% N]
      sel = sel[sel %in% plot_react$allowed_regions]
      if(length(sel)==0) sel = N[1]
      if(identical(input$plot_regions, sel)) {
        plot_react$allowed_regions = sel
        updateSelectInput(session=session, inputId = "plot_regions", choices = sel, selected = input$plot_regions)
      } else {
        plot_react$allowed_regions = sel
        updateSelectInput(session=session, inputId = "plot_regions", choices = sel, selected = sel)
      }
      
      if(input$reg_manager_visible) {
        if(R %in% input$reg_selection) {
          if(length(obj_react$obj$regions) == 0) {
            runjs("Shiny.onInputChange('pop_edit', null)")
            runjs("Shiny.onInputChange('pop_manager_visible', false)")
          } else {
            updateSelectInput(session=session, inputId = "reg_selection", selected = names(obj_react$obj$regions)[1])
          }
        } else {
          updateSelectInput(session=session, inputId = "reg_selection", selected = character())
        }
      }
    },
    error = function(e) {
      obj_react$obj = obj_back
      mess_global(title = "removing region", msg = e$message, type = "error", duration = 10)
    }, finally = {
      obs_reg[["confirm"]]$suspend()
      removeModal(session=session)
    })
  }),
  observeEvent(input$reg_close_nok, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = input$reg_selection
    removeModal(session=session)
    if(!any(R %in% N)) return(NULL)
    reg_back = obj_react$obj$regions[[R]]
    reg_back$cx = signif(reg_back$cx, digits = 3)
    reg_back$cy = signif(reg_back$cy, digits = 3)
    reg_back$x = signif(reg_back$x, digits = 3)
    reg_back$y = signif(reg_back$y, digits = 3)
    reg_def(reg = regions_react$pre, reg_back = reg_back, all_names = N, check = "both")
  }),
  observeEvent(input$reg_close_ok, suspended = TRUE,{
    removeModal(session=session)
    runjs("Shiny.onInputChange('reg_manager_visible', false)")
  }),
  observeEvent(input$reg_getback, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = regions_react$pre$name
    removeModal(session=session)
    if(!any(R %in% N)) return(NULL)
    regions_react$back = TRUE
    updateSelectInput(session = session, inputId = "reg_selection", selected = R)
  }),
  observeEvent(input$reg_abort, suspended = TRUE,{
    N = names(obj_react$obj$regions)
    R = input$reg_selection
    removeModal(session=session)
    if(!any(R %in% N)) return(NULL)
    regions_react$back = FALSE
    hideFeedback(session=session,inputId = "reg_def_label")
    removeClass(id = "reg_color_dark_reset", class = "modified")
    removeClass(id = "reg_color_light_reset", class = "modified")
    hideFeedback(session=session,inputId = "reg_def_cx")
    hideFeedback(session=session,inputId = "reg_def_cy")
    hideFeedback(session=session,inputId = "reg_def_table_feedback")
    reg = obj_react$obj$regions[[R]]
    reg$cx = signif(reg$cx, digits = 3)
    reg$cy = signif(reg$cy, digits = 3)
    reg$x = signif(reg$x, digits = 3)
    reg$y = signif(reg$y, digits = 3)
    updateNumericInput(session = session, inputId = "reg_def_cx", value = reg$cx)
    updateNumericInput(session = session, inputId = "reg_def_cy", value = reg$cy)
    colourpicker::updateColourInput(session = session, inputId = "reg_color_light", value = tolower(reg$lightcolor))
    colourpicker::updateColourInput(session = session, inputId = "reg_color_dark", value = tolower(reg$color))
    updateTextInput(session = session, inputId = "reg_def_label", value = reg$label)
    regions_react$pre = reg
    regions_react$pre$name = R
    reg_def(reg = regions_react$pre, reg_back = reg, all_names = N, check = "both")
  }))

# output
output$reg_def_viz <- renderPrint({
  N = names(obj_react$obj$regions)
  if(!any(regions_react$pre$name %in% N)) return(NULL)
  reg = obj_react$obj$regions[[regions_react$pre$name]]
  str(list(type = reg[["type"]],
           "LinLog x" = reg[["xlogrange"]],
           "LinLog y" = reg[["ylogrange"]]))})
output$reg_def_table <- DT::renderDataTable(expr = {
  N = names(obj_react$obj$regions)
  if(!any(regions_react$pre$name %in% N)) return(NULL)
  reg = obj_react$obj$regions[[regions_react$pre$name]]
  reg$x = signif(reg$x, digits = 3)
  reg$y = signif(reg$y, digits = 3)
  data.frame(x = reg$x, y = reg$y, stringsAsFactors = FALSE)
}, editable = TRUE, server = FALSE, # escape = FALSE,
rownames = FALSE, extensions = 'Buttons',
selection = list(mode = 'none'), #style = "bootstrap",
options = list(pageLength = -1,
               buttons = c('csv', 'excel', 'pdf'),
               dom = 'Btr',
               autoWidth = FALSE,
               columnDefs = list(list(orderable = FALSE, targets = "_all"))))