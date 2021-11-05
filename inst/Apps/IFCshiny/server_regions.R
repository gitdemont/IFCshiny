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
  observeEvent(input$reg_validate, suspended = TRUE,{
    reg_back = obj_react$obj$regions[[regions_react$pre$name]]
    reg_back$cx = signif(reg_back$cx, digits = 3)
    reg_back$cy = signif(reg_back$cy, digits = 3)
    reg_back$x = signif(reg_back$x, digits = 3)
    reg_back$y = signif(reg_back$y, digits = 3)
    if(!all(reg_def(reg = regions_react$pre, reg_back = reg_back, all_names = N, check = "info", session = getDefaultReactiveDomain()))) {
      toredraw = data_rm_regions(obj = obj_react$obj, regions = regions_react$pre$name, list_only = TRUE, session=session)
      names(obj_react$obj$graphs)[toredraw$graphs] <- NA
      N = names(obj_react$obj$graphs)
      if(length(N) > 0) {
        lapply(1:length(N), FUN = function(i) {
          if(!is.na(N[i])) return(NULL)
          runjs(code = JS(sprintf("IFCshiny.grid.remove(IFCshiny.grid.getItems().filter(function (item) { return item._element.id === 'report_graph_%0.4i' }), { removeElements: true, layout: false })", i)))
        })
      }
      obj_react$obj = data_modify_regions(obj = obj_react$obj, 
                                          regions = structure(list(regions_react$pre), names = regions_react$pre$name), 
                                          display_progress = TRUE, 
                                          title = "recomputing populations", 
                                          session = session)
    }
  }),
  observeEvent(input$reg_def_label, suspended = TRUE,{
    regions_react$pre$label <- input$reg_def_label
    reg_def(reg = regions_react$pre, all_names = names(obj_react$obj$regions), check = "valid", session = getDefaultReactiveDomain())
  }),
  observeEvent(input$reg_def_label_reset, suspended = TRUE,{
    regions_react$pre$label <- obj_react$obj$regions[[input$reg_selection]]$label
    reg_def(reg = regions_react$pre, all_names = names(obj_react$obj$regions), check = "valid", session = getDefaultReactiveDomain())
    updateTextInput(session = session, inputId = "reg_def_label", value = regions_react$pre$label)
  }),
  observeEvent(input$reg_color_light, suspended = TRUE,{
    regions_react$pre$lightcolor <- match_col(input$reg_color_light)
  }),
  observeEvent(input$reg_color_light_reset, suspended = TRUE,{
    removeClass(id = "reg_color_light_reset", class = "modified")
    regions_react$pre$lightcolor <- obj_react$obj$regions[[input$reg_selection]]$lightcolor
    updateColourInput(session = session, inputId = "reg_color_light", value = regions_react$pre$lightcolor)
  }),
  observeEvent(input$reg_color_dark, suspended = TRUE,{
    regions_react$pre$color <- match_col(input$reg_color_dark)
  }),
  observeEvent(input$reg_color_dark_reset, suspended = TRUE,{
    removeClass(id = "reg_color_dark_reset", class = "modified")
    regions_react$pre$color <- obj_react$obj$regions[[input$reg_selection]]$color
    updateColourInput(session = session, inputId = "reg_color_dark", value = regions_react$pre$color)
  }),
  observeEvent(input$reg_def_cx, suspended = TRUE,{
    regions_react$pre$cx <- input$reg_def_cx
    reg_def(reg = regions_react$pre, all_names = names(obj_react$obj$regions), check = "valid", session = getDefaultReactiveDomain())
  }),
  observeEvent(input$reg_def_cx_reset, suspended = TRUE,{
    regions_react$pre$cx <- obj_react$obj$regions[[input$reg_selection]]$cx
    reg_def(reg = regions_react$pre, all_names = names(obj_react$obj$regions), check = "valid", session = getDefaultReactiveDomain())
    updateNumericInput(session = session, inputId = "reg_def_cx", value = regions_react$pre$cx)
  }),
  observeEvent(input$reg_def_cy, suspended = TRUE,{
    regions_react$pre$cy <- input$reg_def_cy
    reg_def(reg = regions_react$pre, all_names = names(obj_react$obj$regions), check = "valid", session = getDefaultReactiveDomain())
  }),
  observeEvent(input$reg_def_cy_reset, suspended = TRUE,{
    regions_react$pre$cy <- obj_react$obj$regions[[input$reg_selection]]$cy
    reg_def(reg = regions_react$pre, all_names = names(obj_react$obj$regions), check = "valid", session = getDefaultReactiveDomain())
    updateNumericInput(session = session, inputId = "reg_def_cy", value = regions_react$pre$cy)
  }),
  observeEvent(input$reg_def_table_cell_edit, suspended = TRUE,{
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
    reg_def(reg = regions_react$pre, all_names = names(obj_react$obj$regions), check = "valid", session = getDefaultReactiveDomain())
  }),
  observeEvent(input$reg_def_table_reset, suspended = TRUE,{
    regions_react$pre$x <- signif(obj_react$obj$regions[[input$reg_selection]]$x, 3)
    regions_react$pre$y <- signif(obj_react$obj$regions[[input$reg_selection]]$y, 3)
    reg_def(reg = regions_react$pre, all_names = names(obj_react$obj$regions), check = "valid", session = getDefaultReactiveDomain())
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
    reg_back = obj_react$obj$regions[[regions_react$pre$name]]
    reg_back$cx = signif(reg_back$cx, digits = 3)
    reg_back$cy = signif(reg_back$cy, digits = 3)
    reg_back$x = signif(reg_back$x, digits = 3)
    reg_back$y = signif(reg_back$y, digits = 3)
    if(!all(reg_def(reg = regions_react$pre, reg_back = reg_back, all_names = names(obj_react$obj$regions), check = "edit", session = getDefaultReactiveDomain()))) {
      showModal(modalDialog(tags$p("Are you sure you want to discard applied changes in region '", tags$b(regions_react$pre$name), "' ?"),
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
    if(length(regions_react$pre$name) == 0) return(NULL)
    obs_reg[["confirm"]]$resume()
    to_remove = data_rm_regions(obj = obj_react$obj, regions = regions_react$pre$name, list_only = TRUE, session=session)
    if((length(to_remove$regions) > 1 ) || (length(to_remove$pops) > 0)) {
      to_remove_msg = list(tags$p("Removing region '", tags$b(regions_react$pre$name), "' will also induce the removal of"))
      if(length(to_remove$regions) > 1) to_remove_msg = c(to_remove_msg, list(tags$p("- region(s):"), tags$ul(lapply(to_remove$regions[-1], FUN = function(x) tags$li(x)))))
      if(length(to_remove$pops) > 0) to_remove_msg = c(to_remove_msg, list(tags$p("- populations(s):"), tags$ul(lapply(to_remove$pops, FUN = function(x) tags$li(x)))))
      showModal(modalDialog(to_remove_msg, 
                            size = "s",
                            easyClose = FALSE,
                            footer = list(actionButton(inputId = "reg_confirm_removal", label = "Proceed"),
                                          modalButton(label = "Abort"))),
                session = session) 
    } else {
      showModal(modalDialog(tags$p("Are you sure to remove '", tags$b(regions_react$pre$name), "' region ?"),
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
      toredraw = data_rm_regions(obj = obj_react$obj, regions = regions_react$pre$name, list_only = TRUE, session=session)
      obj_react$obj = data_rm_regions(obj = obj_react$obj, regions = regions_react$pre$name, list_only = FALSE, session=session)
      if(length(toredraw$graph) > 0) obj_react$obj = reinit_layout(obj_react$obj)
      
      N =  names(obj_react$obj$regions)
      sel = input$plot_regions
      sel = sel[sel %in% N]
      sel = sel[sel %in% plot_react$allowed_regions]
      if(length(sel)==0) sel = N[1]
      if(identical(input$plot_regions, sel)) {
        updateSelectInput(session=session, inputId = "plot_regions", choices = sel)
      } else {
        plot_react$allowed_regions = sel
        updateSelectInput(session=session, inputId = "plot_regions", choices = sel, selected = sel)
      }
      
      if(input$reg_manager_visible) {
        if(regions_react$pre$name %in% input$reg_selection) {
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
    removeModal(session=session)
    valid = reg_def(reg = regions_react$pre, all_names = names(obj_react$obj$regions), check = "valid", session = getDefaultReactiveDomain())
    if(all(valid)) {
      enable("reg_validate")
    } else {
      disable("reg_validate")
    }
  }),
  observeEvent(input$reg_close_ok, suspended = TRUE,{
    removeModal(session=session)
    runjs("Shiny.onInputChange('reg_manager_visible', false)")
  }),
  observeEvent(input$reg_getback, suspended = TRUE,{
    removeModal(session=session)
    regions_react$back = TRUE
    updateSelectInput(session = session, inputId = "reg_selection", selected = regions_react$pre$name)
  }),
  observeEvent(input$reg_abort, suspended = TRUE,{
    removeModal(session=session)
    regions_react$back = FALSE
    hideFeedback(session=session,inputId = "reg_def_label")
    removeClass(id = "reg_color_dark_reset", class = "modified")
    removeClass(id = "reg_color_light_reset", class = "modified")
    hideFeedback(session=session,inputId = "reg_def_cx")
    hideFeedback(session=session,inputId = "reg_def_cy")
    hideFeedback(session=session,inputId = "reg_def_table_feedback")
    reg = obj_react$obj$regions[[input$reg_selection]]
    reg$cx = signif(reg$cx, digits = 3)
    reg$cy = signif(reg$cy, digits = 3)
    reg$x = signif(reg$x, digits = 3)
    reg$y = signif(reg$y, digits = 3)
    updateNumericInput(session = session, inputId = "reg_def_cx", value = reg$cx)
    updateNumericInput(session = session, inputId = "reg_def_cy", value = reg$cy)
    colourpicker::updateColourInput(session = session, inputId = "reg_color_light", value = reg$lightcolor)
    colourpicker::updateColourInput(session = session, inputId = "reg_color_dark", value = reg$color)
    updateTextInput(session = session, inputId = "reg_def_label", value = reg$label)
    output$reg_def_viz <- renderPrint({
      str(list(type = reg[["type"]],
               "LinLog x" = reg[["xlogrange"]], 
               "LinLog y" = reg[["ylogrange"]]))})
    output$reg_def_table <- DT::renderDataTable(data.frame(x = reg$x, y = reg$y, stringsAsFactors = FALSE), editable = TRUE, server = FALSE, # escape = FALSE,
                                            rownames = FALSE, extensions = 'Buttons',
                                            selection = list(mode = 'none'), #style = "bootstrap",
                                            options = list(pageLength = -1,
                                                           buttons = c('csv', 'excel', 'pdf'),
                                                           dom = 'Btr',
                                                           autoWidth = FALSE,
                                                           columnDefs = list(list(orderable = FALSE, targets = "_all"))))
    regions_react$pre = reg
    regions_react$pre$name = input$reg_selection
  }))