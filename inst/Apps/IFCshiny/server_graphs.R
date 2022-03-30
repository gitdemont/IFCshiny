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

# small trick so that fontawesome circle looks like an ellipse
runjs(code = "$('#plot_sel_ellipse>.fa.fa-circle').addClass('far').removeClass('fa').css('transform', 'rotate(-25deg) scale(1.5,1)');")

# adds cell count to density sliders in 2D and 3D graphs
insertUI(selector = "label[for=plot_type_2D_main_option03]", immediate = TRUE, multiple = FALSE, where = "afterEnd",
         ui = tags$b(id="plot_type_2D_main_option03-count",
                     style="display:inline-block; float:right;",
                     "NA"))
insertUI(selector = "label[for=plot_type_3D_option03]", immediate = TRUE, multiple = FALSE, where = "afterEnd",
         ui = tags$b(id="plot_type_3D_option03-count",
                     style="display:inline-block; float:right;",
                     "NA"))

reinit_plot_3D <- function(session) {
  updateToggle3D(session = session, inputId = "plot_3D_draw_axes", value = TRUE)
  updateToggle3D(session = session, inputId = "plot_3D_draw_pts", value = TRUE)
  updateToggle3D(session = session, inputId = "plot_3D_draw_ell", value = FALSE)
  updateToggle3D(session = session, inputId = "plot_3D_draw_txt", value = FALSE)
  updateSliderInput(session = session, inputId = "plot_type_3D_option02", value = 2)
  updateSelectInput(session=session, inputId = "plot_3D_mouse_ctrl", selected = "trackball")
  hideElement(id = "plot_3D_img_ctrl")
  if(any(obj_react$back$info$found)) showElement(id = "plot_3D_img_ctrl")
}

set_tool <- function(tool = "init", plotId = plot_react$current) {
  disable("plot_sel_line")
  disable("plot_sel_rectangle")
  disable("plot_sel_polygon")
  disable("plot_sel_lasso")
  disable("plot_sel_ellipse")
  disable("plot_sel_edit")
  disable("plot_sel_remove")
  disable("plot_sel_zoomin")
  disable("plot_sel_zoomreset")
  disable("plot_sel_add")
  disable("plot_sel_stack")
  session$sendCustomMessage("getOffset", sprintf("%s",plot_react$current))
  session$resetBrush("plot_brush")
  runjs(code="$('.selected.plot_tool').removeClass('selected')")
  runjs(code=sprintf("$('#plot_sel_%s').parent().addClass('selected')", tool))
  runjs(code = "Shiny.onInputChange('shape_param', null)")
  runjs(code = sprintf("$('%s').off('mousemove', IFCshiny.draw_shape )", plotId))
  runjs(code = sprintf("$('%s').off('click', IFCshiny.draw_path )", plotId))
  removeUI(selector = "#brush_visibility", immediate = TRUE)
  insertUI(selector = "head", ui = tags$style(id='brush_visibility', HTML(sprintf("%s_brush { visibility: hidden; }", plotId))),
           where = "beforeEnd", multiple = FALSE, immediate = TRUE)
  removeUI(selector = sprintf("%s>.draw_shape", plotId), immediate = TRUE)
  removeUI(selector = sprintf("%s>.edit_shape", plotId), immediate = TRUE)
  removeUI(selector = sprintf("%s>.hover_point", plotId), immediate = TRUE)
  removeUI(selector = sprintf("%s>.click_point", plotId), immediate = TRUE)
  removeUI(selector = sprintf("%s>.click_bar", plotId), immediate = TRUE)
  plot_react$region = data.frame(matrix(NA, ncol = 4, nrow = 0, dimnames = list(NULL, c("x", "y", "css_x", "css_y"))))
  plot_react$tool = tool
  plot_react$action = switch(tool,
                             "init" = { 
                               runjs(code = sprintf("$('%s').css('cursor', 'crosshair')", plotId))
                               enable("plot_sel_edit")
                               enable("plot_sel_remove")
                               enable("plot_sel_zoomin")
                               enable("plot_sel_zoomreset")
                               enable("plot_sel_add")
                               "none"
                             },
                             "edit" = { 
                               runjs(code = sprintf("$('%s').css('cursor', 'pointer')", plotId))
                               enable("plot_sel_edit")
                               "editing"
                             },
                             "remove" = { 
                               runjs(code = sprintf("$('%s').css('cursor', 'pointer')", plotId))
                               enable("plot_sel_remove")
                               "removing" },
                             "zoomin" = { 
                               runjs(code = sprintf("$('%s').css('cursor', 'zoom-in')", plotId))
                               removeUI(selector = "#brush_visibility", immediate = TRUE)
                               insertUI(selector = "head", ui = tags$style(id='brush_visibility', HTML(sprintf("%s_brush { visibility: visible; }", plotId))),
                                        where = "beforeEnd", multiple = FALSE, immediate = TRUE)
                               enable("plot_sel_zoomin")
                               "zooming" },
                             "add" = {
                               enable("plot_sel_add")
                               "adding"
                             },
                             # "batch" = {
                               # enable("plot_sel_stack")
                               # "batch"
                             # },
                             { "drawing" })
  if(plot_react$action %in% c("drawing", "none")) {
    if(plot_react$action == "drawing") {
      if(grepl("^ML_", input$population)) {
        mess_global(title = "creating region", 
                    msg = c("Be aware that you are creating a new population depending on ML pop",
                            "IDEAS will not be able to use it for applying on other files"),
                    # msg = "Creating a region based on ML pop is not allowed",
                    type = "warning", duration = 10)
        # click("plot_sel_init")
      }
      if(any(grepl("^ML_", c(input$plot_x_feature, input$plot_y_feature)))) {
        mess_global(title = "creating region",
                    msg = c("Be aware that you are creating a new population depending on ML feature",
                            "IDEAS will not be able to compute these feature for applying on other files"),
                    # msg = "Creating a region based on ML feature is not allowed",
                    type = "warning", duration = 10)
        # click("plot_sel_init")
      }
    }
    if(length(obj_react$batch) != 0) enable("plot_sel_stack") 
    if(input$plot_type == "2D") {
      enable("plot_sel_rectangle")
      enable("plot_sel_polygon")
      enable("plot_sel_lasso")
      enable("plot_sel_ellipse")
    } else {
      enable("plot_sel_line") 
    }
  }
  obs_reg[["remove"]]$suspend()
  return(NULL)
}

obs_plot <- list(
  observeEvent(list(input$plot_dens_color, input$plot_dens_order), suspended = TRUE, {
    if(length(input$file) == 0) return(NULL)
    if(input$plot_dens_color == "initial") {
      if(length(plot_react$g$BasePop[[1]]$densitycolorslightmode) == 0) return(NULL)
      col = colConv(plot_react$densitycolorslightmode)
    } else {
      if(requireNamespace("viridisLite", quietly = TRUE) && (input$plot_dens_color %in% ls(asNamespace("viridisLite")))) {
        col = do.call(what = input$plot_dens_color, args = list(n = 5))
      } else {
        col = RColorBrewer::brewer.pal(n = 5, name = input$plot_dens_color)
      }
    }
    if(input$plot_dens_order %% 2) col = rev(col)
    runjs(code = sprintf("$('#plot_dens_color').parent().find('.selectize-input').css({background:'linear-gradient(to right, %s)'});", paste0(col,collapse=",")))
  }),
  limits = observeEvent({list(input$plot_xmin,
                              input$plot_xmax,
                              input$plot_ymin,
                              input$plot_ymax)}, suspended = TRUE, {
                                foo = sapply(c("xmin","xmax","ymin","ymax"), FUN = function(i) { 
                                  tmp = input[[paste0("plot_",i)]]
                                  if(length(tmp[is.finite(tmp)])==0) return(TRUE)
                                  ans = plot_react[[i]] %in% input[[paste0("plot_",i)]]
                                  plot_react[[i]] <<- input[[paste0("plot_",i)]]
                                  return(ans)
                                } )
                                plot_react$zoomed = plot_react$zoomed || any(!foo)
                              }),
  observeEvent(input$reg_edit, suspended = TRUE,  {
    add_log("Region Edition")
    if(length(input$shape_selected) != 0) {
      mess_global(title = "editing region", msg = "please terminate graphical editing first", type = "error", duration = 10)
      return(NULL)
    }
    if(length(obj_react$obj$regions) == 0) {
      mess_global(title = "editing region", msg = "there is no region to edit", type = "error", duration = 10)
      return(NULL)
    }
    runjs("Shiny.onInputChange('reg_manager_visible', true)")
    disable("reg_def_table_feedback")
    pop = names(obj_react$obj$regions)[1]
    reg = obj_react$obj$regions[[pop]]
    reg$cx = signif(reg$cx, digits = 3)
    reg$cy = signif(reg$cy, digits = 3)
    reg$x = signif(reg$x, digits = 3)
    reg$y = signif(reg$y, digits = 3)
    regions_react$pre = reg
    regions_react$pre$name = pop
    updateSelectInput(session = session, inputId = "reg_selection", selected = regions_react$pre$name)
    runjs(code = sprintf("Shiny.onInputChange('reg_selection', '%s')", regions_react$pre$name))
  }),
  observeEvent(input$reg_selection, suspended = TRUE,{
    if((length(input$reg_manager_visible) == 0) || !(input$reg_manager_visible)) return(NULL)
    if((length(input$reg_selection) == 0) || (input$reg_selection == "")) return(NULL)
    hideFeedback(session=session,inputId = "reg_def_label")
    removeClass(id = "reg_color_dark_reset", class = "modified")
    removeClass(id = "reg_color_light_reset", class = "modified")
    hideFeedback(session=session,inputId = "reg_def_cx")
    hideFeedback(session=session,inputId = "reg_def_cy")
    hideFeedback(session=session,inputId = "reg_def_table_feedback")
    if(any(names(obj_react$obj$regions) %in% regions_react$pre$name) && !(input$reg_selection %in% regions_react$pre$name)) {
      reg_back = obj_react$obj$regions[[regions_react$pre$name]]
      reg_back$cx = signif(reg_back$cx, digits = 3)
      reg_back$cy = signif(reg_back$cy, digits = 3)
      reg_back$x = signif(reg_back$x, digits = 3)
      reg_back$y = signif(reg_back$y, digits = 3)
      if(!all(reg_def(reg = regions_react$pre, reg_back = reg_back, all_names = names(obj_react$obj$regions), check = "edit", session = getDefaultReactiveDomain()))) {
        showModal(modalDialog(tags$p("Are you sure you want to discard applied changes in region '", tags$b(regions_react$pre$name), "' ?"),
                              size = "s",
                              easyClose = FALSE,
                              footer = list(actionButton(inputId = "reg_getback",label = "No"),
                                            actionButton(inputId = "reg_abort", label = "Yes"))),
                  session = session)
        return(NULL)
      } 
    }
    if(!regions_react$back) {
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
    }
    reg_def(reg = regions_react$pre, all_names = names(obj_react$obj$regions), check = "valid", session = getDefaultReactiveDomain())
  }),
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
  observeEvent(input$plot_base, suspended = TRUE, {
    if(input$plot_unlock) plot_react$zoomed = FALSE
    if(any(input$plot_base=="")) return(NULL)
    click("plot_sel_init")
  }),
  observeEvent(input$plot_unlock, suspended = TRUE, {
    if(input$plot_unlock) {
      enable("plot_param")
      enable(selector = ".plot_sel_tools")
    } else {
      disable("plot_param")
      disable(selector = ".plot_sel_tools")
    }
    onFlushed(once = TRUE, session=session, fun = function() { 
      updatePrettySwitch(session=session, inputId="plot_unlock", value=TRUE)
    })
  }),
  observeEvent(input$ele_order, suspended = TRUE, {
    foo = do.call(what=rbind, args=lapply(input$ele_order, FUN = function(x) splitn(definition = x, all_names = c("plot_order",names(obj_react$obj$pops)))))
    if(all(unique(foo[, 1]) == "plot_order")) {
      runjs(code = sprintf("Shiny.onInputChange('plot_shown_order', $('#%s>.movable').toArray().map(x => x.innerText))",list("plot_order")))
    }
  }),
  observeEvent(input$plot_shown_order, suspended = TRUE, ignoreNULL = FALSE, {
    if(length(input$plot_shown_order) == 0) {
      plot_react$order = input$plot_shown
    } else {
      plot_react$order = input$plot_shown_order
    }
  }),
  observeEvent(list(input$plot_shown,input$plot_type), suspended = TRUE,  {
    if(length(input$plot_shown) == 0) return(NULL)
    shown = input$plot_shown
    if(input$plot_type == "3D") shown = rev(shown)
    html(id = "plot_order", add = FALSE, 
         html = (do.call(what = paste0, 
                         args = lapply(shown, FUN = function(val) {
                           tags$div(id = paste0("plot_order", "|", val),
                                    val,
                                    class = "movable",
                                    "data-value" = val,
                                    ondragstart = 'IFCshiny.dragstart(event)',
                                    ondragover = 'IFCshiny.dragover(event)',
                                    ondragleave = 'IFCshiny.dragleave(event)',
                                    ondrop = 'IFCshiny.move(event)',
                                    ondrag = 'IFCshiny.dragging(event)',
                                    ondragend ='IFCshiny.dragend(event)',
                                    draggable = 'true',
                                    style = "border-style: outset;
                                    border-width: 1px;
                                    border-color: black;
                                    border-bottom: 1px outset black;
                                    border-top: 1px outset black;
                                    border-radius: 3px;
                                    padding: 3px 3px;
                                    background-color: white; 
                                    width: 100%")
                         }))))
  }),
  observeEvent(list(input$plot_type_3D_option03,input$plot_type_3D_option01,input$plot_shown), suspended = TRUE, {
    if(length(input$plot_shown) == 0) return(NULL)
    if(input$plot_type_3D_option03 == 0) {
      updateSliderInput(session=session, inputId = "plot_type_3D_option03", value=1)
      return(NULL)
    }
    # sub = apply(do.call(what = rbind, args = lapply(obj_react$obj$pops[input$plot_shown], FUN = function(p) p$obj)), 2, any)
    sub = fastAny(lapply(obj_react$obj$pops[input$plot_shown], FUN = function(p) p$obj))
    n_sub = sum(sub) 
    n_cells = as.integer(n_sub * input$plot_type_3D_option03 / 100)
    if(n_cells == n_sub) {
      disable("plot_type_3D_option01")
    } else {
      enable("plot_type_3D_option01")
    }
    runjs(code = sprintf("document.getElementById('plot_type_3D_option03-count').innerHTML='%s'", paste0("n = ", n_cells, "/", n_sub)))
    shown = rev(input$plot_shown)
    plot_react$shown = shown
    plot_react$order = shown
    # sub = apply(do.call(what = rbind, args = lapply(obj_react$obj$pops[input$plot_shown], FUN = function(p) p$obj)), 2, any)
    sub = fastAny(lapply(obj_react$obj$pops[input$plot_shown], FUN = function(p) p$obj))
    sub[-sample(x = which(sub), size = sum(sub) * input$plot_type_3D_option03 / 100, replace = FALSE)] <- FALSE
    plot_react$subset = sub
    plot_react$color = (sapply(obj_react$obj$pops[plot_react$order], FUN = function(p) {
      p$lightModeColor
    }))
    plot_react$symbol = (sapply(obj_react$obj$pops[plot_react$order], FUN = function(p) {
      p$style
    }))
  }),
  observeEvent(list(input$plot_type_2D_main_option03,input$plot_shown), suspended = TRUE, {
    if(length(input$plot_shown) == 0) return(NULL)
    if(input$plot_type_2D_main_option03 == 0) updateSliderInput(session=session, inputId = "plot_type_2D_main_option03", value=1)
    # sub = apply(do.call(what = rbind, args = lapply(obj_react$obj$pops[input$plot_shown], FUN = function(p) p$obj)), 2, any)
    sub = fastAny(lapply(obj_react$obj$pops[input$plot_shown], FUN = function(p) p$obj))
    n_sub = sum(sub) 
    n_cells = as.integer(n_sub * input$plot_type_2D_main_option03 / 100)
    if(n_cells == n_sub) {
      disable("plot_type_2D_main_option01")
    } else {
      enable("plot_type_2D_main_option01")
    }
    runjs(code = sprintf("document.getElementById('plot_type_2D_main_option03-count').innerHTML='%s'", paste0("n = ", n_cells, "/", n_sub)))
  }),
  observeEvent(input$plot_3D_mouse_ctrl, suspended = TRUE, {
    if(length(input$plot_3D_mouse_ctrl) == 0) return(NULL)
    shinyResetBrush(session, "plot_3D_brush")
    session$sendCustomMessage("refresh", "plot_3D")
    shinyGetPar3d(parameters = "mouseMode", tag = "mouse_mode_chg", session = session)
  }),
  observeEvent({
    list(input$plot_base,
         input$plot_shown,
         plot_react$order,
         input$plot_type_3D_option03)}, suspended = TRUE, {
           if(length(input$file) == 0) return(NULL)
           if(input$plot_type != "3D") return(NULL)
           if(length(input$plot_base) < 1) return(NULL)
           if(input$plot_type_3D_option03 == 0) return(NULL)
           over = plot_react$order
           if(length(over) == 0) return(NULL)
           
           sub = plot_react$subset
           plot_react$color = (sapply(obj_react$obj$pops[over], FUN = function(p) {
             p$lightModeColor
           }))
           plot_react$symbol = (sapply(obj_react$obj$pops[over], FUN = function(p) {
             p$style
           }))
           if(length(over) != 0 && all(c(over %in% input$plot_base, over %in% input$plot_shown, input$plot_base %in% over, input$plot_shown %in% over))) {
             # alw = isolate(apply(do.call(what = rbind, args = lapply(obj_react$obj$pops[over], FUN = function(p) p$obj)), 2, any))
             alw = isolate(fastAny(lapply(obj_react$obj$pops[over], FUN = function(p) p$obj)))
             if((length(sub) == 0) || any(sub & !alw)) {
               alw[-sample(x = which(alw), size = sum(alw) * input$plot_type_3D_option03 / 100, replace = FALSE)] <- FALSE
               plot_react$subset = alw
             }
           }
           else {
             plot_react$shown = NULL
             updateSelectInput(session=session, inputId="plot_shown", selected = NULL)
             onFlushed(once = TRUE, fun = function() {
               updateSelectInput(session=session, inputId="plot_shown", selected = (isolate(input$plot_base)))
             })
           }
         }),
  observeEvent(input$plot_3D_create_pop,suspended = TRUE,  {
    session$sendCustomMessage("get3DSelection", "plot_3D")
  }),
  observeEvent(input$IFCshiny_get3DSelection_ret, suspended = TRUE,  {
    if(length(input$IFCshiny_get3DSelection_ret$obj)==0) return(NULL)
    add_log("Population Creation")
    runjs("Shiny.onInputChange('pop_manager_visible', true)")
    pops_react$def = character()
    enable("pop_validate")
    enable("pop_def_name")
    hideElement("pop_plot")
    hideElement("pop_tag")
    hideElement(selector = "#pop_def_edit, .pop_def_feedback")
    hideElement("pop_remove")
    updateSelectInput(session = session, inputId = "pop_symbol", selected = "Simple Dot")
    colourpicker::updateColourInput(session = session, inputId = "pop_color_light", value = "Black")
    colourpicker::updateColourInput(session = session, inputId = "pop_color_dark", value = "White")
    updateTextInput(session = session, inputId = "pop_def_name", value = "")
    obj = unique(as.integer(unlist(input$IFCshiny_get3DSelection_ret$obj)))
    pops_react$revert = list(name = NULL, style = 20, color = "White",
                             lightModeColor = "Black", type = "T",
                             obj = obj)
    output$pop_def_viz <- renderPrint( list("Tagged" = substr(paste0(obj, collapse = ", "), 0, 120)))
    pops_react$new = TRUE
    updateSelectInput(session=session, inputId="plot_3D_mouse_ctrl", selected="trackball")
    plot_react$plot$shared$clearSelection("SharedData_plot_3D_ids")
    runjs("Shiny.onInputChange('IFCshiny_get3DSelection_ret', { rand: null, obj:[] } )")
  }),
  observeEvent(input$par3d, suspended = TRUE,  {
    if(length(input$par3d) == 0) return(NULL)
    if(input$par3d$tag == "mouse_mode_chg") {
      w = ifelse(input$run_on_mobile,2L,1L) # use right btn on mobile device to still be able to scroll screen with main btn
      session$sendCustomMessage("changeMouseMode", list(widgetId = plot_react$current, mode = input$plot_3D_mouse_ctrl, button = w, stayActive = 0))
    }
  }),
  observeEvent(input$plot_siblings, suspended = TRUE, {
    updateSelectInput(session=session, inputId="plot_regions", selected = plot_react$allowed_regions)
    updateSelectInput(session=session, inputId="plot_base", selected = popsGetSiblings(obj_react$obj, pops = input$plot_base)) 
  }),
  
  # plot_click is used for multiple purposes depending on tool selected by the user
  # - to show cell image of the point / bar at the mouse coordinate if tool is init
  # - to validate the selection performed by plot_hover when tool is edited or removed
  # - to create a vertice when action is drawing (tool = line, rect, oval, poly or lasso)
  observeEvent(input$plot_click, suspended = TRUE, {
    if(!input$plot_unlock) return(NULL)
    plot_react$click = NULL
    if(plot_react$action == "drawing") {
      if(length(input$shape_param) !=0) return(NULL)
      runjs(code = "Shiny.onInputChange('plot_region_create_name', null)")
      args = list(data_x = input$plot_click$coords_css$x,
                  data_y = input$plot_click$coords_css$y,
                  data_top = input$plot_click$range$top,
                  data_bottom = input$plot_click$range$bottom,
                  data_left = input$plot_click$range$left,
                  data_right = input$plot_click$range$right,
                  data_ratiox = input$plot_click$img_css_ratio$x,
                  data_ratioy = input$plot_click$img_css_ratio$y)
      shape_args = switch(plot_react$tool,
                          "line" = { list("line", x1 = args$data_x, y1 = args$data_y, x2 = args$data_x, y2 = args$data_y) },
                          "rectangle" = { list("rect", x = args$data_x, y = args$data_y, width = 0, height = 0) },
                          "ellipse" = { list("ellipse", cx = args$data_x, cy = args$data_y, rx = 0, ry = 0) },
                          "polygon" = { list("path", d = paste("M", paste(args$data_x, args$data_y, sep = ","), "Z", sep="")) },
                          "lasso" = { list("polygon", points = paste(args$data_x, args$data_y, sep=","), data_lastx = args$data_x, data_lasty = args$data_y) })
      
      removeUI(selector = sprintf("%s>.draw_shape", plot_react$current), immediate = TRUE)
      if(plot_react$tool == "polygon") {
        runjs(code = sprintf("$('%s').on('click', IFCshiny.draw_path )", plot_react$current))
      } else {
        runjs(code = sprintf("$('%s').on('mousemove', IFCshiny.draw_shape )", plot_react$current))
      }
      runjs(code = sprintf("Shiny.onInputChange('shape_param', { tool:'%s', init:false } );", plot_react$tool))
      insertUI(selector = plot_react$current,
               ui = tags$svg(class = "draw_shape ifcshiny_region_svg",
                             style = sprintf("top: 0px; left: 0px; width: %spx; height: %spx;",
                                             input$IFCshiny_getOffset_ret$width,
                                             input$IFCshiny_getOffset_ret$height),
                             do.call(what = tags[[shape_args[[1]]]], args = c(args, shape_args[-1]))),
               where = "beforeEnd", immediate = TRUE, session = session)
    } else {
      switch(plot_react$tool,
             "init" = {
               plot_hover <- "init"
               D = plot_react$plot$input$data
               if(plot_react$plot$input$type %in% c("count", "percent")) {
                 br = do.breaks(plot_react$plot$input$xlim, plot_react$plot$input$bin)
                 interval = findInterval(D$x2, br)
                 dx = diff(plot_react$plot$input$xlim)
                 dy = diff(plot_react$plot$input$ylim)
                 idx = which.min(((D$x2 - input$plot_click$x)/dx)^2)
                 if((length(plot_react$click) != 0) && (plot_react$click == idx)) return(NULL)
                 plot_react$click = idx
                 idx = interval[idx]
                 coord1 = map_coord_to_css(coord = c(br[idx], plot_react$plot$input$ylim[2]), map = input$plot_click)
                 coord2 = map_coord_to_css(coord = c(br[idx + 1], 0), map = input$plot_click)
                 width = (coord2$coords_css$x -  coord1$coords_css$x) * 2
                 height = (coord2$coords_css$y -  coord1$coords_css$y)
                 removeUI(selector = sprintf("%s>.click_bar", plot_react$current), immediate = TRUE)
                 insertUI(selector = plot_react$current, ui = tags$div(class = "click_bar ifcshiny_click_div", 
                                                                       style = sprintf("top: %spx; left: %spx; height: %spx; width: %spx;",
                                                                                       coord1$coords_css$y,
                                                                                       coord2$coords_css$x - width / 2,
                                                                                       height,
                                                                                       width)),
                          where = "beforeEnd", immediate = TRUE, session = session)
                 idx = interval == idx
               } else {
                 D = D[plot_react$plot$input$subset, , drop = FALSE]
                 dx = diff(plot_react$plot$input$xlim)
                 dy = diff(plot_react$plot$input$ylim)
                 idx = which.min(((D$x2-input$plot_click$x)/dx)^2 + ((D$y2-input$plot_click$y)/dy)^2)
                 if((length(plot_react$click) != 0) && (plot_react$click == idx)) return(NULL)
                 plot_react$click = idx
                 coord1 = map_coord_to_css(coord = c(D[idx,"x2"], D[idx,"y2"]), map = input$plot_click)
                 removeUI(selector = sprintf("%s>.click_point", plot_react$current), immediate = TRUE)
                 insertUI(selector = plot_react$current, ui = tags$div(class = "click_point ifcshiny_click_div", 
                                                                       style = sprintf("top: %spx; left: %spx;",
                                                                                       coord1$coords_css$y - 4, 
                                                                                       coord1$coords_css$x - 4)), 
                          where = "beforeEnd", immediate = TRUE, session = session)
               }
               if(length(obj_react$back$description$FCS)!=0) return(NULL)
               if(!any(obj_react$back$info$found)) {
                 mess_global(title = "clicking on graph", msg = "tips: provide a .cif file to display cell image", type = "info", duration = 10)
                 return(NULL)
               }
               tryCatch({
                 dat = ExportToGallery(param = param_react$param, extract_max = 10,
                                       objects = D[idx,"Object Number"],
                                       offsets = obj_react$back$offsets, export = "base64", image_type = "img",
                                       add_channels = TRUE, add_ids = 1, display_progress = FALSE)
                 html("plot_image_placeholder", dat)
               }, error = function(e) {
                 mess_global(title = "clicking on graph", msg = c("click on plot to display image produced an error:", e$message), type = "error", duration = 10)
               })
             },
             "edit" = {
               if(length(input$shape_moving) != 0) return(NULL)
               if(length(input$shape_selected) == 0) return(NULL)
               label = list(x = as.integer(unlist(input$shape_selected$cx)), y = as.integer(unlist(input$shape_selected$cy)))
               if(input$shape_selected$init) {
                 type = plot_react$plot$input$regions[[input$shape_selected$name]]$type
                 if(type == "poly") {
                   svg_ele = "polygon"
                   if(length(input$shape_selected$x) == 0) {
                     coords = data.frame(x = plot_react$region[, 3],
                                         y = plot_react$region[, 4])
                   } else {
                     coords = data.frame(x = unlist(input$shape_selected$x),
                                         y = unlist(input$shape_selected$y))
                   }
                   # prevent point from being inside shape
                   click = cbind(x = input$plot_click$coords_css$x, y = input$plot_click$coords_css$y)
                   if(cpp_pnt_in_gate(pnts = click, gate = cbind(coords$x, coords$y), algorithm = 1)) return(NULL)
                   # compute distance between points and click
                   d = apply(coords, 1, FUN = function(pt) {
                     (pt[1] - input$plot_click$coords_css$x)^2 + (pt[2] - input$plot_click$coords_css$y)^2
                   })
                   # keep closest one
                   closest = which.min(d)
                   # compute angle between forward or backward insertion
                   if(closest == nrow(coords)) {
                     pt2 = 1
                   } else {
                     pt2 = closest+1
                   }
                   if(closest == 1) {
                     pt1 = nrow(coords)
                   } else {
                     pt1 = closest-1
                   }
                   atan0 = atan2(coords[closest, 1] - click[1], coords[closest, 2] - click[2]);
                   atan1 = atan2(coords[pt1, 1] - click[1], coords[pt1, 2] - click[2]);
                   atan2 = atan2(coords[pt2, 1] - click[1], coords[pt2, 2] - click[2]);
                   angle1 = atan0 - atan1;
                   angle2 = atan0 - atan2;
                   if(abs(angle1) > abs(angle2)) closest = pt1
                   # insert point
                   if(closest == nrow(coords)) {
                     coords = rbind(coords, cbind(x = input$plot_click$coords_css$x, y = input$plot_click$coords_css$y))
                   } else {
                     coords = rbind(coords[1:closest, ],
                                    cbind(x = input$plot_click$coords_css$x, y = input$plot_click$coords_css$y),
                                    coords[(closest+1):nrow(coords), ])
                   }
                   anchors = lapply(1:nrow(coords), FUN = function(i_row) { list(cx = coords[i_row, 1],
                                                                                 cy = coords[i_row, 2],
                                                                                 ondblclick = "IFCshiny.remove_anchor(event)",
                                                                                 id = paste0("anchor_", i_row)) })
                   removeUI(selector = sprintf("%s>.edit_shape", plot_react$current), immediate = TRUE)
                   insertUI(selector = plot_react$current, ui = tags$svg(class = "edit_shape ifcshiny_region_svg",
                                                                         style = sprintf("top: 0px; left: 0px; width: %spx; height: %spx;",
                                                                                         input$IFCshiny_getOffset_ret$width,
                                                                                         input$IFCshiny_getOffset_ret$height),
                                                                         viewBox = sprintf('0 0 %i %i',
                                                                                           input$IFCshiny_getOffset_ret$width,
                                                                                           input$IFCshiny_getOffset_ret$height),
                                                                         tags$g(class = "shapes_layer", style = "pointer-events: all;",
                                                                                tags$clipPath(tags$rect(class = "shapes_clippath", style = "visibility:hidden; cursor:crosshair;",
                                                                                                        x = input$plot_click$range$left / input$plot_click$img_css_ratio$x,
                                                                                                        y = input$plot_click$range$top / input$plot_click$img_css_ratio$y,
                                                                                                        width = (input$plot_click$range$right - input$plot_click$range$left) / input$plot_click$img_css_ratio$x,
                                                                                                        height = (ifelse(plot_react$plot$input$type %in% c("count","percent"), 
                                                                                                                         map_coord_to_css(c(x = 0, y = 0), map = input$plot_click)$coords_css$y,
                                                                                                                         input$plot_click$range$bottom) - input$plot_click$range$top) / input$plot_click$img_css_ratio$y)),
                                                                                tags$g(class = "shape_layer", id = input$shape_selected$name,
                                                                                       tags$polygon(class = "shape draggable", points = paste0(apply(coords, 1, FUN = function(x) {
                                                                                         paste0(x, collapse = ",")}), collapse = " ")),
                                                                                       do.call(what = tags$text, args = c(label, list(input$shape_selected$name, class = "label draggable"))),
                                                                                       lapply(anchors, FUN = function(anchor) do.call(what = tags$circle, args = c(anchor, list(class = "anchor draggable"))))))),
                            where = "beforeEnd", immediate = TRUE, session = session)
                 } 
               } else {
                 runjs(code = sprintf("Shiny.onInputChange('shape_selected', {name:'%s', init:true, cx: %s, cy: %s})", regions_react$pre$name, label$x, label$y))
                 addClass(selector = sprintf("%s>.edit_shape>.shapes_layer>.shape_layer>.shape", plot_react$current), class = "draggable")
                 removeClass(selector = sprintf("%s>.edit_shape>.shapes_layer>.shape_layer>.label", plot_react$current), class = "hidden")
                 removeClass(selector = sprintf("%s>.edit_shape>.shapes_layer>.shape_layer>.anchor", plot_react$current), class = "hidden")
               }
             },
             "remove" = {
               if(length(regions_react$pre$name) == 0) return(NULL)
               obs_reg[["remove"]]$resume()
               click("reg_remove")
               click("plot_sel_init")
             })
    }
  }),
  
  # plot_brush is aimed to allow user to perform a brush selection on the shiny plot
  # it is only used during zoomin action so has to zoom the graph
  # brush information will be collected so has to trigger a zoom in
  observeEvent(input$plot_brush, suspended = TRUE, {
    if(!input$plot_unlock) return(NULL)
    if(!plot_react$tool %in% c("zoomin")) return(NULL)
    switch(plot_react$tool, 
           "zoomin" = {
             trans_x = parseTrans(plot_react$plot$input$trans_x)
             plot_react$xmin = applyTrans(input$plot_brush$xmin, trans_x, inverse = TRUE)
             plot_react$xmax = applyTrans(input$plot_brush$xmax, trans_x, inverse = TRUE)
             if(plot_react$plot$input$type %in% c("count", "percent")) {
               plot_react$ymin = input$plot_brush$ymin * diff(range(plot_react$plot$input$ylim))
               plot_react$ymax = input$plot_brush$ymax * diff(range(plot_react$plot$input$ylim))
             } else {
               trans_y = parseTrans(plot_react$plot$input$trans_y)
               plot_react$ymin = applyTrans(input$plot_brush$ymin, trans_y, inverse = TRUE)
               plot_react$ymax = applyTrans(input$plot_brush$ymax, trans_y, inverse = TRUE)
             }
             plot_react$zoomed = TRUE
           })
    click("plot_sel_init")
  }),
  
  # plot_hover observer is aimed to allow selection of a region already drawn in a plot
  # selection is only allowed when we need to remove or to edit a region
  # selection should not be possible when a region is already selected
  # first we identify all region drawn
  # then we need to test if mouse pointer is close to a region or another if several are drawn
  # - for 1D, we segment the 'line' type segment into 50 equal segments and we compute the distance 
  #   between each of them and the mouse. if this distance is less than 20 px we keep it
  # - for 2D, we compute the distance between mouse pointer and the mean of the shape vertices
  #   we also keep this value only if the mouse pointer is within the shape
  #   and finally we keep the closest one (i.e. the minimal distance)
  # once found we extract the selected region information
  # we then map the raw coordinates to css and redraw the shape by adding a svg other the plot
  # (i.e. the shiny div created by plotOuput / renderPlot)
  # finally, we test if the whole region is within the boundaries of the plot
  # if yes, further editing is possible because we register the region into input$shape_selected
  # if no, only the svg layer is drawn and we set its class to disabled to change its appearance + cursor 
  observeEvent(input$plot_hover, suspended = TRUE, {
    if(!input$plot_unlock) return(NULL)
    if(length(input$plot_hover) == 0) return(NULL)
    if("init" %in% plot_react$tool) {
      if(!any(c("scatter","density") %in% plot_react$plot$input$type)) return(NULL)
      dx = diff(plot_react$plot$input$xlim)
      dy = diff(plot_react$plot$input$ylim)
      
      # FIXME clip region is not working
      if((input$plot_hover$x <= input$plot_hover$domain$left) |
         (input$plot_hover$x >= input$plot_hover$domain$right) |
         (input$plot_hover$y <= input$plot_hover$domain$bottom) |
         (input$plot_hover$y >= input$plot_hover$domain$top)) return(NULL)
      
      #D = plot_react$plot$input$data[plot_react$plot$input$subset, , drop = FALSE]
      idx = which.min(((plot_react$plot$input$data[plot_react$plot$input$subset,"x2"]-input$plot_hover$x)/dx)^2 +
                        ((plot_react$plot$input$data[plot_react$plot$input$subset,"y2"]-input$plot_hover$y)/dy)^2)
      if(length(idx) != 0) {
        foo = plot_react$plot$input$data[plot_react$plot$input$subset,c("x2","y2","Object Number"),drop=FALSE][idx,]
      } else {
        return(NULL)
      }
      if((length(plot_react$closest) != 0) && (plot_react$closest == foo[[3]])) return(NULL)
      plot_react$closest = foo[[3]]
      coord1 = map_coord_to_css(coord = c(foo[[1]], foo[[2]]), map = input$plot_hover)
      
      removeUI(selector = sprintf("%s>.hover_point", plot_react$current), immediate = TRUE)
      insertUI(selector = plot_react$current, ui = tags$div(class = "hover_point ifcshiny_hover_div", 
                                                            style = sprintf("top: %spx; left: %spx;display:flex;",
                                                                            coord1$coords_css$y - 4,
                                                                            coord1$coords_css$x - 4
                                                                            ),
                                                            tags$div(style="top: -15px; left: 10px; position: absolute; background-color: black; color: white;", tags$p(foo[[3]]))), 
               where = "beforeEnd", immediate = TRUE, session = session)
      if(any(obj_react$back$info$found)) {
        tryCatch({
          info = obj_react$obj$info
          info$Images = param_react$param$channels
          param = objectParam(info = info,
                              mode = "rgb",
                              export = "base64",
                              write_to = "%s_%o.png",
                              base64_id = TRUE,
                              base64_att = "class='plot2D-hover-img' alt='plot2D hover img'",
                              overwrite = TRUE,
                              composite = "",
                              size = c(80,80),
                              force_width = FALSE,
                              random_seed = NULL,
                              removal = "none",
                              add_noise = TRUE,
                              full_range = "full_range" %in% input$chan_force,
                              force_range = "force_range" %in% input$chan_force)
          ifd = getIFD(fileName = param$fileName_image,
                       offsets = subsetOffsets(offsets = obj_react$back$offsets, objects= foo[[3]], image_type = "img"),
                       trunc_bytes = 1, force_trunc = TRUE, verbose = FALSE, display_progress = FALSE, bypass = TRUE)
          base64 = objectExtract(ifd = ifd, param = param, verbose = FALSE, bypass = TRUE)[[1]]
          runjs(code = paste0("var ele = document.getElementById('plot_1or2D');
                               if(ele != null) {;
                               var panel = ele.querySelector('.ifcshiny_hover_div');
                               if(panel != null) {;
                               var to_remove = panel.querySelector('.plot2D-hover-img');
                               if(to_remove != null) to_remove.remove();
                              var b = ",toJSON(rev(base64)),";
                              for (const [key, value] of Object.entries(b)) panel.insertAdjacentHTML('afterbegin', value[0]);
                              }}"))
        })
      }
      return(NULL)
    }
    if(!any(c("remove", "edit") %in% plot_react$tool)) return(NULL)
    trans_x = parseTrans(plot_react$plot$input$trans_x)
    trans_y = parseTrans(plot_react$plot$input$trans_y)
    if((length(input$shape_moving) != 0) || ((length(input$shape_selected) !=0) && input$shape_selected$init)) return(NULL)
    if(plot_react$plot$input$type %in% c("count","percent")) {
      d = sapply(plot_react$plot$input$regions, FUN = function(r) {
        r$x = applyTrans(r$x, trans_x)
        dy = diff(range(plot_react$plot$input$ylim))
        coords = rbind(unlist(map_coord_to_css(coord = c(r$x[1], r$y[1] * dy), map = input$plot_hover)$coords_css), 
                       unlist(map_coord_to_css(coord = c(r$x[2], r$y[2] * dy), map = input$plot_hover)$coords_css))
        min((seq(from = coords[1, "x"], to = coords[2, "x"] , length.out = 50) - input$plot_hover$coords_css$x)^2 +
              (coords[1, "y"]  - input$plot_hover$coords_css$y)^2, na.rm = TRUE)
      })
      d[d > 400] <- +Inf # 20 px diff
    } else {
      d = sapply(plot_react$plot$input$regions, FUN = function(r) {
        coords = r[c("x", "y")]
        coords$x = applyTrans(coords$x, trans_x)
        coords$y = applyTrans(coords$y, trans_y)
        alg = switch(r$type, "poly" = 1, "rect" = 2, "oval" = 3)
        # trick to test if mouse pointer is within the region
        # if yes, we compute the distance between mouse pointer and the vertices mean
        # if no, we claim that the distance if positive infinty
        if(cpp_pnt_in_gate(pnts = cbind(x = input$plot_hover$x, y = input$plot_hover$y), gate = cbind(coords$x, coords$y), algorithm = alg)) {
          return((mean(r$x) - input$plot_hover$x)^2 + (mean(r$y) - input$plot_hover$y)^2)
        } else {
          return(+Inf)
        }
      })
    }
    # if computed distance between pointer and and shape is not finite
    # no shape is selected, we remove the svg layer other the shiny plot
    if(!any(is.finite(unlist(d)))) {
      removeUI(selector = sprintf("%s>.edit_shape", plot_react$current), immediate = TRUE)
      regions_react$pre$name <- NULL
      return(NULL)
    }
    # we keep the closest, but first we check that is has not already been determined has 
    # the closest region. no need to rebuild the svg layer once again
    closest = names(plot_react$plot$input$regions)[which.min(d)]
    if(plot_react$tool == plot_react$hover) {
      if(closest %in% regions_react$pre$name) {
        return(NULL)
      } else {
        regions_react$pre$name <- closest
      }
    } else {
      plot_react$hover <- plot_react$tool
    }
    # we gather information about the region so has to build the svg layer over the shiny plot
    type = plot_react$plot$input$regions[[closest]]$type
    reg = obj_react$obj$regions[[closest]]
    reg$cx = signif(reg$cx, digits = 3)
    reg$cy = signif(reg$cy, digits = 3)
    reg$x = signif(reg$x, digits = 3)
    reg$y = signif(reg$y, digits = 3)
    regions_react$pre = reg
    regions_react$pre$name <- closest
    cx = plot_react$plot$input$regions[[closest]]$cx
    cy = plot_react$plot$input$regions[[closest]]$cy
    switch(type, 
           "line" = {
             svg_ele = "line"
             plot_react$region = data.frame(x = plot_react$plot$input$regions[[closest]]$x,
                                            y = plot_react$plot$input$regions[[closest]]$y * diff(range(plot_react$plot$input$ylim)))
             cy = cy * diff(range(plot_react$plot$input$ylim))
           },
           "rect" = {
             svg_ele = "rect"
             plot_react$region =data.frame(x = plot_react$plot$input$regions[[closest]]$x,
                                           y = plot_react$plot$input$regions[[closest]]$y)
             plot_react$region = rbind(plot_react$region[1,], plot_react$region)
           },
           "poly" = {
             svg_ele = "polygon"
             plot_react$region =data.frame(x = plot_react$plot$input$regions[[closest]]$x,
                                           y = plot_react$plot$input$regions[[closest]]$y)
           },
           "oval" = {
             svg_ele = "ellipse"
             plot_react$region = data.frame(x = plot_react$plot$input$regions[[closest]]$x,
                                            y = plot_react$plot$input$regions[[closest]]$y)
           })
    coords = plot_react$region 
    coords$x = applyTrans(coords$x, trans_x)
    cx = applyTrans(cx, trans_x)
    coords$y = applyTrans(coords$y, trans_y)
    cy = applyTrans(cy, trans_y)
    css = apply(coords, 1, FUN = function(coord) {
      unlist(map_coord_to_css(coord = coord, map = input$plot_hover)$coords_css)
    })
    plot_react$region$css_x = css[1,]
    plot_react$region$css_y = css[2,]
    label = map_coord_to_css(coord = c(cx, cy), map = input$plot_hover)$coords_css
    svg_args = switch(type, 
                      "line" = list(x1 = plot_react$region[1, 3], y1 = plot_react$region[1, 4],
                                    x2 = plot_react$region[2, 3], y2 = plot_react$region[2, 4]),
                      "rect" = list(x = min(plot_react$region[2:3, 3]),
                                    y = min(plot_react$region[2:3, 4]),
                                    width = diff(range(plot_react$region[2:3, 3])),
                                    height = diff(range(plot_react$region[2:3, 4]))),
                      "poly" = {
                        list(points = paste0(apply(plot_react$region, 1, FUN = function(x) {
                          paste0(x[c(3, 4)], collapse = ",")}
                        ), collapse = " "))
                      },
                      "oval" = list(cx = mean(plot_react$region[, 3]), 
                                    cy = mean(plot_react$region[, 4]),
                                    rx = mean(plot_react$region[, 3]) - min(plot_react$region[, 3]),
                                    ry = mean(plot_react$region[, 4]) - min(plot_react$region[, 4])))
    anchors = switch(type, 
                     "line" = lapply(1:2, FUN = function(i_row) { list(cx = plot_react$region[i_row, 3],
                                                                       cy = plot_react$region[i_row, 4],
                                                                       id = paste0("anchor_", i_row)) }),
                     "rect" = lapply(2:3, FUN = function(i_row) { list(cx = plot_react$region[i_row, 3],
                                                                       cy = plot_react$region[i_row, 4],
                                                                       id = paste0("anchor_", i_row - 1)) }),
                     "poly" = lapply(1:nrow(plot_react$region), FUN = function(i_row) { list(cx = plot_react$region[i_row, 3],
                                                                                             cy = plot_react$region[i_row, 4],
                                                                                             ondblclick = "IFCshiny.remove_anchor(event)",
                                                                                             id = paste0("anchor_", i_row)) }),
                     "oval" = list(list(cx = plot_react$region[1, 3], 
                                        cy = plot_react$region[1, 4],
                                        id = "anchor_0"),
                                   list(cx = plot_react$region[2, 3],
                                        cy = plot_react$region[2, 4],
                                        id = "anchor_1")))
    removeUI(selector = sprintf("%s>.edit_shape", plot_react$current), immediate = TRUE)
    insertUI(selector = plot_react$current, ui = tags$svg(class = "edit_shape ifcshiny_region_svg", 
                                                          style = sprintf("top: 0px; left: 0px; width: %spx; height: %spx;",
                                                                          input$IFCshiny_getOffset_ret$width,
                                                                          input$IFCshiny_getOffset_ret$height),
                                                          viewBox = sprintf('0 0 %i %i',
                                                                            input$IFCshiny_getOffset_ret$width,
                                                                            input$IFCshiny_getOffset_ret$height),
                                                          tags$g(class = "shapes_layer", style = "pointer-events: all;", 
                                                                 tags$clipPath(tags$rect(class = "shapes_clippath", style = "visibility:hidden; cursor:crosshair;",
                                                                                         x = input$plot_hover$range$left / input$plot_hover$img_css_ratio$x,
                                                                                         y = input$plot_hover$range$top / input$plot_hover$img_css_ratio$y,
                                                                                         width = (input$plot_hover$range$right - input$plot_hover$range$left) / input$plot_hover$img_css_ratio$x,
                                                                                         height = (ifelse(plot_react$plot$input$type %in% c("count","percent"), 
                                                                                                         map_coord_to_css(c(x = 0, y = 0), map = input$plot_hover)$coords_css$y, # trans_x = plot_react$plot$input$trans_x, trans_y = plot_react$plot$input$trans_y)$coords_css$y,
                                                                                                         input$plot_hover$range$bottom) - input$plot_hover$range$top) / input$plot_hover$img_css_ratio$y)),
                                                                 tags$g(class = "shape_layer", id = closest, 
                                                                        do.call(what = tags[[svg_ele]], args = c(svg_args, list(class = "shape"))),
                                                                        do.call(what = tags$text, args = c(label, list(closest, class = "label draggable hidden"))),
                                                                        lapply(anchors, FUN = function(anchor) do.call(what = tags$circle, args = c(anchor, list(class = "anchor draggable hidden"))))))),
             where = "beforeEnd", immediate = TRUE, session = session)
    # now we test if the whole selected region is within the graph displayed
    # if not we disable region editing
    ran_x = range(css[1, ])
    ran_y = range(css[2, ])
    if(ran_x[2] > input$plot_hover$range$right / input$plot_hover$img_css_ratio$x ||
       ran_x[1] < input$plot_hover$range$left / input$plot_hover$img_css_ratio$x||
       ran_y[2] > input$plot_hover$range$bottom / input$plot_hover$img_css_ratio$y ||
       ran_y[1] < input$plot_hover$range$top / input$plot_hover$img_css_ratio$y) {
      runjs(code = "$('.edit_shape').addClass('disabled')")
    } else {
      runjs(code = sprintf("Shiny.onInputChange('shape_selected', {name:'%s', init:false, cx: %s, cy: %s})", regions_react$pre$name, label$x, label$y))
    }
  }),
  
  # plot_dblclick observer is intended to terminate a drawing action
  # it could eventually also terminate a remove action
  # before, allowing termination of drawing we test if shape is correctly drawn (i.e. with minimum amount of point)
  # otherwise a message is thrown. If correct, the coordinates are retrieved and mapped from css to raw
  # finally, for remove or drawing termination a modal is launched to allow user to further validate the process
  observeEvent(input$plot_dblclick, suspended = TRUE, {
    if(!input$plot_unlock) return(NULL)
    if(any(c("line", "rectangle", "polygon", "lasso", "ellipse") %in% plot_react$tool)) {
      if((length(input$shape_param) != 0) && !input$shape_param$init) {
        runjs(code = "Shiny.onInputChange('shape_param', null)")
        mess_global(title = "creating region", msg = paste(plot_react$tool, ": draw more points", sep = ""), type = "info", duration = 10)
        removeModal(session = session)
        return(NULL)
      }
      plot_react$action="none"
      if(any(c("line", "rectangle","ellipse") %in% plot_react$tool)) {
        coords1 = map_css_to_coord(css = c(x = input$shape_param$x1, y = input$shape_param$y1), map = input$plot_dblclick)
        coords2 = map_css_to_coord(css = c(x = input$shape_param$x2, y = input$shape_param$y2), map = input$plot_dblclick)
        plot_react$region = data.frame(x = c(coords1$x, coords2$x),
                                       y = c(coords1$y, coords2$y),
                                       css_x = c(input$shape_param$x1, input$shape_param$x2),
                                       css_y = c(input$shape_param$y1, input$shape_param$y2))
      }
      if(any(c("lasso", "polygon") %in% plot_react$tool)) {
        if(any("lasso" %in% plot_react$tool)) {
          points = try(strsplit(x = input$shape_param$points, split = " ", fixed = TRUE)[[1]], silent = TRUE)
        } else {
          points = try(strsplit(x = substring(input$shape_param$d, 2, nchar(input$shape_param$d)-1), split = "L", fixed = TRUE)[[1]], silent = TRUE)
        }
        if(inherits(x = points, what = "try-error") || (length(points) < 3)) {
          runjs(code = "Shiny.onInputChange('shape_param', null)")
          mess_global(title = "creating region", msg = paste(plot_react$tool, ": draw more points", sep = ""), type = "info", duration = 10)
          removeModal(session = session)
          return(NULL)
        }
        points = do.call(what = rbind, args = strsplit(x = points, split = ",", fixed = TRUE))
        coords = apply(points, 1, FUN = function(pt) {
          pt = as.numeric(pt)
          unlist(map_css_to_coord(css = c(x = pt[1], y = pt[2]), map = input$plot_dblclick))
        })
        plot_react$region = data.frame(x = coords[1, ],
                                       y = coords[2, ],
                                       css_x = as.numeric(points[, 1]),
                                       css_y = as.numeric(points[, 2]))
      }
      runjs(code = "Shiny.onInputChange('shape_param', null)")
      removeUI(selector = sprintf("%s>.draw_shape", plot_react$current), immediate = TRUE)
      showModal(modalDialog(title = "Create region",
                            tags$div(id = "plot_region_add_ui",
                                     tags$div(style = "display:inline-block; vertical-align:top; width:50%",
                                              textInput(inputId = "plot_region_create_name", label = "name", value = "")),
                                     tags$div(style = "display:inline-block; vertical-align:top; width:34%",
                                              colourpicker::colourInput(inputId = "plot_region_create_color", label = "Color", showColour = "background", returnName = TRUE, value  = "Red",
                                                                        allowTransparent = FALSE, palette = "limited", allowedCols = unique(unlist(paletteIFC()[,  c("color_R",  "lightModeColor_R")]))))),
                            footer = list(actionButton(inputId = "plot_region_create_ok", label = "Create"),
                                          actionButton(inputId = "plot_region_create_abort", label = "Cancel")),
                            easyClose = FALSE), 
                session = session)
    } else {
      if("edit" %in% plot_react$tool) {
        if(length(input$shape_selected) != 0) {
          # runjs("Shiny.onInputChange('reg_manager_visible', true)")
          return(NULL)
        }
        add_log("Region Edition")
        runjs("Shiny.onInputChange('reg_manager_visible', true)")
        disable("reg_def_table_feedback")
        
        reg = regions_react$pre
        updateSelectInput(session = session, inputId = "reg_selection", selected = regions_react$pre$name)
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
        reg_def(reg = regions_react$pre, all_names = names(obj_react$obj$regions), check = "valid", session = getDefaultReactiveDomain())
        click("plot_sel_init")
      }
      if("remove" %in% plot_react$tool) {
        obs_reg[["remove"]]$resume()
        click("reg_remove")
        click("plot_sel_init")
      }
    }
  }),
  observeEvent(input$plot_region_create_name, suspended = TRUE, {
    hideFeedback(session = session, inputId = "plot_region_create_name")
    if(any(input$population=="")) return(NULL)
    if((input$plot_region_create_name %in% c(names(obj_react$obj$regions), "And", "Or", "Not", "(", ")", "All", "") || grepl("^ML_", input$plot_region_create_name))) {
      if(length(input$plot_region_create_name) == 0) showFeedbackDanger(session = session, inputId = "plot_region_create_name", text = "empty")
      if(input$plot_region_create_name == "") showFeedbackDanger(session = session, inputId = "plot_region_create_name", text = "empty")
      if(input$plot_region_create_name %in% names(obj_react$obj$regions)) showFeedbackDanger(session = session, inputId = "plot_region_create_name", text = "already exists") 
      if(grepl("^ML_", input$plot_region_create_name)) showFeedbackDanger(session = session, inputId = "plot_region_create_name", text = "not allowed") 
      if(input$plot_region_create_name %in% c("And", "Or", "Not", "(", ")", "All")) showFeedbackDanger(session = session, inputId = "plot_region_create_name", text = "not allowed") 
      disable("plot_region_create_ok")
    } else {
      enable("plot_region_create_ok")
    }
  }),
  observeEvent(input$plot_region_create_abort, suspended = TRUE,{
    click("plot_sel_init")
    removeModal(session=session)
  }),
  observeEvent(input$plot_region_create_ok, suspended = TRUE, {
    if(length(obj_react$back$info$in_use) == 0) return(NULL)
    if(any(input$population=="")) return(NULL)
    if(input$plot_type=="") return(NULL)
    if(input$navbar!="tab3") return(NULL)
    if(input$plot_x_feature != plot_react$x_feat) return(NULL)
    if(input$plot_y_feature != plot_react$y_feat) return(NULL)
    if(input$plot_x_transform != plot_react$x_trans) return(NULL)
    if(input$plot_y_transform != plot_react$y_trans) return(NULL)
    obj_back = obj_react$obj
    tryCatch({
      xx = plot_react$region[, "x"]
      yy = plot_react$region[, "y"]
      switch(plot_react$tool, 
             "line" = {
               yy = yy[1] / diff(range(plot_react$plot$input$ylim))
               cx = min(xx) + diff(range(xx)) / 3
               cy = yy + 0.1
               yy = c(yy, yy)
               type = "line"
             },
             "rectangle" = {
               cx = min(xx) 
               cy = max(yy) + diff(range(plot_react$plot$input$ylim))/10
               type = "rect"
             },
             "lasso" = {
               cx = min(xx) + diff(range(xx)) / 3
               cy = min(yy) + diff(range(yy)) / 3
               type = "poly"
             },
             "polygon" = {
               cx = min(xx) + diff(range(xx)) / 3
               cy = min(yy) + diff(range(yy)) / 3
               type = "poly"
             },
             "ellipse" = {
               cx = min(xx) + diff(range(xx)) / 3
               cy = min(yy) + diff(range(yy)) / 3
               type = "oval"
             },
             {
               stop("not allowed")
             })
      x_trans = plot_react$x_trans
      trans_x = parseTrans(plot_react$x_trans)
      xx = applyTrans(xx, trans_x, inverse = TRUE)
      cx = applyTrans(cx, trans_x, inverse = TRUE)
      fy = NULL
      y_trans = "P"
      if(input$plot_type == "2D") {
        fy = input$plot_y_feature
        y_trans = plot_react$y_trans
        trans_y = parseTrans(plot_react$y_trans)
        yy = applyTrans(yy, trans_y, inverse = TRUE)
        cy = applyTrans(cy, trans_y, inverse = TRUE)
      }
      reg = buildRegion(type = type, label = input$plot_region_create_name,
                        color = match_col(input$plot_region_create_color),
                        xlogrange = x_trans, 
                        ylogrange = y_trans,
                        x = xx, y = yy, cx = cx, cy = cy)
      
      pop = lapply(plot_react$plot$input$base, FUN = function(p) {
        if(p$name == "All") {
          pop_name = input$plot_region_create_name
        } else {
          pop_name = paste(p$name, input$plot_region_create_name, sep = " & ")
        }
        buildPopulation(name = pop_name, type = "G", base = p$name,
                        color = reg$color, lightModeColor = reg$lightcolor, 
                        region = reg$label, fx = input$plot_x_feature, fy = fy)
      })
      obj_react$obj = data_add_regions(obj_react$obj, regions = list(reg), session = session)
      obj_react$obj = data_add_pops(obj_react$obj, pops = pop, display_progress = TRUE, session = session)
      updateSelectInput(session=session, inputId = "plot_regions", choices = sort(c(plot_react$allowed_regions, reg$label)), 
                        selected = c(input$plot_regions, reg$label))
      plot_react$allowed_regions = sort(c(plot_react$allowed_regions, reg$label))
    }, error = function(e) {
      mess_global(title = "creating region", msg = e$message, type = "error", duration = 10)
      obj_react$obj = obj_back
    }, finally = {
      runjs("Shiny.onInputChange('pop_edit', null)")
      click("plot_sel_init")
      removeModal()
    })
  }),
  observeEvent(input$plot_reset, suspended = TRUE, {
    reinit_plot_3D(session=session)
    type = as.integer(gsub("D", "", input$plot_type))
    updateSelectInput(session=session, inputId = "plot_x_feature", selected = "Object Number")
    updateTextInput(session = session, inputId = "plot_x_transform", value = "P")
    if(type >= 2) {
      updateSelectInput(session=session, inputId = "plot_y_feature", selected = "Object Number")
      updateTextInput(session = session, inputId = "plot_y_transform", value = "P")
    }
    if(type >= 3) {
      updateSelectInput(session=session, inputId = "plot_z_feature", selected = "Object Number")
      updateTextInput(session = session, inputId = "plot_z_transform", value = "P")
      # } else {
      #   updateSelectInput(session = session, inputId = "plot_font_main", selected = plot_react$g$graphtitlefontsize)
      #   updateSelectInput(session = session, inputId = "plot_font_axis_lab", selected = plot_react$g$axislabelsfontsize)
      #   updateSelectInput(session = session, inputId = "plot_font_tick_lab", selected = plot_react$g$axistickmarklabelsfontsize)
      #   updateSelectInput(session = session, inputId = "plot_font_region", selected = plot_react$g$regionlabelsfontsize)
    }
    plot_react$zoomed = FALSE
    runjs(code = "Shiny.onInputChange('plot_shown', null)")
    plot_react$allowed_regions = NULL
    runjs(code = "Shiny.onInputChange('plot_regions', null)")
    updateSelectInput(session=session, inputId="plot_regions", choices=list(), selected=NULL)
    runjs(code = "Shiny.onInputChange('plot_shown_order', null)")
  }),
  observeEvent(input$plot_type, suspended = TRUE, {
    hideElement(id = "plot_y")
    hideElement(id = "plot_z")
    hideElement(id = "plot_1D_options")
    hideElement(id = "plot_2D_options")
    hideElement(id = "plot_2D_options_main")
    hideElement(id = "plot_3D_options")
    showElement(selector = ".plot_sel_tools")
    # showElement(id = "plot_fonts")
    hideElement(id = "plot_image_placeholder")
    html("plot_image_placeholder", NULL)
    hideElement(id = "plot_stats_placeholder")
    
    output$plot_1or2D_placeholder <- renderUI({
      args = list(outputId = "plot_1or2D", width = "600px", height = "600px",
                  brush = brushOpts(
                    id = "plot_brush",
                    fill = input$plot_region_color,
                    stroke = "#036",
                    opacity = 0.25,
                    delay = 300000,
                    delayType = c("debounce"),
                    clip = TRUE,
                    direction = ifelse(input$plot_type == "2D", "xy", "x"),
                    resetOnNew = TRUE))
      args = c(args, list(click = clickOpts(id = "plot_click",
                                            clip = TRUE),
                          dblclick = "plot_dblclick",
                          hover = hoverOpts(id = "plot_hover",
                                            delay = 50,
                                            delayType = "throttle",
                                            clip = TRUE,
                                            nullOutside = TRUE)))
      do.call(what = "plotOutput", args = args)
    })
    disable("plot_manager")
    switch(input$plot_type,
           "1D" = {
             enable("plot_manager")
             hideElement(id = "plot_shown")
             showElement(id = "plot_1D_options")
             showElement(id = "order_placeholder")
             hideElement(id = "plot_3D_placeholder")
             showElement(id = "plot_1or2D_placeholder")
             showElement(id = "plot_image_placeholder")
             showElement(id = "plot_stats_placeholder")
             updateRadioButtons(session = session, inputId = "graph_save_type", choices = "pdf", selected = "pdf", inline = TRUE)
             plot_react$current = "#plot_1or2D"
             session$sendCustomMessage("getOffset", sprintf("%s",plot_react$current))
             set_tool(tool = "init")
           },
           "2D" = {
             enable("plot_manager")
             hideElement(id = "plot_shown")
             hideElement(id = "order_placeholder")
             hideElement(id = "plot_3D_placeholder")
             showElement(id = "plot_1or2D_placeholder")
             showElement(id = "plot_2D_options")
             showElement(id = "plot_2D_options_main")
             if(input$plot_type_2D_option01 == "scatter") {
               showElement(id = "plot_shown")
               showElement(id = "order_placeholder")
               updateSelectInput(session=session, inputId="plot_shown", selected=NULL)
             } 
             showElement(id = "plot_y")
             showElement(id = "plot_image_placeholder")
             showElement(id = "plot_stats_placeholder")
             updateRadioButtons(session = session, inputId = "graph_save_type", choices = "pdf", selected = "pdf", inline = TRUE)
             plot_react$current = "#plot_1or2D"
             session$sendCustomMessage("getOffset", sprintf("%s",plot_react$current))
             set_tool(tool = "init")
           },
           "3D" = {
             plot_react$current = "plot_3D"
             if(!input$webgl_available) {
               mess_global(title = "3D graph", msg = c("It looks like your WebGL is not supported by your browser", "WebGL is required for 3D graphs", "Consequently, 3D graph is not possible"), type = "warning")
               updateRadioButtons(session=session, inputId="plot_type", selected="2D", inline = TRUE)
               return(NULL)
             }
             hideElement(id = "plot_shown")
             showElement(id = "plot_3D_options")
             hideElement(id = "plot_1or2D_placeholder")
             showElement(id = "plot_3D_placeholder")
             showElement(id = "order_placeholder")
             showElement(id = "plot_y")
             showElement(id = "plot_z")
             hideElement(id = "plot_regions")
             hideElement(selector = ".plot_sel_tools")
             # hideElement(id = "plot_fonts")
             reinit_plot_3D(session=session)
             if((length(input$plot_z_feature) == 0) || !(input$plot_z_feature %in% names(obj_react$obj$features))) {
               updateSelectInput(session=session, inputId = "plot_z_feature", selected = "Object Number")
               updateTextInput(session = session, inputId = "plot_z_transform", value = "P")
             }
             updateRadioButtons(session = session, inputId = "graph_save_type", choices = "html", selected = "html", inline = TRUE)
           })
    if(!input$plot_unlock) {
      runjs(code = "Shiny.onInputChange('plot_shown', null)")
      runjs(code = "Shiny.onInputChange('plot_shown_order', null)")
    }
  }),
  observeEvent(input$plot_type_3D_option03, suspended = TRUE,{
    if(length(obj_react$back$info$in_use) == 0) return(NULL)
    if(any(input$population=="")) return(NULL)
    if(input$plot_type=="") return(NULL)
    if(input$navbar!="tab3") return(NULL)
    # sub = apply(do.call(what = rbind, args = lapply(obj_react$obj$pops[input$plot_base], FUN = function(p) p$obj)), 2, any)
    sub = fastAny(lapply(obj_react$obj$pops[input$plot_base], FUN = function(p) p$obj))
    sub[-sample(x = which(sub), size = sum(sub) * input$plot_type_3D_option03 / 100, replace = FALSE)] <- FALSE
    plot_react$subset = sub 
    plot_react$color = (sapply(obj_react$obj$pops[plot_react$order], FUN = function(p) {
      p$lightModeColor
    }))
    plot_react$symbol = (sapply(obj_react$obj$pops[plot_react$order], FUN = function(p) {
      p$style
    }))
  }),
  observeEvent(input$plot_type_3D_option01,suspended = TRUE, {
    if(length(obj_react$back$info$in_use) == 0) return(NULL)
    if(any(input$population=="")) return(NULL)
    if(input$plot_type=="") return(NULL)
    if(input$navbar!="tab3") return(NULL)
    # sub = apply(do.call(what = rbind, args = lapply(obj_react$obj$pops[input$plot_base], FUN = function(p) p$obj)), 2, any)
    sub = fastAny(lapply(obj_react$obj$pops[input$plot_base], FUN = function(p) p$obj))
    sub[-sample(x = which(sub), size = sum(sub) * input$plot_type_3D_option03 / 100, replace = FALSE)] <- FALSE
    plot_react$subset = sub 
    plot_react$color = (sapply(obj_react$obj$pops[plot_react$order], FUN = function(p) {
      p$lightModeColor
    }))
    plot_react$symbol = (sapply(obj_react$obj$pops[plot_react$order], FUN = function(p) {
      p$style
    }))
  }),
  hover_3D = observeEvent(input$rgl_3D_hover, suspended = TRUE, {
    if(length(input$rgl_3D_hover) == 0) return(NULL)
    if(packageVersion("rgl") < '0.106.19') {
      foo = as.integer(unlist(input$rgl_3D_hover)[3])
      if(plot_react$closest != foo) {
        plot_react$closest <- foo
        if(any(obj_react$back$info$found)) {
          tryCatch({
            info = obj_react$obj$info
            info$Images = param_react$param$channels
            param = objectParam(info = info,
                                mode = "rgb",
                                export = "base64",
                                write_to = "%s_%o.png",
                                base64_id = TRUE,
                                base64_att = "class='plot3D-hover-img' alt='plot3D hover img'",
                                overwrite = TRUE,
                                composite = "",
                                selection = as.integer(input$plot_3D_draw_chan),
                                size = c(as.integer(input$chan_height), as.integer(input$chan_width)), # or c(80,80) ?
                                force_width = FALSE,
                                random_seed = NULL,
                                removal = "none",
                                add_noise = TRUE,
                                full_range = "full_range" %in% input$chan_force,
                                force_range = "force_range" %in% input$chan_force)
            ifd = getIFD(fileName = param$fileName_image,
                         offsets = subsetOffsets(offsets = obj_react$back$offsets, objects= foo, image_type = "img"),
                         trunc_bytes = 1, force_trunc = TRUE, verbose = FALSE, display_progress = FALSE, bypass = TRUE)
            base64 = objectExtract(ifd = ifd, param = param, verbose = FALSE, bypass = TRUE)[[1]][[1]]
            runjs(code = paste0("var ele = document.getElementById('plot_3D');
                                   if(ele != null) {;
                                   var panel = ele.querySelector('.plot3D-hover');
                                   if(panel != null) {;
                                   var to_remove = panel.querySelector('.plot3D-hover-img');
                                   if(to_remove != null) to_remove.remove();
                                   panel.insertAdjacentHTML('afterbegin', \"", base64, "\");}}"))
          })
        }
      }
    } else {
      foo = plot_react$plot$input$data[plot_react$plot$input$clust[plot_react$plot$input$sub] == input$rgl_3D_hover$closest[1], "Object Number"][as.integer(input$rgl_3D_hover$closest[2]) + 1]
      if(plot_react$closest != foo) {
        plot_react$closest <- foo
        tryCatch({
          if(any(obj_react$back$info$found)) {
            info = obj_react$obj$info
            info$Images = param_react$param$channels
            param = objectParam(info = info,
                                mode = "rgb",
                                export = "base64",
                                write_to = "%s_%o.png",
                                base64_id = FALSE,
                                base64_att = "",
                                overwrite = TRUE,
                                composite = "",
                                selection = as.integer(input$plot_3D_draw_chan),
                                size = c(64,64),
                                force_width = FALSE,
                                random_seed = NULL,
                                removal = "none",
                                add_noise = TRUE,
                                full_range = "full_range" %in% input$chan_force,
                                force_range = "force_range" %in% input$chan_force)
            ifd = getIFD(fileName = param$fileName_image,
                         offsets = subsetOffsets(offsets = obj_react$back$offsets, objects= foo, image_type = "img"),
                         trunc_bytes = 1, force_trunc = TRUE, verbose = FALSE, display_progress = FALSE, bypass = TRUE)
            base64 = objectExtract(ifd = ifd, param = param, verbose = FALSE, bypass = TRUE)[[1]][[1]]
            runjs(code = sprintf("var ele = document.getElementById('plot_3D');
                              if(ele != null) {
                                var rgl = ele.rglinstance;
                                if(rgl != null) {
                                  if((Object.keys(rgl.scene.objects).indexOf('' + %i) >= 0) && (Object.keys(rgl.scene.objects).indexOf('' + %i) >= 0)) {
                                    var txt = rgl.getObj(%i);
                                    var img = rgl.getObj(%i);
                                    img.initialized = txt.initialized = false;
                                    txt.texts[0] = '%s';
                                    img.material.uri = %s;
                                    rgl.drawScene();
                                  }
                                }
                              }", input$rgl_3D_hover$hover$text, input$rgl_3D_hover$hover$image, 
                                 input$rgl_3D_hover$hover$text, input$rgl_3D_hover$hover$image,
                                 foo, gsub("^<img.*src=('.*')>$", "\\1",base64)))
          } else {
            runjs(code = sprintf("var ele = document.getElementById('plot_3D');
                              if(ele != null) {
                                var rgl = ele.rglinstance;
                                if(rgl != null) {
                                if(Object.keys(rgl.scene.objects).indexOf('' + %i) >= 0) {
                                  var txt = rgl.getObj(%i);
                                  txt.initialized = false;
                                  txt.texts[0] = '%s';
                                  rgl.drawScene();
                                  }
                                }
                              }", input$rgl_3D_hover$hover$text, input$rgl_3D_hover$hover$text, foo))
          }
        })
      }
    }
  }),
  observeEvent(input$plot_type_2D_option01, suspended = TRUE, {
    if(!input$plot_unlock) {
      html(id = "plot_order", add = FALSE, html = NULL)
      updateSelectInput(session = session, inputId = "plot_shown", selected = NULL)
      plot_react$shown = NULL
      plot_react$order = NULL
      runjs(code = "Shiny.onInputChange('plot_shown', null)")
      runjs(code = "Shiny.onInputChange('plot_shown_order', null)")
    }
    if((input$plot_type_2D_option01 == "scatter") && (input$plot_type == "2D")) {
      showElement(id = "plot_shown")
      showElement(id = "order_placeholder")
    } else {
      hideElement(id = "plot_shown")
      hideElement(id = "order_placeholder")
    }
  }),
  observeEvent(input$plot_x_feature, suspended = TRUE,{
    plot_react$x_feat = input$plot_x_feature
    if(input$plot_unlock) plot_react$zoomed = FALSE
  }),
  observeEvent(input$plot_x_transform, suspended = TRUE,{
    plot_react$x_trans = input$plot_x_transform
    if(input$plot_unlock) plot_react$zoomed = FALSE
  }),
  observeEvent(input$plot_y_feature, suspended = TRUE,{
    plot_react$y_feat = input$plot_y_feature
    if(input$plot_unlock) plot_react$zoomed = FALSE
  }),
  observeEvent(input$plot_y_transform,suspended = TRUE, {
    plot_react$y_trans = input$plot_y_transform
    if(input$plot_unlock) plot_react$zoomed = FALSE
  }),
  observeEvent(input$plot_z_feature,suspended = TRUE, {
    plot_react$z_feat = input$plot_z_feature
  }),
  observeEvent(input$plot_z_transform,suspended = TRUE, {
    plot_react$z_trans = input$plot_z_transform
  }),
  observeEvent(input$plot_manager, suspended = TRUE,{
    runjs("Shiny.setInputValue('graph_manager_visible', true)")
  }),
  observeEvent(input$graph_close, suspended = TRUE,{
    runjs("Shiny.onInputChange('graph_manager_visible', false)")
  }),
  observe(#list(input$plot_type,
                    # plot_react$x_feat,
                    # plot_react$x_trans,
                    # plot_react$y_feat,
                    # plot_react$y_trans,
                    # plot_react$order,
                    # input$plot_type_2D_option01,
                    # input$plot_type_1D_option01,
                    # input$plot_type_1D_option02,
                    # input$plot_type_1D_option03,
                    # input$plot_regions,
                    # plot_react$allowed_regions,
                    # plot_react$xmin,
                    # plot_react$xmax,
                    # plot_react$ymin,
                    # plot_react$ymax),
  suspended = TRUE,
               {
                 # plot_react$param_ready = FALSE
                 if(length(input$file) == 0) return(NULL)
                 if(input$navbar!="tab3") return(NULL)
                 if(input$plot_type == "3D") return(NULL)
                 if(length(input$plot_base) < 1) return(NULL)
                 if(any(input$plot_base=="")) return(NULL)
                 if(input$plot_x_feature != plot_react$x_feat) return(NULL)
                 if(input$plot_y_feature != plot_react$y_feat) return(NULL)
                 if(input$plot_x_transform != plot_react$x_trans) return(NULL)
                 if(input$plot_y_transform != plot_react$y_trans) return(NULL)
                 if(input$plot_type_2D_main_option03 == 0) return(NULL)
                 runjs("document.getElementById('msg_busy_txt2').innerText = 'updating plot';")
                 runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
                 if(input$plot_type == "1D") {
                   hideElement("plot_shown")
                   showElement("order_placeholder")
                   if(!all(input$plot_shown %in% input$plot_base) || !all(input$plot_base %in% input$plot_shown)) {
                     updateSelectInput(session=session, inputId="plot_shown", selected = input$plot_base)
                     return(NULL)
                   }
                 } else {
                   if(!all(input$plot_base %in% input$plot_shown)) {
                     if(input$plot_type_2D_option01 == "scatter") {
                       showElement("plot_shown")
                       showElement("order_placeholder")
                       updateSelectInput(session=session, inputId="plot_shown", selected = input$plot_base)
                       return(NULL)
                     } else {
                       hideElement("plot_shown")
                       hideElement("order_placeholder")
                       updateSelectInput(session=session, inputId="plot_shown", selected = input$plot_base[1])
                       if(length(input$plot_base) != 1) {
                         updateSelectInput(session=session, inputId="plot_base", selected = input$plot_base[1])
                         mess_global(title = paste0("plot_", input$plot_type), msg = "Density graphs can only display one BasePop population", type = "warning", duration = 10)
                         return(NULL)
                       }
                     }
                   }
                 }
                 allowed_regions = lapply(obj_react$obj$pops, FUN = function(p) {
                   if(p$type != "G") return(NULL)
                   r = obj_react$obj$regions[[p$region]]
                   if(input$plot_type == "1D") {
                     if(r$type != "line") return(NULL)
                     if((p$fx == plot_react$x_feat) &&
                        (r$xlogrange == plot_react$x_trans)) return(r$label)
                   } else {
                     if(length(p$fy) == 0) return(NULL)
                     if((p$fx == plot_react$x_feat) &&
                        (r$xlogrange == plot_react$x_trans) &&
                        (p$fy == plot_react$y_feat) &&
                        (r$ylogrange == plot_react$y_trans)) return(r$label)
                   }
                 })
                 allowed_regions = sort(unname(unique(unlist(allowed_regions)), force = TRUE))
                 if(!identical(allowed_regions, plot_react$allowed_regions)) {
                   plot_react$allowed_regions <- allowed_regions
                   N = allowed_regions; if(length(N) == 0) N = list()
                   updateSelectInput(session=session, inputId="plot_regions", choices = N, selected = NULL)
                   return(NULL)
                 }
                 if(input$plot_type == "1D") {
                   args = list(type = "histogram",
                               f1 = plot_react$x_feat,
                               f2 = "Object Number",
                               scaletype = 0,
                               xlogrange = plot_react$x_trans,
                               ylogrange = "P",
                               histogramsmoothingfactor = input$plot_type_1D_option02 == "smooth",
                               freq = ifelse((length(input$plot_type_1D_option03) != 0) && input$plot_type_1D_option03, "T", "F"),
                               BasePop = lapply(plot_react$order, FUN = function(p) list(name = p, inestyle = "Solid", fill = "true")),
                               ShownPop = list(list()))
                 }
                 if(input$plot_type == "2D") {
                   args = list(type = ifelse(input$plot_type_2D_option01 == "level","density",input$plot_type_2D_option01),
                               f1 = plot_react$x_feat,
                               f2 = plot_react$y_feat,
                               scaletype = 0,
                               xlogrange = plot_react$x_trans,
                               ylogrange = plot_react$y_trans,
                               BasePop = lapply(input$plot_base, FUN = function(p) list(name = p, inestyle = "Solid", fill = "true")))
                   if(input$plot_type_2D_option01 == "scatter") {
                     args = c(args,list(ShownPop = lapply(plot_react$order[plot_react$order %in% names(obj_react$obj$pops)], FUN = function(x) list("name" = x))))
                   } else {
                     args = c(args,list(ShownPop = list(list())))
                   }
                 }
                 tryCatch({
                   g = do.call(what = buildGraph, args = args)
                 # level/density
                 if(input$plot_type_2D_option01 == "level") {
                   g$BasePop[[1]]$densitylevel = paste(ifelse(input$plot_level_fill,"true","false"),
                                                       ifelse(input$plot_level_lines,"true","false"),
                                                       input$plot_level_nlevels,input$plot_level_lowest,sep="|")
                 }
                 if(g$type == "density") {
                   # if(!plot_react$param_ready) {
                   if(length(plot_react$g$BasePop[[1]]$densitycolorslightmode) == 0) {
                     plot_react$g$BasePop[[1]]$densitycolorslightmode <- "-16776961|-13447886|-256|-23296|-65536|"
                   }
                   plot_react$densitycolorslightmode <- plot_react$g$BasePop[[1]]$densitycolorslightmode
                   plot_react$densitytrans <- plot_react$g$BasePop[[1]]$densitytrans
                   plot_react$densitycolorslightmode_selected = "initial"
                   plot_react$densitytrans_selected = "initial"
                   if(input$plot_dens_order %% 2) click("plot_dens_order")
                   # }
                   g$BasePop[[1]]$densitycolorslightmode <- plot_react$g$BasePop[[1]]$densitycolorslightmode
                   if(input$plot_type_2D_option01 != "level") g$BasePop[[1]]$densitytrans <- plot_react$g$BasePop[[1]]$densitytrans
                 }
                 # treat region
                 g$GraphRegion = list()
                 R = obj_react$obj$regions[input$plot_regions[input$plot_regions %in% allowed_regions]]
                 if(length(R) > 0) g$GraphRegion = list(list("name" = R[[1]]$label, def = c(R[[1]]$def, names(R)[1])))
                 if(length(R) > 1) for(i_reg in 2:length(R)) {
                   defined = sapply(g$GraphRegion, FUN = function(r) r$name) %in% R[[i_reg]]$label
                   if(any(defined)) {
                     g$GraphRegion[[defined]] = list("name" = R[[i_reg]]$label, def = c(g$GraphRegion[[defined]]$def, names(R)[i_reg]))
                   } else {
                     g$GraphRegion = c(g$GraphRegion, list(list("name" = R[[i_reg]]$label, def = names(R)[i_reg])))
                   }
                 }
                 if(length(g$GraphRegion) > 0) {
                   tocheck = expand.grid(base = input$plot_base, region = input$plot_regions[input$plot_regions %in% allowed_regions], stringsAsFactors = FALSE)
                   alreadyexist = apply(tocheck, 1, FUN = function(x) any(sapply(obj_react$obj$pops, FUN = function(p) { p$base == x["base"] && p$region == x["region"] })))
                   tocreate = tocheck[!alreadyexist, ]
                   if(nrow(tocreate) > 0) {
                     pop = lapply(1:nrow(tocreate), FUN = function(i_row) {
                       args = list(name = paste(tocreate[i_row, "base"], tocreate[i_row, "region"], sep=" & "),
                                   base = tocreate[i_row, "base"], type = "G",
                                   color = obj_react$obj$regions[[tocreate[i_row, "region"]]]$color,
                                   lightModeColor = obj_react$obj$regions[[tocreate[i_row, "region"]]]$lightcolor,
                                   region = tocreate[i_row, "region"], fx = plot_react$x_feat)
                       if(input$plot_type == "2D") args = c(args, list(fy = plot_react$y_feat))
                       do.call(what = buildPopulation, args = args)
                     })
                     tryCatch({
                       obj_react$obj = data_add_pops(obj = obj_react$obj, pops = pop, display_progress = FALSE, session=session)
                       mess_global(title = paste0("plot_", input$plot_type),
                                   msg = c(ifelse(length(pop) == 1, "new population has been created:", "new populations have been created:"),
                                           sapply(pop, FUN = function(p) paste0("-", p$name))),
                                   type = "info", duration = 10)
                     }, error = function(e) {
                       mess_global(title = paste0("plot_", input$plot_type),
                                   msg = c(paste0("error while trying to create new population",ifelse(length(pop) == 1, ":", "s:")),
                                           sapply(pop, FUN = function(p) paste0("-", p$name)),
                                           e$message),
                                   type = "error", duration = 10)
                     })
                   }
                 }
                 # aesthetics & zooming
                 for(i in c("graphtitlefontsize", "title",
                            "axislabelsfontsize", "axistickmarklabelsfontsize",
                            "regionlabelsfontsize", "xlabel", "ylabel",
                            "xmin", "ymin",
                            "xmax", "ymax")) {
                   g[[i]] <- plot_react[[i]]
                 }
                 # maxpoints
                 input$plot_type_2D_main_option01 # allows to trigger resampling
                 g$maxpoints <- as.numeric(input$plot_type_2D_main_option03)/100
                 if(#ifelse(g$type == "histogram", all("1D" %in% input$plot_type), all(g$type %in% input$plot_type_2D_option01)) &&
                   all(g$order %in% plot_react$g$order) &&
                   all(g$xstatsorder %in% plot_react$g$xstatsorder) &&
                   all(sapply(g$BasePop, FUN = function(x) x$name) %in% sapply(plot_react$g$BasePop, FUN = function(x) x$name)) &&
                   ifelse(length(g$GraphRegion) == 0, TRUE, (all(sapply(g$GraphRegion, FUN = function(x) x$name) %in% sapply(plot_react$g$GraphRegion, FUN = function(x) x$name)))) &&
                   ifelse(length(g$ShownPop) == 0, TRUE, (all(sapply(g$ShownPop, FUN = function(x) x$name) %in% sapply(plot_react$g$ShownPop, FUN = function(x) x$name)))) &&
                   all(g$f1 %in% plot_react$g$f1) &&
                   ifelse(g$type == "histogram", TRUE, all(g$f2 %in% plot_react$g$f2))) {
                   plot_react$param_ready = TRUE
                   plot_react$g = g
                   isolate({
                     now = 1000*as.numeric(Sys.time())
                       delay(50, { plot_react$id = now } )
                   })
                   # print(paste0("build done: ",dev_check_plot))
                 } 
                 plot_react$g = g
                 }, error = function(e) {
                   mess_global(title = paste0("plot_", input$plot_type), msg = e$message, type = "error", duration = 10)
                   click("plot_reset")
                   plot_react$id = 1000*as.numeric(Sys.time())
                   runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
                   return(NULL)
                 },warning = function(w) {
                   mess_global(title = paste0("plot_", input$plot_type), msg = w$message, type = "warning", duration = 10)
                   click("plot_reset")
                   plot_react$id = 1000*as.numeric(Sys.time())
                   runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
                   return(NULL)
                 })
               }),
  # plot_sel
  observeEvent(input$plot_sel_init, suspended = TRUE,{
    if(length(input$shape_selected$x) != 0) {
      showModal(modalDialog(tags$p("Are you sure you want to discard applied changes in region '", tags$b(input$shape_selected$name), "' ?"),
                            size = "s",
                            easyClose = FALSE,
                            footer = list(modalButton(label = "No"),
                                          actionButton(inputId = "editing_close_ok",label = "Yes"))),
                session = session)
      return(NULL)
    }
    plot_react$hover ="init"
    session$resetBrush("plot_brush")
    runjs(code = "Shiny.onInputChange('shape_selected', null)")
    runjs(code = "Shiny.onInputChange('shape_param', null)")
    runjs(code = "Shiny.onInputChange('shape_moving', null)")
    set_tool("init")
  }),
  observeEvent(input$editing_close_ok,suspended = TRUE, {
    removeModal(session=session)
    runjs(code = "Shiny.onInputChange('shape_selected', null)")
    click("plot_sel_init")
  }),
  observeEvent(input$plot_sel_line, suspended = TRUE,{
    set_tool("line")
  }),
  observeEvent(input$plot_sel_rectangle,suspended = TRUE, {
    set_tool("rectangle")
  }),
  observeEvent(input$plot_sel_polygon,suspended = TRUE, {
    set_tool("polygon")
  }),
  observeEvent(input$plot_sel_lasso, suspended = TRUE,{
    set_tool("lasso")
  }),
  observeEvent(input$plot_sel_ellipse, suspended = TRUE,{
    set_tool("ellipse")
  }),
  observeEvent(input$plot_sel_edit, suspended = TRUE,{
    if(length(plot_react$plot)==0) return(NULL)
    if(!check_managers(alw = "cell")) return(NULL)
    runjs(code = "Shiny.onInputChange('shape_selected', null)")
    L = length(input$shape_selected$x)
    if(L != 0) {
      regions_react$pre = plot_react$plot$input$regions[[input$shape_selected$name]]
      regions_react$pre$name = input$shape_selected$name
      coords = do.call(what = "rbind", args = lapply(1:L, FUN = function(i) {
        map_css_to_coord(css = c(x = input$shape_selected$x[[i]], y = input$shape_selected$y[[i]]), map = input$plot_click)
      }))
      label = unlist(map_css_to_coord(css = c(x = as.numeric(input$shape_selected$cx), y = as.numeric(input$shape_selected$cy)), map = input$plot_click))
      trans_x = parseTrans(plot_react$plot$input$trans_x)
      trans_y = parseTrans(plot_react$plot$input$trans_y)
      regions_react$pre$x = unlist(coords[, "x"])
      regions_react$pre$y = unlist(coords[, "y"])
      regions_react$pre$cx = unname(label[1])
      regions_react$pre$cy = unname(label[2])
      regions_react$pre$x = applyTrans(regions_react$pre$x, trans_x, inverse = TRUE)
      regions_react$pre$cx = applyTrans(regions_react$pre$cx, trans_x, inverse = TRUE)
      if(plot_react$plot$input$type %in% c("count","percent")) {
        regions_react$pre$y = regions_react$pre$y / diff(range(plot_react$plot$input$ylim))
        regions_react$pre$cy = regions_react$pre$cy / diff(range(plot_react$plot$input$ylim))
      } else {
        regions_react$pre$y = applyTrans(regions_react$pre$y, trans_y, inverse = TRUE)
        regions_react$pre$cy = applyTrans(regions_react$pre$cy, trans_y, inverse = TRUE)
      }
      regions_react$pre$x = signif(regions_react$pre$x, digits = 3)
      regions_react$pre$y = signif(regions_react$pre$y, digits = 3)
      regions_react$pre$cx = signif(regions_react$pre$cx, digits = 3)
      regions_react$pre$cy = signif(regions_react$pre$cy, digits = 3)
      click("reg_validate")
      set_tool("init")
    } else {
      set_tool("edit") 
    }
  }),
  observeEvent(input$plot_sel_remove, suspended = TRUE,{
    if(length(plot_react$plot)==0) return(NULL)
    if(length(plot_react$plot$input$regions) == 0) {
      mess_global(title = "removing region", msg = "there is no region to remove", type = "error", duration = 10)
      click("plot_sel_init")
      return(NULL)
    }
    set_tool("remove")
  }),
  observeEvent(input$plot_sel_zoomin, suspended = TRUE,{
    set_tool("zoomin")
  }),
  observeEvent(input$plot_sel_zoomreset, suspended = TRUE, {
    foo = plotGraph(obj_react$obj, g, viewport = "max", draw = FALSE)
    plot_react$g$xmin = foo$input$Xlim[1]
    plot_react$g$xmin = foo$input$Xlim[2]
    plot_react$g$ymin = foo$input$Ylim[1]
    plot_react$g$ymax = foo$input$Ylim[2]
    plot_react$param_ready <- FALSE
    plot_react$zoomed = FALSE
    click("plot_sel_init")
  }),
  observeEvent(input$plot_sel_add, suspended = TRUE,{
    if(any(grepl("^ML_", c(plot_react$g$f1, plot_react$g$f2, sapply(c(plot_react$g$BasePop, plot_react$g$ShownPop), FUN = function(p) p$name))))) {
      mess_global(title = "adding graph", 
                  msg = c("Be aware that you are creating a new visualization depending on ML",
                          "IDEAS will not be able to use it for applying on other files"),
                  # msg = "Creating a visualization for graph report based on ML is not allowed",
                  type = "warning", duration = 10)
      # click("plot_sel_init")
      # return(NULL)
    }
    tryCatch({
      plot_react$layout = rbind(plot_react$layout, rep(NA, ncol(plot_react$layout)))
      plot_react$layout[nrow(plot_react$layout), 1] <- length(obj_react$obj$graphs) + 1
      plot_react$g$xsize <- input$report_size
      plot_react$g$ysize <- input$report_size
      plot_react$g$ylocation <- (nrow(plot_react$layout) - 1) * input$report_size
      plot_react$g$xlocation <- 0
      
      N = names(obj_react$obj$graphs)
      K = class(obj_react$obj$graphs)
      
      trans_x = parseTrans(plot_react$g$xlogrange)
      trans_y = parseTrans(plot_react$g$ylogrange)
      plot_react$g$xmin = applyTrans(plot_react$plot$input$xlim[1], trans_x, inverse = TRUE)
      plot_react$g$xmax = applyTrans(plot_react$plot$input$xlim[2], trans_x, inverse = TRUE)
      plot_react$g$ymin = applyTrans(plot_react$plot$input$ylim[1], trans_y, inverse = TRUE)
      plot_react$g$ymax = applyTrans(plot_react$plot$input$ylim[2], trans_y, inverse = TRUE)
      obj_react$obj$graphs = c(obj_react$obj$graphs, list(do.call(what = buildGraph, args = plot_react$g)))
      
      names(obj_react$obj$graphs) = c(N, NA)
      class(obj_react$obj$graphs) = K
      runjs(code = "$('#navbar [data-value=\"tab6\"]').trigger('click');" )
      runjs(code = "Shiny.onInputChange('navbar', 'tab6');")
      mess_global(title = "adding graph to report", msg = "graph has been successfully added to report layout", type = "success")
    }, error = function(e) {
      mess_global(title = "adding graph to report", msg = e$message, type = "error")
    })
    set_tool("add")
  }),
  observeEvent(input$plot_sel_stack, suspended = TRUE,{
    runjs(code = "$('#navbar [data-value=\"tab7\"]').trigger('click');" )
    runjs(code = "Shiny.onInputChange('navbar', 'tab7');")
    runjs(code = "$('#navbar_batch [data-value=\"Stack\"]').trigger('click');" )
    runjs(code = "Shiny.onInputChange('navbar_batch', 'Stack');")
    click("plot_sel_init")
  }))
