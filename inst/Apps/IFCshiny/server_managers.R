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

# we set up values uses to trigger draggable panels
runjs("Shiny.setInputValue('img_manager_visible', false)")
runjs("Shiny.setInputValue('reg_manager_visible', false)")
runjs("Shiny.setInputValue('pop_manager_visible', false)")
runjs("Shiny.setInputValue('cell_manager_visible', false)")
runjs("Shiny.setInputValue('graph_manager_visible', false)")
runjs("Shiny.setInputValue('comp_manager_visible', false)")

reinit_managers <- function(what = c("reg", "pop", "img", "cell", "graph", "comp")) {
  if("pop" %in% what) {
    runjs("Shiny.onInputChange('pop_manager_visible', false)")
    pops_react$def = character()
    pops_react$new = FALSE
    pops_react$revert = list()
  }
  if("img" %in% what) {
    plot_react$param_ready = FALSE
    runjs("Shiny.onInputChange('img_manager_visible', false)")
  }
  if("reg" %in% what) {
    runjs("Shiny.onInputChange('reg_manager_visible', false)")
    regions_react$back = FALSE
  }
  if("cell" %in% what) {
    runjs("Shiny.onInputChange('cell_manager_visible', false)")
    html(id = "cell_image_placeholder", NULL)
  }
  if("graph" %in% what) {
    runjs("Shiny.onInputChange('comp_manager_visible', false)")
  }
  if("comp" %in% what) {
    runjs("Shiny.onInputChange('comp_manager_visible', false)")
  }
}

check_managers <- function(alw = "img", all = c("cell", "img", "pop", "reg", "graph")) {
  tocheck = setdiff(all, c(alw, ""))
  f <- function(x) c("Cell", "Image", "Population", "Region", "Graph")[match(x, c("cell", "img", "pop", "reg","graph"))]
  msg = unlist(lapply(tocheck, FUN = function(x) {if(input[[paste0(x, "_manager_visible")]]) {return(paste0("'", f(x), " Manager"))} else {return(NULL)}} ))
  if(length(msg) == 0) return(structure(TRUE, names = "true"))
  mess_global(title = paste(f(alw), " Manager"),  msg = paste0("Please close first:\n\t-", paste0(msg, collapse = "\n\t-")), 
              type = "error", duration = 10)
  return(structure(FALSE, names = "false"))
}

observeEvent(input$comp_manager_visible, {
  if(input$comp_manager_visible) {
    runjs(code="$('html, body').animate({scrollTop: '0px' }, 300);")
    runjs("$('#comp_manager').animate($('#general').position());")
    shinyjs::showElement("comp_manager")
    session$sendCustomMessage("reach", "comp_manager")
  } else {
    runjs(sprintf("Shiny.onInputChange('comp_compute', %i)", ifelse(length(input$comp_compute) == 0, 0, input$comp_compute + 1L)))
    # shinyjs::click("comp_compute")
    shinyjs::hideElement("comp_manager")
  }
})
observeEvent(input$graph_manager_visible, {
  if(input$graph_manager_visible) {
    runjs(code="$('html, body').animate({scrollTop: '0px' }, 300);")
    runjs("$('#graph_manager').animate($('#general').position());")
    shinyjs::showElement("graph_manager")
    session$sendCustomMessage("reach", "graph_manager")
    updateSelectInput(session = session, inputId = "plot_font_main", selected = plot_react$g$graphtitlefontsize)
    updateSelectInput(session = session, inputId = "plot_font_axis_lab", selected = plot_react$g$axislabelsfontsize)
    updateSelectInput(session = session, inputId = "plot_font_tick_lab", selected = plot_react$g$axistickmarklabelsfontsize)
    updateSelectInput(session = session, inputId = "plot_font_region", selected = plot_react$g$regionlabelsfontsize)
    updateTextInput(session = session, inputId = "plot_lab_main", value = plot_react$g$title)
    updateTextInput(session = session, inputId = "plot_lab_x", value = plot_react$g$xlabel)
    updateTextInput(session = session, inputId = "plot_lab_y", value = plot_react$g$ylabel)
    if(plot_react$g$type == "density") {
      if(!requireNamespace("viridisLite", quietly = TRUE)) {
        msg_react$queue = c(msg_react$queue, "viridisLite")
        updateSelectInput(session = session, inputId = "msg_once", choices = msg_react$queue, selected = msg_react$queue)
      }
      updateSelectInput(session = session, inputId = "plot_dens_color", selected = plot_react$densitycolorslightmode_selected)
      updateSelectInput(session = session, inputId = "plot_dens_feature", selected = ifelse(plot_react$g$BasePop[[1]]$densitytrans %in% names(obj_react$obj$features), plot_react$g$BasePop[[1]]$densitytrans, "initial"))
      col = colConv(plot_react$g$BasePop[[1]]$densitycolorslightmode)
      runjs(code = sprintf("$('#plot_dens_color').parent().find('.selectize-input').css({background:'linear-gradient(to right, %s)'});", paste0(col,collapse=",")))
      # args_level = strsplit(plot_react$g$BasePop[[1]]$densitylevel, split="|", fixed=TRUE)[[1]]
      # if(length(args_level) == 4) {
      #   updatePrettyCheckbox(session=session, inputId="plot_level_fill", value=args_level[1]=="true")
      #   updatePrettyCheckbox(session=session, inputId="plot_level_lines", value=args_level[2]=="true")
      #   nlevels=na.omit(as.integer(args_level[3])); if(length(nlevels)==1) updateNumericInput(session=session, inputId="plot_level_nlevels", value=nlevels)
      #   lowest =na.omit(as.numeric(args_level[4])); if(length(lowest)==1) updateNumericInput(session=session, inputId="plot_level_lowest", value=lowest)
      # }
    }
    trans = parseTrans(plot_react$x_trans)
    updateNumericInput(session = session, inputId = "plot_xmin", value = applyTrans(plot_react$plot$input$xlim[1], trans, inverse = TRUE))
    updateNumericInput(session = session, inputId = "plot_xmax", value = applyTrans(plot_react$plot$input$xlim[2], trans, inverse = TRUE))
    trans = parseTrans(plot_react$y_trans)
    updateNumericInput(session = session, inputId = "plot_ymin", value = applyTrans(plot_react$plot$input$ylim[1], trans, inverse = TRUE))
    updateNumericInput(session = session, inputId = "plot_ymax", value = applyTrans(plot_react$plot$input$ylim[2], trans, inverse = TRUE))
    if(plot_react$g$type == "histogram") {
      disable(id = "plot_lab_y")
    } else {
      enable(id = "plot_lab_y")
    }
    if(plot_react$g$type == "density") {
      showElement(selector = ".plot_density")
      if(length(plot_react$g$BasePop[[1]]$densitylevel) != 0 && plot_react$g$BasePop[[1]]$densitylevel != "") {
        hideElement(selector = ".plot_density_feature")
      } else {
        hideElement(selector = ".plot_density_level")
      }
    } else {
      hideElement(selector = ".plot_density")
    }
    # shinyjs::toggleElement(selector = ".plot_density", condition = (plot_react$g$type == "density" && (length(plot_react$g$densitylevel) == 0 || plot_react$g$densitylevel == "")))
    obs_plot$limits$suspend()
  } else {
    plot_react$graphtitlefontsize <- input$plot_font_main
    plot_react$axislabelsfontsize <- input$plot_font_axis_lab
    plot_react$axistickmarklabelsfontsize <- input$plot_font_tick_lab
    plot_react$regionlabelsfontsize <- input$plot_font_region
    plot_react$title <- input$plot_lab_main
    plot_react$xlabel <- input$plot_lab_x
    plot_react$ylabel <- input$plot_lab_y
    if("density" %in% plot_react$g$type) {
      if("initial" %in% input$plot_dens_feature) {
        plot_react$g$BasePop[[1]]$densitytrans <- plot_react$densitytrans
      } else {
        if("default" %in% input$plot_dens_feature) {
          plot_react$g$BasePop[[1]]$densitytrans <- "asinh"
        } else {
          plot_react$g$BasePop[[1]]$densitytrans <- input$plot_dens_feature
        }
      }
      if(input$plot_dens_color == "initial") {
        col = colConv(plot_react$densitycolorslightmode)
      } else {
        if(requireNamespace("viridisLite", quietly = TRUE) && (input$plot_dens_color %in% ls(asNamespace("viridisLite")))) {
          col = do.call(what = get(input$plot_dens_color, envir = asNamespace("viridisLite")), args = list(n = 5))
        } else {
          col = RColorBrewer::brewer.pal(n = 5, name = input$plot_dens_color)
        } 
      }
      if(input$plot_dens_order %% 2) col = rev(col)
      plot_react$g$BasePop[[1]]$densitycolorslightmode <- inv_colConv(col)
      plot_react$densitycolorslightmode_selected <- input$plot_dens_color
      plot_react$densitytrans_selected <- input$plot_dens_feature
      # if(input$plot_type_2D_option01 == "level") plot_react$g$BasePop[[1]]$densitylevel = paste(ifelse(input$plot_level_fill,"true","false"),
      #                                                                                     ifelse(input$plot_level_lines,"true","false"),
      #                                                                                     input$plot_level_nlevels,input$plot_level_lowest,sep="|")
    }
    check_finite <- function(x) { length(x[is.finite(x)]) != 0 }
    obs_plot$limits$resume()
    shinyjs::hideElement("graph_manager")
  }
})
observeEvent(input$cell_manager_visible, {
  # cell manager can't show up when there is no cell image found
  if(!any(obj_react$back$info$found)) {
    runjs("Shiny.onInputChange('cell_manager_visible', false)")
    return(NULL)
  }
  add_log(sprintf("Cell Manager Visible: %s", input$cell_manager_visible))
  runjs(code = "Shiny.setInputValue('IFCshiny_modalGetSelection_ret', { rand:0, obj:[], from:'start' })")
  if(input$cell_manager_visible) {
    runjs(sprintf("Shiny.onInputChange('cell_manager_visible', %s)", names(check_managers(alw=c("img","cell")))))
    lapply(obs_cell, FUN = function(x) x$resume())
    runjs(code="$('html, body').animate({scrollTop: '0px' }, 300);")
    runjs("$('#cell_manager').animate($('#general').position());")
    showElement("cell_manager")
    session$sendCustomMessage("reach", "cell_manager")
  } else {
    sapply(grep("^cell_", names(session$input), value = TRUE), FUN = function(x) shinyjs::reset(x))
    runjs(code = "Shiny.setInputValue('cell_selected', [])")
    lapply(obs_cell, FUN = function(x) x$suspend())
    hideElement("cell_manager")
  }
})
observeEvent(input$pop_manager_visible, {
  add_log(sprintf("Population Manager Visible: %s", input$pop_manager_visible))
  if(input$pop_manager_visible) {
    runjs(sprintf("Shiny.onInputChange('pop_manager_visible', %s)", names(check_managers(alw="pop"))))
    lapply(obs_pop, FUN = function(x) x$resume())
    runjs(code="$('html, body').animate({scrollTop: '0px' }, 300);")
    runjs("$('#pop_manager').animate($('#general').position());")
    disable("pop_create")
    disable("pop_sample")
    showElement("pop_manager")
    session$sendCustomMessage("reach", "pop_manager")
  } else {
    lapply(obs_pop, FUN = function(x) x$suspend())
    enable("pop_create")
    enable("pop_sample")
    hideElement("pop_manager")
  }
})
observeEvent(input$reg_manager_visible, {
  add_log(sprintf("Region Manager Visible: %s", input$reg_manager_visible))
  N = names(obj_react$obj$regions)
  if(input$reg_manager_visible) {
    if(length(N) <= 0) {
      runjs("Shiny.onInputChange('reg_manager_visible', false)")
      return(NULL)
    }
    updateSelectInput(session = session, inputId = "reg_selection", choices = N, selected = N[1])
    reg = obj_react$obj$regions[[N[1]]]
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
    regions_react$pre$name = N[1]
    reg_def(reg = regions_react$pre, reg_back = reg, all_names = names(obj_react$obj$regions), check = "both", session = getDefaultReactiveDomain())
    runjs(sprintf("Shiny.onInputChange('reg_manager_visible', %s)", names(check_managers(alw="reg"))))
    lapply(obs_reg, FUN = function(x) x$resume())
    runjs(code="$('html, body').animate({scrollTop: '0px' }, 300);")
    runjs("$('#reg_manager').animate($('#general').position());")
    showElement("reg_manager")
    session$sendCustomMessage("reach", "reg_manager")
  } else {
    lapply(obs_reg, FUN = function(x) x$suspend())
    hideElement("reg_manager")
  }
})
observeEvent(input$img_manager_visible, {
  # image manager can't show up when there is no cell image found
  if(!any(obj_react$back$info$found)) {
    runjs("Shiny.onInputChange('img_manager_visible', false)")
    return(NULL)
  }
  add_log(sprintf("Image Manager Visible: %s", input$img_manager_visible))
  if(input$img_manager_visible) {
    runjs(sprintf("Shiny.onInputChange('img_manager_visible', %s)", names(check_managers(alw=c("img","cell")))))
    lapply(obs_img, FUN = function(x) x$resume())
    runjs(code="$('html, body').animate({scrollTop: '0px' }, 300);")
    runjs("$('#img_manager').animate($('#general').position());")
    showElement("img_manager")
    session$sendCustomMessage("reach", "img_manager")
  } else {
    lapply(obs_img, FUN = function(x) x$suspend())
    hideElement("img_manager")
  }
})