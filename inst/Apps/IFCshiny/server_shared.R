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

observeEvent(input$pop_create, suspended = FALSE, {
  add_log("Population Creation")
  runjs("Shiny.onInputChange('pop_manager_visible', true)")
  disable("pop_validate")
  disable("pop_def_feedback")
  updateSelectInput(session = session, inputId = "pop_symbol", selected = "Simple Dot")
  colourpicker::updateColourInput(session = session, inputId = "pop_color_light", value = "Black")
  colourpicker::updateColourInput(session = session, inputId = "pop_color_dark", value = "White")
  updateTextInput(session = session, inputId = "pop_def_name", value = "")
  updateSelectInput(session = session, inputId = "pop_def_sel", choices = grep("^ML_", names(obj_react$obj$pops), value = TRUE, invert = TRUE))
  pops_react$def = character()
  pops_react$revert = list(name = NULL, style = 20, color = "White", lightModeColor = "Black", type = "C", definition = NULL, split = NULL, names = NULL)
  pop_def(pops_react$def)
  output$pop_def_viz <- renderPrint(list("definition" = paste0(pops_react$revert$split, collapse = " ")))
  showFeedbackDanger(session = session, inputId = "pop_def_feedback", text = "invalid")
  hideElement("pop_plot")
  hideElement("pop_tag")
  pops_react$new = TRUE
  showElement(selector = "#pop_def_edit, .pop_def_feedback")
  hideElement("pop_remove")
})
observeEvent(input$pop_sample_name, suspended = FALSE, {
  M = sum(obj_react$obj$pops[[input$pop_sample_name]]$obj)
  updateNumericInput(session = session, inputId = "pop_sample_size", min = 1, max = M, value = M)
})
observeEvent(input$pop_sample, suspended = FALSE, {
  s = input$pop_sample_size; s = s[!is.na(s) && s > 0]
  new_name = "All"
  while(new_name %in% names(obj_react$obj$pops)) new_name = paste0("sampled_",input$pop_sample_name,"[",random_name(special = "NULL"),"]")
  tryCatch({
    obj_react$obj = data_add_pop_sample(obj = obj_react$obj, pop = input$pop_sample_name,
                                        size = input$pop_sample_size, new_name = new_name, session = session)
    mess_global("Population Sampling", msg = c("Sampled pop has been successfully created:", new_name), type = "success")
  }, warning = function(w) {
    mess_global("Population Sampling", msg = c("Sampled pop was not created:", w$message), type = "warning")
  }, error = function(e) {
    mess_global("Population Sampling", msg = c("Sampled pop was not created:", e$message), type = "stop")
  })
})
observeEvent(input$pop_alt_click, suspended = FALSE, {
  if(length(input$pop_alt_click)==0 || !(input$pop_alt_click %in% names(obj_react$obj$pops))) {
    runjs("Shiny.onInputChange('pop_alt_click', null)")
    return(NULL)
  }
  if(!check_managers(alw=NULL)) {
    runjs("Shiny.onInputChange('pop_alt_click', null)")
    return(NULL)
  }
  pop = obj_react$obj$pops[[input$pop_alt_click]]
  if((pop$type == "T") && any(obj_react$back$info$found)) {
    nam = pop$name
    runjs("Shiny.onInputChange('pop_manager_visible', false)")
    runjs(code = "Shiny.setInputValue('cell_selected', [])")
    runjs(code = "Shiny.setInputValue('IFCshiny_modalGetSelection_ret', { rand:0, obj:[], from:'start' })")
    updateSwitchInput(session=session, inputId="cell_mode", value=TRUE)
    updateSelectInput(session=session, inputId="cell_population", selected=nam)
    updateSelectInput(session=session, inputId="cell_pop_tagged", selected=nam)
    onFlush(once = TRUE, fun = function() {
      runjs("Shiny.onInputChange('cell_manager_visible', true)")
    })
    cell_react$pop_tagged = ""
    runjs("Shiny.onInputChange('pop_alt_click', null)")
    return(NULL)
  }
  if(any(pop$name == c("ML_subset"))) {
    lapply(obs_plot, FUN = function(x) x$resume())
    dims = "ML_pca_"
    if("lda" %in% input$training_model) dims = "ML_lda_0"
    if("tsne" %in% input$training_model) dims = "ML_tSNE_"
    if("umap" %in% input$training_model) dims = "ML_umap_"
    if("som" %in% input$training_model) dims = "ML_som_"
    dims = paste(dims, c("1", "2", "3"), sep = "")
    dims = paste(dims, "extra", sep="_")
    type = "1D"
    if(dims[1] %in% names(obj_react$obj$features)) {
      updateSelectInput(session = session, inputId = "plot_x_feature", selected = dims[1])
      plot_react$x_feat <- dims[1]
      if(dims[2] %in% names(obj_react$obj$features)) {
        updateSelectInput(session = session, inputId = "plot_y_feature", selected = dims[2])
        plot_react$y_feat <- dims[2]
        type = "2D"
      } 
      if(dims[3] %in% names(obj_react$obj$features) && input$webgl_available) {
        updateSelectInput(session = session, inputId = "plot_z_feature", selected = dims[3])
        plot_react$z_feat <- dims[3]
        type = "3D"
        updateToggle3D(session = session, inputId = "plot_3D_draw_axes", value = TRUE)
        updateToggle3D(session = session, inputId = "plot_3D_draw_ell", value = FALSE)
        updateToggle3D(session = session, inputId = "plot_3D_draw_txt", value = TRUE)
      }
    } else {
      updateSelectInput(session = session, inputId = "plot_x_feature", selected = "Object Number")
    }
    subset_names = obj_react$obj$pops[["ML_subset"]]$names
    if(length(subset_names) == 1) {
      if(type == "3D") {
        plot_react$order <- grep("ML_meta", names(obj_react$obj$pops), value = TRUE)
      } else {
        if(type == "2D") updateRadioButtons(session=session, inputId="plot_type_2D_option01", selected="scatter")
        plot_react$order <- c(grep("ML_meta", names(obj_react$obj$pops), value = TRUE), subset_names)
      }
    } else {
      plot_react$order <- subset_names
    }
    plot_react$shown <- (plot_react$order)
    
    # sub = apply(do.call(what = rbind, args = lapply(obj_react$obj$pops[plot_react$order], FUN = function(p) p$obj)), 2, any)
    sub = fastAny(lapply(obj_react$obj$pops[plot_react$order], FUN = function(p) p$obj))
    sub[-sample(x = which(sub), size = sum(sub) * input$plot_type_3D_option03 / 100, replace = FALSE)] <- FALSE
    plot_react$subset = sub 
    
    plot_react$color = (sapply(obj_react$obj$pops[plot_react$order], FUN = function(p) {
      p$lightModeColor
    }))
    plot_react$symbol = (sapply(obj_react$obj$pops[plot_react$order], FUN = function(p) {
      p$style
    }))
    
    if(type == "3D") {
      plot_react$shown = plot_react$order 
      updateSelectInput(session = session, inputId = "plot_base", selected = plot_react$order)
      updateSelectInput(session = session, inputId = "plot_shown", selected = plot_react$order)
      runjs(code = sprintf("Shiny.onInputChange('plot_base', %s );",toJSON(plot_react$shown , null = "null")))
      # if(length(plot_react$order) > 1) {
      #   runjs(code = sprintf("Shiny.onInputChange('plot_base', { '%s' });", plot_react$shown ))
      # } else {
      #   runjs(code = sprintf("Shiny.onInputChange('plot_base', '%s');", plot_react$shown ))
      # }
    } else {
      updateSelectInput(session = session, inputId = "plot_base", selected = subset_names)
      updateSelectInput(session = session, inputId = "plot_shown", selected = plot_react$shown)
      runjs(code = sprintf("Shiny.onInputChange('plot_base', %s );",toJSON(subset_names , null = "null")))
      # if(length(plot_react$order) > 1) {
      #   runjs(code = sprintf("Shiny.onInputChange('plot_base', { '%s' });", (subset_names)))
      # } else {
      #   runjs(code = sprintf("Shiny.onInputChange('plot_base', '%s');", (subset_names)))
      # }
    }
    updateRadioButtons(session = session, inputId = "plot_type", selected = type, inline = TRUE)
    runjs(code = sprintf("Shiny.onInputChange('plot_shown', %s );",toJSON(plot_react$shown, null = "null")))
    runjs(code = sprintf("Shiny.onInputChange('plot_shown_order', %s );",toJSON(plot_react$order, null = "null")))
    # if(length(plot_react$shown) > 1) {
    #   runjs(code = sprintf("Shiny.onInputChange('plot_shown', { '%s' });", plot_react$shown))
    # } else {
    #   runjs(code = sprintf("Shiny.onInputChange('plot_shown', '%s');", plot_react$shown))
    # }
    # if(length(plot_react$order) > 1) {
    #   runjs(code = sprintf("Shiny.onInputChange('plot_shown_order', { '%s' });", plot_react$order))
    # } else {
    #   runjs(code = sprintf("Shiny.onInputChange('plot_shown_order', '%s');", plot_react$order))
    # }
    updatePrettySwitch(session=session, inputId="plot_unlock", value=FALSE)
    runjs(code = "$('#navbar [data-value=\"tab3\"]').trigger('click');" )
    runjs(code = "Shiny.onInputChange('navbar', 'tab3');")
    plot_react$zoomed = FALSE
  } else {
    if(pop$type != "G") {
      runjs("Shiny.onInputChange('pop_alt_click', null)")
      return(NULL)
    }
    lapply(obs_plot, FUN = function(x) x$resume())
    plot_react$g <- do.call(what = buildGraph, args = popsRetrieveGraph(obj_react$obj, pops=input$pop_alt_click))
    runjs("Shiny.onInputChange('report_graph_dblclick', 'report_graph_retrieved')")
  }
  runjs("Shiny.onInputChange('pop_alt_click', null)")
})
observeEvent({
  list(input$pop_plot,
       input$pop_tag)
}, suspended = FALSE, {
  runjs("Shiny.onInputChange('pop_manager_visible', false)")
  runjs(code = sprintf("Shiny.onInputChange('pop_alt_click', '%s');", input$pop_edit))
  runjs("Shiny.onInputChange('pop_edit', null)")
})
observeEvent(input$pop_edit,suspended = FALSE, {
  if(!input$pop_edit %in% names(obj_react$obj$pops)) return(NULL)
  add_log("Population Edition")
  runjs("Shiny.onInputChange('pop_manager_visible', true)")
  pops_react$new = FALSE
  disable("pop_def_feedback")
  pop = obj_react$obj$pops[[input$pop_edit]]
  if(length(attr(pop, "reserved")) == 0) {
    showElement("pop_remove")
    enable("pop_def_name")
  } else {
    hideElement("pop_remove")
    disable("pop_def_name")
  }
  pops_react$revert = pop
  style = match_style(pop$style)
  
  hideElement("pop_plot")
  hideElement("pop_tag")
  output$pop_def_viz <- renderPrint(switch(pop$type,
                                           "B" = { list("Base" = NULL) },
                                           "C" = { if("ML_subset" %in% input$pop_edit) showElement("pop_plot")
                                             list("Definition" = paste0(pop$split, collapse = " ")) },
                                           "G" = { showElement("pop_plot")
                                             foo = list("Region" = pop$region, 
                                                        "Applied on" = pop$base,
                                                        "fx" = pop$fx)
                                             if(length(pop$fy) !=0) foo = c(foo, list("fy" = pop$fy))
                                             foo = c(foo, list("coordinates" = do.call(cbind, args = obj_react$obj$regions[[pop$region]][c("x","y")])))
                                             foo
                                           },
                                           "T" = { if(length(attr(pop, "reserved")) == 0) showElement("pop_tag")
                                             list("Tagged" = substr(paste0(which(pop$obj)-1L,collapse = ", "),0,120))
                                           }))
  updateSelectInput(session = session, inputId = "pop_symbol", selected = style)
  colourpicker::updateColourInput(session = session, inputId = "pop_color_light", value = pop$lightModeColor)
  colourpicker::updateColourInput(session = session, inputId = "pop_color_dark", value = pop$color)
  updateTextInput(session = session, inputId = "pop_def_name", value = pop$name)
  updateSelectInput(session = session, inputId = "pop_def_sel", choices = setdiff(names(obj_react$obj$pops), input$pop_edit))
  enable("pop_def_edit")
  if(pop$type == "C") {
    pops_react$def = pop$split
    pop_def(pops_react$def)
    enable("pop_validate")
    showElement(selector = "#pop_def_edit, .pop_def_feedback")
    if(grepl("^ML_", input$pop_edit)) {
      disable("pop_def_edit")
    }
  } else {
    pops_react$def = character()
    enable("pop_validate")
    hideElement(selector = "#pop_def_edit, .pop_def_feedback")
  }
})
observeEvent(input$report_graph_dblclick, {
  if(length(input$report_graph_dblclick)==0) return(NULL)
  lapply(obs_plot, FUN = function(x) x$resume())
  g = input$report_graph_dblclick
  runjs(code = "Shiny.onInputChange('report_graph_dblclick', null);")
  if(g == "report_graph_retrieved") {
    g = plot_react$g
  } else {
    g = obj_react$obj$graphs[[g]] 
    plot_react$g <- g
  }
  plot_react$xmin <- g$xmin
  plot_react$xmax <- g$xmax
  plot_react$ymin <- g$ymin
  plot_react$ymax <- g$ymax
  updatePrettySwitch(session=session, inputId="plot_unlock", value=FALSE)
  runjs(code = "Shiny.onInputChange('plot_unlock', false);")
  
  updateSelectInput(session=session, inputId="plot_x_feature", selected=g$f1)
  maxpoints = plot_react$g$maxpoints
  onFlushed(once = TRUE, fun = function() {
    plot_react$zoomed <- TRUE
    updatePrettySwitch(session=session, inputId="plot_unlock", value=FALSE)
    runjs(code = "Shiny.onInputChange('plot_unlock', false);")
    updateSliderInput(session=session, inputId="plot_type_2D_main_option03", value = min(maxpoints*100, 100))
  })  
  plot_react$x_feat = g$f1
  updateTextInput(session=session, inputId="plot_x_transform", value=g$xlogrange)
  plot_react$x_trans = g$xlogrange
  updateSelectInput(session=session, inputId="plot_base", selected=sapply(g$BasePop, FUN = function(p) p$name))
  
  if(g$type=="histogram") {
    updateRadioButtons(session=session, inputId="plot_type", selected="1D", inline = TRUE)
    updateRadioButtons(session=session, inputId="plot_type_1D_option02", selected=ifelse(g$histogramsmoothingfactor==0,"bar","smooth"), inline = TRUE)
    updateCheckboxInput(session=session, inputId="plot_type_1D_option03", value=g$freq=="T")
    v = unique(splitn(definition = g$order, all_names = names(obj_react$obj$pops)))
    plot_react$order = v
    plot_react$shown = v
    updateSelectInput(session=session, inputId="plot_shown", selected=v)
    runjs(code = sprintf("Shiny.onInputChange('plot_shown', %s );",  toJSON(v, null = "null")))
  } else {
    updateRadioButtons(session=session, inputId="plot_type", selected="2D", inline = TRUE)
    updateSelectInput(session=session, inputId="plot_y_feature", selected=g$f2)
    plot_react$y_feat = g$f2
    updateTextInput(session=session, inputId="plot_y_transform", value=g$ylogrange)
    plot_react$y_trans = g$ylogrange
    if(g$type == "density") {
      if((length(g$BasePop[[1]]$densitylevel) != 0) && (g$BasePop[[1]]$densitylevel != "")) {
        updateRadioButtons(session=session, inputId="plot_type_2D_option01", selected="level", inline = TRUE)
        runjs(code = "Shiny.onInputChange('plot_type_2D_option01', 'level');")
        args_level = strsplit(g$BasePop[[1]]$densitylevel, split="|", fixed=TRUE)[[1]]
        if(length(args_level) == 4) {
          updatePrettyCheckbox(session=session, inputId="plot_level_fill", value=args_level[1]=="true")
          updatePrettyCheckbox(session=session, inputId="plot_level_lines", value=args_level[2]=="true")
          nlevels=na.omit(as.integer(args_level[3])); if(length(nlevels)==1) updateNumericInput(session=session, inputId="plot_level_nlevels", value=nlevels)
          lowest =na.omit(as.numeric(args_level[4])); if(length(lowest)==1) updateNumericInput(session=session, inputId="plot_level_lowest", value=lowest)
        }
      } else {
        updateRadioButtons(session=session, inputId="plot_type_2D_option01", selected="density", inline = TRUE) 
        runjs(code = "Shiny.onInputChange('plot_type_2D_option01', 'density');")
        updatePrettyCheckbox(session=session, inputId="plot_level_fill", value=TRUE)
        updatePrettyCheckbox(session=session, inputId="plot_level_lines", value=FALSE)
        updateNumericInput(session=session, inputId="plot_level_nlevels", value=10)
        updateNumericInput(session=session, inputId="plot_level_lowest", value=0.1)
      }
      updateSelectInput(session=session, inputId="plot_shown", selected=NULL)
      runjs(code = sprintf("Shiny.onInputChange('plot_shown', null);"))
      runjs(code = sprintf("Shiny.onInputChange('plot_shown_order', null);"))
    } else {
      updateRadioButtons(session=session, inputId="plot_type_2D_option01", selected="scatter", inline = TRUE) 
      runjs(code = "Shiny.onInputChange('plot_type_2D_option01', 'scatter');")
      v = unique(splitn(definition = g$order, all_names = names(obj_react$obj$pops)))
      plot_react$order = v
      plot_react$shown = v
      updateSelectInput(session=session, inputId="plot_shown", selected=v)
      onFlushed(once = TRUE, session = session,
                fun = function() { runjs(code = sprintf("Shiny.onInputChange('plot_shown_order', %s );",  toJSON(v, null = "null"))) })
    }
  }
  allowed_regions = lapply(obj_react$obj$pops, FUN = function(p) {
    if(p$type != "G") return(NULL)
    r = obj_react$obj$regions[[p$region]]
    if(g$type == "histogram") {
      if(length(g$f1) == 0) return(NULL)
      if(length(p$fy) != 0) return(NULL)
      if((p$fx == g$f1) &&
         (r$xlogrange == g$xlogrange)) return(r$label)
    } else {
      if(length(p$fy) == 0) return(NULL)
      if((p$fx == g$f1) && 
         (r$xlogrange == g$xlogrange) &&
         (p$fy == g$f2) && 
         (r$ylogrange == g$ylogrange)) return(r$label)
    }
  })
  # updateSelectInput(session = session, inputId = "plot_font_main", selected = g$graphtitlefontsize)
  # updateSelectInput(session = session, inputId = "plot_font_axis_lab", selected = g$axislabelsfontsize)
  # updateSelectInput(session = session, inputId = "plot_font_tick_lab", selected = g$axistickmarklabelsfontsize)
  # updateSelectInput(session = session, inputId = "plot_font_region", selected = g$regionlabelsfontsize)
  # updateSelectInput(session = session, inputId = "plot_lab_main", selected = g$title)
  # updateSelectInput(session = session, inputId = "plot_lab_x", selected = g$xlabel)
  # updateSelectInput(session = session, inputId = "plot_lab_y", selected = g$ylabel)
  
  plot_react$allowed_regions = sort(unname(unique(unlist(allowed_regions)), force = TRUE))
  updateSelectInput(session=session, inputId="plot_base", selected=sapply(g$BasePop, FUN = function(p) p$name))
  if(length(g$GraphRegion) != 0) {
    updateSelectInput(session=session, inputId="plot_regions",
                      choices=allowed_regions,
                      selected=sapply(g$GraphRegion, FUN=function(r) r$name))
  } else {
    updateSelectInput(session=session, inputId="plot_regions",
                      choices=allowed_regions,
                      selected=NULL)
  }
  runjs(code = "$('#navbar [data-value=\"tab3\"]').trigger('click');" )
  runjs(code = "Shiny.onInputChange('navbar', 'tab3');")
})
