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

obs_pop <- list(
  observeEvent(input$pop_def_name, suspended = TRUE,{
    disable("pop_validate")
    showFeedbackWarning(session = session, inputId = "pop_def_name", text = "empty")
    if(any(input$population=="")) return(NULL)
    hideFeedback(session = session, inputId = "pop_def_name") 
    valid = TRUE
    if((length(input$pop_def_name) == 0) || (input$pop_def_name == "")) {
      valid = FALSE
      showFeedbackWarning(session = session, inputId = "pop_def_name", text = "empty")
    } else {
      if(input$pop_def_name %in% c("And", "Or", "Not", "(", ")")) {
        valid = FALSE
        showFeedbackDanger(session = session, inputId = "pop_def_name", text = "invalid")
      }
      if(grepl("^ML_", input$pop_def_name)) {
        valid = FALSE
        showFeedbackDanger(session = session, inputId = "pop_def_name", text = "not allowed")
      }
      if(input$pop_def_name %in% setdiff(names(obj_react$obj$pops), pops_react$revert$name)) {
        valid = FALSE
        showFeedbackDanger(session = session, inputId = "pop_def_name", text = "already exists")
      }
    }
    if(length(pops_react$revert$type) != 0) {
      if(pops_react$revert$type == "C") {
        if(valid && pop_def(pops_react$def)) enable("pop_validate")
      } else {
        if(valid) enable("pop_validate")
      }
    }
    if(!pops_react$new && (input$pop_def_name == pops_react$revert$name) &&
       (input$pop_def_name == pops_react$revert$name) && (length(attr(pops_react$revert, "reserved")) == 0)) {
      showElement("pop_remove")
    } else{
      hideElement("pop_remove")
    }
  }),
  observeEvent(input$pop_symbol_reset,suspended = TRUE, {
    if(!pops_react$new && (length(input$pop_edit) == 0)) return(NULL)
    hideFeedback(session=session, inputId="pop_symbol")
    if(pops_react$new) {
      style = "Simple Dot"
    } else {
      style = match_style(obj_react$obj$pops[[input$pop_edit]]$style)
    }
    updateTextInput(session = session, inputId = "pop_symbol", value = style)
  }),
  observeEvent(input$pop_def_name_reset,suspended = TRUE, {
    if(!pops_react$new && (length(input$pop_edit) == 0)) return(NULL)
    hideFeedback(session=session, inputId="pop_def_name")
    if(pops_react$new) {
      name = ""
    } else {
      name = obj_react$obj$pops[[input$pop_edit]]$name
    }
    updateTextInput(session = session, inputId = "pop_def_name", value = name)
  }),
  observeEvent(input$pop_color_light_reset,suspended = TRUE, {
    if(!pops_react$new && (length(input$pop_edit) == 0)) return(NULL)
    removeClass(id = "pop_color_light_reset", class = "modified")
    if(pops_react$new) {
      col = "Black"
    } else {
      col = obj_react$obj$pops[[input$pop_edit]]$lightModeColor
    }
    updateTextInput(session = session, inputId = "pop_color_light", value = col)
  }),
  observeEvent(input$pop_color_dark_reset,suspended = TRUE, {
    if(!pops_react$new && (length(input$pop_edit) == 0)) return(NULL)
    removeClass(id = "pop_color_dark_reset", class = "modified")
    if(pops_react$new) {
      col = "White"
    } else {
      col = obj_react$obj$pops[[input$pop_edit]]$color
    }
    updateTextInput(session = session, inputId = "pop_color_dark", value = col)
  }),
  observeEvent(input$pop_def_reset, suspended = TRUE,{
    if(!pops_react$new && (length(input$pop_edit) == 0)) return(NULL)
    hideFeedback(session=session, inputId="pop_def_feedback")
    if(pops_react$new) {
      pops_react$def = NULL
      pops_react$revert$definition = NULL
      pops_react$revert$split = NULL
      pops_react$revert$names = NULL
      output$pop_def_viz <- renderPrint(list("Definition" = paste0(pops_react$def, collapse = " ")))
      showFeedbackDanger(session = session, inputId = "pop_def_feedback", text = "invalid")
    } else {
      if(obj_react$obj$pops[[input$pop_edit]]$type == "C") {
        pops_react$def = obj_react$obj$pops[[input$pop_edit]]$split
        output$pop_def_viz <- renderPrint(list("Definition" = paste0(pops_react$def, collapse = " ")))
      }
    }
  }),
  observeEvent(input$pop_def_add, suspended = TRUE, {
    if(pops_react$revert$type != "C") return(NULL)
    pops_react$def = c(pops_react$def, input$pop_def_sel)
    output$pop_def_viz <- renderPrint(list("Definition" = paste0(pops_react$def, collapse = " ")))
    if(pop_def(pops_react$def)) {
      hideFeedback(session = session, inputId = "pop_def_feedback")
      if((length(input$pop_def_name) != 0) && !(input$pop_def_name %in% c("" , "And", "Or", "Not", "(", ")", setdiff(names(obj_react$obj$pops), pops_react$revert$name)))&&
         !grepl("^ML_", input$pop_def_name)) enable("pop_validate")
    } else {
      showFeedbackDanger(session = session, inputId = "pop_def_feedback", text = "invalid")
    }
    if(!pops_react$new && identical(pops_react$revert$split, pops_react$def) &&
       (input$pop_def_name == pops_react$revert$name) && (length(attr(pops_react$revert, "reserved")) == 0)) {
      showElement("pop_remove")
    } else{
      hideElement("pop_remove")
    }
  }),
  observeEvent(input$pop_def_and,suspended = TRUE, {
    if(pops_react$revert$type != "C") return(NULL)
    showFeedbackWarning(session = session, inputId = "pop_def_feedback", text = "invalid")
    pops_react$def = c(pops_react$def, "And")
    output$pop_def_viz <- renderPrint(list("Definition" = paste0(pops_react$def, collapse = " ")))
    pop_def(pops_react$def)
    if(!pops_react$new && identical(pops_react$revert$split, pops_react$def) && 
       (input$pop_def_name == pops_react$revert$name) && (length(attr(pops_react$revert, "reserved")) == 0)) {
      showElement("pop_remove")
    } else{
      hideElement("pop_remove")
    }
  }),
  observeEvent(input$pop_def_or, suspended = TRUE,{
    if(pops_react$revert$type != "C") return(NULL)
    showFeedbackWarning(session = session, inputId = "pop_def_feedback", text = "invalid")
    pops_react$def = c(pops_react$def, "Or")
    output$pop_def_viz <- renderPrint(list("Definition" = paste0(pops_react$def, collapse = " ")))
    pop_def(pops_react$def)
    if(!pops_react$new && identical(pops_react$revert$split, pops_react$def) && 
       (input$pop_def_name == pops_react$revert$name) && (length(attr(pops_react$revert, "reserved")) == 0)) {
      showElement("pop_remove")
    } else{
      hideElement("pop_remove")
    }
  }),
  observeEvent(input$pop_def_not, suspended = TRUE,{
    if(pops_react$revert$type != "C") return(NULL)
    showFeedbackWarning(session = session, inputId = "pop_def_feedback", text = "invalid")
    pops_react$def = c(pops_react$def, "Not")
    output$pop_def_viz <- renderPrint(list("Definition" = paste0(pops_react$def, collapse = " ")))
    pop_def(pops_react$def)
    if(!pops_react$new && identical(pops_react$revert$split, pops_react$def) && 
       (input$pop_def_name == pops_react$revert$name) && (length(attr(pops_react$revert, "reserved")) == 0)) {
      showElement("pop_remove")
    } else{
      hideElement("pop_remove")
    }
  }),
  observeEvent(input$pop_def_bkt1,suspended = TRUE, {
    if(pops_react$revert$type != "C") return(NULL)
    showFeedbackWarning(session = session, inputId = "pop_def_feedback", text = "invalid")
    pops_react$def = c(pops_react$def, "(")
    output$pop_def_viz <- renderPrint(list("Definition" = paste0(pops_react$def, collapse = " ")))
    pop_def(pops_react$def)
    if(!pops_react$new && identical(pops_react$revert$split, pops_react$def) && 
       (input$pop_def_name == pops_react$revert$name) && (length(attr(pops_react$revert, "reserved")) == 0)) {
      showElement("pop_remove")
    } else{
      hideElement("pop_remove")
    }
  }),
  observeEvent(input$pop_def_bkt2, suspended = TRUE,{
    if(pops_react$revert$type != "C") return(NULL)
    pops_react$def = c(pops_react$def, ")")
    output$pop_def_viz <- renderPrint(list("Definition" = paste0(pops_react$def, collapse = " ")))
    if(pop_def(pops_react$def)) {
      hideFeedback(session = session, inputId = "pop_def_feedback")
      if((length(input$pop_def_name) != 0) && !(input$pop_def_name %in% c("" , "And", "Or", "Not", "(", ")", setdiff(names(obj_react$obj$pops), pops_react$revert$name)))&&
         !grepl("^ML_", input$pop_def_name)) enable("pop_validate")
    } else {
      showFeedbackDanger(session = session, inputId = "pop_def_feedback", text = "invalid")
    }
    if(!pops_react$new && identical(pops_react$revert$split, pops_react$def) && 
       (input$pop_def_name == pops_react$revert$name) && (length(attr(pops_react$revert, "reserved")) == 0)) {
      showElement("pop_remove")
    } else{
      hideElement("pop_remove")
    }
  }),
  observeEvent(input$pop_def_rm, suspended = TRUE,{
    if(pops_react$revert$type != "C") return(NULL)
    if(length(pops_react$def) > 0) pops_react$def = pops_react$def[-length(pops_react$def)]
    output$pop_def_viz <- renderPrint(list("Definition" = paste0(pops_react$def, collapse = " ")))
    if(pop_def(pops_react$def)) {
      hideFeedback(session = session, inputId = "pop_def_feedback")
      if((length(input$pop_def_name) != 0) && 
         !(input$pop_def_name %in% c("" , "And", "Or", "Not", "(", ")", setdiff(names(obj_react$obj$pops), pops_react$revert$name))) &&
         !grepl("^ML_", input$pop_def_name)) enable("pop_validate")
    } else {
      showFeedbackDanger(session = session, inputId = "pop_def_feedback", text = "invalid")
    }
    if(!pops_react$new && identical(pops_react$revert$split, pops_react$def) && 
       (input$pop_def_name == pops_react$revert$name) && (length(attr(pops_react$revert, "reserved")) == 0)) {
      showElement("pop_remove")
    } else{
      hideElement("pop_remove")
    }
  }),
  observeEvent(input$pop_validate, suspended = TRUE,{
    if(any(input$population=="")) return(NULL)
    style = c("Simple Dot", "Cross", "Plus", 
              "Empty Circle", "Empty Diamond", "Empty Square", 
              "Empty Triangle", "Solid Diamond", "Solid Square", 
              "Solid Triangle")
    style = c(20, 4, 3, 1, 5, 0, 2, 18, 15, 17)[input$pop_symbol == style]
    pops_back = obj_react$obj$pops
    regions_back = obj_react$obj$regions
    graphs_back = obj_react$obj$graphs
    
    tryCatch({
      if(!pops_react$new && (pops_react$revert$name %in% names(obj_react$obj$pops))) {
        # we check that population name is not already used for another pop
        if(input$pop_def_name %in% setdiff(names(obj_react$obj$pops), pops_react$revert$name)) {
          stop(paste0("a population named '",input$pop_def_name,"' already exists"))
        }
        # we use the current pop as a draft and with fill it with user input
        new_pop = pops_react$revert
        new_pop$name <- input$pop_def_name
        new_pop$color <- match_col(input$pop_color_dark)
        new_pop$lightModeColor <- match_col(input$pop_color_light)
        new_pop$style <- style
        if(pops_react$revert$type == "C") new_pop$definition <- paste0(pops_react$def, collapse = "|")
        
        # we identify all graphs that will be impacted by population modification
        toredraw = data_rm_pops(obj = obj_react$obj, pops = pops_react$revert$name, list_only = TRUE, session=session)
        names(obj_react$obj$graphs)[toredraw$graphs] <- NA
        N = names(obj_react$obj$graphs)
        if(length(N) > 0) {
          lapply(1:length(N), FUN = function(i) {
            if(!is.na(N[i])) return(NULL)
            runjs(code = JS(sprintf("IFCshiny.grid.remove(IFCshiny.grid.getItems().filter(function (item) { return item._element.id === 'report_graph_%0.4i' }), { removeElements: true, layout: false })", i)))
          })
        }
        
        # we apply the modification
        obj_react$obj = data_modify_pops(obj = obj_react$obj,
                                         pops = structure(list(new_pop), names = pops_react$revert$name), 
                                         display_progress = TRUE,
                                         session = session)
        
        # if edited population was used for the current graph we modify the graph styling
        if(pops_react$revert$name %in% names(plot_react$color)) {
          plot_react$color[pops_react$revert$name] <- new_pop$lightModeColor
          names(plot_react$color)[names(plot_react$color) == pops_react$revert$name] <- input$pop_def_name
        }
        if(pops_react$revert$name %in% names(plot_react$symbol)) {
          plot_react$symbol[pops_react$revert$name] <- new_pop$style 
          names(plot_react$symbol)[names(plot_react$symbol) == pops_react$revert$name] <- input$pop_def_name
        }
        
        # everything has been applied and current pop can now be the reference 
        pops_react$revert = obj_react$obj$pops[[input$pop_def_name]]
        
        if(length(attr(pops_react$revert, "reserved")) == 0) {
          showElement("pop_remove")
        } else {
          hideElement("pop_remove")
        }
      } else {
        if(input$pop_def_name %in% names(obj_react$obj$pops)) {
          stop(paste0("a population named '",input$pop_def_name,"' already exists"))
        } 
        if(pops_react$revert$type == "C") {
          if((length(pops_react$def)==0) || (pops_react$def[length(pops_react$def)] %in% c("And", "Or", "Not", "("))) stop("bad population definition")
          count = 0
          for(i in 1:length(pops_react$def)) {
            if(pops_react$def[i] == "(") count = count + 1
            if(pops_react$def[i] == ")") count = count - 1
          }
          if(count != 0) stop("bad population definition")
          obj_react$obj = data_add_pops(obj_react$obj, list(list(name = input$pop_def_name,
                                                                 type = "C",
                                                                 color = match_col(input$pop_color_dark),
                                                                 lightModeColor = match_col(input$pop_color_light),
                                                                 style = style,
                                                                 definition = paste0(pops_react$def, collapse = "|"))),
                                        display_progress = TRUE, session = session) 
          pops_react$new = FALSE
          runjs("Shiny.onInputChange('pop_edit', null)")
          runjs("Shiny.onInputChange('pop_manager_visible', false)")
          pops_react$revert = obj_react$obj$pops[[input$pop_def_name]]
        } 
        if(pops_react$revert$type == "T") {
          obj_react$obj = data_add_pops(obj_react$obj, list(list(name = input$pop_def_name,
                                                                 type = "T",
                                                                 color = match_col(input$pop_color_dark),
                                                                 lightModeColor = match_col(input$pop_color_light),
                                                                 style = style,
                                                                 obj = pops_react$revert$obj,
                                                                 display_progress = TRUE, session = session)))
        }
        pops_react$new = FALSE
        pops_react$revert = obj_react$obj$pops[[input$pop_def_name]]
        click("pop_close")
      }
      if((pops_react$revert$type == "T") && obj_react$back$info$found) {
        nam = pops_react$revert$name
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
      }
    },
    error = function(e) {
      obj_react$obj$regions = regions_back
      obj_react$obj$pops = pops_back
      obj_react$obj$graphs = graphs_back
      mess_global(title = "building population", msg = e$message, type = "error", duration = 10)
    },
    finally = {
    })
  }),
  observeEvent(input$pop_remove, suspended = TRUE,{
    to_remove = data_rm_pops(obj = obj_react$obj, pops = input$pop_def_name, list_only = TRUE, session=session)
    if((length(to_remove$regions) > 0 ) || (length(to_remove$pops) > 1)) {
      to_remove_msg = list(tags$p("Removing population '", tags$b(input$pop_selection), "' will also induce the removal of"))
      if(length(to_remove$regions) > 1) to_remove_msg = c(to_remove_msg, list(tags$p("- region(s):"), tags$ul(lapply(to_remove$regions, FUN = function(x) tags$li(x)))))
      if(length(to_remove$pops) > 0) to_remove_msg = c(to_remove_msg, list(tags$p("- populations(s):"), tags$ul(lapply(to_remove$pops[-1], FUN = function(x) tags$li(x)))))
      showModal(modalDialog(to_remove_msg,
                            size = "s",
                            easyClose = FALSE,
                            footer = list(actionButton(inputId = "pop_confirm_removal", label = "Proceed"),
                                          modalButton(label = "Abort"))),
                session = session) 
    } else {
      showModal(modalDialog(tags$p("Are you sure to remove '", tags$b(input$pop_def_name), "' population ?"),
                            size = "s",
                            easyClose = FALSE,
                            footer = list(actionButton(inputId = "pop_confirm_removal", label = "Yes"),
                                          modalButton(label = "Abort"))),
                session = session) 
    }
  }),
  observeEvent(input$pop_confirm_removal,suspended = TRUE, {
    tryCatch({
      toredraw = data_rm_pops(obj = obj_react$obj, pops = input$pop_def_name, list_only = TRUE, session=session)
      L = length(toredraw$graphs)
      if(L > 0) {
        lapply(1:L, FUN = function(i) {
          runjs(code = JS(sprintf("IFCshiny.grid.remove(IFCshiny.grid.getItems().filter(function (item) { return item._element.id === 'report_graph_%0.4i' }), { removeElements: true, layout: false })", i)))
        })
        names(obj_react$obj$graphs)[toredraw$graphs] <- NA
      }
      obj_react$obj = data_rm_pops(obj = obj_react$obj, pops = input$pop_def_name, list_only = FALSE, session=session)
      obj_react$obj = reinit_layout(obj_react$obj)
    },
    error = function(e) {
      mess_global(title = "removing population", msg = e$message, type = "error", duration = 10)
    }, finally = {
      removeModal(session = session)
      runjs("Shiny.onInputChange('pop_edit', null)")
      runjs("Shiny.onInputChange('pop_manager_visible', false)")
    })
  }),
  observeEvent(input$pop_close,suspended = TRUE, {
    if(pops_react$new) {
      showModal(modalDialog(tags$p("Are you sure you want to abort population creation ?"),
                            size = "s",
                            easyClose = FALSE,
                            footer = list(actionButton(inputId = "pop_abort", label = "Yes"),
                                          modalButton(label = "No"))),
                session = session)
    } else {
      style = c("Simple Dot", "Cross", "Plus", 
                "Empty Circle", "Empty Diamond", "Empty Square", 
                "Empty Triangle", "Solid Diamond", "Solid Square", 
                "Solid Triangle")
      style = c(20, 4, 3, 1, 5, 0, 2, 18, 15, 17)[input$pop_symbol == style]
      if(!all(c(
        pops_react$revert$name == input$pop_def_name,
        pops_react$revert$color == match_col(input$pop_color_dark),
        pops_react$revert$lightModeColor == match_col(input$pop_color_light),
        pops_react$revert$style == style,
        ifelse(pops_react$revert$type == "C", identical(pops_react$revert$split, pops_react$def), TRUE)))) {
        showModal(modalDialog(tags$p("Are you sure you want to revert applied changes to population ?"),
                              size = "s",
                              easyClose = FALSE,
                              footer = list(actionButton(inputId = "pop_abort", label = "Yes"),
                                            modalButton(label = "No"))),
                  session = session)
      } else {
        pops_react$new = FALSE
        runjs("Shiny.onInputChange('pop_edit', null)")
        runjs("Shiny.onInputChange('pop_manager_visible', false)")
      }
    }
  }),
  observeEvent(input$pop_abort, suspended = TRUE,{
    pops_react$new = FALSE
    removeModal(session = session)
    runjs("Shiny.onInputChange('pop_edit', null)")
    runjs("Shiny.onInputChange('pop_manager_visible', false)")
  }))