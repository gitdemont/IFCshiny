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

# observers
obs_comp <- list(
  observeEvent(list(input$comp_resample,
                    input$comp_size1,
                    input$comp_size2), suspended = TRUE,
               ignoreNULL = FALSE, {
                 if(length(unlist(obj_react$obj$features_comp)) == 0) return(NULL)
                 comp_react$sub1 = sample(1:nrow(obj_react$obj$features_comp), size = min(nrow(obj_react$obj$features_comp), input$comp_size1))
                 comp_react$sub2 = sample(comp_react$sub1, size = min(length(comp_react$sub1), input$comp_size2))
               }),
  observeEvent(input$comp_close, suspended = TRUE, {
    shinyjs::runjs("Shiny.onInputChange('comp_manager_visible', false)")
  }),
  observeEvent(input$comp_reset, suspended = TRUE, {
    raw = decompensate(obj_react$obj$features_comp, spillover = comp_react$spillover)
    if(inherits(x = raw, what = "try-error")) return(NULL)
    obj_react$obj$features_comp = compensate(raw, spillover = comp_react$back)
    comp_react$spillover <- comp_react$back
    comp_react$pre <- comp_react$back
    comp_react$last <- comp_react$spillover
    if(input$comp_manager_visible) {
      shinyWidgets::updateNoUiSliderInput(session=session,inputId="comp_sliderX",
                                          value=100*unname(comp_react$spillover[input$comp_plot_2, input$comp_plot_1]))
      shinyWidgets::updateNoUiSliderInput(session=session,inputId="comp_sliderY",
                                          value=100*unname(comp_react$spillover[input$comp_plot_1, input$comp_plot_2]))
    }
  }),
  observeEvent(comp_react$pre, ignoreNULL = TRUE, suspended = TRUE, {
    toggleClass(id = "comp_apply", condition = !identical(comp_react$last, comp_react$pre), class = "clickme")
  }),
  observeEvent(input$comp_new, suspended = TRUE, {
    lapply(obs_comp, FUN = function(x) x$suspend())
    comp_react$spillover = as.matrix(data.frame())
    comp_react$pre = as.matrix(data.frame())
    comp_react$back = as.matrix(data.frame())
    obj_react$obj$features_comp = NULL
    runjs(code = "$('#navbar [data-value=\"tab0\"]').trigger('click');" )
    runjs(code = "Shiny.onInputChange('navbar', 'tab0');")
    onFlushed(once = TRUE, session=session, fun = function() { 
      runjs(code = "$('#navbar [data-value=\"tab9\"]').trigger('click');" )
      runjs(code = "Shiny.onInputChange('navbar', 'tab9');")
    })
  }),
  observeEvent(input$comp_compute, suspended = TRUE, {
    if(length(unlist(obj_react$obj$features_comp)) == 0) return(NULL)
    raw = decompensate(obj_react$obj$features_comp, spillover = comp_react$spillover)
    if(inherits(x = raw, what = "try-error")) return(NULL)
    obj_react$obj$features_comp = compensate(raw, spillover = comp_react$pre)
    comp_react$spillover <- comp_react$pre
    if(input$comp_manager_visible) {
      shinyWidgets::updateNoUiSliderInput(session=session,inputId="comp_sliderX",
                                          value=100*unname(comp_react$spillover[input$comp_plot_2, input$comp_plot_1]))
      shinyWidgets::updateNoUiSliderInput(session=session,inputId="comp_sliderY",
                                          value=100*unname(comp_react$spillover[input$comp_plot_1, input$comp_plot_2]))
    }
  }),
  observeEvent(input$comp_apply, suspended = TRUE, {
    if(length(unlist(obj_react$obj$features_comp)) == 0) return(NULL)
    showModal(modalDialog("Are you sure you want to apply compensation ?",
                          size = "s",
                          easyClose = FALSE,
                          footer = list(actionButton(inputId = "comp_modal_apply",label = "Proceed"),
                                        modalButton(label = "Abort"))),
              session = session)
    
    observeEvent(input$comp_modal_apply, once = TRUE, ignoreNULL = TRUE, ignoreInit = TRUE, autoDestroy = TRUE, {
      obj_back <- obj_react$obj
      tryCatch({
        obj_react$obj$description$spillover = t(recompensate(t(obj_react$obj$description$spillover), comp_react$last, comp_react$spillover))
        comp_react$last <- comp_react$spillover
        obj_react$obj$features[, colnames(obj_react$obj$features_comp)] <- obj_react$obj$features_comp
        pops = popsCompute(pops = obj_react$obj$pops,
                                         regions = obj_react$obj$regions,
                                         features = obj_react$obj$features,
                                         display_progress = TRUE, 
                                         session = session)
        obj_react$obj$pops = pops
        stats = data.frame(stringsAsFactors = FALSE, check.rows = FALSE, check.names = FALSE, t(sapply(names(pops), FUN=function(p) {
          count = sum(pops[[p]]$obj)
          base = pops[[p]]$base
          type = pops[[p]]$type
          if(base=="") base = "All"
          parent = sum(pops[[base]]$obj)
          c("type" = type, "parent" = base, "count" = count, "perc_parent" = count/parent*100, "perc_tot" = count/obj_react$obj$description$ID$objcount*100)
        })))
        stats[,3] = as.numeric(stats[,3])
        stats[,4] = as.numeric(stats[,4])
        stats[,5] = as.numeric(stats[,5])
        obj_react$obj$stats = stats
        
        obj_react$obj = reinit_layout(obj_react$obj)
      }, error = function(e) {
        obj_react$obj = obj_back
        mess_global(title = "applying compensation", msg = e$message, type = "error", duration = 10)
      },finally = removeModal(session = session))
    })
  }),
  observeEvent(input$comp_graphs_click, suspended = TRUE, {
    if(length(unlist(obj_react$obj$features_comp)) == 0) return(NULL)
    if(length(input$comp_graphs_click) == 0) return(NULL)
    if(input$comp_graphs_click$x < 0.04 || input$comp_graphs_click$x > 0.96 ||
       input$comp_graphs_click$y < 0.04 || input$comp_graphs_click$y > 0.96) return(NULL)
    id1 = (floor((input$comp_graphs_click$x - 0.04) / 0.92 * ncol(comp_react$spillover))) + 1
    id2 = ncol(comp_react$spillover) - (floor((input$comp_graphs_click$y - 0.04) / 0.92 * ncol(comp_react$spillover)))
    if(id1 <= id2 || id2 <= 0 || id1 > ncol(comp_react$spillover)) return(NULL)
    shinyjs::runjs(sprintf("Shiny.onInputChange('comp_manager_visible', %s)", ifelse(input$comp_manager_visible, "false", "true")))
    # if((input$comp_plot_1 != id1) || (input$comp_plot_2 != id2)) {
      shinyjs::runjs(sprintf("Shiny.onInputChange('comp_plot_1', %i)", id1))
      shinyjs::runjs(sprintf("Shiny.onInputChange('comp_plot_2', %i)", id2))
      shinyWidgets::updateNoUiSliderInput(session=session,inputId="comp_sliderX",value=100*unname(comp_react$spillover[id2, id1]))
      shinyWidgets::updateNoUiSliderInput(session=session,inputId="comp_sliderY",value=100*unname(comp_react$spillover[id1, id2]))
    # }
  }),
  observeEvent(input$comp_table_cell_edit, suspended = TRUE, {
    value = suppressWarnings(signif(as.numeric(input$comp_table_cell_edit$value), 3))
    i_row = input$comp_table_cell_edit$row
    i_col = 1+input$comp_table_cell_edit$col
    if(is.na(value) || (i_row == i_col)) {
      comp_react$pre[i_row, i_col] <- comp_react$spillover[i_row, i_col]
      return(NULL)
    } else {
      comp_react$pre[i_row, i_col] <- value
      click("comp_compute")
    }
    if(!input$comp_manager_visible) return(NULL)
    if(i_row == input$comp_plot_2 &&
       i_col == input$comp_plot_1) shinyWidgets::updateNoUiSliderInput(session=session,inputId="comp_sliderX",value=100*value)
    if(i_row == input$comp_plot_1 &&
       i_col == input$comp_plot_2) shinyWidgets::updateNoUiSliderInput(session=session,inputId="comp_sliderY",value=100*value)
  }),
  observeEvent(list(input$comp_sliderX,input$comp_sliderY), suspended = TRUE, {
    comp_react$pre[input$comp_plot_2, input$comp_plot_1] <- input$comp_sliderX / 100
    comp_react$pre[input$comp_plot_1, input$comp_plot_2] <- input$comp_sliderY / 100
  })
)
# output
output$comp_graphs <- renderPlot({
  if(length(unlist(obj_react$obj$features_comp)) == 0) return(NULL)
  raw = decompensate(obj_react$obj$features_comp[comp_react$sub2, ], spillover = comp_react$spillover)
  if(inherits(x = raw, what = "try-error")) return(NULL)
  raw = cbind(data.frame(raw, stringsAsFactors =  FALSE), type = "raw")
  colnames(raw) = c(param_react$param$channels$name, "type")
  int = cbind(data.frame(obj_react$obj$features_comp[comp_react$sub2, ], stringsAsFactors =  FALSE), type = "comp")
  colnames(int) = c(param_react$param$channels$name, "type")
  df = rbind(raw, int, deparse.level = 0, make.row.names = FALSE)
  df$type = factor(df$type, levels = c("raw", "comp"))
  hyper = 1000
  df[, -ncol(df)] = apply(df[, -ncol(df)], 2, smoothLinLog, hyper = hyper)
  panel <- function(x,y, ...) {
    par(new = TRUE)
    # plot(x = y, y = x,  ...)
    rasterplot(x = x, y = y, draw = TRUE, new = FALSE, ...)
    try(suppressWarnings({
    Xlim = (range(y, na.rm = TRUE))
    Ylim = (range(x, na.rm = TRUE))
    x_ticks = base_axis_constr(lim = Xlim, trans = hyper, nint = 10)
    y_ticks = base_axis_constr(lim = Ylim, trans = hyper, nint = 10)
    x_axis = axis(side = 3, at = x_ticks$at, labels = FALSE)
    # text(x = x_axis, y = Ylim[1] - diff(Ylim) * 0.07, labels = x_ticks$labels, 
    #      xpd = TRUE,
    #      adj = c(1, 1), srt = 45)
    y_axis = axis(side = 4, at = y_ticks$at, labels = FALSE)
    # text(y = y_axis, x = Xlim[1] - diff(Xlim) * 0.07, labels = y_ticks$labels, 
    #      xpd = TRUE,
    #      adj = c(1, 0.5), las = 1)
    }), silent = TRUE)
    box()
  }
  try(suppressWarnings(pairs(df[, -ncol(df)], 
        pch = 20, 
        # asp = 1,
        col= sapply(c("lightgrey", "chartreuse4"), FUN = function(x) { 
          paste0(c("#",sprintf("%02X", col2rgb(x)),"50"),collapse = "")
        })[as.integer(df$type)], 
        lower.panel = NULL,
        upper.panel = panel,
        gap = 0.2, 
        xaxt = "n",
        yaxt = "n"
  )), silent = TRUE)
})
output$comp_plot <- renderPlot({
  if(length(unlist(obj_react$obj$features_comp)) == 0) return(NULL)
  raw = decompensate(obj_react$obj$features_comp[comp_react$sub1, ], spillover = comp_react$spillover)
  rec = compensate(raw, spillover = comp_react$pre)
  raw = cbind(data.frame(raw, stringsAsFactors =  FALSE), type = "raw")
  names(raw) = c(param_react$param$channels$name, "type")
  int = cbind(data.frame(obj_react$obj$features_comp[comp_react$sub1, ], stringsAsFactors =  FALSE), type = "comp")
  names(int) = c(param_react$param$channels$name, "type")
  rec = cbind(data.frame(rec, stringsAsFactors =  FALSE), type = "rec")
  names(rec) = c(param_react$param$channels$name, "type")
  df = rbind(raw, int, rec)
  df$type = factor(df$type, levels = c("raw", "comp", "rec"))
  hyper = 1000
  df[, -ncol(df)] = apply(df[, -ncol(df)], 2, smoothLinLog, hyper = hyper)
  rasterplot(x = df[, input$comp_plot_2], y = df[, input$comp_plot_1], axes = FALSE,
       pch = ifelse(df$type == "comp", 21, 19), 
       col= sapply(c("lightgrey", "chartreuse4", "firebrick"), FUN = function(x) { 
         paste0(c("#",sprintf("%02X", col2rgb(x)),"FF"),collapse = "")
       })[as.integer(df$type)],
       ylab = #colnames(df)[input$comp_plot_2],
       param_react$param$channels$name[input$comp_plot_2],
       xlab = #colnames(df)[input$comp_plot_1] 
       param_react$param$channels$name[input$comp_plot_1]
  )
  try({
  Xlim = (range(df[, input$comp_plot_2], na.rm = TRUE))
  Ylim = (range(df[, input$comp_plot_1], na.rm = TRUE))
  x_ticks = base_axis_constr(lim = Xlim, trans = hyper, nint = 10)
  y_ticks = base_axis_constr(lim = Ylim, trans = hyper, nint = 10)
  x_axis = axis(side = 1, at = x_ticks$at, labels = FALSE)
  text(x = x_axis, y = Ylim[1] - diff(Ylim) * 0.07, labels = x_ticks$labels, 
       xpd = TRUE,
       adj = c(1, 1), srt = 45)
  y_axis = axis(side = 2, at = y_ticks$at, labels = FALSE)
  text(y = y_axis, x = Xlim[1] - diff(Xlim) * 0.07, labels = y_ticks$labels, 
       xpd = TRUE,
       adj = c(1, 0.5), las = 1)
  }, silent = TRUE)
  box()
})
output$comp_table <- DT::renderDT({
  if(length(unlist(obj_react$obj$features_comp)) == 0) return(NULL)
  if(length(comp_react$spillover) == 0) return(NULL)
  foo = comp_react$pre
  colnames(foo) <- gsub("^(.*) <.*>$", "\\1", param_react$param$channels$name)
  DT::formatRound(DT::datatable(foo,
                                editable = TRUE,
                                rownames = FALSE, #extensions = 'Buttons',
                                selection = list(mode = 'none'),
                                options = list(pageLength = -1,
                                               # buttons = c('csv', 'excel', 'pdf'),
                                               dom = 'tr',#'Btr',
                                               autoWidth = FALSE,
                                               columnDefs = list(list(orderable = FALSE, targets = "_all"),
                                                                 list(width = "10%", targets="_all"),
                                                                 list(searchable = FALSE, targets = "_all"),
                                                                 list(className = "dt-center",targets = "_all")))),
                  columns = 1:ncol(comp_react$spillover), digits = 3)
})