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

runjs("Shiny.setInputValue('report_draw', 0)")
runjs("Shiny.setInputValue('report_recover', 0)")

reinit_layout <- function(obj) {
  L = length(obj$graphs)
  if(L > 0) {
    lay=lapply(obj$graphs, FUN=function(g) with(g, c(x=xlocation,y=ylocation)))
    lay=as.data.frame(do.call(rbind, lay))
    row.names(lay)=1:L
    lay=by(lay, lay$y, FUN=function(d) {
      d=d[order(d$x), ]
      d$x=seq_along(d$x)
      cbind("N"=as.integer(row.names(d)), d, stringsAsFactors=FALSE)
    })
    lay=lapply(1:length(lay), FUN=function(i) {
      d=lay[[i]]
      d$y=i
      return(d)
    })
    lay=do.call("rbind", c(lay, make.row.names=FALSE))
    plot_react$layout = ftable(by(lay$N, lay[,c("y","x")], FUN=function(x) x))
    names(obj$graphs) = rep(NA, L)
    for(id in 1:L) {
      pos = which(id == plot_react$layout, arr.ind = TRUE)
      obj$graphs[[id]]$xsize <- input$report_size
      obj$graphs[[id]]$ysize <- input$report_size
      obj$graphs[[id]]$ylocation <- unname(pos[1, "row"] - 1) * input$report_size
      obj$graphs[[id]]$xlocation <- unname(pos[1, "col"] - 1) * input$report_size
    }
    showElement(selector = "#navbar [data-value='tab6']")
  } else {
    if(input$navbar == "tab6") {
      runjs(code = "$('#navbar [data-value=\"tab0\"]').trigger('click');" )
      runjs(code = "Shiny.onInputChange('navbar', 'tab0');")
    }
    hideElement(selector = "#navbar [data-value='tab6']")
  }
  return(obj)
}

# the report is generated thanks to grid.js
# the layout function has been tweaked to prevent empty tiles from being 
# before (in left to right order) a graph in a same row
obs_report <- list(
  observeEvent(input$report_recover, suspended = TRUE, {
    runjs(JS("if(IFCshiny.hasOwnProperty('grid')) {",
             "  IFCshiny.grid.destroy(true);",
             "  delete IFCshiny.grid;",
             "}",
             "$('#report_placeholder').children().remove();"))
    session$sendCustomMessage("init_grid", "")
    plot_react$layout = matrix(ncol=1, nrow=0)
    obj_react$obj = reinit_layout(obj_react$obj)
    if(input$navbar == "tab6") runjs(sprintf("Shiny.onInputChange('report_draw', %i)", input$report_draw + 1L))
  }),
  
  # observer to draw the plot in a grid
  # if a plot is found at the grid coordinate it is drawn.
  # if not, an empty image is drawn
  # everytime this observer is triggered empty tiles and obj_react$obj$graphs with no name are redrawn
  observeEvent(input$report_draw, suspended = TRUE, {
    # while drawing we prevent undesired interaction with the app
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Updating graphs", reset = FALSE)
    
    # we extract graphs names
    N = names(obj_react$obj$graphs)
    # remove empty tile
    runjs(code = "IFCshiny.grid.remove(IFCshiny.grid.getItems().filter(function (item) { return item._element.classList.contains('empty_tile') }), { removeElements: true, layout: false })")
    
    # fill the report grid
    if(length(obj_react$obj$graphs) != 0) {
      i_tile = -1
      for(i_row in 1:nrow(plot_react$layout)) {
        for(i_col in 1:ncol(plot_react$layout)) {
          redraw = TRUE
          i_tile = i_tile + 1
          # plot_react contains graph position in the grid
          i_graph = plot_react$layout[i_row, i_col]
          id = sprintf("report_graph_%0.4i",i_graph)
          outfile = tempfile(fileext = ".png")
          if(is.na(i_graph)) {
            i_graph <- 0
          } else {
            if(is.na(N[i_graph])) {
              N[i_graph] <- basename(outfile)
            } else {
              redraw = FALSE
            }
          }
          if(redraw) {
            png(filename = outfile, width = 600, height = 600, units = "px")
            tryCatch({
              if(i_graph == 0) {
                plot.new()
              } else {
                obj_react$obj$graphs[[i_graph]]$xsize <- input$report_size
                obj_react$obj$graphs[[i_graph]]$ysize <- input$report_size
                obj_react$obj$graphs[[i_graph]]$ylocation <- (i_row - 1) * input$report_size
                obj_react$obj$graphs[[i_graph]]$xlocation <- (i_col - 1) * input$report_size
                plot_raster(plotGraph(obj = obj_react$obj, graph = obj_react$obj$graphs[[i_graph]], draw = FALSE, stats_print = FALSE,
                                    viewport = "ideas", precision = "full"))
              }
            }, error = function(e) {
              print(e$message)
              # obj_react$obj$graphs = obj_react$obj$graphs[-i_graph]
              # N <<- N[-i_graph]
              # i_graph <<- 0
              # plot_react$layout[i_row, i_col] <- NA
            },
            finally = dev.off())
            
            # we need to recreate an image each time we want to pass it to the grid
            if(i_graph == 0) {
              insertUI(selector = "#empty_tile_placeholder", where = "beforeEnd",
                       multiple = FALSE, immediate = TRUE, session = session,
                       ui = tags$div(class = "report_item empty_tile",
                                     "data-id" = "", id = "report_graph_0000",
                                     tags$img(class = "report_item-content",
                                              "data-id" = "",
                                              src = session$fileUrl(NULL, outfile, contentType='image/png'))))
              runjs(code = JS(sprintf("var tile = $('#empty_tile_placeholder>.report_item')[0]", i_graph),
                              sprintf("IFCshiny.grid.add(tile, { index: %i, layout: false } );", i_tile)))
            } else {
              insertUI(selector = "#plot_tile_placeholder", where = "beforeEnd",
                       multiple = FALSE, immediate = TRUE, session = session,
                       ui = tags$div(class = "report_item",
                                     "data-id" = basename(outfile), id = id,
                                     tags$img(class = "report_item-content",
                                              "data-id" = basename(outfile),
                                              src = session$fileUrl(NULL, outfile, contentType='image/png'),
                                              ondblclick = "IFCshiny.report_dblclick(event)"),
                                     tags$div(class="report_item_msg",
                                              tags$div(tags$p("drag me to change layout"),
                                                       tags$p("double click me to edit"),
                                                       tags$p("close to remove"))),
                                     tags$div(class="report_item_btn",
                                              onclick = "IFCshiny.report_close(event)",
                                              icon("window-close", lib = "font-awesome"))))
              runjs(code = JS(sprintf("var tile = $('#plot_tile_placeholder>.report_item')[0]", i_graph),
                              sprintf("IFCshiny.grid.add(tile, { index: %i, layout: false } );", i_tile)))
            }
            unlink(outfile)
          }
        }
      }
    }
    
    # once the whole grid has been drawn we rename the obj_react$obj$graphs
    # this is important since not named graphs will be redrawn
    names(obj_react$obj$graphs) <- N
    
    # set grid dimensions
    # we set the size of each grid elements 
    session$sendCustomMessage('resize_grid', list(eleId = 'report_placeholder',
                                                  width = 5 + (input$report_size + 10) * ncol(plot_react$layout),
                                                  height = 5 + (input$report_size + 10) * nrow(plot_react$layout),
                                                  i_width = input$report_size,
                                                  i_height = input$report_size,
                                                  blink = FALSE,
                                                  layout = TRUE))
    
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
  }),
  
  # this observer reacts on grid layout changed by the user
  # it is dedicated to recompute new position of tiles (empty images or plot)
  # and to store these information in plot_react
  observeEvent(input$report_layout, suspended = TRUE,{
    if(length(input$report_layout)==0) return(NULL)
    graph_size = na.omit(as.integer(input$report_size))
    if(length(graph_size) == 0) return(NULL)
    tile_size = graph_size + 10
    lay = input$report_layout
    n_col = sapply(lay, FUN = function(l) l$position$left)
    n_row = sapply(lay, FUN = function(l) l$position$top)
    ids = sapply(lay, FUN = function(i) i$id)
    names(lay) = ids
    plot_react$layout = matrix(NA, ncol=length(unique(n_col)), nrow=length(unique(n_row)))
    for(id in ids) {
      if(is.na(id)) next
      if(id == "") next
      if(!(id %in% names(obj_react$obj$graphs))) next
      i_row = (lay[[id]]$position$top / tile_size)
      i_col = (lay[[id]]$position$left / tile_size)
      obj_react$obj$graphs[[id]]$xsize <- graph_size
      obj_react$obj$graphs[[id]]$ysize <- graph_size
      obj_react$obj$graphs[[id]]$ylocation <- i_row * graph_size
      obj_react$obj$graphs[[id]]$xlocation <- i_col * graph_size
      plot_react$layout[i_row + 1, i_col + 1] <- which(id == names(obj_react$obj$graphs))
    }
    # row with only empty images will be dropped
    # we allow only one extra row
    # so we need to indentify them and to remove them from grid layout
    extra_row = apply(plot_react$layout, 1, FUN = function(row) all(is.na(row)))
    extra_row = extra_row[-length(extra_row)]
    if(any(extra_row)) {
      to_remove = c()
      i_tile = -1
      for(i_row in 1:(nrow(plot_react$layout)-1)) {
        for(i_col in 1:ncol(plot_react$layout)) {
          i_tile = i_tile + 1
          if(extra_row[i_row]) {
            to_remove = c(to_remove, i_tile)
          }
        }
      }
      if(length(to_remove) != 0) {
        runjs(code = sprintf("IFCshiny.grid.remove(IFCshiny.grid.getItems([%s]), { removeElements: true, layout: false });", paste0(to_remove, collapse=",")))
        runjs(code = "IFCshiny.grid.refreshItems().layout()")
      }
      plot_react$layout = subset.matrix(x = plot_react$layout, subset = c(!extra_row, TRUE), drop = FALSE)
    }
    # we recompute each element size and the whole grid dimension
    session$sendCustomMessage('resize_grid', list(eleId = 'report_placeholder',
                                                  width = 5 + (input$report_size + 10) * ncol(plot_react$layout),
                                                  height = 5 + (input$report_size + 10) * nrow(plot_react$layout),
                                                  i_width = input$report_size,
                                                  i_height = input$report_size,
                                                  blink = FALSE,
                                                  layout = FALSE))
    # we inspect the -1 last column, if it is empty we trigger a last column removal
    n_col = ncol(plot_react$layout)
    if(n_col > 2) if(all(is.na(plot_react$layout[, n_col:(n_col-1)]))) click("report_col_rm")
    # finally, if there is no more graphs stored after user action
    # we exit report tab and hide it
    if(length(obj_react$obj$graphs) == 0) {
      plot_react$layout = matrix(NA, ncol=1, nrow=0)
      runjs(code = "IFCshiny.grid.remove(IFCshiny.grid.getItems().filter(function (item) { return item._element.classList.contains('empty_tile') }), { removeElements: true, layout: false })")
      runjs(code = "IFCshiny.grid.refreshItems().layout()")
      hideElement(selector = "#navbar [data-value='tab6']")
      runjs(code = "$('#navbar [data-value=\"tab0\"]').trigger('click');" )
      runjs(code = "Shiny.onInputChange('navbar', 'tab0');")
    } else {
      showElement(selector = "#navbar [data-value='tab6']")
    }
  }),
  
  # observer to control item and grid dimension
  # item are 5 px padded in each direction
  observeEvent(input$report_size,suspended = TRUE, ignoreInit = TRUE, {
    if(length(na.omit(input$report_size)) == 0) return(NULL)
    if(length(obj_react$back)==0) return(NULL)
    session$sendCustomMessage('resize_grid', list(eleId = 'report_placeholder',
                                                  width = 5 + (input$report_size + 10) * ncol(plot_react$layout),
                                                  height = 5 + (input$report_size + 10) * nrow(plot_react$layout),
                                                  i_width = input$report_size,
                                                  i_height = input$report_size,
                                                  blink = TRUE,
                                                  layout = TRUE))
  }),
  
  # observer to allow the addition of a new empty row at the bottom of the grid
  # a new row will be added only if there is no empty row already at the bottom
  observeEvent(input$report_row_add,suspended = TRUE, {
    if(nrow(plot_react$layout) != 0) if(all(is.na(plot_react$layout[nrow(plot_react$layout), ]))) {
      mess_global(title = "change report layout", msg = "can't add row when the last one is empty", type = "info", duration = 5)
      return(NULL)
    } 
    plot_react$layout = rbind(plot_react$layout, rep(NA, ncol(plot_react$layout)))
    i_tile = -1
    for(i_row in 1:nrow(plot_react$layout)) {
      for(i_col in 1:ncol(plot_react$layout)) {
        i_tile = i_tile + 1
        if(i_row == nrow(plot_react$layout)) {
          # create en empty tile
          outfile = tempfile(fileext = ".png")
          png(filename = outfile, width = 600, height = 600, units = "px")
          tryCatch({ plot.new() }, error = function(e) { print(e$message) },finally = dev.off())
          insertUI(selector = "#empty_tile_placeholder", where = "beforeEnd",
                   multiple = FALSE, immediate = TRUE, session = session,
                   ui = tags$div(class = "report_item", 
                                 "data-id" = "", id = "report_graph_0000",
                                 tags$img(class = "report_item-content",
                                          "data-id" = "",
                                          src = session$fileUrl(NULL, outfile, contentType='image/png'))))
          unlink(outfile)
          runjs(code = JS("var empty = $('#empty_tile_placeholder>.report_item')[0]",
                          sprintf("IFCshiny.grid.add(empty, { index: %i, layout: false } );",i_tile)))
        } 
      }
    }
    session$sendCustomMessage('resize_grid', list(eleId = 'report_placeholder',
                                                  width = 5 + (input$report_size + 10) * ncol(plot_react$layout),
                                                  height = 5 + (input$report_size + 10) * nrow(plot_react$layout),
                                                  i_width = input$report_size,
                                                  i_height = input$report_size,
                                                  blink = TRUE,
                                                  layout = TRUE))
    runjs(code = "$('#report_placeholder>#report_graph_0000').addClass('empty_tile')")
  }),
  
  # observer to remove an empty row at the bottom of the grid
  observeEvent(input$report_row_rm, suspended = TRUE,{
    if(nrow(plot_react$layout) != 0) if(!all(is.na(plot_react$layout[nrow(plot_react$layout),]))) {
      mess_global(title = "change report layout", msg = "can't remove row when the last one is not empty", type = "info", duration = 5)
      return(NULL)
    } 
    runjs(code = sprintf("IFCshiny.grid.remove(IFCshiny.grid.getItems().filter(function(item) { return item.getPosition().top === %i }), { removeElements: true, layout: false });", (nrow(plot_react$layout) - 1) * (input$report_size + 10)))
    if(is.null(nrow(plot_react$layout))) plot_react$layout = matrix(plot_react$layout, nrow = 1)
    session$sendCustomMessage('resize_grid', list(eleId = 'report_placeholder',
                                                  width = 5 + (input$report_size + 10) * ncol(plot_react$layout),
                                                  height = 5 + (input$report_size + 10) * nrow(plot_react$layout),
                                                  i_width = input$report_size,
                                                  i_height = input$report_size,
                                                  blink = TRUE,
                                                  layout = TRUE))
  }),
  
  # observer to allow the addition of a new empty column at the right of the grid
  # a new column will be added only if there is no empty column already at the end
  observeEvent(input$report_col_add,suspended = TRUE, {
    if(ncol(plot_react$layout) != 0) if(all(is.na(plot_react$layout[,ncol(plot_react$layout)]))) {
      mess_global(title = "change report layout", msg = "can't add col when the last one is empty", type = "info", duration = 5)
      return(NULL)
    } 
    plot_react$layout = cbind(plot_react$layout, rep(NA, nrow(plot_react$layout)))
    i_tile = -1
    o_tile = -1
    for(i_row in 1:nrow(plot_react$layout)) {
      for(i_col in 1:ncol(plot_react$layout)) {
        i_tile = i_tile + 1
        if(i_col == ncol(plot_react$layout)) {
          # create en empty tile
          outfile = tempfile(fileext = ".png")
          png(filename = outfile, width = 600, height = 600, units = "px")
          tryCatch({ plot.new() }, error = function(e) { print(e$message) },finally = dev.off())
          insertUI(selector = "#empty_tile_placeholder", where = "beforeEnd",
                   multiple = FALSE, immediate = TRUE, session = session,
                   ui = tags$div(class = "report_item",
                                 "data-id" = "", id = "report_graph_0000",
                                 tags$img(class = "report_item-content",
                                          "data-id" = "",
                                          src = session$fileUrl(NULL, outfile, contentType='image/png'))))
          unlink(outfile)
          runjs(code = JS("var empty = $('#empty_tile_placeholder>.report_item')[0]",
                          sprintf("IFCshiny.grid.add(empty, { index: %i, layout: false } );",i_tile)))
        } else {
          o_tile = o_tile + 1
        }
      }
    }
    session$sendCustomMessage('resize_grid', list(eleId = 'report_placeholder',
                                                  width = 5 + (input$report_size + 10) * ncol(plot_react$layout),
                                                  height = 5 + (input$report_size + 10) * nrow(plot_react$layout),
                                                  i_width = input$report_size,
                                                  i_height = input$report_size,
                                                  blink = TRUE,
                                                  layout = TRUE))
    runjs(code = "$('#report_placeholder>#report_graph_0000').addClass('empty_tile')")
  }),
  # observe to remove the last column if it is empty
  observeEvent(input$report_col_rm, suspended = TRUE,{
    if(ncol(plot_react$layout) != 0) if(!all(is.na(plot_react$layout[, ncol(plot_react$layout)]))) {
      mess_global(title = "change report layout", msg = "can't remove col when the last one is not empty", type = "info", duration = 5)
      return(NULL)
    } 
    runjs(code = sprintf("IFCshiny.grid.remove(IFCshiny.grid.getItems().filter(function(item) { return item.getPosition().left === %i }), { removeElements: true, layout: false });", (ncol(plot_react$layout) - 1) * (input$report_size + 10)))
    plot_react$layout = plot_react$layout[, -ncol(plot_react$layout)]
    if(is.null(ncol(plot_react$layout))) plot_react$layout = matrix(plot_react$layout, ncol = 1)
    session$sendCustomMessage('resize_grid', list(eleId = 'report_placeholder',
                                                  width = 5 + (input$report_size + 10) * ncol(plot_react$layout),
                                                  height = 5 + (input$report_size + 10) * nrow(plot_react$layout),
                                                  i_width = input$report_size,
                                                  i_height = input$report_size,
                                                  blink = TRUE,
                                                  layout = TRUE))
  }),
  
  # observer to allow the removal of a graph from the grid
  # if clicked it will remove the selected item and replace it by an empty one
  # then we remove the corresponding graph from obj_react$obj$graphs
  # finally, we trigger a grid layout to reposition the items which will end by sending back
  # these new positions
  observeEvent(input$report_close, suspended = TRUE,{
    if(length(input$report_close) == 0) return(NULL)
    # create en empty tile
    outfile = tempfile(fileext = ".png")
    png(filename = outfile, width = 600, height = 600, units = "px")
    tryCatch({ plot.new() }, error = function(e) { print(e$message) },finally = dev.off())
    insertUI(selector = "#empty_tile_placeholder", where = "beforeEnd",
             multiple = FALSE, immediate = TRUE, session = session,
             ui = tags$div(class = "report_item", 
                           "data-id" = "", id = "report_graph_0000",
                           tags$img(class = "report_item-content",
                                    "data-id" = "",
                                    src = session$fileUrl(NULL, outfile, contentType='image/png'))))
    unlink(outfile)
    runjs(code = JS(sprintf("var item = IFCshiny.grid.getItems().filter(function (item) {return item._element.firstElementChild.getAttribute('data-id') === '%s'});", input$report_close),
                    "var ele = item[0].getElement();",
                    "var index = IFCshiny.grid.getItems().indexOf(item[0]);",
                    "var empty = $('#empty_tile_placeholder>.report_item')[0]",
                    "IFCshiny.grid.remove(item, { removeElements: true, layout: false });",
                    "IFCshiny.grid.add(empty, { index: index, layout: false } );"))
    to_remove = names(obj_react$obj$graphs) %in% input$report_close
    K = class(obj_react$obj$graphs)
    obj_react$obj$graphs = obj_react$obj$graphs[-which(to_remove)]
    class(obj_react$obj$graphs) = K
    runjs(code = sprintf("$('.report_item').each(function(index) { $(this).width('%ipx'); });", input$report_size))
    runjs(code = sprintf("$('.report_item').each(function(index) { $(this).height('%ipx'); });", input$report_size))
    runjs(code = sprintf("$('#report_placeholder').width('%ipx')", 5 + (input$report_size + 10) * ncol(plot_react$layout)))
    runjs(code = sprintf("$('#report_placeholder').height('%ipx')", 5 + (input$report_size + 10) * nrow(plot_react$layout)))
    runjs(code = "$('#report_placeholder>#report_graph_0000').addClass('empty_tile')") # once added we can set class 'empty_tile' to prevent dragging
    runjs(code = "IFCshiny.grid.refreshItems().layout()")
  }))