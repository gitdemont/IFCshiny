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

if(.access_fs) shinyFiles::shinyFileChoose(input, "local_file_batch",  roots = .app_volumes, session = getDefaultReactiveDomain())

observeEvent(obj_react$obj$haschanged_objects, {
  # observer to sync obj_react$batch with current obj_react$obj modification, always on
  if(length(obj_react$obj$haschanged_objects) != 0) obj_react$syncbatch <- TRUE
})

obs_batch_report = list(
  observeEvent(list(input$batch_grid,input$batch_rows), suspended = TRUE, {
    L = length(input$batch_grid)
    if(L < 1) return(NULL)
    k = input$batch_cols
    k = na.omit(k[is.finite(k)])
    o = input$batch_rows
    o = na.omit(o[is.finite(o)])
    if((length(k)==0) || (L < 1)) {
      updateNumericInput(session=session, inputId="batch_cols",value=1)
      return(NULL)
    }
    if((length(o)==0) || (L < 1)) {
      updateNumericInput(session=session, inputId="batch_rows",value=1)
      return(NULL)
    }
    if(o > L) updateNumericInput(session=session, inputId="batch_rows",value=L, max=L)
    if(L > (k * input$batch_rows)) updateNumericInput(session=session, inputId="batch_cols", value=k + 1L)
  }),
  observeEvent(list(input$batch_grid,input$batch_cols), suspended = TRUE, {
    L = length(input$batch_grid)
    if(L < 1) return(NULL)
    k = input$batch_rows
    k = na.omit(k[is.finite(k)])
    o = input$batch_cols
    o = na.omit(o[is.finite(o)])
    if(length(k)==0) {
      updateNumericInput(session=session, inputId="batch_rows",value=1)
      return(NULL)
    }
    if(length(o)==0) {
      updateNumericInput(session=session, inputId="batch_cols",value=1)
      return(NULL)
    }
    if(o > L) updateNumericInput(session=session, inputId="batch_cols",value=L, max=L)
    if(L > (k * input$batch_cols)) updateNumericInput(session=session, inputId="batch_rows", value=k + 1L)
  }),
  observeEvent(list(input$batch_layout,input$batch_grid), suspended = TRUE, {
    lay = input$batch_layout
    n_col = length(unique(sapply(lay, FUN = function(l) l$position$left)))
    n_row = length(unique(sapply(lay, FUN = function(l) l$position$top)))
    ids = sapply(lay, FUN = function(i) i$id)
    M = matrix(as.integer(ids), ncol=n_col, nrow=n_row, byrow = TRUE)
    if(n_col > 1) updateNumericInput(session=session, inputId="batch_cols", max=ifelse(all(is.na(M[, n_col])),n_col,n_col + 1L))
    if(n_row > 1) updateNumericInput(session=session, inputId="batch_rows", max=ifelse(all(is.na(M[n_row, ])),n_row,n_row + 1L))
  }),
  observeEvent(list(input$batch_grid, input$batch_cols, input$batch_rows), suspended = TRUE, {
    L = length(input$batch_grid)
    if(length(L) == 0) return(NULL)
    img_pad = 5
    img_siz = 100
    N = names(na.omit(obj_react$obj$graphs))
    removeUI("#batch_placeholder>#batch_grid", multiple = FALSE, immediate = TRUE, session = session)
    k = input$batch_rows * input$batch_cols * L
    k = na.omit(k[is.finite(k)])
    if((length(N) > 0) && (length(k) > 0) && (k > 0)) {
      if(L > (input$batch_rows * input$batch_cols)) return(NULL)
      insertUI("#batch_placeholder", where = "afterBegin", multiple = FALSE, immediate = TRUE, session = session,
               ui = tags$div(id = "batch_grid", style="position:relative"))
      # initialize batch muuri obj
      shinyjs::runjs(code = "IFCshiny.batch = new Muuri('#batch_placeholder>#batch_grid',{ dragEnabled: true });
                           IFCshiny.batch.on('layoutEnd', function (items) {
                             var out = {};
                             for(var i = 0; i < items.length; i++) {
                               var item = items[i];
                               out[i] = { 'id': item.getElement().getAttribute('data-graph'), 'position': item.getPosition() };
                             }
                             Shiny.onInputChange('batch_layout', out);
                           });")
      # fill batch muuri with nodes
      lapply(1:(input$batch_rows * input$batch_cols), FUN = function(i) {
        id = ""
        i_graph = ""
        if(i <= L) {
          i_graph = na.omit(c(plot_react$layout))[as.integer(input$batch_grid[i])]
          id = N[i_graph]
          tile = list(tags$p(input$batch_grid[i],style = "position:absolute; top:50%; left:50%; transform:translate(-50%,-50%);
                      font-weight:bold; font-size:xx-large; -webkit-font-smoothing:antialiased;
                      color:#fff; text-shadow:#000 0px 0px 5px"),
                      tags$img('data-id' = id, style="max-width:-webkit-fill-available"))
        } else {
          tile = tags$p("")
        }
        insertUI("#batch_placeholder>#batch_grid", where = "beforeEnd", multiple = FALSE, immediate = TRUE, session = session,
                 ui = tags$div(class = "batch_item",
                               'data-id' = id,
                               'data-graph' = i_graph,
                               style = sprintf("display:block; position:absolute; width:%spx; height:%spx; margin:%spx; z-index:1; background:#8c8c8c; color:#fff;",img_siz,img_siz,img_pad),
                               tags$div(class = "batch_item-content", tile)
                 )
        )
        # copy img from report to batch
        if(i <= L) {
          runjs(code = sprintf("var foo = $('#report_placeholder').find('[data-id=\"%s\"]').find('img').attr('src');
                              var bar = $('#batch_placeholder>#batch_grid').find('[data-id=\"%s\"]').find('img').attr('src', foo)", id, id))
        }
        shinyjs::runjs(code = sprintf("IFCshiny.batch.add($('#batch_placeholder>#batch_grid').children().last()[0], { index: %i, layout: false } );", i - 1))
      })
      runjs(code = sprintf("$('#batch_placeholder>#batch_grid').width('%ipx')", img_pad + input$batch_cols * (img_siz + 2 * img_pad)))
      runjs(code = sprintf("$('#batch_placeholder>#batch_grid').height('%ipx')", img_pad + input$batch_rows * (img_siz + 2 * img_pad)))
      runjs(code = "IFCshiny.batch.refreshItems().layout();")
      if(length(obj_react$batch) > 0) enable("batch_report_save_btn")
    } else {
      disable("batch_report_save_btn")
    }
  })
)

obs_batch = list(
  observeEvent(obj_react$syncbatch, {
    if(obj_react$syncbatch) {
      n = strsplit(basename(obj_react$obj$fileName), split = " _ ", fixed = TRUE)[[1]][1]
      if(n %in% names(obj_react$batch)) obj_react$batch[[n]] <- obj_react$obj
      obj_react$syncbatch <- FALSE
    }
  }),
  observeEvent(input$local_file_batch, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(inherits(input$local_file_batch, "shinyActionButtonValue")) return(NULL)
    if(length(input$local_file_batch) == 0) return(NULL)
    fileinfo = shinyFiles::parseFilePaths(.app_volumes, input$local_file_batch)
    fileinfo = sapply(fileinfo, simplify = FALSE, USE.NAMES = TRUE, FUN = function(x) unname(x, force = TRUE))
    runjs(code = sprintf("Shiny.onInputChange('file_batch', %s)", toJSON(fileinfo)))
    # runjs(code = sprintf("Shiny.onInputChange('file_batch', %s)", toJSON(x = as.data.frame(shinyFiles::parseFilePaths(.app_volumes, input$local_file_batch), data.frame = "columns"))))
  }),
  observeEvent(input$file_batch, {
    if(length(input$file_batch) == 0) return(NULL)
    # creates unique random names, this allows to navigate between batch files
    L = length(obj_react$batch)
    NN = unlist(input$file_batch$name, recursive = TRUE, use.names = FALSE)
    ids = character()
    while(length(ids) != length(NN)) ids = c(ids, random_name(special = NULL, forbidden = ids))
    file_b <- paste0(ids, " _ ",  NN)
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Batching files", reset = FALSE)
    tryCatch({
      # place files in batch_raw dir
      dir.create(file.path(session_dir, "batch_raw"), showWarnings = FALSE)
      # define new names
      new_names = file.path(session_dir, "batch_raw", file_b)
      # rename or copy input$file_batch
      if(.access_fs) {
        file.copy(from = unlist(input$file_batch$datapath, recursive = TRUE, use.names = FALSE), to = new_names)
      } else {
        file.rename(from = unlist(input$file_batch$datapath, recursive = TRUE, use.names = FALSE), to = new_names) 
      }
      
      # retrieve current gating strategy
      # TODO extract gs without writing a file
      # read files and apply gating strategy 
      obj1 = obj_react$obj
      gs_ML <- suppressMessages(readGatingStrategy(writeGatingStrategy(obj1, write_to = file.path(session_dir, "batch_raw", "batch_gating_ML.xml"), overwrite = TRUE)))
      if(length(model_react$fit) != 0) {       # we need to remove "ML" from current obj because "ML" features may not be present in input files
        check = grep("^ML_", names(obj1$features), value = TRUE)
        obj1 = suppressWarnings(data_rm_features(obj = obj1, features = check, list_only = FALSE))
        check = grep("^ML_", names(obj1$pops), value = TRUE)
        obj1 = suppressWarnings(data_rm_pops(obj = obj1, pops = check, list_only = FALSE))
        gs <- suppressMessages(readGatingStrategy(writeGatingStrategy(obj1, write_to = file.path(session_dir, "batch_raw", "batch_gating.xml"), overwrite = TRUE)))
        model_ = reactiveValuesToList(x = model_react)
        batch = lapply(1:length(new_names), FUN = function(i_file) {
          obj <- suppressMessages(suppressWarnings(readIFC(new_names[i_file], extract_features = TRUE, 
                                                           extract_images = FALSE, extract_stats = TRUE, 
                                                           extract_offsets = TRUE, recursive = TRUE, 
                                                           display_progress = TRUE, 
                                                           force_header = TRUE)))
          to_keep = names(obj$pops)[sapply(obj$pops, FUN = function(p) p$type == "T")]
          if(input$apply_gating) {
            obj <- applyGatingStrategy(obj = obj, gating = gs, keep = to_keep, display_progress = TRUE)
            tryCatch({
              # apply ML in 'mode'="predict_norm"
              nv = apply.IFCml(model = model_, obj = obj, mode = "predict_norm", 
                               newdata = "all",
                               verbose = FALSE)
              # force to keep new tagged populations (included new ML_ ones)
              to_keep = names(nv$obj$pops)[sapply(nv$obj$pops, FUN = function(p) p$type == "T")]
              # apply the initial gating strategy with eventually ML_ features and pops 
              obj <- applyGatingStrategy(nv$obj, gating = gs_ML, keep = to_keep, display_progress = TRUE)
            }, error = function(e) {
              mess_global(title = "file batch", msg = c(paste0("can't apply model on ",basename(obj$fileName)), e$message), type = "error")
              return(obj)
            })
          }
          obj
        })
      } else {
        batch = lapply(1:length(new_names), FUN = function(i_file) {
          obj <- suppressMessages(suppressWarnings(readIFC(new_names[i_file], extract_features = TRUE, 
                                                           extract_images = FALSE, extract_stats = TRUE, 
                                                           extract_offsets = TRUE, recursive = TRUE, 
                                                           display_progress = TRUE,
                                                           force_header = TRUE)))
          to_keep = names(obj$pops)[sapply(obj$pops, FUN = function(p) p$type == "T")]
          if(input$apply_gating) obj <- applyGatingStrategy(obj = obj, gating = gs_ML, keep = to_keep, display_progress = TRUE)
          obj
        })
      }
      N = names(obj_react$batch)
      # add new uploaded files to previous obj_react$batch list
      file_b = c(unname(sapply(obj_react$batch, FUN = function(b) basename(b$fileName))), file_b)
      obj_react$batch = c(obj_react$batch, batch)
      # set names to obj_react$batch
      names(obj_react$batch) <- c(N, ids)
      # update selector
      updateSelectInput(session = session, inputId = "file_main", choices = file_b, selected = file_b[1])
      # udpate ridge space
      updateSliderInput(session = session, inputId = "ridge_space", value = min(10,max(0.5, 2*length(file_b)-1)))
    }, error = function(e) {
      mess_global(title = "file batch", msg = e$message, type = "error")
      return(NULL)
    }, finally = {
      if(length(obj_react$batch) <= 1) {
        shinyjs::disable(id = "remove_main")
      } else {
        shinyjs::enable(id = "remove_main")
      }
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
    })
  }),
  observeEvent(input$file_main, {
    if(length(input$file_main) == 0 || input$file_main == "") return(NULL)
    add_log(paste0("moving to batch file: '", input$file_main, "'"))
    updateSelectInput(session=session, inputId="batch_grid", choices=character(), selected=NULL)
    new_main = strsplit(input$file_main, split = " _ ", fixed = TRUE)[[1]][1]
    obj_react$batch[[obj_react$curr]] <- obj_react$obj
    obj_react$obj <- compare(reinit_app(obj_react$batch[[new_main]]), obj_react$obj) 
    obj_react$back <- obj_react$obj
    obj_react$curr = new_main
    # modify current file_react
    file_react$input = list(name = basename(obj_react$obj$fileName),
                            size = file.size(obj_react$obj$fileName),
                            type = "",
                            datapath = obj_react$obj$fileName)
    file_react$id = random_name(n = 20)
    updateTabsetPanel(session = session, "navbar", selected = "tab7")
  }),
  observeEvent(input$remove_main, {
    if(length(obj_react$batch) <= 1) return(NULL)
    if(length(input$file_main) == 0 || input$file_main == "") return(NULL)
    add_log(paste0("removing from batch: '", input$file_main))
    to_rm = strsplit(input$file_main, split = " _ ", fixed = TRUE)[[1]][1]
    to_rm = to_rm == names(obj_react$batch)
    obj_react$batch = obj_react$batch[!to_rm]
    obj_react$stats = array(numeric(), dim=c(0,0,4))
    if(length(obj_react$batch) != 0) {
      N = unname(sapply(obj_react$batch, FUN = function(b) basename(b$fileName)))
      obj_react$curr = names(obj_react$batch)[1L]
      obj_react$obj <- compare(reinit_app(obj_react$batch[[obj_react$curr]]), obj_react$obj)
      updateSelectInput(session=session, inputId = "file_main", choices = N, selected = N[1])
    } else { # should not happen since button is disabled when obj_react$batch length is <= 1
      updateSelectInput(session=session, inputId = "file_main", selected = c(), choices = list())
      hideElement("batch_plot_controls")
      hideElement("batch_save")
      # modify current file_react
      file_react$input = list(name = basename(obj_react$obj$fileName),
                              size = file.size(obj_react$obj$fileName),
                              type = "",
                              datapath = obj_react$obj$fileName)
    }
    file_react$id = random_name(n = 20)
    if(length(obj_react$batch) <= 1) shinyjs::disable(id = "remove_main")
  }),
  observeEvent(list(input$navbar,input$navbar_batch), suspended = TRUE, ignoreInit = FALSE, {
    if(input$navbar != "tab7") return(NULL)
    lapply(obs_batch_report, FUN = function(x) x$suspend())
    hideElement("batch_feature")
    hideElement("violin_controls")
    hideElement("ridge_controls")
    hideElement("volcano_controls")
    hideElement("heatmap_controls")
    hideElement("stack_controls")
    hideElement("batch_report_controls")
    hideElement("batch_population")
    hideElement("batch_plot_controls")
    if(length(obj_react$obj$graphs) > 0) {
      showElement(selector = "#navbar_batch [data-value='Report']")
    } else {
      hideElement(selector = "#navbar_batch [data-value='Report']")
    }
    switch(input$navbar_batch, 
           "Violin" = {
             showElement("batch_population")
             showElement("batch_feature")
             showElement("batch_plot_controls")
             showElement("violin_controls")
           }, 
           "Ridge" = {
             showElement("batch_population")
             showElement("batch_feature")
             showElement("batch_plot_controls")
             showElement("ridge_controls")
           }, 
           "Volcano" = {
             showElement("batch_population")
             showElement("batch_plot_controls")
             showElement("volcano_controls")
           },
           "Heatmap" = {
             showElement("batch_population")
             showElement("batch_plot_controls")
             showElement("heatmap_controls")
           },
           "Stack" = {
             showElement("stack_controls")
             showElement("batch_plot_controls")
           },
           "Report" = {
             if(length(obj_react$obj$graphs) == 0) {
               runjs(code = "$('#navbar_batch [data-value=\"Violin\"]').trigger('click');" )
               runjs(code = "Shiny.onInputChange('#navbar_batch', 'Violin');")
               return(NULL)
             }
             lapply(obs_batch_report, FUN = function(x) x$resume())
             if(!any(input$report_ready)) {
               if(length(obj_react$obj$graphs) > 0) {
                 runjs("document.getElementById('msg_busy_txt2').innerText = 'updating batch graphs';")
                 runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
                 tryCatch({
                   runjs(code = "$('#navbar [data-value=\"tab6\"]').trigger('click');" )
                   runjs(code = "Shiny.onInputChange('navbar', 'tab6');")
                   observeEvent(input$report_ready, once = TRUE, autoDestroy = TRUE, ignoreInit = TRUE, {
                     updateSelectInput(session=session, inputId="batch_grid",
                                       choices=as.character(seq_along(obj_react$obj$graphs)),
                                       selected=as.character(seq_along(obj_react$obj$graphs)))
                     runjs(code = "$('#navbar [data-value=\"tab7\"]').trigger('click');" )
                     runjs(code = "Shiny.onInputChange('navbar', 'tab7');")
                   })
                 },
                 finally = {
                   runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
                 })
               }
             }
             showElement("batch_report_controls")
           })
  }),
  observeEvent(input$plot_batch_height, suspended = TRUE, ignoreInit = FALSE, ignoreNULL = TRUE, {
    runjs(code=sprintf("$('.obs_plot_invalidate,.obs_plot_invalidate>.html-widget').height(%i)", input$plot_batch_height))
  })
)

output$violin_plot <- renderPlotly({
  if((input$navbar != "tab7") ||
     (input$navbar_batch != "Violin") ||
     (length(obj_react$batch) == 0)) return(NULL)
  runjs("document.getElementById('msg_busy_txt2').innerText = 'updating violin plot';")
  runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
  tryCatch({plotly_batch_violin(obj_react$batch, 
                                pop = input$plot_batch_population,
                                feat = input$plot_batch_feature,
                                trans = input$plot_batch_feature_transform,
                                type = "violin",
                                height = input$plot_batch_height,
                                points = input$violin_points)
  }, error = function(e) {
    mess_global(title = "batch violin", msg = e$message, type = "error", duration = 10)
    return(NULL)
  },
  finally = {
    runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
  })
})
output$ridge_plot <- renderPlotly({
  if((input$navbar != "tab7") ||
     (input$navbar_batch != "Ridge") ||
     (length(obj_react$batch) == 0)) return(NULL)
  runjs("document.getElementById('msg_busy_txt2').innerText = 'updating ridge plot';")
  runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
  tryCatch({plotly_batch_violin(obj_react$batch, 
                                pop = input$plot_batch_population,
                                feat = input$plot_batch_feature,
                                trans = input$plot_batch_feature_transform,
                                type = "ridge",
                                space = input$ridge_space,
                                height = input$plot_batch_height,
                                points = "none")
  }, error = function(e) {
    mess_global(title = "batch ridge", msg = e$message, type = "error", duration = 10)
    return(NULL)
  },
  finally = {
    runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
  })
})
output$volcano_plot <- renderPlotly({
  if((input$navbar != "tab7") ||
     (input$navbar_batch != "Volcano") ||
     (length(obj_react$batch) == 0)) return(NULL)
  runjs("document.getElementById('msg_busy_txt2').innerText = 'updating volcano plot';")
  runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
  m = switch(input$volcano_method,"wilcoxon"="wilcox","t-test"="t")
  if(!any(attr(obj_react$stats, "method") == m) ||
     !any(attr(obj_react$stats, "pop") == input$plot_batch_population)) {
    obj_react$stats = batch_stats(obj_react$batch, pop=input$plot_batch_population, method=m)
    return(NULL)
  }
  tryCatch({plotly_batch_volcano(obj_react$stats, fold = input$volcano_fold, height = input$plot_batch_height)  }, error = function(e) {
    mess_global(title = "batch volcano", msg = e$message, type = "error", duration = 10)
    return(NULL)
  },
  finally = {
    runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
  })
})
output$heatmap_plot <- renderPlotly({
  if((input$navbar != "tab7") ||
     (input$navbar_batch != "Heatmap") ||
     (length(obj_react$batch) == 0)) return(NULL)
  runjs("document.getElementById('msg_busy_txt2').innerText = 'updating heatmap';")
  runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
  w = switch(input$heatmap_what,"zscore"="zscore","mean"="fold_avg","median"="fold_med")
  if(!any(attr(obj_react$stats, "pop") == input$plot_batch_population)) {
    obj_react$stats = batch_stats(obj_react$batch, pop=input$plot_batch_population, method="none")
    return(NULL)
  }
  tryCatch({plotly_batch_heatmap(obj_react$stats,
                                 height = input$plot_batch_height,
                                 what = w,
                                 dendro = input$heatmap_dendro == "yes")
  }, error = function(e) {
    mess_global(title = "batch heatmap", msg = e$message, type = "error", duration = 10)
    return(NULL)
  },
  finally = {
    runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
  })
})
output$stack_plot <- renderPlotly({
  if((input$navbar != "tab7") ||
     (input$navbar_batch != "Stack") ||
     (length(obj_react$batch) == 0)) return(NULL)
  runjs("document.getElementById('msg_busy_txt2').innerText = 'updating batch 3D stack';")
  runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
  tryCatch({
    plotly_batch_stack(obj_react$batch, 
                       g = plot_react$stack,
                       batch_mode = TRUE,
                       height = input$plot_batch_height)
  }, error = function(e) {
    mess_global(title = "batch 3D stack", msg = e$message, type = "error", duration = 10)
    return(NULL)
  },
  finally = {
    runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';")
  })
})
