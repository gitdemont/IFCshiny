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

obs_batch = list(
  observeEvent(input$file_batch, {
    if(length(input$file_batch) == 0) return(NULL)
    # creates names, names are digit-basename(file)
    # this will allow to navigate between batch files
    L = length(obj_react$batch)
    start = ifelse(L == 0, 1, L)
    file_b <- paste0(seq_along(unlist(input$file_batch$name)) + start, "-",  unlist(input$file_batch$name))
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Batching files", reset = FALSE)
    tryCatch({
      # place files in batch_raw dir
      dir.create(file.path(session_react$dir, "batch_raw"), showWarnings = FALSE)
      # define new names
      new_names = file.path(session_react$dir, "batch_raw", file_b)
      # rename input$file_batch
      file.rename(from = input$file_batch$datapath, to = new_names)
      
      # retrieve current gating strategy
      # TODO extract gs without writing a file
      # read files and apply gating strategy 
      obj1 = obj_react$obj
      gs_ML <- suppressMessages(readGatingStrategy(writeGatingStrategy(obj1, write_to = file.path(session_react$dir, "batch_raw", "batch_gating_ML.xml"), overwrite = TRUE)))
      if(length(model_react$fit) != 0) {       # we need to remove "ML" from current obj because "ML" features may not be present in input files
        check = grep("^ML_", names(obj1$features), value = TRUE)
        obj1 = suppressWarnings(data_rm_features(obj = obj1, features = check, list_only = FALSE, session=session))
        check = grep("^ML_", names(obj1$pops), value = TRUE)
        obj1 = suppressWarnings(data_rm_pops(obj = obj1, pops = check, list_only = FALSE, session=session))
        gs <- suppressMessages(readGatingStrategy(writeGatingStrategy(obj1, write_to = file.path(session_react$dir, "batch_raw", "batch_gating.xml"), overwrite = TRUE)))
        model_ = reactiveValuesToList(x = model_react)
        batch = lapply(1:length(new_names), FUN = function(i_file) {
          obj <- suppressMessages(suppressWarnings(readIFC(new_names[i_file], extract_features = TRUE, 
                                                           extract_images = FALSE, extract_stats = TRUE, 
                                                           extract_offsets = TRUE, recursive = TRUE, 
                                                           display_progress = TRUE, 
                                                           force_header = TRUE, session = session)))
          to_keep = names(obj$pops)[sapply(obj$pops, FUN = function(p) p$type == "T")]
          obj <- applyGatingStrategy(obj = obj, gating = gs, keep = to_keep, display_progress = TRUE, session = session)
          tryCatch({
            # apply ML in 'mode'="predict_norm"
            nv = apply.IFCml(model = model_, obj = obj, mode = "predict_norm", 
                             newdata = "all",
                             session = session, verbose = FALSE)
            # force to keep new tagged populations (included new ML_ ones)
            to_keep = names(nv$obj$pops)[sapply(nv$obj$pops, FUN = function(p) p$type == "T")]
            # apply the initial gating strategy with eventually ML_ features and pops 
            obj <- applyGatingStrategy(nv$obj, gating = gs_ML, keep = to_keep, display_progress = TRUE, session = session)
          }, error = function(e) {
            mess_global(title = "file batch", msg = c(paste0("can't apply model on ",basename(obj$fileName)), e$message), type = "error")
            return(obj)
          })
          obj
        })
      } else {
        batch = lapply(1:length(new_names), FUN = function(i_file) {
          obj <- suppressMessages(suppressWarnings(readIFC(new_names[i_file], extract_features = TRUE, 
                                                           extract_images = FALSE, extract_stats = TRUE, 
                                                           extract_offsets = TRUE, recursive = TRUE, 
                                                           display_progress = TRUE,
                                                           force_header = TRUE, session = session)))
          to_keep = names(obj$pops)[sapply(obj$pops, FUN = function(p) p$type == "T")]
          obj <- applyGatingStrategy(obj = obj, gating = gs_ML, keep = to_keep, display_progress = TRUE, session = session)
          obj
        })
      }
      if(L == 0) {
        # add current obj_react$obj in 1st position of the batch list
        file_b = c(paste0("1-",basename(obj_react$obj$fileName)), file_b)
        obj_react$obj$fileName <- file_b[1]
        # in this process we remove images from current obj and param_react
        # FIXME maybe it could be interesting to keep images ?
        obj_react$obj <- set_default_info(obj_react$obj)
        obj_react$back <- obj_react$obj
        # set current batch # to 1 (i.e, the current obj_react$obj)
        obj_react$curr = 1
        obj_react$batch = c(list(obj_react$obj), batch)
      } else {
        # add new uploaded files to previous obj_react$batch list
        file_b = c(names(obj_react$batch), file_b)
        obj_react$batch = c(obj_react$batch, batch)
      }
      
      # set names to obj_react$batch
      names(obj_react$batch) <- file_b
      # update selector
      updateSelectInput(session = session, inputId = "file_main", choices = file_b, selected = file_b[obj_react$curr])
      # udpate ridge space
      updateSliderInput(session = session, inputId = "ridge_space", value = min(10,max(0.5, 2*length(file_b)-1)))
    }, error = function(e) {
      mess_global(title = "file batch", msg = e$message, type = "error")
      return(NULL)
    }, finally = mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE))
  }),
  observeEvent(input$file_main, {
    if(length(input$file_main) == 0 || input$file_main == "") return(NULL)
    new_main = as.integer(strsplit(input$file_main, split = "-", fixed = TRUE)[[1]][1])
    if(names(obj_react$batch)[obj_react$curr] != input$file_main) add_log(paste0("moving from batch file: '", names(obj_react$batch)[obj_react$curr], "', to: '", input$file_main, "'"))
    obj_react$batch[[obj_react$curr]] <- obj_react$obj
    obj_react$obj <- compare(reinit_app(obj_react$batch[[new_main]]),obj_react$obj, session=session) 
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
    if(length(obj_react$batch) == 0) return(NULL)
    if(length(input$file_main) == 0 || input$file_main == "") return(NULL)
    add_log(paste0("removing from batch: '", input$file_main))
    to_rm = !input$file_main == names(obj_react$batch)
    obj_react$batch = obj_react$batch[to_rm]
    obj_react$stats = array(numeric(), dim=c(0,0,4))
    if(length(obj_react$batch) != 0) {
      N = names(obj_react$batch)
      N = sapply(N, FUN = function(x) paste0(strsplit(x, "-", fixed = TRUE)[[1]][-1], collapse = ""))
      N = paste(1:length(N), N, sep = "-")
      names(obj_react$batch) = N
      updateSelectInput(session=session, inputId = "file_main", choices = N, selected = N[1])
    } else {
      updateSelectInput(session=session, inputId = "file_main", selected = c(), choices = list())
      obj_react$stats = array(numeric(), dim=c(0,0,4))
      hideElement("batch_plot_controls")
      hideElement("batch_save")
      obj_react$obj$fileName <- paste0(strsplit(obj_react$obj$fileName, "-", fixed = TRUE)[[1]][-1], collapse = "")
      obj_react$back$fileName <- obj_react$obj$fileName
      # modify current file_react
      file_react$input = list(name = basename(obj_react$obj$fileName),
                              size = file.size(obj_react$obj$fileName),
                              type = "",
                              datapath = obj_react$obj$fileName)
      file_react$id = random_name(n = 20)
    }
  }),
  observeEvent(input$navbar_batch, suspended = TRUE, ignoreInit = FALSE, {
    if(input$navbar != "tab7") return(NULL)
    hideElement("batch_feature")
    hideElement("violin_controls")
    hideElement("ridge_controls")
    hideElement("volcano_controls")
    hideElement("heatmap_controls")
    hideElement("stack_controls")
    showElement("batch_population")
    switch(input$navbar_batch, 
           "Violin" = {
             showElement("batch_feature")
             showElement("violin_controls")
           }, 
           "Ridge" = {
             showElement("batch_feature")
             showElement("ridge_controls")
           }, 
           "Volcano" = {
             showElement("volcano_controls")
           },
           "Heatmap" = {
             showElement("heatmap_controls")
           },
           "Stack" = {
             hideElement("batch_population")
             showElement("stack_controls")
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
                       g = plot_react$g,
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
