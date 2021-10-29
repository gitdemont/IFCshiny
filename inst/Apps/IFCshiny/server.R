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

# Pay attention to run global.R first, since ui.R rely on variables created in global.R
server <- function(input, output, session) {
  # generate unique session id + unique temp directory
  shinyEnv = environment()
  while(TRUE) {
    sessionid <- paste0(sample(x = c(sample(x = 1:9, size = 20, replace = TRUE),
                                                           sample(x = LETTERS, size = 26, replace = TRUE),
                                                           sample(x = letters, size = 26, replace = TRUE)),
                                                     size = 20, replace = FALSE), collapse = "", sep = "")
    if(!dir.exists(file.path(tempdir(check = FALSE), sessionid))) break
  }
  dir_tmp = file.path(tempdir(check = TRUE), sessionid)
  dir.create(path = dir_tmp, recursive = TRUE)
  sessiondir =normalizePath(dir_tmp, winslash = "/", mustWork = TRUE)
  
  # function to record LOGS + trigger LOGS download on app error
  session_react <- reactiveValues(id = sessionid, dir = sessiondir)
  
  # we use shiny:outputinvalidated to show/hide msg_busy_ctn on training_plot update
  # it taining_plot can be long when flowsom is used because of metaClustering computation
  # TODO find something to test if webgl is available for interaction once rgl has sent scene to javascript
  # the idea is to hide IFCshiny app on output invalidation and to show it back only when the whole scene is ready
  runjs(code = paste(sep = ";\n;",
                     "$(document).on('shiny:outputinvalidated', function(event) {",
                     "  var id = event.target.id;",
                     "  if (id === undefined) {",
                     "    return null;",
                     "  } else {",
                     # "    console.log(id)",
                     # "    if(id === 'plot_3D') {",
                     # # "      var timer = window.setInterval(function() {",
                     # # "        var wdg = document.getElementById('plot_3D');",
                     # # "        if(wdg == null) return null;",
                     # # "        var rgl = wdg.rglinstance;",
                     # # "        if(rgl == null) return null;",
                     # # "        haschanged = null;",
                     # # "        change = rgl.opaquePass;",
                     # # "        if(haschanged == null) haschanged = change;",
                     # # "        if(change !== haschanged) clearInterval(timer);",
                     # # "        console.log(change);",
                     # # "      }, 50)",
                     # "      document.getElementById('msg_busy_ctn2').style.display = 'block';",
                     # # "        window.requestIdleCallback(function() {",
                     # # "          console.log('js,idle');",
                     # # "          document.getElementById('msg_busy_ctn2').style.display = 'none';",
                     # # "        }, { timeout: 5000 })",
                     # "      $(document).one('shiny:idle', function(event) {",
                     # "        window.requestIdleCallback(function() {",
                     # "          console.log('shiny,idle');",
                     # "          document.getElementById('msg_busy_ctn2').style.display = 'none';",
                     # "        })",
                     # # "        }, { timeout: 5000 })",
                     # # "        document.getElementById('msg_busy_ctn').style.display = 'none'",
                     # "      })",
                     # "    }",
                     # "    if(id === 'plot_3D') {",
                     # "    }",
                     "    if(id === 'training_plot') {",
                     "      document.getElementById('msg_busy_txt2').innerText = 'updating training graph';",
                     "      document.getElementById('msg_busy_ctn2').style.display = 'block';",
                     "      $(document).one('shiny:idle', function(event) {",
                     "        document.getElementById('msg_busy_ctn2').style.display = 'none';",
                     "      })",
                     "    }",
                     "  }",
                     "})"))
  
  # use shiny:inputchanged to keep an eye on select input changes so has to maintain our tootltip
  # for long names in features, pops ... 
  runjs(code = paste(sep = "\n;",
                     "$(document).on('shiny:inputchanged', function(event) {",
                     "  var id = event.target.id;",
                     "  var ele = document.getElementById(id);",
                     "  if((ele != null) && (ele.tagName.toLowerCase() == 'select')) {",
                     "    $(ele).parent().find('option,.option').each(function(index,val) {var el = $(this);var txt=el.attr('data-value');el.attr({'data-toggle':'tooltip','title':txt,'data-placement':'top','data-delay':{'show':100,'hide':300},'data-trigger':'hover'})});",
                     "  }",
                     "})"))
  
  # we ensure fab (shinymanager button) will be under our important messages and thus not clickable
  runjs(code = "$('.container-fab').css('z-index', '98')")
  
  # test if we are on mobile device
  runjs(code = "Shiny.setInputValue('run_on_mobile', /((iPhone)|(iPad)|(Android)|(BlackBerry))/.test(navigator.userAgent));")
  
  # test if webgl is available
  runjs(code = "var canvas=document.createElement('canvas');var gl=canvas.getContext('webgl')||canvas.getContext('experimental-webgl');Shiny.setInputValue('webgl_available',!!(false | (gl && gl instanceof WebGLRenderingContext)));delete canvas; delete gl;")
  
  # general move up / down
  observeEvent(input$movetop, {
    runjs(code="$('html, body').animate({scrollTop: '0px' }, 300);")
  })
  observeEvent(input$movedown, {
    runjs(code="$('html, body').animate({ scrollTop: $(document).height()-$(window).height() }, 300);")
  })
  # observeEvent(input$closeapp, {
  #   showModal(modalDialog(
  #     title = "Closing application",
  #     "Are you sure you want to close ?",
  #     easyClose = TRUE,
  #     footer =  list(actionButton(inputId = "close_app_anyway", label = "Yes"),
  #                    modalButton(label = "Abort"))
  #   ))
  #   observeEvent(input$close_app_anyway, autoDestroy = TRUE, ignoreInit = TRUE, {
  #     # session$reload()
  #     runjs(code = "window.close()")
  #   })
  # })
  
  tryCatch({
    source(file.path(.rundir, "server_msg.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    # log javascript 
    showLog()
    add_log("App started")
    
    source(file.path(.rundir, "server_auth.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    # we setup reactiveValues that will be used within the app
    source(file.path(.rundir, "server_default.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    reinit_default(reactiveValues, env = shinyEnv)

    # observers for draggable panels
    source(file.path(.rundir, "server_managers.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    # observer for credits
    source(file.path(.rundir, "server_credits.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    ##### init end
    
    ##### file input
    # reactive use to process file input
    react_dat <- reactive({
      # reinit values
      reinit_default(list, not = c("param_react"))
      
      # reinit display (go to top, hide all tabs, remove msg, close draggable panels)
      updateTabsetPanel(session = session, "navbar", selected = "tab0")
      runjs(code="$('html, body').animate({scrollTop: '0px' }, 300);")
      hideElement("msg_app", anim = FALSE, animType = "fade", time = 0)
      runjs("Shiny.onInputChange('pop_edit', null)")
      runjs("Shiny.onInputChange('pop_alt_click', null)")
      updateRadioButtons(session=session, inputId = "plot_type", selected = "1D", inline = TRUE)
      plot_react$shown = NULL
      plot_react$order = NULL
      reinit_managers()
      hideElement("infos_save")
      hideElement("logs_save")
      hideElement("compute_features")
      updateSelectInput(session = session, inputId = "channels", selected = NULL)
      disable(selector = "#channels input[type='checkbox']")
      hideElement(selector = "#navbar [data-value='tab1']")
      hideElement(selector = "#navbar [data-value='tab2']")
      hideElement(selector = "#navbar [data-value='tab3']")
      hideElement(selector = "#navbar [data-value='tab4']")
      hideElement(selector = "#navbar [data-value='tab5']")
      hideElement(selector = "#navbar [data-value='tab6']")
      hideElement(selector = "#navbar [data-value='tab7']")
      hideElement(selector = "#navbar [data-value='tab8']")
      hideElement(selector = "#navbar [data-value='tab9']")
      # treat example as file input
      if(requireNamespace("IFCdata", quietly = TRUE) && input$use_example) {
        fileinfo = list(name = paste0("example.", input$example_file), type = rep("", length(input$example_file)))
        fileinfo$datapath = system.file(package = "IFCdata", "extdata", fileinfo$name)
        fileinfo$size = sapply(X = fileinfo$datapath, FUN = file.size, simplify = TRUE, USE.NAMES = FALSE)
        runjs(code = sprintf("Shiny.onInputChange('file', %s)", toJSON(fileinfo)))
        fileName <- file.path(dirname(unlist(fileinfo$datapath)), unlist(fileinfo$name))
      } else {
        fileinfo <- lapply(input$file, unlist)
        file_react$input <- input$file
        if(all(sapply(fileinfo$datapath, dirname) %in% file.path(system.file(package = "IFCdata"), "extdata")) || length(fileinfo$datapath)==0) return(NULL)
        fileName <- file.path(dirname(unlist(fileinfo$datapath)), unlist(fileinfo$name))
        file.rename(from = unlist(fileinfo$datapath), to = fileName)
      }
      # process input
      # check extension
      ext <- getFileExt(fileName)
      daf_file = ext %in% "daf"
      cif_file = ext %in% "cif"
      rif_file = ext %in% "rif"
      msg = NULL
      if(all(ext == "")) {
        msg = c(msg, "You should select at least one file")
      }
      if(any(c(sum(daf_file), sum(cif_file), sum(rif_file))>1)) {
        msg = c(msg, "-You should not select more than one [rif, cif, daf, daf+rif, daf+cif]")
      }
      if(all(c(any(cif_file), any(rif_file)))) {
        msg = c(msg, "-You should not select rif and cif at the same time")
      }
      if((sum(!(daf_file | rif_file | cif_file)) > 0) && any(c(daf_file, rif_file, cif_file))) {  
        msg = c(msg, "-You should not select fcs and [rif, cif or daf] at the same time")
      }
      if(length(msg) != 0) {
        mess_global(title = "file input", msg = msg, type = "error", duration = 10)
        return(NULL)
      }
      # add_log(fileName)
      msg = NULL
      fileName_image = fileName[which(rif_file | cif_file)]
      if(any(daf_file)) {
        if(!any(c(cif_file, rif_file))) {
          mess_global(title = "file input", msg = "You selected daf only without its corresponding image file: Images will not be available", type = "info", duration = 10)
        } else{
          fileName_image = fileName[which(rif_file | cif_file)]
        }
        fileName = fileName[daf_file]
      }
      ##### Extracts data from file
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Reading file", reset = FALSE)
      tryCatch({
        if(any(daf_file, rif_file, cif_file)) {
          # at first we extract daf if any, otherwise it will read rif or cif
          dat = suppressMessages(suppressWarnings(readIFC(fileName = fileName, extract_features = TRUE, 
                                                          extract_images = FALSE, extract_stats = TRUE, 
                                                          extract_offsets = TRUE, recursive = TRUE, 
                                                          display_progress = TRUE, session = session)))
          # then we store images parameters from this 1st file
          Images = dat$description$Images
          if(nrow(Images) != 0) Images[, "gamma"] = apply(Images[, c("xmin", "xmax", "xmid", "ymid")], MARGIN = 1, computeGamma)
          # finally we try to extract info from other files if any were provided
          info = try(getInfo(fileName = fileName_image, from = "analysis",
                             verbose = FALSE, warn = FALSE, ntry = 1,
                             cifdir = dirname(fileName), display_progress = TRUE, session = session),
                     silent = TRUE)
          if(!("try-error" %in% class(info))) {
            if(any(daf_file) && any(c(cif_file, rif_file))) {
              if(dat$checksum != info$checksum) {
                class(info) <- "try-error"
                mess_global(title = "file input", msg = "Selected image file does not correspond to .daf: images will not be available", type = "error", duration = 10)
              }
            } 
          }
        } else {
          # here we use dedicated ExtractFromFCS to read FCS file
          dat = try(ExtractFromFCS(fileName = fileName, session = session), silent = TRUE)
          if(("try-error" %in% class(dat))) stop("fcs file does not seem to be well formatted:\n", attr(dat, "condition")$message)
          info = try(stop(""), silent = TRUE)
          dat$info = list(found = FALSE, instrument = dat$description$FCS$instrument)
        }
        # if info were obtained without error, it means that a rif or a cif was input
        # so we can extract image parameters from this file.
        # we also extract masks + illumination + magnification information
        # however, in case we had a daf and an cif we will use daf image parameter for the display
        if(!("try-error" %in% class(info))) {
          info$Images <- Images
          param_react$info = info
          param_react$param = objectParam(info = info,
                                          mode = "rgb",
                                          export="base64",
                                          write_to = "%s_%o_%c.png",
                                          base64_id = TRUE,
                                          overwrite = FALSE,
                                          composite = "",
                                          selection = "all",
                                          size = c(0,0),
                                          force_width = TRUE,
                                          random_seed = NULL,
                                          removal = "none",
                                          add_noise = TRUE,
                                          full_range = FALSE,
                                          force_range = FALSE)
          dat$info = info
          dat$fileName_image = info$fileName_image
          if(nrow(dat$description$masks) == 0) {
            dat$description$masks = info$masks
            if(nrow(dat$description$masks) == 0) {
              dat$description$masks = data.frame(type = "C", name = "MC", def = paste0(sprintf("M%02i",daf$description$Images$physicalChannel), collapse="|Or|"))
            }
            class(dat$description$masks) = list("IFC_masks", "data.frame")
          } 
          dat$info$illumination = dat$info$illumination[dat$info$illumination$powered, c("wavelength", "power","min","max")]
          dat$info$found = TRUE
        } else {
          dat$info = list("fileName" = fileName,
                          "fileName_image" = paste0(dat$description$ID$file, ", not found"),
                          "Merged_rif" = "N/A, no image file found",
                          "Merged_cif" = "N/A, no image file found",
                          "masks" = dat$description$masks,
                          "objcount" = dat$description$ID$objcount,
                          "date" = dat$description$Assay$date,
                          "instrument" = dat$info$instrument,
                          "sw_process" = dat$description$Assay$IDEAS_version,
                          "fcs_version" = dat$description$Assay$FCS_version,
                          "Images" = dat$description$Images,
                          "illumination" = "N/A, no image file found",
                          "CrossTalkMatrix" = "N/A, no image file found",
                          "magnification" = "N/A, no image file found",
                          "high-gain" = "N/A, no image file found",
                          "brightfield" = "N/A, no image file found",
                          "in_use" = rep(FALSE, 12),
                          "found" = FALSE)
          dat$info$in_use[dat$description$Images$physicalChannel] <- TRUE
        }
        dat$daf_file <- daf_file
        return(checkObj(dat))
      },
      error = function(e) {
        mess_global(title = "file input", msg = e$message, type = "error")
        return(NULL)
      },
      finally = {
        mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
      })
    })
    source(file.path(.rundir, "server_files_input.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    # navbar observer
    source(file.path(.rundir, "server_navbar.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    ##### detachable cell panel
    source(file.path(.rundir, "server_cell_manager.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    ##### detachable cell panel end
    
    ##### compensation, # compensation is under dev
    if(file.exists(file.path(.rundir, "server_compensation.R"))) source(file.path(.rundir, "server_compensation.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    ##### report
    source(file.path(.rundir, "server_report.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    ##### report end
    
    ##### extra feat
    source(file.path(.rundir, "server_extra_feat.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    ##### extra feat end
    
    ##### ML logic
    source(file.path(.rundir, "server_ML.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    # shared between network and report and graphs
    source(file.path(.rundir, "server_shared.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    ##### pop create / edit / remove
    source(file.path(.rundir, "server_populations.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    ##### reg create / edit / remove
    source(file.path(.rundir, "server_regions.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    ##### cells
    source(file.path(.rundir, "server_cells.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    ##### images settings
    source(file.path(.rundir, "server_img_manager.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    ##### plot
    source(file.path(.rundir, "server_graphs.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    ##### export
    source(file.path(.rundir, "server_files_output.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
    ##### outputs
    output$infos <- renderPrint({
      if(length(react_dat()$info$in_use) == 0) return(cat(""))
      if(any(input$population=="")) return(cat(""))
      if(input$navbar!="tab0") return(cat(""))
      cat(sep = "\n",
          paste0("input: ", input$file$name),
          paste0("date: " , react_dat()$info$date),
          paste0("instrument: " , react_dat()$info$instrument),
          paste0("sw_raw: " , react_dat()$info$sw_raw),
          paste0("sw_process: " , react_dat()$info$sw_process),
          paste0("fcs_version: " , react_dat()$info$fcs_version),
          "\nMerged_rif: ", react_dat()$info$Merged_rif,
          "\nMerged_cif: ", react_dat()$info$Merged_cif,
          paste0("\nobjcount: ", react_dat()$info$objcount),
          paste0("magnification: ", react_dat()$info$magnification),
          paste0("high-gain: ", react_dat()$info$evmode),
          paste0("in_use: ", paste0(sprintf("%0.02i", which(react_dat()$info$in_use)), collapse = ",")),
          paste0("brightfield: ", ifelse(react_dat()$info$found, paste0(sprintf("%0.02i", which(react_dat()$info$brightfield$channel)), collapse = ","), react_dat()$info$brightfield)),
          "\nIllumination:")
      print(react_dat()$info$illumination)
      cat("\nCompensation:\n")
      print(react_dat()$info$CrossTalkMatrix)
      cat("\nDisplay:\n")
      tmp = grepl("^name$|^color$|^physicalChannel$|^xmin$|^xmax$|^xmid$|^ymid$|^scalemin^|^scalemax$", colnames(react_dat()$info$Images))
      print(react_dat()$info$Images[, tmp])
      if(ncol(react_dat()$description$masks)!=0) {
        cat("\nMasks:\n")
        print(react_dat()$description$masks[, c("name","def")])
      }
    })
    output$table <- DT::renderDataTable(server = TRUE, expr = {
      if(length(react_dat()$info$in_use) == 0) return(NULL)
      if(any(input$population=="")) return(NULL)
      if(input$navbar!="tab1") return(NULL)
      if(length(input$table_page)==0) return(NULL)
      df = subset(obj_react$obj$features, subset = obj_react$obj$pops[[input$population]]$obj)
      P = input$table_page
      if(input$table_sort_order%%2 != DT_react$order) {
        DT_react$order = isolate(input$table_sort_order%%2)
        updateNumericInput(session = session, inputId = "table_page", value = 1)
        P = 1
        DT_react$loaded = FALSE
        if(DT_react$order) {
          updateActionButton(session = session, inputId = "table_sort_order", icon = icon(name="sort-amount-up", lib="font-awesome"))#label="\u21c5 \u21a5")
        } else {
          updateActionButton(session = session, inputId = "table_sort_order", icon = icon(name="sort-amount-down-alt", lib="font-awesome"))#label="\u21c5 \u21a7")
        }
      }
      if(input$table_sort_feature != DT_react$sort) {
        DT_react$sort = isolate(input$table_sort_feature)
        updateNumericInput(session = session, inputId = "table_page", value = 1)
        P = 1
        DT_react$loaded = FALSE
      }
      if(input$table_length != DT_react$length) {
        DT_react$length = isolate(input$table_length)
        updateNumericInput(session = session, inputId = "table_page", value = 1)
        P = 1
        DT_react$loaded = FALSE
      }
      if(DT_react$order) {
        df = df[rev(order(df[,DT_react$sort])), c("Object Number", input$table_sort_feature)]
      } else {
        df = df[order(df[,DT_react$sort]), c("Object Number", input$table_sort_feature)]
      }
      val = df[, c("Object Number", input$table_sort_feature)]
      val[, 1] = as.integer(val[, 1])
      v = 1:nrow(val)
      chunks = split(v, ceiling(v/as.integer(input$table_length)))
      L = length(chunks)
      if(is.null(P)) P = 1
      if(!is.finite(P)) P = 1
      if(P < 1) {
        P = 1
        updateNumericInput(session = session, inputId = "table_page", max = L, value = 1)
      } else {
        if(P > L) {
          P = L
          updateNumericInput(session = session, inputId = "table_page", max = L, value = L) 
        } else {
          updateNumericInput(session = session, inputId = "table_page", max = L)   
        }
      }
      if(nrow(val) == 0) {
        disable("cells_save_btn")
        mess_global(title = "extracting images", msg = "No object to display", type = "info", duration = 10)
        return(NULL)
      } else {
        enable("cells_save_btn")
      }
      if(DT_react$loaded && !input$img_manager_visible) {
        tryCatch({
          if(length(param_react$param$chan_to_keep) != 0) {
            ids = NULL
            table_imgs <- objectExtract(ifd = getIFD(param_react$param$fileName_image, trunc_bytes = 1, force_trunc = TRUE,
                                                     offsets = subsetOffsets(react_dat()$offsets, objects = val$`Object Number`[chunks[[P]]], input$chan_type),
                                                     verbose = FALSE, display_progress = FALSE, bypass = TRUE, session = session),
                                        param = param_react$param,
                                        verbose = FALSE,
                                        bypass = TRUE)
            ids = as.integer(unlist(sapply(table_imgs, attr, which = "object_id")))
            if(length(ids) == 0) ids = as.integer(gsub("^(img|msk)_", "", unlist(sapply(table_imgs, attr, which = "offset_id"))))
            if(anyDuplicated(ids)) stop("Some objects have same 'ids'.")
            if(!identical(ids,  val$`Object Number`[chunks[[P]]])) stop("Extracted object_ids differ from expected ones.")
            # table_imgs = lapply(table_imgs, FUN = function(i_img) lapply(i_img, FUN = function(i_chan) paste0("<div>",i_chan,"</div>")))
            if(input$table_sort_feature == "Object Number") {
              DT_react$temp = data.frame("Object Number" = val[chunks[[P]], 1], do.call(what="rbind", args=table_imgs),
                                         check.rows = FALSE, check.names = FALSE, stringsAsFactors = FALSE)#, do.call(what="rbind", args=table_imgs))
            } else {
              DT_react$temp = cbind(val[chunks[[P]], ], do.call(what="rbind", args=table_imgs))#, do.call(what="rbind", args=table_imgs))
            }
          } else {
            if(input$table_sort_feature == "Object Number") {
              DT_react$temp = data.frame("Object Number" = val[chunks[[P]], 1],
                                         check.rows = FALSE, check.names = FALSE, stringsAsFactors = FALSE)#, do.call(what="rbind", args=table_imgs))
            } else {
              DT_react$temp = val[chunks[[P]], ]#, do.call(what="rbind", args=table_imgs))
            }
          }
        }, error = function(e) {
          mess_global(title = "extracting images", msg = e$message, type = "stop")
          DT_react$temp = data.frame()
        }, finally = {
        })
      } else {
        if(input$table_page==1 & P==1) DT_react$loaded = TRUE
      }
      
      DT::datatable(data = {
        # print(paste0("i o:",input$table_sort_order," s:",input$table_sort_feature," l:",input$table_length," l:",input$population," p:",input$table_page)) # for debugging
        # print(paste0("g L:",DT_react$loaded," o:",DT_react$order," s:",DT_react$sort," l:",DT_react$displayLength," v:",DT_react$displayView," p:",DT_react$page)) # for debugging
        isolate(DT_react$temp)
      }, escape = FALSE, rownames = FALSE, extensions = 'Buttons',
      selection = list(mode = 'none'), #style = "bootstrap",
      # filter = "top",
      options = list(pageLength = as.numeric(input$table_length),
                     dom = 'Btr',
                     # lengthMenu = FALSE,
                     autoWidth = FALSE,
                     # scrollX=TRUE,
                     buttons = list(list(
                       extend = 'pdf',
                       title = 'exported with IFCshiny',
                       text = 'PDF',
                       pageSize = "A2",
                       extension = '.pdf',
                       header = TRUE,
                       footer = TRUE,
                       orientation = "landscape",
                       customize = JS("function (doc) {",
                                      "if (doc) {",
                                      "doc.margin = [0,0,0,12];",
                                      "for (var i = 1; i < doc.content[1].table.body.length; i++) {",
                                      "for (var j = 1; j < doc.content[1].table.body[i].length; j++) {",
                                      "var foo = doc.content[1].table.body[i][j].text;",
                                      "var w = foo.indexOf('width=');",
                                      "var h = foo.indexOf('height=');",
                                      "var s = foo.indexOf('src=');",
                                      "var e = foo.length;",
                                      sprintf("var wid = parseInt(foo.substring(w + 7, h - 2))*%s;",0.75),
                                      sprintf("var hei = parseInt(foo.substring(h + 8, s - 2))*%s;",0.75),
                                      "foo = foo.substring(s + 5, e - 2);",
                                      "doc.content[1].table.body[i][j] = { image: foo, alignment: 'center', width: wid, height: hei };",
                                      "}",
                                      "}",
                                      sprintf("doc.header = { text: '%s', alignment: 'center', fontSize: 15};", basename(react_dat()$fileName)),
                                      "doc.footer = function(currentPage, pageCount) { return [ { text: currentPage.toString() + ' - ' + pageCount, alignment: 'center' } ] };",
                                      "}",
                                      "}"),
                       exportOptions = list(
                         columns = c(0,1:length(input$channels)+ifelse(input$table_sort_feature == "Object Number",0,1)),
                         stripHtml = FALSE)
                     )),
                     columnDefs = list(list(orderable = FALSE, targets = "_all"),
                                       list(searchable = FALSE, targets = 1:length(input$channels)+ifelse(input$table_sort_feature == "Object Number",0,1)),
                                       list(className = "dt-center",targets = "_all")),
                     drawCallback = JS("function( settings ) {;",
                                       sprintf("$(this).find('thead').find('input.form-control').slice(%i).each(function(i) {", ifelse(input$table_sort_feature == "Object Number",1,2)),
                                       "$(this).hide();",
                                       "});",
                                       "$('.display.dataTable').css({ 'width':'auto','margin':'0' })",
                                       "}"),
                     rowCallback = JS("function(row, data, index) {;",
                                      "$(row).on('click', 'td', function() {;",
                                      "Shiny.onInputChange('chanclicked', $(\"#\" + $(this).parents('table')[0].id ).DataTable().columns().header().toArray().map(x => x.innerText)[this._DT_CellIndex.column]);",
                                      "Shiny.onInputChange('cellclicked',data[0]);",
                                      "null;})}")
      ))
    })
    output$chan_cell_range <- renderPlot({
      if(is.null(input$chan_cell_todisplay)) return(NULL)
      if(!is.finite(input$chan_cell_todisplay)) return(NULL)
      if(input$chan_cell_todisplay < 0) return(NULL)
      if(input$chan_cell_todisplay > react_dat()$description$ID$objcount-1) return(NULL)
      par(mar=c(2,1,1,1))
      k = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
      if(length(k) == 0) return(NULL)
      IFD = getIFD(fileName = react_dat()$fileName_image, 
                   offsets = subsetOffsets(offsets = react_dat()$offsets, objects = input$chan_cell_todisplay, image_type = input$chan_type),
                   verbose = FALSE,
                   trunc_bytes = 8,
                   force_trunc = TRUE,
                   display_progress = FALSE,
                   bypass = TRUE)
      
      info = obj_react$obj$info
      info$Images = param_react$param$channels
      
      img = objectExtract(ifd = IFD,  info = info,
                          mode = "rgb", export = "matrix",
                          # base64_id = TRUE, base64_att ='style = "display: block; margin: 0px auto; transform: scale(3);"', write_to = "%o_%c.png",
                          selection = as.numeric(input$chan_sel), force_width = FALSE, removal = "none", 
                          full_range = "full_range" %in% input$chan_force,
                          force_range = "force_range" %in% input$chan_force,
                          display_progress = FALSE,
                          bypass = FALSE)[[1]][[1]]
      dat = attr(img, "raw")
      
      x = c(dat)
      R = range(dat)
      br = seq(R[1], R[2], length.out = 512)
      # x = x[which(x>lims[1] & x<lims[2])]
      h = hist(x, breaks=br, plot = FALSE)
      h$counts = h$counts / max(h$counts)
      h$density = h$density / max(h$density)
      
      html(id = "controls_cell", add = FALSE,
           sprintf('<img id="chan_cell_image" style="display: block; margin: 0px auto; transform: scale(3);" width="%i" height="%i" src="data:image/png;base64,%s">',
                   dim(img)[2],
                   dim(img)[1],
                   cpp_base64_encode(png::writePNG(image = img, target = raw()))))
      
      plot(h, xlim = c(param_react$param$channels$scalemin[k],param_react$param$channels$scalemax[k]), main=NULL, xlab=NULL, ylab=NULL, col="grey", border="black")
      # axis(side = 2, at= c(0, 100/255,200/255, 1), labels=c(0, "100", "200", "255"))
      x = seq( param_react$param$channels$xmin[k], param_react$param$channels$xmax[k])
      points(x = param_react$param$channels$xmid[k], y = param_react$param$channels$ymid[k] / 255, col = "yellow", pch = 10, cex = 2)
      points(y = ((x - param_react$param$channels$xmin[k]) / (param_react$param$channels$xmax[k] - param_react$param$channels$xmin[k])) ^ param_react$param$channels$gamma[k], x = x, col = "green", type = "l", lty = 3, lwd = 2)
      box()
      # polygon(x=c(input$chan_range[1],input$chan_range[1],input$chan_range[2],input$chan_range[2]), y=c(0,1,1,0), col = "#00FF0010")
    })
    output$network <- renderVisNetwork({
      if(input$navbar!="tab2") return(NULL)
      if(length(obj_react$obj$pops) == 0) return(NULL)
      foo = popsNetwork(obj_react$obj, session = session)
      runjs(sprintf("$('#network').css({'width':'%spx', 'height':'%spx'})", max(300, foo$width), max(300, foo$height)))
      session$sendCustomMessage("reach", "network")
      # disable dragView on mobile device to still be able to scroll screen on touch
      args = list(click = "function(e) { if(e.event.srcEvent.altKey) Shiny.onInputChange('pop_alt_click', e.nodes[0]); }")
      if(!input$pop_manager_visible) args = c(args, list(doubleClick = "function(e) { Shiny.onInputChange('pop_edit', e.nodes[0]); }"))
      do.call(what = visEvents, args = c(list(graph = foo %>% visInteraction(dragView = !input$run_on_mobile)), args))
    })
    output$plot_1or2D <- renderPlot({
      if(length(react_dat()$info$in_use) == 0) return(NULL)
      if(length(input$plot_base) < 1) return(NULL)
      if(any(input$plot_base=="")) return(NULL)
      if(!(input$plot_type %in%c("1D","2D"))) return(NULL)
      if(input$navbar!="tab3") return(NULL)
      if(input$plot_x_feature != plot_react$x_feat) return(NULL)
      if(input$plot_y_feature != plot_react$y_feat) return(NULL)
      if(input$plot_x_transform != plot_react$x_trans) return(NULL)
      if(input$plot_y_transform != plot_react$y_trans) return(NULL)
      if(input$plot_type_2D_main_option03 == 0) return(NULL)
      tryCatch({
        if(input$plot_unlock) {
          if(input$plot_type == "1D") {
            hideElement("plot_shown")
            showElement("order_placeholder")
            if(!all(input$plot_shown %in% input$plot_base) || !all(input$plot_base %in% input$plot_shown)) {
              updateSelectInput(session=session, inputId="plot_shown", selected = (input$plot_base))
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
          
          hideElement("plot_regions")
          allowed_regions = lapply(obj_react$obj$pops, FUN = function(p) {
            if(p$type != "G") return(NULL)
            r = obj_react$obj$regions[[p$region]]
            if(input$plot_type == "1D") {
              if(r$type != "line") return(NULL)
              if((p$fx == input$plot_x_feature) &&
                 (r$xlogrange == plot_react$x_trans)) return(r$label)
            } else {
              if(length(p$fy) == 0) return(NULL)
              if((p$fx == input$plot_x_feature) &&
                 (r$xlogrange == plot_react$x_trans) &&
                 (p$fy == input$plot_y_feature) &&
                 (r$ylogrange == plot_react$y_trans)) return(r$label)
            }
          })
          allowed_regions = sort(unname(unique(unlist(allowed_regions)), force = TRUE))
          if(!identical(allowed_regions, plot_react$allowed_regions)) {
            plot_react$allowed_regions <- allowed_regions
            updateSelectInput(session=session, inputId="plot_regions", choices = plot_react$allowed_regions, selected = NULL)
            return(NULL)
          }
          
          hideElement("plot_siblings")
          if(length(allowed_regions) > 0) {
            showElement("plot_siblings")
            showElement("plot_regions")
          }
          
          isolate({
            if((length((input$shape_selected)) != 0) && (length(input$shape_selected$x) != 0)) {
              mess_global(title = "editing region", msg = paste0("'", input$shape_selected$name, "': region edition has been aborted"), type = "warning", duration = 10)
              runjs(code = "Shiny.onInputChange('shape_selected', null)")
            }
            if(length((input$shape_param)) != 0){
              mess_global(title = "drawing region", msg = paste0("'", input$shape_param$tool, "': region drawing of has been aborted"), type = "warning", duration = 10)
              runjs(code = "Shiny.onInputChange('shape_param', null)")
            }
          })
          
          output$graph_saved_msg <- renderText({NULL})
          click("plot_sel_init")
          enable(selector = ".plot_sel_tools")
          switch(input$plot_type,
                 "1D" = {
                   enable("plot_sel_line")
                   disable("plot_sel_rectangle")
                   disable("plot_sel_polygon")
                   disable("plot_sel_lasso")
                   disable("plot_sel_ellipse")
                 },
                 "2D" = {
                   disable("plot_sel_line")
                   enable("plot_sel_rectangle")
                   enable("plot_sel_polygon")
                   enable("plot_sel_lasso")
                   enable("plot_sel_ellipse")
                 })
          R = obj_react$obj$regions[input$plot_regions[input$plot_regions %in% allowed_regions]]
          if(input$plot_type == "1D") {
            args = list(type = "histogram",
                        f1 = input$plot_x_feature,
                        f2 = "Object Number",
                        scaletype = 0,
                        xlogrange = plot_react$x_trans,
                        ylogrange = "P",
                        histogramsmoothingfactor = input$plot_type_1D_option02 == "smooth",
                        freq = ifelse((length(input$plot_type_1D_option03) != 0) && input$plot_type_1D_option03, "T", "F"),
                        BasePop = lapply(plot_react$order, FUN = function(p) list(name = p, inestyle = "Solid", fill = "true")),
                        ShownPop = list(list()))
          } else {
            args = list(type = ifelse(input$plot_type_2D_option01 == "level","density",input$plot_type_2D_option01),
                        f1 = input$plot_x_feature,
                        f2 = input$plot_y_feature,
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
          g = do.call(what = buildGraph, args = args)
          g$GraphRegion = list()
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
                            region = tocreate[i_row, "region"], fx = input$plot_x_feature)
                if(input$plot_type == "2D") args = c(args, list(fy = input$plot_y_feature))
                do.call(what = buildPopulation, args = args)
              })
              tryCatch({
                obj_react$obj = data_add_pops(obj = obj_react$obj, pops = pop, display_progress = TRUE, session=session)
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
          g$xmin = plot_react$xmin
          g$xmax = plot_react$xmax
          g$ymin = plot_react$ymin
          g$ymax = plot_react$ymax
          g$maxpoints <- as.numeric(input$plot_type_2D_main_option03)/100
          if(input$plot_type_2D_option01 == "level") g$BasePop[[1]]$densitylevel = paste(ifelse(input$plot_level_fill,"true","false"),
                                                                                         ifelse(input$plot_level_lines,"true","false"),
                                                                                         input$plot_level_nlevels,input$plot_level_lowest,sep="|")
          
          if(#ifelse(g$type == "histogram", "1D" %in% input$plot_type, g$type %in% input$plot_type_2D_option01) &&
            (g$order %in% plot_react$g$order) &&
            (g$xstatsorder %in% plot_react$g$xstatsorder) &&
            #  all(sapply(g$BasePop, FUN = function(x) x$name) %in% sapply(plot_react$g$BasePop, FUN = function(x) x$name))) &&
            # ifelse(length(g$GraphRegion) == 0, TRUE, (all(sapply(g$GraphRegion, FUN = function(x) x$name) %in% sapply(plot_react$g$GraphRegion, FUN = function(x) x$name)))) &&
            # ifelse(length(g$ShownPop) == 0, TRUE, (all(sapply(g$ShownPop, FUN = function(x) x$name) %in% sapply(plot_react$g$ShownPop, FUN = function(x) x$name)))) &&
            (g$f1 %in% plot_react$g$f1) &&
            ifelse(g$type == "histogram", TRUE, g$f2 %in% plot_react$g$f2)) {
            add_log(paste0("plot_",input$plot_type))
            for(i in c("graphtitlefontsize", "axislabelsfontsize", "axistickmarklabelsfontsize", "regionlabelsfontsize", "title", "xlabel", "ylabel")) {
              # if((length(plot_react$g[[i]]) != 0) && (plot_react$g[[i]] != "") && !(g[[i]] %in% plot_react$g[[i]]))
              g[[i]] <- plot_react$g[[i]]
            }
            if(plot_react$g$type == "density") {
              if(!plot_react$param_ready) {
                if(length(plot_react$g$BasePop[[1]]$densitycolorslightmode) == 0) {
                  plot_react$g$BasePop[[1]]$densitycolorslightmode <- "-16776961|-13447886|-256|-23296|-65536|"
                } 
                plot_react$densitycolorslightmode <- plot_react$g$BasePop[[1]]$densitycolorslightmode
                plot_react$densitytrans <- plot_react$g$BasePop[[1]]$densitytrans
                plot_react$densitycolorslightmode_selected = "initial"
                plot_react$densitytrans_selected = "initial"
                if(input$plot_dens_order %% 2) click("plot_dens_order")
              }
              g$BasePop[[1]]$densitycolorslightmode <- plot_react$g$BasePop[[1]]$densitycolorslightmode
              g$BasePop[[1]]$densitytrans <- plot_react$g$BasePop[[1]]$densitytrans
            }
            plot_react$param_ready = TRUE
            plot_react$g = g
          } else {
            if(g$type == "density") {
              g$BasePop[[1]]$densitycolorslightmode <- plot_react$g$BasePop[[1]]$densitycolorslightmode
              g$BasePop[[1]]$densitytrans <- plot_react$g$BasePop[[1]]$densitytrans
            }
            plot_react$param_ready = FALSE
            plot_react$g = g
            return(NULL)
          }
          plot_react$plot = plotGraph(obj = obj_react$obj, graph = g, draw = FALSE, stats_print = FALSE, viewport = ifelse(plot_react$zoomed, "ideas", "max"), precision = "full")
        } else {
          plot_react$plot = plotGraph(obj = obj_react$obj, graph = plot_react$g, draw = FALSE, stats_print = FALSE, viewport = "ideas", precision = "full")
        }
        if(nrow(plot_react$plot$input$data) == 0) {
          disable("graph_save_btn")
          disable(selector = ".plot_sel_tools")
          mess_global(title = paste0("plot_", input$plot_type), msg = "No object to plot", type = "info", duration = 10)
          return(NULL)
        } else {
          enable("graph_save_btn")
        }
        plot_react$current = "#plot_1or2D"
        session$sendCustomMessage("getOffset", sprintf("%s",plot_react$current))
        output$plot_stats <- suppressWarnings(renderPrint(plot_react$plot$stats[,!grepl("Qu", colnames(plot_react$plot$stats))]))
        convert_to_baseplot(plot_react$plot)
      }, error = function(e) {
        mess_global(title = paste0("plot_", input$plot_type), msg = e$message, type = "error", duration = 10)
      })
    })
    output$plot_3D <- renderRglwidget({
      devs = rgl.dev.list()
      if(!any(names(devs) == "null")) open3d(useNULL = TRUE)
      if(length(input$plot_base) < 1) return({ my_clear3d();rglwidget() })
      if(any(input$plot_base=="")) return({ my_clear3d();rglwidget() })
      if(input$navbar!="tab3") return({ my_clear3d();rglwidget() })
      if(input$plot_type != "3D") return({ my_clear3d();rglwidget() })
      over = plot_react$order
      if(length(over) == 0) return({ my_clear3d();rglwidget() })
      if(length(plot_react$shown) == 0) return({ my_clear3d();rglwidget() })
      if(!all(c(over %in% input$plot_base, over %in% input$plot_shown, input$plot_base %in% over, input$plot_shown %in% over))) return({ my_clear3d();rglwidget() })
      
      updateSelectInput(session = session, inputId = "plot_3D_mouse_ctrl", selected = "trackball")
      disable(selector = ".plot_sel_tools")
      hideElement(id = "plot_shown")
      hideElement(id = "plot_siblings")
      sub = plot_react$subset
      shinyResetBrush(session, "plot_3D_brush")
      
      if(sum(sub) == 0) {
        mess_global(title = "plot_3D", msg = "No object to plot", type = "info", duration = 10)
        my_clear3d()
        return(rglwidget(shared = plot_react$plot$shared,
                         reuse = NA,
                         snapshot = FALSE,
                         shinyBrush = "plot_3D_brush",
                         webGLoptions = list(preserveDrawingBuffer = TRUE, alpha = FALSE, antialias = TRUE)))
      }
      clust = isolate(sapply(obj_react$obj$pops[over], FUN = function(p) {
        p$obj & sub
      }))
      clust = isolate(apply(clust, 1, FUN = function(x) {
        over[which(x)[1]]
      }))
      clust = isolate(factor(clust, levels = over))
      
      dat = isolate(obj_react$obj$features[sub, ])
      dat$x_feat = dat[, input$plot_x_feature]
      dat$y_feat = dat[, input$plot_y_feature]
      dat$z_feat = dat[, input$plot_z_feature]
      
      trans_x = parseTrans(plot_react$x_trans)
      trans_y = parseTrans(plot_react$y_trans)
      trans_z = parseTrans(plot_react$z_trans)
      dat$tx_feat = applyTrans(dat[, "x_feat"], trans_x)
      dat$ty_feat = applyTrans(dat[, "y_feat"], trans_y)
      dat$tz_feat = applyTrans(dat[, "z_feat"], trans_z)
      
      dat = dat[, c("tx_feat","ty_feat","tz_feat","x_feat","y_feat","z_feat","Object Number")]
      
      my_clear3d()
      # maybe this part can be removed and IFC shiny can depend on rgl >= 0.104.16
      if((packageVersion("rgl") < 0.104) && (nrow(dat) >= 160)) {
        shared = rbind(dat[1:159, c("tx_feat","ty_feat","tz_feat","Object Number")],
                       dat[     , c("tx_feat","ty_feat","tz_feat","Object Number")])
        key = as.character(shared[, "Object Number"])
        key[1:159] = paste0("_", key[1:159])
      } else {
        shared = dat[, c("tx_feat","ty_feat","tz_feat","Object Number")]
        key = as.character(shared[, "Object Number"])
      }
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Creating 3D graph", reset = FALSE)
      tryCatch({
        obj3D = plot_obj3D(obj3D = create_obj3D(data = dat, dims = 1:3, clust = clust[sub],
                                                symbol = plot_react$symbol, color = plot_react$color, level = 0.68,
                                                pt_size = input$plot_type_3D_option02),
                           draw_ell = TRUE, draw_pts = TRUE, draw_txt = TRUE,
                           useNULL = TRUE, session = session)
        plot_react$plot = list(plot = obj3D,
                               shared = rglShared(id = text3d(shared[, 1:3], texts = shared[, "Object Number"], adj = -0.5),
                                                  key = key,
                                                  group = "SharedData_plot_3D_ids", selectedColor = "black",
                                                  deselectedFade = 0,
                                                  selectedIgnoreNone = FALSE),
                               input = list(data = dat,
                                            clust = clust,
                                            dims = c(input$plot_x_feature, input$plot_y_feature, input$plot_z_feature),
                                            trans = c(plot_react$x_trans, plot_react$y_trans, plot_react$z_trans),
                                            sub = sub, color = plot_react$color, symbol = plot_react$symbol))
        plot_react$plot$shared$clearSelection("SharedData_plot_3D_ids")
        pre <- rglwidget(shared = plot_react$plot$shared,
                         reuse = NA,
                         snapshot = FALSE,
                         shinyBrush = "plot_3D_brush",
                         webGLoptions = list(preserveDrawingBuffer = TRUE, alpha = FALSE, antialias = TRUE))
        
        onFlushed(once = TRUE, fun = function() {
          updateToggle3D(session = session, inputId = "plot_3D_draw_axes", widgetId = "plot_3D", objIds = obj3D$axes)
          updateToggle3D(session = session, inputId = "plot_3D_draw_points", widgetId = "plot_3D", objIds = obj3D$pts)
          updateToggle3D(session = session, inputId = "plot_3D_draw_ell", widgetId = "plot_3D", objIds = obj3D$ell)
          updateToggle3D(session = session, inputId = "plot_3D_draw_txt", widgetId = "plot_3D", objIds = obj3D$txt)
          observeEvent(once = TRUE, input$run_on_mobile, {
            par_defaults <- r3dDefaults$mouseMode
            nam_defaults <- c("left", "right", "middle", "wheel")
            if(length(par_defaults) == 5) nam_defaults <- c("none", nam_defaults)
            names(par_defaults) <- nam_defaults
            if(input$run_on_mobile) { par_defaults["left"] <- "none"; par_defaults["right"] <- "trackball" }
            if(packageVersion("rgl") >= "0.106.19") par_defaults["none"] <- "user"
            shinySetPar3d(mouseMode = as.list(par_defaults), session = session)
          })
          session$sendCustomMessage("refresh", "plot_3D")
        })
        return(pre)
      },
      error = function(e) {
        return({ my_clear3d();rglwidget() })
      },
      finally = {
        mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
      })
    })
    if(packageVersion("rgl") < "0.106.19") session$sendCustomMessage("hover3D", list(widgetId = 'plot_3D', with_img = FALSE))
    output$stats <- suppressWarnings(renderPrint({
      if(length(react_dat()$info$in_use) == 0) return(NULL)
      if(any(input$population=="")) return(NULL)
      if(input$navbar!="tab4") return(NULL)
      stats = obj_react$obj$stats
      stats$parent = sapply(stats$parent, FUN=function(x) which(x==rownames(stats)))
      if(input$stats_feature=="") return(stats)
      tryCatch({
        stats = extractStats(obj = obj_react$obj, feat_name = input$stats_feature, trans = input$stats_transform)
        stats$parent = sapply(stats$parent, FUN=function(x) which(x==rownames(stats)))
        rownames(stats) = paste0(sprintf(paste0("[%0",nchar(sprintf("%1.f", nrow(stats))),".f]"), 1:nrow(stats)), rownames(stats))
        enable("stats_save_btn")
        print(stats, digits = 2, na.print = "NA")
      }, error = function(e) {
        mess_global("computing stats", msg = e$message, type = "error", duration = 10)
        disable("stats_save_btn")
        print(NULL)
      })
    }))
    output$logs <- renderPrint({
      if(input$navbar != "tab8") return(NULL)
      lines = readLines(file.path(session_react$dir, "LOGS.txt"))
      L = length(lines)
      if(L >= 20) lines = c("...", lines[L - (19:0)])
      cat(lines, sep = "\n")
    })
    ##### outputs end
  },
  finally = {
    onSessionEnded(session = session, function() {
      unlink(sessiondir, recursive = TRUE, force = TRUE)    # empty temp files
      cat(paste0("[",sessionid,"] App stopped\n"))          # goodbye message
      if(exists(".only_once")) {
        if(identical(as.logical(.only_once), TRUE)) {
          stopApp()
          q("no")
        }
      }
    })
  })
}