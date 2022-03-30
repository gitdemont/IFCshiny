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
  
  # we use shiny:outputinvalidated to show/hide msg_busy_ctn on training_plot because it can be long
  # TODO find something to test if webgl is available for interaction once rgl has sent scene to javascript
  # the idea is to hide IFCshiny app on output invalidation and to show it back only when the whole scene is ready
  runjs(code = paste(sep = ";\n;",
                     "$(document).on('shiny:outputinvalidated', function(event) {",
                     "  var id = event.target.id;",
                     "  if (id === undefined) {",
                     "    return null;",
                     "  } else {",
                     "    if(document.getElementById(id).parentNode.classList.contains('obs_plot_invalidate')) {",
                     "      $(document).one('shiny:idle', function(event) {",
                     "        document.getElementById('msg_busy_ctn2').style.display = 'none';",
                     "      })",
                     "    }",
                     "    if(id === 'training_plot') {",
                     "      document.getElementById('msg_busy_txt2').innerText = 'updating training graph';",
                     "      document.getElementById('msg_busy_ctn2').style.display = 'block';",
                     "      $(document).one('shiny:idle', function(event) {",
                     "        document.getElementById('msg_busy_ctn2').style.display = 'none';",
                     "      })",
                     "    }",
                     "    if(id === 'plot_1or2D') {",
                     "      document.getElementById('msg_busy_txt2').innerText = 'updating plot';",
                     "      document.getElementById('msg_busy_ctn2').style.display = 'block';",
                     "      $(document).one('shiny:idle', function(event) {",
                     "        document.getElementById('msg_busy_ctn2').style.display = 'none';",
                     "      })",
                     "    }",
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
    
    ##### Batch
    source(file.path(.rundir, "server_batch.R"), local = TRUE, echo = FALSE, verbose = FALSE)
    
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
      cat(sep = "\n", paste0("input: ", file_react$input$name))
      if(length(obj_react$back$info$in_use) == 0) return(cat(""))
      if(any(input$population=="")) return(cat(""))
      if(input$navbar!="tab0") return(cat(""))
      cat(sep = "\n",
          # paste0("input: ", input$file$name),
          paste0("date: " , obj_react$back$info$date),
          paste0("instrument: " , obj_react$back$info$instrument),
          paste0("sw_raw: " , obj_react$back$info$sw_raw),
          paste0("sw_process: " , obj_react$back$info$sw_process),
          paste0("fcs_version: " , obj_react$back$info$fcs_version),
          "\nMerged_rif: ", obj_react$back$info$Merged_rif,
          "\nMerged_cif: ", obj_react$back$info$Merged_cif,
          paste0("\nobjcount: ", obj_react$back$info$objcount),
          paste0("magnification: ", obj_react$back$info$magnification),
          paste0("high-gain: ", obj_react$back$info$evmode),
          # paste0("in_use: ", paste0(sprintf("%0.02i", which(obj_react$back$info$in_use)), collapse = ",")),
          paste0("brightfield: ", ifelse(obj_react$back$info$found, paste0(sprintf("%0.02i", which(obj_react$back$info$brightfield$channel)), collapse = ","), obj_react$back$info$brightfield)),
          "\nIllumination:")
      print(obj_react$back$info$illumination)
      cat("\nCompensation:\n")
      print(obj_react$back$info$CrossTalkMatrix)
      cat("\nDisplay:\n")
      tmp = grepl("^name$|^color$|^physicalChannel$|^xmin$|^xmax$|^xmid$|^ymid$|^scalemin^|^scalemax$", colnames(obj_react$back$info$Images))
      print(obj_react$back$info$Images[, tmp])
      if(ncol(obj_react$back$description$masks)!=0) {
        cat("\nMasks:\n")
        print(obj_react$back$description$masks[, c("name","def")])
      }
    })
    output$table <- DT::renderDataTable(server = TRUE, expr = {
      if(length(obj_react$back$info$in_use) == 0) return(NULL)
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
                                                     offsets = subsetOffsets(obj_react$back$offsets, objects = val$`Object Number`[chunks[[P]]], input$chan_type),
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
                                      sprintf("doc.header = { text: '%s', alignment: 'center', fontSize: 15};", basename(obj_react$back$fileName)),
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
      if(input$chan_cell_todisplay > obj_react$back$description$ID$objcount-1) return(NULL)
      par(mar=c(2,1,1,1))
      k = which(param_react$param$channels$physicalChannel %in% as.numeric(input$chan_sel))
      if(length(k) == 0) return(NULL)
      IFD = getIFD(fileName = obj_react$back$fileName_image, 
                   offsets = subsetOffsets(offsets = obj_react$back$offsets, objects = input$chan_cell_todisplay, image_type = input$chan_type),
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
           objectExtract(ifd = IFD,  info = info,
                         mode = "rgb", export = "base64", write_to = "%o_%c.png",
                         base64_att="id='chan_cell_image' style='display: block; margin: 0px auto; transform: scale(3);'",
                         selection = as.numeric(input$chan_sel), force_width = FALSE, removal = "none", 
                         full_range = "full_range" %in% input$chan_force,
                         force_range = "force_range" %in% input$chan_force,
                         display_progress = FALSE,
                         bypass = FALSE)[[1]][[1]])
      
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
    
    # dev_check_plot <- 0
    output$plot_1or2D <- renderCachedPlot(cache = "session",
                                          cacheKeyExpr = list(#input$plot_type,
                                                              #plot_react$x_feat,
                                                              #plot_react$x_trans,
                                                              #plot_react$y_feat,
                                                              #plot_react$y_trans,
                                                              #plot_react$order,
                                                              #plot_react$g$maxpoints,
                                                              file_react$id,
                                                              plot_react$id,
                                                              plot_react$plot$input$subset,
                                                              #plot_react$id,
                                                              sapply(input$plot_regions, FUN = function(r) unlist(obj_react$obj$regions[[r]]))
                                                              #unlist(plot_react$g[c("title","xlabel","ylabel","axislabelsfontsize","axistickmarklabelsfontsize","graphtitlefontsize","regionlabelsfontsize")]),
                                                              #unlist(plot_react$g$BasePop[[1]]),
                                                              #input$plot_type_2D_option01,
                                                              #input$plot_type_1D_option01,
                                                              #input$plot_type_1D_option02,
                                                              #input$plot_type_1D_option03,
                                                              #input$plot_regions,
                                                              #input$plot_unlock
                                                              #plot_react$allowed_regions,
                                                              #plot_react$zoomed
                                                              #plot_react$xmin,
                                                              #plot_react$xmax,
                                                              #plot_react$ymin,
                                                              #plot_react$ymax,
                                                              #plot_react$param_ready
                                                              ), bg = NA,expr =  {
      if(input$navbar!="tab3") return(NULL)
      if(length(obj_react$back$info$in_use) == 0) return(NULL)
      if(!(input$plot_type %in%c("1D","2D"))) return(NULL)
      if(!any(plot_react$param_ready)) return(NULL)
      plot_react$current = "#plot_1or2D"
      removeUI(paste0(plot_react$current,">.ifcshiny_image_bg"), session = session, immediate = TRUE)
      runjs("document.getElementById('msg_busy_txt2').innerText = 'updating plot';")
      runjs("document.getElementById('msg_busy_ctn2').style.display = 'block';")
      click("plot_sel_init")
      enable(selector = ".plot_sel_tools")
      hideElement("plot_regions")
      hideElement("plot_siblings")
      if(length(plot_react$allowed_regions) > 0) {
        showElement("plot_siblings")
        showElement("plot_regions")
      }
      tryCatch({
        add_log(isolate(paste0("plot_",ifelse(plot_react$g$type=="histogram","histogram",input$plot_type_2D_option01))))
        # print(paste0("output: ",dev_check_plot))
        # dev_check_plot <<- dev_check_plot + 1
        g = plot_react$g
        output$graph_saved_msg <- renderText({NULL})
          if(input$plot_unlock) {
            plot_react$plot = plotGraph(obj = obj_react$obj, graph = g, viewport = ifelse(plot_react$zoomed, "ideas", "max"), precision = "full", draw = FALSE, stats_print = FALSE)
          } else {
            plot_react$plot = plotGraph(obj = obj_react$obj, graph = g, viewport = "ideas", precision = "full", draw = FALSE, stats_print = FALSE)
          }
          
          if(nrow(plot_react$plot$input$data) == 0) {
            disable("graph_save_btn")
            disable(selector = ".plot_sel_tools")
            mess_global(title = paste0("plot_", input$plot_type), msg = "No object to plot", type = "info", duration = 10)
            return(NULL)
          } else {
            enable("graph_save_btn")
          }
          # draw plot
          plot_raster(plot_react$plot)
      }, error = function(e) {
        mess_global(title = paste0("plot_", input$plot_type), msg = e$message, type = "error", duration = 10)
      }, warning = function(w) {
        mess_global(title = paste0("plot_", input$plot_type), msg = w$message, type = "warning", duration = 10)
      },
      finally = runjs("document.getElementById('msg_busy_ctn2').style.display = 'none';"))
    })
    # output stats
    output$plot_stats <- try(suppressWarnings(renderPrint({
      if(!inherits(plot_react$plot, "IFC_plot")) return(NULL)
      stats = plot_stats(plot_react$plot)
      stats[,!grepl("Qu", colnames(stats))]
    })), silent = TRUE)
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
      if(length(obj_react$back$info$in_use) == 0) return(NULL)
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