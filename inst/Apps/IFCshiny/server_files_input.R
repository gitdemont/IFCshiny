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

observeEvent(input$use_example, {
  if(requireNamespace("IFCdata", quietly = TRUE) && input$use_example) {
    disable(selector = ".btn-file")
    enable("example_file")
    showElement("example_save")
    runjs(code = "Shiny.onInputChange('file', null)")
  } else {
    if(input$use_example) {
      updateSwitchInput(session=session, inputId="use_example", value=FALSE)
      mess_global(title = "package required", 
                  msg = c("'IFCdata' package is not installed",
                          "To install 'IFCdata' package, run `install.packages('IFCdata', repos = 'https://gitdemont.github.io/IFCdata/', type = 'source')`"), type = "info")
    }
    disable("example_file")
    enable(selector = ".btn-file")
    hideElement("example_save")
    if(length(file_react$input) != 0) {
      file.rename(to = unlist(file_react$input$datapath), from = file.path(dirname(unlist(file_react$input$datapath)), unlist(file_react$input$name)))
      runjs(code = sprintf("Shiny.onInputChange('file', %s)", toJSON(file_react$input)))
    } else {
      runjs(code = sprintf("Shiny.onInputChange('file', null)"))
    }
  }
})
observeEvent(input$file, {
  # keep / remove former file input
  if((length(input$file) != 0) && !input$use_example) {
    if(length(file_react$input) != 0) {
      to_rm = file.path(dirname(unlist(file_react$input$datapath)), unlist(file_react$input$name))
      to_rm = to_rm[!(system.file(package = "IFCdata", "extdata") %in% file.path(dirname(unlist(file_react$input$datapath))))]
      file.remove(to_rm)
    }
  }
  file_react$input <- input$file
  if(length(react_dat())==0) return(NULL)
  
  add_log("file input")
  obj_react$obj <- react_dat()
  plot_react$init = TRUE
  lapply(obs_plot, FUN = function(x) x$resume())
  
  # depending on input we allow some tabs
  if(obj_react$obj$description$ID$objcount == nrow(obj_react$obj$features)) {
    showElement(selector = "#navbar [data-value='tab2']")
    showElement(selector = "#navbar [data-value='tab4']")
    showElement(selector = "#navbar [data-value='tab5']")
  } else {
    hideElement(selector = "#navbar [data-value='tab2']")
    hideElement(selector = "#navbar [data-value='tab3']")
    hideElement(selector = "#navbar [data-value='tab4']")
    hideElement(selector = "#navbar [data-value='tab5']")
  }
  if(ncol(obj_react$obj$features) == 1 && (names(obj_react$obj$features) == "Object Number")) {
    hideElement(selector = "#navbar [data-value='tab3']")
    hideElement(selector = "#navbar [data-value='tab4']")
    hideElement(selector = "#navbar [data-value='tab5']")
  } else {
    showElement(selector = "#navbar [data-value='tab3']")
    showElement(selector = "#navbar [data-value='tab4']")
    showElement(selector = "#navbar [data-value='tab5']")
  }
  showElement("infos_save")
  
  # if a file containing images was read we allow some other tabs
  # + we update app input with collected channels
  reinit_plot_3D(session = session)
  if(obj_react$obj$info$found) {
    showElement(id = "compute_features")
    if(!requireNamespace(package = "IFCip", quietly = TRUE) || (react_dat()$info$XIF_test != 1)) {
      if(!requireNamespace(package = "IFCip", quietly = TRUE)) mess_global(title = "package required", msg = c("'IFCip' package is required to compute extra features from images", "Features computation module has been disabled"), type = "info")
      if((react_dat()$info$XIF_test != 1))  mess_global(title = "rif file", msg = c("can't compute extra features from images on this type of rif file", "Features computation module has been disabled"), type = "info")
      hideElement(id = "compute_features")
      hideElement(selector = "#navbar [data-value='tab9']")
    } else {
      showElement(id = "compute_features")
      if(all(file.exists(file.path(.rundir, c("server_compensation.R", "server_navbar_compensation.R"))))) showElement(selector = "#navbar [data-value='tab9']")
    }
    param_react$back = param_react$param
    showElement(selector = "#navbar [data-value='tab1']")
    showElement(selector = "#navbar [data-value='tab8']")
    showElement(id = "plot_3D_img_ctrl")
    foo = sprintf("%02i",param_react$param$channels$physicalChannel)
    sel = which(obj_react$obj$info$brightfield$channel)
    if(length(sel) == 0) {
      sel = which(obj_react$obj$info$in_use)[1]
    } else {
      sel = sel[1]
    }
    updateSelectInput(session=session, inputId = "plot_3D_draw_chan", choices = foo, selected = sprintf("%02i",sel))
    
    updateCheckboxInput(session = session, inputId = "channels", value = foo)
    updateRadioButtons(session = session, inputId = "chan_type", choices = c("img", "msk")[1:((param_react$param$XIF_test == 1) + 1L)])
    updateSelectInput(session=session, inputId = "chan_sel", choices = foo, selected = sprintf("%02i",sel))
    updateSliderInput(session = session, inputId = "chan_range",
                      value = c(param_react$param$channels$xmin[sel],
                                param_react$param$channels$xmax[sel]))
    updateSliderInput(session = session, inputId = "chan_view",
                      value = c(param_react$param$channels$scalemin[sel],
                                param_react$param$channels$scalemax[sel]))
    updateSliderInput(session = session, inputId = "chan_gamma_x",
                      value = param_react$param$channels$xmid[sel],
                      min = param_react$param$channels$xmin[sel],
                      max = param_react$param$channels$xmax[sel])
    updateSliderInput(session = session, inputId = "chan_gamma_y",
                      value = param_react$param$channels$ymid[sel])
    updateTextInput(session = session, inputId = "chan_name", value = param_react$param$channels$name[sel])
    colourpicker::updateColourInput(session = session, inputId = "chan_color", allowedCols = tolower(paletteIFC("palette_R")), value = tolower(param_react$param$channels$color[sel]))
    for(i in param_react$param$channels$physicalChannel) enable(selector = paste0("#channels input[type='checkbox'][value='",sprintf("%02i",i),"']"))
  } else {
    hideElement(id = "plot_3D_img_ctrl")
    hideElement(selector = "#navbar [data-value='tab1']")
  }
  
  # check whether the input file was a FCS / LMD or other
  # if so change ML to use Arcsinh
  # + to use all that is not SS of FS as features names for transformation
  # + to change file extension when save
  if(length(obj_react$obj$description$FCS) == 0) {
    runjs(code = "document.getElementById('features_inp').parentNode.parentNode.setAttribute('title', 'regular expression to select intensity to lin/log transform. The default is to select all that start with Intnsity, Bright Detail Intensity or Uncompensated')")
    updateTextInput(session=session, inputId = "features_inp", value = "^Intensity|^Bright Detail Intensity|^Uncompensated")
    updateRadioButtons(session=session, inputId = "features_inm", selected = "LinLog", inline = TRUE)
    updateRadioButtons(session=session, inputId = "daf_save_type", choiceNames = c("daf", "fcs"), choiceValues = c("daf", "fcs") , selected = "daf", inline = TRUE)
    updateRadioButtons(session=session, inputId = "ML_save_type", choiceNames = c("daf only", "daf + R"), choiceValues = c("daf", "zip") , selected = "zip", inline = TRUE)
    showElement(id = "features_pre_daf")
  } else {
    runjs(code = "document.getElementById('features_inp').parentNode.parentNode.setAttribute('title', 'regular expression to select intensity to lin/log transform. The default is to select all except FS and SS and already LOG transformed parameters')")
    updateTextInput(session=session, inputId = "features_inp", value = "^(?!.*FS|.*SS|.*LOG).*$")
    updateRadioButtons(session=session, inputId = "features_inm", selected = "Arcsinh", inline = TRUE)
    updateRadioButtons(session=session, inputId = "daf_save_type", choiceNames = c("fcs", "fcs + xml"), choiceValues = c("fcs", "zip") , selected = "zip", inline = TRUE)
    updateRadioButtons(session=session, inputId = "ML_save_type", choiceNames = c("fcs only", "fcs + xml + R"), choiceValues = c("fcs", "zip") , selected = "zip", inline = TRUE)
    hideElement(id = "features_pre_daf")
    if(all(file.exists(file.path(.rundir, c("server_compensation.R", "server_navbar_compensation.R"))))) showElement(selector = "#navbar [data-value='tab9']")
  }
  # compensation is in dev and not available on shinyapps.io
  # if(Sys.getenv('SHINY_PORT') != "") hideElement(selector = "#navbar [data-value='tab9']")
  
  # we reinit style of plot
  plot_react$color = (sapply(obj_react$obj$pops["All"], FUN = function(p) {
    p$lightModeColor
  }))
  plot_react$symbol = (sapply(obj_react$obj$pops["All"], FUN = function(p) {
    p$style
  }))
  
  # we reinit report layout
  session$sendCustomMessage("init_grid", "")
  plot_react$layout = matrix(ncol=1, nrow=0)
  obj_react$obj = reinit_layout(obj_react$obj)
  
  # we update sliders
  updateSelectInput(session, "sel_left", choices = sort(names(obj_react$obj$features)), selected = names(obj_react$obj$features))
  update_pops(session = session, obj = obj_react$obj, init = TRUE)
  update_regions(session = session, obj = obj_react$obj, init = TRUE)
  update_features(session = session, obj = obj_react$obj, init = TRUE)
  obj_react$obj$haschanged_objects = character()
})

# observer for tagged population file input
observeEvent(input$file_tagged, {
  add_log("pops input: tagged")
  if(!check_managers(alw = NULL)) return(NULL)
  fileinfo <- input$file_tagged
  if(length(fileinfo$datapath)==0) return(NULL)
  ext <- getFileExt(fileinfo$datapath)
  obj_back = obj_react$obj
  tryCatch({
    r_in = switch(ext,
                  "csv" = read.csv(fileinfo$datapath, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = ",", dec = "."),
                  "txt" = read.table(fileinfo$datapath, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = "\t", dec = "."),
                  "xlsx" = read.xlsx(fileinfo$datapath, startRow = 1, check.names = FALSE),
                  { stop("input populations is not of correct extension: please input .txt, .csv or .xlsx") })
    r_in = r_in[, !grepl("^All$", colnames(r_in))]
    oo = rep(FALSE, as.integer(obj_react$obj$description$ID$objcount))
    pops = lapply(1:ncol(r_in), FUN = function(i_col) {
      obj = oo
      obj[na.omit(as.integer(r_in[,i_col])) + 1L] <- TRUE # add +1 example for object 0
      list(name = names(r_in)[i_col], type = "T", obj = obj)
    })
    already_pop = names(r_in)[names(r_in) %in% names(obj_react$obj$pops)]
    obj_react$obj = data_add_pops(obj_react$obj, pops, display_progress = TRUE, session = session)
    runjs("Shiny.onInputChange('pop_edit', null)")
    runjs("Shiny.onInputChange('pop_alt_click', null)")
    plot_react$shown = NULL
    plot_react$order = NULL
    if(length(already_pop) !=0) mess_global(title = "population import", msg = c("some population(s) were already present in 'obj' and were not imported:", paste0("  - ", already_pop)), type = "warning")
  }, error = function(e) {
    obj_react$obj = obj_back
    mess_global(title = "population import", msg = e$message, type = "stop")
  })
})

# observer for compensation file input
observeEvent(input$file_comp, {
  add_log("compenation input")
  if(!check_managers(alw = NULL)) return(NULL)
  fileinfo <- input$file_comp
  if(length(fileinfo$datapath)==0) return(NULL)
  ext <- getFileExt(fileinfo$datapath)
  obj_back = obj_react$obj
  tryCatch({
    Mat = switch(ext,
                 "rif" = {
                   info = getInfo(fileinfo$datapath, from = "acquisition", warn = FALSE)
                   info$CrossTalkMatrix[which(info$in_use), which(info$in_use)]
                 },
                 "cif" = {
                   info = getInfo(fileinfo$datapath, from = "analysis", warn = FALSE)
                   info$CrossTalkMatrix[which(info$in_use), which(info$in_use)]
                 },
                 "ctm" = {
                   tmp = xml2::read_xml(fileinfo$datapath)
                   val = as.numeric(strsplit(xml_attr(xml_find_first(tmp, "//MatrixValues"), "values"), split = "|", fixed = T)[[1]])
                   matrix(val, ncol = sqrt(length(val)), byrow = TRUE)
                 },
                 "csv" = read.csv(fileinfo$datapath, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = ",", dec = "."),
                 "txt" = read.table(fileinfo$datapath, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = "\t", dec = "."),
                 "xlsx" = read.xlsx(fileinfo$datapath, startRow = 1, check.names = FALSE),
                 { stop("input compensation is not of correct extension: please input .ctm, .txt, .csv or .xlsx") })
    if(ncol(Mat) != sum(react_dat()$info$in_use)) stop("compensation can not be applied: number of acquired channels and number of channels in compensation file are different")
    
    colnames(Mat) = sprintf("Ch%02i",react_dat()$info$Images$physicalChannel) # react_dat()$info$Images$name
    comp_react$spillover = Mat
    comp_react$pre = Mat
    
  }, error = function(e) {
    obj_react$obj = obj_back
    mess_global(title = "population import", msg = e$message, type = "stop")
  })
})

# observer for gating strategy file input
observeEvent(input$file_network, {
  add_log("pops input: network")
  if(!check_managers(alw = NULL)) return(NULL)
  fileinfo <- input$file_network
  if(length(fileinfo$datapath)==0) return(NULL)
  ext <- getFileExt(fileinfo$datapath)
  if(!ext %in% c("ist", "ast", "xml", "rif", "cif", "daf")) {
    mess_global(title = "population import", msg = "input populations is not of correct extension: please input .ist, .ast, .xml, .rif, .cif, .daf", type = "stop")
    return(NULL)
  }
  network_ok$resume()
  network_abort$resume()
  showModal(modalDialog(tags$b("Importing network will overwrite all regions, pops, graphs and stats already present !"),
                        size = "s",
                        easyClose = FALSE,
                        footer = list(actionButton(inputId = "import_ok",label = "Proceed"),
                                      actionButton(inputId = "import_abort", label = "Abort"))))
})
network_ok = observeEvent(input$import_ok, suspended = TRUE, {
  network_ok$suspend()
  network_abort$suspend()
  removeModal()
  fileinfo <- input$file_network
  if(length(fileinfo$datapath)==0) return(NULL)
  ext <- getFileExt(fileinfo$datapath)
  obj_back = obj_react$obj
  tryCatch({
    gs = readGatingStrategy(fileName = fileinfo$datapath)
    obj_react$obj = applyGatingStrategy(obj = obj_react$obj, gating = gs, display_progress = TRUE, session = session)
  }, error = function(e) {
    obj_react$obj = obj_back
    mess_global(title = "population import", msg = unlist(strsplit(e$message, split = "\n")), type = "stop")
  }, finally = {
    obj_react$obj = compare(obj_react$obj, obj_back, session = session)
    runjs("Shiny.onInputChange('pop_edit', null)")
    runjs("Shiny.onInputChange('pop_alt_click', null)")
    plot_react$shown = NULL
    plot_react$order = NULL
    session$sendCustomMessage("init_grid", "")
    plot_react$layout = matrix(ncol=1, nrow=0)
    obj_react$obj = reinit_layout(obj_react$obj)
  })
})
network_abort = observeEvent(input$import_abort, suspended = TRUE, {
  network_ok$suspend()
  network_abort$suspend()
  removeModal()
})