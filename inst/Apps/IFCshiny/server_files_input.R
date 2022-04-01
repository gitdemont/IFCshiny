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

newfileinput <- function(files, session = getDefaultReactiveDomain()) {
  # reinit values
  reinit_default(TRUE)
  reinit_app(list())
  unlink(file.path(session_react$dir, "batch_raw"), recursive = TRUE, force = TRUE)
  file_react$input <- files
  file_react$id = random_name(n = 20)
  if(length(files) == 0) return(NULL)
  
  fileinfo <- lapply(files, unlist)
  fileName <- file.path(dirname(unlist(fileinfo$datapath)), unlist(fileinfo$name))
  file.rename(from = unlist(fileinfo$datapath), to = fileName)
  
  # process input
  # check extension
  ext <- getFileExt(fileName)
  daf_file = ext %in% "daf"
  cif_file = ext %in% "cif"
  rif_file = ext %in% "rif"
  msg = NULL
  if(all(ext == "")) {
    msg = c(msg, "-You should select at least one file")
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
  if(sum(!(daf_file | rif_file | cif_file)) > 1) {
    msg = c(msg, "-You should not select not select more than one fcs at the same time")
  }
  if(length(msg) != 0) {
    mess_global(title = "file input", msg = msg, type = "error", duration = 10)
    return(NULL)
  }
  
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
      info = try(stop(""), silent = TRUE)
      dat = try(ExtractFromFCS(fileName = fileName, 
                               force_header = TRUE, session = session), silent = TRUE)
      if(("try-error" %in% class(dat))) stop("fcs file does not seem to be well formatted:\n", attr(dat, "condition")$message)
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
      if(!requireNamespace(package = "IFCip", quietly = TRUE)) {
        msg_react$queue = c(msg_react$queue, "IFCip")
        updateSelectInput(session = session, inputId = "msg_once", choices = msg_react$queue, selected = msg_react$queue)
      }
    }
    obj_react$obj = checkObj(reinit_app(dat))
    obj_react$back = obj_react$obj
  },
  error = function(e) {
    mess_global(title = "file input", msg = e$message, type = "error")
    return(NULL)
  },
  finally = {
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", reset = TRUE)
  })
}

set_default_info <- function(obj) {
  obj$info = list("fileName" = try(basename(obj$fileName), silent=TRUE),
                  "fileName_image" = paste0(obj$description$ID$file, ", not found"),
                  "Merged_rif" = "N/A, no image file found",
                  "Merged_cif" = "N/A, no image file found",
                  "masks" = obj$description$masks,
                  "objcount" = obj$description$ID$objcount,
                  "date" = obj$description$Assay$date,
                  "instrument" = obj$info$instrument,
                  "sw_process" = obj$description$Assay$IDEAS_version,
                  "fcs_version" = obj$description$Assay$FCS_version,
                  "Images" = obj$description$Images,
                  "illumination" = "N/A, no image file found",
                  "CrossTalkMatrix" = "N/A, no image file found",
                  "magnification" = "N/A, no image file found",
                  "high-gain" = "N/A, no image file found",
                  "brightfield" = "N/A, no image file found",
                  "in_use" = rep(FALSE, 12),
                  "found" = FALSE)
  obj$info$in_use[obj$description$Images$physicalChannel] <- TRUE
  obj
}

reinit_app <- function(obj, session = getDefaultReactiveDomain()) {
  # reinit display (go to top, hide all tabs, remove msg, close draggable panels)
  updateTabsetPanel(session = session, "navbar", selected = "tab0")
  runjs(code="$('html, body').animate({scrollTop: '0px' }, 300);")
  hideElement("msg_app", anim = FALSE, animType = "fade", time = 0)
  hideElement("infos_save")
  hideElement("logs_save")
  hideElement("compute_features")
  hideElement(selector = "#navbar [data-value='tab1']")
  hideElement(selector = "#navbar [data-value='tab2']")
  hideElement(selector = "#navbar [data-value='tab3']")
  hideElement(selector = "#navbar [data-value='tab4']")
  hideElement(selector = "#navbar [data-value='tab5']")
  hideElement(selector = "#navbar [data-value='tab6']")
  hideElement(selector = "#navbar [data-value='tab7']")
  hideElement(selector = "#navbar [data-value='tab8']")
  hideElement(selector = "#navbar [data-value='tab9']")
  
  # reinit managers
  reinit_managers()
  # reinit obj info
  if(!any(obj$info$found)) {
    obj <- set_default_info(obj)
  }
  # we reinit report layout
  session$sendCustomMessage("init_grid", "")
  plot_react$layout = matrix(ncol=1, nrow=0)
  obj = reinit_layout(obj)
  
  # we reinit plot
  runjs("Shiny.onInputChange('pop_edit', null)")
  runjs("Shiny.onInputChange('pop_alt_click', null)")
  lapply(obs_plot, FUN = function(x) x$suspend())
  updateRadioButtons(session=session, inputId = "plot_type", selected = "1D", inline = TRUE)
  plot_react$shown = NULL
  plot_react$order = NULL
  plot_react$init = TRUE
  plot_react$color = sapply(obj$pops["All"], FUN = function(p) p$lightModeColor)
  plot_react$symbol = sapply(obj$pops["All"], FUN = function(p) p$style)
  lapply(obs_plot, FUN = function(x) x$resume())
  
  # we reinit plot3D
  reinit_plot_3D(session = session)

  # reinit parameters link with images
  hideElement(id = "plot_3D_img_ctrl")
  hideElement(selector = "#navbar [data-value='tab1']")
  updateSelectInput(session = session, inputId = "channels", selected = NULL)
  disable(selector = "#channels input[type='checkbox']")
  
  # we update sliders
  updateSelectInput(session, "sel_left", choices = sort(names(obj$features)), selected = names(obj$features))
  sel = tolower(obj$fileName)
  names(sel) = remove_ext(basename(sel))
  update_pops(session = session, obj = obj, init = TRUE)
  update_regions(session = session, obj = obj, init = TRUE)
  update_features(session = session, obj = obj, init = TRUE)
  obj$haschanged_objects = character()
  if(length(obj$fileName) == 0) return(NULL)
  
  # depending on input we allow some tabs
  if(obj$description$ID$objcount == nrow(obj$features)) {
    showElement(selector = "#navbar [data-value='tab2']")
    showElement(selector = "#navbar [data-value='tab4']")
    showElement(selector = "#navbar [data-value='tab5']")
    showElement(selector = "#navbar [data-value='tab7']")
  } else {
    hideElement(selector = "#navbar [data-value='tab2']")
    hideElement(selector = "#navbar [data-value='tab3']")
    hideElement(selector = "#navbar [data-value='tab4']")
    hideElement(selector = "#navbar [data-value='tab5']")
    hideElement(selector = "#navbar [data-value='tab7']")
  }
  if(ncol(obj$features) == 1 && (names(obj$features) == "Object Number")) {
    hideElement(selector = "#navbar [data-value='tab3']")
    hideElement(selector = "#navbar [data-value='tab4']")
    hideElement(selector = "#navbar [data-value='tab5']")
    hideElement(selector = "#navbar [data-value='tab7']")
  } else {
    showElement(selector = "#navbar [data-value='tab3']")
    showElement(selector = "#navbar [data-value='tab4']")
    showElement(selector = "#navbar [data-value='tab5']")
    showElement(selector = "#navbar [data-value='tab7']")
  }
  showElement(selector = "#navbar [data-value='tab8']")
  showElement("infos_save")
  # update maxpoints values in plots
  M = 100
  updateSliderInput(session=session, inputId = "plot_type_2D_main_option03", value = max(M/100, 100 * 5000 / nrow(obj$features)), min = M/100, max = M, step = M/100)
  M = as.integer(min(100, 100 * 20000 / nrow(obj$features)))
  updateSliderInput(session=session, inputId = "plot_type_3D_option03", value = max(M/100, 100 * 2000 / nrow(obj$features)), min = M/100, max = M, step = M/100)
  # if a file containing images was read we allow some other tabs
  # + we update app input with collected channels
  if(any(obj$info$found)) {
    showElement(id = "compute_features")
    if(!requireNamespace(package = "IFCip", quietly = TRUE) || (obj$info$XIF_test != 1)) {
      if((obj$info$XIF_test != 1))  mess_global(title = "rif file", msg = c("can't compute extra features from images on this type of rif file", "Features computation module has been disabled"), type = "info")
      hideElement(id = "compute_features")
      hideElement(selector = "#navbar [data-value='tab9']")
    } else {
      showElement(id = "compute_features")
      if(all(file.exists(file.path(.rundir, c("server_compensation.R", "server_navbar_compensation.R"))))) showElement(selector = "#navbar [data-value='tab9']")
    }
    param_react$back = param_react$param
    showElement(selector = "#navbar [data-value='tab1']")
    showElement(id = "plot_3D_img_ctrl")
    foo = sprintf("%02i",param_react$param$channels$physicalChannel)
    sel = which(obj$info$brightfield$channel)
    if(length(sel) == 0) {
      sel = which(obj$info$in_use)[1]
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
  }
  
  # check whether compensation is available
  if(all(file.exists(file.path(.rundir, c("server_compensation.R", "server_navbar_compensation.R"))))) showElement(selector = "#navbar [data-value='tab9']")
  # compensation is in dev and not available on shinyapps.io
  # if(Sys.getenv('SHINY_PORT') != "") hideElement(selector = "#navbar [data-value='tab9']")
  
  # check whether the input file was a FCS / LMD or other
  # if so change ML to use Arcsinh
  # + to use all that is not SS of FS as features names for transformation
  # + to change file extension when save
  if(length(obj$description$FCS) == 0) {
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
  }
  obj
}

observeEvent(list(input$use_example, input$example_file), ignoreInit = TRUE, {
  # by default example files are disabled
  disable("example_file")
  enable(selector = ".btn-file")
  hideElement("example_save")
  runjs(code = sprintf("Shiny.onInputChange('file', %s)", toJSON(NULL)))
  # check if IFCdata package is installed
  if(!requireNamespace("IFCdata", quietly = TRUE)) {
    # not installed, we send a message to user and switch use_example to FALSE
    updateSwitchInput(session=session, inputId="use_example", value=FALSE)
    msg_react$queue = c(msg_react$queue, "IFCdata")
    updateSelectInput(session = session, inputId = "msg_once", choices  = msg_react$queue, selected = msg_react$queue)
  } else { # if IFCdata pkg is installed
    if(input$use_example) { # if use_example switch is TRUE
      # we allow example_file selection and disable input$file upload
      disable(selector = ".btn-file")
      enable("example_file")
      showElement("example_save")
      # we look at files selected by user and use IFCdata selected files as input path for input$file
      fileinfo = list(name = paste0("example.", input$example_file), type = rep("", length(input$example_file)))
      fileinfo$datapath = system.file(package = "IFCdata", "extdata", fileinfo$name)
      fileinfo$size = sapply(X = fileinfo$datapath, FUN = file.size, simplify = TRUE, USE.NAMES = FALSE)
      runjs(code = sprintf("Shiny.onInputChange('file', %s)", toJSON(fileinfo)))
    }
  }
})

observeEvent(input$file, ignoreNULL = FALSE, ignoreInit = FALSE, {
  # modify current file_react
  file_react$input <- input$file
  file_react$id = random_name(n = 20)
  add_log("file input")
  newfileinput(input$file, session = session)
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
    if(ncol(Mat) != sum(obj_react$back$info$in_use)) stop("compensation can not be applied: number of acquired channels and number of channels in compensation file are different")
    
    colnames(Mat) = sprintf("Ch%02i",obj_react$back$info$Images$physicalChannel) # obj_react$back$info$Images$name
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
