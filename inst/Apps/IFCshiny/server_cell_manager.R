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

# allow to detach cells to show it everywhere + initialize some values
runjs(code = "$('#navbar [data-value=\"tab1\"]').on('dragstart', function(e) { Shiny.onInputChange('cell_manager_visible', true) } );" )
runjs(code = "Shiny.setInputValue('IFCshiny_modalGetSelection_ret', { rand:0, obj:[], from:'stop' })")
runjs(code = "Shiny.setInputValue('cell_selected', [])")

obs_cell <- list(
  # observer used when we want to:
  # change mode (create or edit)
  # change tagged population (in edit mode)
  # close 
  # but the selection is changed 
  # if proceed is choosen selection is cancelled  and action is applied
  obs1 = observeEvent(input$cell_modal_proceed, suspended = TRUE, { 
    runjs(code = "Shiny.setInputValue('cell_selected',[])")
    runjs(code = "$('.cell_checkbox').each(function() { this.checked = false; } )")
    obs_cell$obs1$suspend()
    obs_cell$obs2$suspend()
    switch(input$IFCshiny_modalGetSelection_ret$from,
           "mode" = {
             runjs(code = "$('.cell_tagged').addClass('hidden');")
             updateSelectInput(session=session, inputId="cell_pop_tagged", selected = "")
             cell_react$pop_tagged = ""
             cell_react$propagation = TRUE
           },
           "close"= {
             updateSwitchInput(session=session, inputId="cell_mode", value= FALSE)
             updateSelectInput(session=session, inputId="cell_pop_tagged", selected = "")
             cell_react$pop_tagged = ""
             cell_react$propagation = TRUE
             reinit_managers("cell")
           },
           "pop" = {
             runjs(code = "$('.cell_tagged').addClass('hidden');")
             if((length(input$cell_pop_tagged) != 0) && (input$cell_pop_tagged != "")) {
               cell_react$pop_tagged = input$cell_pop_tagged
               tagged = toJSON(which(obj_react$obj$pops[[input$cell_pop_tagged]]$obj)-1L)
               if(length(tagged)!=0) {
                 # we inject tagged population definition in cell_selected: i.e current selection
                 runjs(code = sprintf("Shiny.setInputValue('cell_selected', %s)", tagged))
                 # we apply selection on cell_tagged to display blue point (original tagged elements that defunes tagged population)
                 runjs(code = sprintf("$('.cell_tagged').filter(function(x) { return %s.includes($(this).data('value')); }).removeClass('hidden');", tagged))
                 # we modify shown cell that belongs belongs to tagged population to be shown as checked
                 runjs(code = sprintf("$('.cell_checkbox').each(function() { this.checked = %s.includes($(this).data('value')); } )", tagged))
               }
             }
           })
    removeModal()
    return(NULL)
  }),
  # observer used when we want to:
  # change mode (create or edit)
  # change tagged population (in edit mode)
  # close 
  # but the selection is changed 
  # if abort is choosen action is reverted
  obs2 = observeEvent(input$cell_modal_abort, suspended = TRUE, { 
    switch(input$IFCshiny_modalGetSelection_ret$from,
           "mode" = {
             updateSwitchInput(session=session, inputId="cell_mode", value= !input$cell_mode)
             if(input$cell_mode) {
               updateSelectInput(session=session, inputId="cell_pop_tagged", selected = "")
               runjs(code = "Shiny.setInputValue('cell_pop_tagged', '')")
               cell_react$pop_tagged = ""
             }
             cell_react$propagation = FALSE
           },
           "close" = {
             
           },
           "pop" = {
             updateSelectInput(session=session, inputId="cell_pop_tagged", selected = cell_react$pop_tagged)
             runjs(sprintf("Shiny.onInputChange('cell_pop_tagged', %s)",cell_react$pop_tagged))
             cell_react$propagation = FALSE
           })
    obs_cell$obs1$suspend()
    obs_cell$obs2$suspend()
    removeModal()
    return(NULL)
  }),
  # observe if close btn has been pressed if so trigger modalGetSelection
  observeEvent(input$cell_close, suspended = TRUE, {
    cell_react$propagation = TRUE
    session$sendCustomMessage("modalGetSelection", list(selector = ".cell_checkbox", inputId = "cell_selected", from = "close"))
  }),
  # observe if mode has been changed (create / edit) if so trigger modalGetSelection
  observeEvent({
    input$cell_mode
  }, suspended = TRUE, {
    # a masking div preventing cell selection is shown:
    # if cell_pop_tagged is reset to "" but cell_mode is edit
    if(input$cell_mode) { 
      showElement("cell_pop_edit")
      hideElement("cell_pop_create")
      if(input$cell_pop_tagged == '') {
        runjs(code="$('#cell_table_mask').css('display', 'block');") 
      } else {
        runjs(code="$('#cell_table_mask').css('display', 'none');")
      }
    } else { # in create mode i.e. cell_mode = FALSE the mask is always hidden to allow cells to be clicked
      runjs(code="$('#cell_table_mask').css('display', 'none');")
      showElement("cell_pop_create")
      hideElement("cell_pop_edit")
    }
    # we trigger cell_selected update and inspect if we need to inform the user if applied changes
    # are compatible with current selection
    if(input$IFCshiny_modalGetSelection_ret$from != "stop") {
      session$sendCustomMessage("updateSelection", list(selector = ".cell_checkbox", inputId = "cell_selected"))
      session$sendCustomMessage("modalGetSelection", list(selector = ".cell_checkbox", inputId = "cell_selected", from = "mode"))
    }
  }),
  # observe if pop tagged has been changed or if its definition has been changed if so trigger modalGetSelection
  observeEvent({
    list(input$cell_pop_tagged,
         obj_react$obj$haschanged_objects)
  }, suspended = TRUE, {
    if(!input$cell_mode) return(NULL) # edit mode is TRUE, if not TRUE we escape
    # a masking div preventing cell selection is shown if cell_pop_tagged is invalid
    # it is not valid when if its not in tagged population of obj
    # in such case:
    # cell_pop_tagged is reset to "",
    # cell_selected to an empty array []
    # and the html elements allowing to tag and display selected cells are also reset
    if(input$cell_pop_tagged == '') {
      runjs(code="$('#cell_table_mask').css('display', 'block');") 
      return(NULL)
    } else {
      if(!(input$cell_pop_tagged %in% names(obj_react$obj$pops)[sapply(obj_react$obj$pops, FUN = function(p) p$type == "T")])) {
        updateSelectInput(session=session, inputId="cell_pop_tagged", selected = "")
        runjs(code = "Shiny.setInputValue('cell_pop_tagged', '')")
        runjs(code = "Shiny.setInputValue('cell_selected', []);")
        runjs(code = "$('.cell_tagged').addClass('hidden');")
        runjs(code = "$('.cell_checkbox').each(function() { this.checked = false; } );") 
      } else{
        runjs(code="$('#cell_table_mask').css('display', 'none');") 
      }
    }
    # we trigger cell_selected update and inspect if we need to inform the user if applied changes
    # are compatible with current selection
    if(input$IFCshiny_modalGetSelection_ret$from != "stop") {
      session$sendCustomMessage("updateSelection", list(selector = ".cell_checkbox", inputId = "cell_selected"))
      session$sendCustomMessage("modalGetSelection", list(selector = ".cell_checkbox", inputId = "cell_selected", from = "pop"))
    }
  }),
  # observer triggered on mode change / tagged population changed / closing
  # this observer is aimed to inspect if user action will not result in selection lost
  # if not, see below, the action is performed , otherwise a modal is shown :
  # cell mode is changed from create to edit but no selection has been done
  # close is pressed but no selection has been done in create mode
  # close is pressed but selection is the same has original pop_tagged
  # pop_tagged is changed but selected element were not modified 
  observeEvent({
    input$IFCshiny_modalGetSelection_ret
  }, suspended = TRUE, {
    if(length(input$IFCshiny_modalGetSelection_ret$obj) != 0) {
      if(!cell_react$propagation) { 
        cell_react$propagation = TRUE
        return(NULL)
      }
    } 
    switch(input$IFCshiny_modalGetSelection_ret$from,
           "mode" = {
             obj = sort(unique(as.integer(unlist(input$IFCshiny_modalGetSelection_ret$obj))))
             domodal = TRUE
             if(length(obj) == 0) {
               domodal = FALSE
             } else {
               if(!input$cell_mode && 
                  (length(input$cell_pop_tagged) != 0) &&
                  (input$cell_pop_tagged != "") &&
                  (input$cell_pop_tagged %in% names(obj_react$obj$pops)) &&
                  identical(obj, sort(which(obj_react$obj$pops[[input$cell_pop_tagged]]$obj)-1L))) {
                 domodal = FALSE
               }
             }
             if(domodal) {
               showModal(modalDialog("Changing mode will clear current selection",
                                     size = "s",
                                     easyClose = FALSE,
                                     footer = list(actionButton(inputId = "cell_modal_proceed",label = "Proceed"),
                                                   actionButton(inputId = "cell_modal_abort", label = "Abort"))),
                         session = session)
               obs_cell$obs1$resume()
               obs_cell$obs2$resume()
             } else {
               runjs(code = "Shiny.setInputValue('cell_selected',[])")
               runjs(code = "$('.cell_checkbox').each(function() { this.checked = false; } )")
               runjs(code = "$('.cell_tagged').addClass('hidden');")
               updateSelectInput(session=session, inputId="cell_pop_tagged", selected = "")
               runjs("Shiny.onInputChange('cell_pop_tagged', '')")
               cell_react$propagation = TRUE
             }
           },
           "close" = {
             obj = sort(unique(as.integer(unlist(input$IFCshiny_modalGetSelection_ret$obj))))
             domodal = TRUE
             if(length(obj) == 0) {
               domodal = FALSE
             } else {
               if(input$cell_mode && 
                  (length(input$cell_pop_tagged) != 0) &&
                  (input$cell_pop_tagged != "") &&
                  (input$cell_pop_tagged %in% names(obj_react$obj$pops)) &&
                  identical(obj, sort(which(obj_react$obj$pops[[input$cell_pop_tagged]]$obj)-1L))) {
                 domodal = FALSE
               }
             }
             if(domodal) {
               showModal(modalDialog("Closing Cell Manager will clear current selection",
                                     size = "s",
                                     easyClose = FALSE,
                                     footer = list(actionButton(inputId = "cell_modal_proceed",label = "Proceed"),
                                                   actionButton(inputId = "cell_modal_abort", label = "Abort"))),
                         session = session)
               obs_cell$obs1$resume()
               obs_cell$obs2$resume()
             } else {
               updateSwitchInput(session=session, inputId="cell_mode", value= FALSE)
               updateSelectInput(session=session, inputId="cell_pop_tagged", selected = "")
               runjs("Shiny.onInputChange('cell_pop_tagged', '')")
               runjs(code = "Shiny.setInputValue('cell_selected',[])")
               runjs(code = "$('.cell_checkbox').each(function() { this.checked = false; } )")
               runjs(code = "$('.cell_tagged').addClass('hidden');")
               cell_react$pop_tagged = ""
               cell_react$propagation = TRUE
               reinit_managers("cell")
             }
           },
           "pop" = {
             obj = sort(unique(as.integer(unlist(input$IFCshiny_modalGetSelection_ret$obj))))
             if((cell_react$pop_tagged %in% names(obj_react$obj$pops)) &&
                (length(obj) != 0) &&
                !identical(obj, sort(which(obj_react$obj$pops[[cell_react$pop_tagged]]$obj)-1L))) {
               showModal(modalDialog("Changing tagged population will remove all applied changes",
                                     size = "s",
                                     easyClose = FALSE,
                                     footer = list(actionButton(inputId = "cell_modal_proceed",label = "Proceed"),
                                                   actionButton(inputId = "cell_modal_abort", label = "Abort"))),
                         session = session)
               obs_cell$obs1$resume()
               obs_cell$obs2$resume()
             } else {
               runjs(code = "$('.cell_tagged').addClass('hidden');")
               if(input$cell_pop_tagged %in% names(obj_react$obj$pops)) {
                 cell_react$pop_tagged = input$cell_pop_tagged
                 tagged = toJSON(which(obj_react$obj$pops[[input$cell_pop_tagged]]$obj)-1L)
                 if(length(tagged)!=0) {
                   # we inject tagged population definition in cell_selected: i.e current selection
                   runjs(code = sprintf("Shiny.setInputValue('cell_selected', %s)", tagged))
                   # we apply selection on cell_tagged to display blue point (original tagged elements that defunes tagged population)
                   runjs(code = sprintf("$('.cell_tagged').filter(function(x) { return %s.includes($(this).data('value')); }).removeClass('hidden');", tagged))
                   # we modify shown cell that belongs belongs to tagged population to be shown as checked
                   runjs(code = sprintf("$('.cell_checkbox').each(function() { this.checked = %s.includes($(this).data('value')); } )", tagged))
                 }
               } 
             } 
           })
  }),
  # this observer is intended to compute the objects indices that belong to the displayed population
  # it will also set maximal page number according to total objects in tagged population 
  # and also set grid if number of objects is lesser than current value
  observeEvent({
    list(obj_react$obj$haschanged_objects,
         input$cell_population,
         input$cell_grid)
  }, suspended = TRUE,  {
    if(!(input$cell_population %in% names(obj_react$obj$pops)) ||
       (length(input$cell_grid) == 0) ||
       (length(na.omit(input$cell_grid[input$cell_grid > 0 & input$cell_grid <= 7])) == 0)) {
      if(input$cell_population %in% names(obj_react$obj$pops)) {
        obj = obj_react$obj$pops[[input$cell_population]]$obj
        updateNumericInput(session=session, inputId="cell_grid", value=min(as.integer(ceiling(sqrt(sum(obj)))), 7))
      }
      cell_react$objects = data.frame()
      return(NULL)
    }
    obj = obj_react$obj$pops[[input$cell_population]]$obj
    n = sum(obj)
    if(n < input$cell_grid) {
      extract_max = n
      updateNumericInput(session=session, inputId="cell_grid", value=n)
    } else {
      extract_max = input$cell_grid * input$cell_grid 
    }
    session$sendCustomMessage("updateSelection", list(selector = ".cell_checkbox", inputId = "cell_selected"))
    updateNumericInput(session=session, inputId="cell_page", max=max(1, ceiling(n/extract_max)),value=1)
    if(input$cell_feature_sort %% 2) runjs(sprintf("Shiny.onInputChange('cell_feature_sort', %i)", ifelse(length(input$cell_feature_sort) == 0, 0, input$cell_feature_sort + 1L)))
    # shinyjs::click("cell_feature_sort")
    df = obj_react$obj$features[obj_react$obj$pops[[input$cell_population]]$obj, c("Object Number", input$cell_feature)]
    df = df[!is.na(df[, 1]), ]
    df = obj_react$obj$features[obj_react$obj$pops[[input$cell_population]]$obj, c("Object Number", input$cell_feature)]
    df = df[!is.na(df[, 1]), ]
    if(nrow(df) == 0) {
      cell_react$objects = data.frame()
      return(NULL)
    }
    df = df[order(df[, 2]), ]
    df = df[1:extract_max, ]
    df = df[!is.na(df[, 1]), ]
    cell_react$objects = data.frame(df[, 1], df[, 2])
  }),
  # this observer will extract the indices of objects in displayed population that corresponds to
  # page number, grid size, and sorting feature order
  observeEvent({
    list(obj_react$obj$haschanged_objects,
         input$cell_grid,
         input$cell_feature,
         input$cell_feature_sort,
         input$cell_page)
  }, suspended = TRUE, {
    extract_max = na.omit(input$cell_grid * input$cell_grid)
    if(!(input$cell_population %in% names(obj_react$obj$pops)) || 
       (length(input$cell_grid) == 0) ||
       (length(na.omit(input$cell_grid[input$cell_grid > 0 & input$cell_grid <= 7])) == 0) ||
       (length(input$cell_page) == 0) ||
       (length(na.omit(input$cell_page[input$cell_page != 0])) == 0)) {
      cell_react$objects = data.frame()
      return(NULL)
    }
    if(length(obj_react$obj$haschanged_objects) !=0) {
      updateNumericInput(session=session, inputId="cell_page", max=max(1, ceiling(sum(obj_react$obj$pops[[input$cell_population]]$obj)/extract_max)))
      if(!(input$cell_population %in% obj_react$obj$haschanged_objects)) {
        cell_react$objects = data.frame()
        obj_react$obj$haschanged_objects <- character()
        return(NULL)
      }
    }
    df = obj_react$obj$features[obj_react$obj$pops[[input$cell_population]]$obj, c("Object Number", input$cell_feature)]
    df = df[!is.na(df[, 1]), ]
    if(nrow(df) == 0) {
      cell_react$objects = data.frame()
      return(NULL)
    }
    df = df[order(df[, 2]), ]
    objects = df[, 1]
    feat = df[, 2]
    
    if(input$cell_feature_sort %%2) {
      objects = rev(objects)
      feat = rev(feat)
      updateActionButton(session = session, inputId = "cell_feature_sort", icon = icon(name="sort-amount-up", lib="font-awesome", verify_fa=FALSE))#label="\u21c5 \u21a5")
    } else {
      updateActionButton(session = session, inputId = "cell_feature_sort", icon = icon(name="sort-amount-down-alt", lib="font-awesome", verify_fa=FALSE))#label="\u21c5 \u21a7")
    }
    
    i = input$cell_page
    objects = split(objects, ceiling(seq_along(objects)/extract_max))
    feat = split(feat, ceiling(seq_along(feat)/extract_max))
    if(i < 1) i = 1
    if(i > length(objects)) i = length(objects)
    cell_react$objects=data.frame(objects[[i]], feat[[i]])
  }),
  # here we use some css to quickly and easily transform cell images
  observeEvent({
    list(input$cell_height,
         input$cell_width,
         input$cell_zoom,
         input$cell_rotate,
         input$cell_grid)
  }, suspended = TRUE, {
    runjs(code = sprintf("$('div.cell_size').css( { height:'%ipx', width:'%ipx'})", input$cell_height, input$cell_width))
    runjs(code = sprintf("$('img.cell_img').css('transform', 'scale(%s) rotate(%ideg)' )", 2^input$cell_zoom, input$cell_rotate))
    runjs(code = sprintf("$('#cell_table').css({ width:'%ipx'})", (input$cell_width + 4) * input$cell_grid))
  }),
  # this observer will react to previously computed objects and extrct corresponded image from file
  observeEvent({
    list(cell_react$objects,
         # input$chan_sel,
         input$cell_manager_visible,
         input$img_manager_visible) # allow reactivity on image parameter changes
  }, suspended = TRUE, {
    if((length(input$img_manager_visible) !=0) && input$img_manager_visible) return(NULL) # changes in image parameters are applied when img_manager is closed
    if((length(input$cell_manager_visible) ==0) || !input$cell_manager_visible) return(NULL)
    if((length(input$chan_sel) == 0) || (input$chan_sel == "")) return(NULL)
    if(!(input$cell_population %in% names(obj_react$obj$pops))) return(NULL)
    
    if(length(cell_react$objects) == 0) {
      html(id = "cell_image_placeholder", NULL)
      return(NULL)
    }
    # at the beginning we inspect already drawn images and extract selected elements
    session$sendCustomMessage("updateSelection", list(selector = ".cell_checkbox", inputId = "cell_selected"))
    
    # during computation we disable the grid this allow to visually identify that is is computing
    addClass(id = "cell_image_placeholder", "disabled")
    
    extract_max = input$cell_grid * input$cell_grid
    
    # we set styling of html components
    # color of the <p> object number
    col = ifelse(as.integer(input$chan_sel) %in% which(obj_react$back$info$brightfield$channel), "black", "white")
    # style of the images, this style will be included directly in <img> returned by objectExtract()
    base64_att = sprintf("class='cell_img' style='object-position:center center; object-fit:none; transform: scale(%s) rotate(%ideg);' )", 2^input$cell_zoom, input$cell_rotate)
    
    # image extration
    sel = split(cell_react$objects[, 1], ceiling(seq_along(cell_react$objects[, 1])/20))
    L = length(sel)
    # pb = newPB(title = "cells", label = "extracting images", min = 1, max = L)
    tryCatch({
      imgs = lapply(1:L, FUN=function(i) {
        # setPB(pb = pb, value = i)
        info = obj_react$obj$info
        info$Images = param_react$param$channels
        param = objectParam(info = info,
                            mode = c("rgb"),
                            export = "base64",
                            write_to = "%s_%o.png",
                            base64_id = TRUE,
                            base64_att = base64_att,
                            overwrite = TRUE,
                            composite = "",
                            selection = as.integer(input$chan_sel),
                            size = c(0, 0),
                            force_width = FALSE,
                            random_seed = NULL,
                            removal = "none",
                            add_noise = TRUE,
                            full_range = "full_range" %in% input$chan_force,
                            force_range = "force_range" %in% input$chan_force)
        objectExtract(ifd = getIFD(obj_react$back$fileName_image, offsets = subsetOffsets(obj_react$back$offsets, sel[[i]], "img"),
                                   trunc_bytes = 1, force_trunc = TRUE, verbose = FALSE, verbosity = 1, bypass = TRUE, display_progress = FALSE),
                      param = param, bypass = TRUE)
      })
      if(L>1) {
        imgs = do.call(what="c", args=imgs)
      } else {
        imgs = imgs[[1]]
      }
      ids = sapply(imgs, attr, which="object_id")
      if(!all(cell_react$objects[, 1] == ids)) mess_global(title = "extracting images",
                                                           msg = "Extracted object_ids differ from expected one.",
                                                           type = "warning", duration = 10)
      names(imgs) = cell_react$objects[, 1]
    },
    error = function(e) {
      mess_global(title = "extracting images", msg = e$message, type = "error")
      return(NULL)
    }, finally = {
      # endPB(pb)
    })
    
    # if we are in edit mode and cell_pop_tagged is defined we identify if the extracted image is part 
    # of this cell_pop_tagged, if so, we remove hidden class to a <div> that thanks to css will be shown
    # a blue point to inform user that this element is part ot cell_pop_taged
    if(input$cell_mode && (length(input$cell_pop_tagged) != 0) && (input$cell_pop_tagged != "") && (input$cell_pop_tagged %in% names(obj_react$obj$pops))) {
      tagged = which(obj_react$obj$pops[[input$cell_pop_tagged]]$obj)-1L
      tagged = ids %in% tagged
      tagged_class = sapply(tagged, FUN = function(x) ifelse(x, "cell_tagged", c("cell_tagged hidden")))
    } else {
      tagged_class = rep("cell_tagged hidden", length.out = length(ids))
    }
    # we compute images position (splitting by row) 
    foo = split(imgs, ceiling(seq_along(imgs)/input$cell_grid))
    # we compute object number
    ids = split(ids, ceiling(seq_along(ids)/input$cell_grid))
    # we compute feature value
    feat = split(cell_react$objects[, 2], ceiling(seq_along(cell_react$objects[, 2])/input$cell_grid))
    tagged_class = split(tagged_class, ceiling(seq_along(tagged_class)/input$cell_grid))
    # we define style of the former elements
    div_style = sprintf("overflow:hidden; width:%ipx; height:%ipx; position: relative;", input$cell_width, input$cell_height)
    ids_style = sprintf("position: absolute; top: -2px; left: 2px; pointer-events:none; cursor: not-allowed; color:%s;", col)
    # if feat is Object Number we don't display it
    feat_style = sprintf("position: absolute; top: 12px; left: 2px; color:%s; display:%s;", "yellow", ifelse(input$cell_feature == "Object Number", "none", "block"))
    # we remove former table and add the new one
    removeUI(session = session, selector = "#cell_table", immediate = TRUE, multiple = FALSE)
    removeUI(session = session, selector = "#cell_table_mask", immediate = TRUE, multiple = FALSE)
    # we create a table with each cell being composed of
    # <table>
    #   <div>
    #     <input>
    #     <label for input>
    #       <img>
    #       <p> feat
    #       <div> to show a blue circle if part of tagged pop
    #       <div> to show a yellow circle on selection
    #     <p> object number
    # <div> a mask to disable click on cell
    insertUI(session = session, selector = "#cell_image_placeholder", immediate=TRUE, multiple=FALSE, where="beforeEnd", 
             ui = list(tags$table(id = "cell_table",
                                  style = sprintf("cursor: no-drop; position:relative; background-color:#4c4c4c; border-collapse:collapse; table-layout:fixed; width:%ipx",(input$cell_width+4)*input$cell_grid),
                                  lapply(1:length(foo), FUN = function(r) {
                                    tags$tr(
                                      lapply(1:input$cell_grid, FUN = function(c) {
                                        if(length(foo[[r]]) < c) return(NULL)
                                        uniq_name = paste0(sample(c(letters, 0:9), 12), collapse = "")
                                        tags$td(style = "text-align: center; padding:2px;", 
                                                tags$div(class = "cell_size",
                                                         style = div_style, 
                                                         tags$input(style = "display:none;", type="checkbox", class ="cell_checkbox", "data-value" = ids[[r]][c], id=uniq_name),
                                                         tags$label("for" = uniq_name, style = "cursor:pointer;",
                                                                    HTML(foo[[r]][[c]][[1]]),
                                                                    tags$p(style = feat_style, feat[[r]][c]),
                                                                    tags$div(class = tagged_class[[r]][c], "", "data-value" = ids[[r]][c]),
                                                                    tags$div(class="cell_selection", "")),
                                                         tags$p(style = ids_style, ids[[r]][c])
                                                ))
                                      })
                                    )
                                  })),
                       tags$div(id="cell_table_mask",
                                style = paste0("position:absolute; width:100%; height:100%; top:0; left:0; display:", 
                                               ifelse(input$cell_mode, ifelse((length(input$cell_pop_tagged) == 0) || (input$cell_pop_tagged == ""), 'block;','none;'),'none;')))))
    # we check if displayed objects are part of selected cells and if so we check input befor leaving
    runjs(code = "if(Shiny.shinyapp.$inputValues.cell_selected != null) $('.cell_checkbox').each(function() { this.checked = Shiny.shinyapp.$inputValues.cell_selected.includes($(this).data('value')); } );")
    # everything is computed we remove disabled class
    removeClass(id = "cell_image_placeholder", "disabled")
  }),
  # observer on cell_settings to start image_manager on click
  observeEvent(input$cell_settings, suspended = TRUE, {
    runjs("Shiny.onInputChange('img_manager_visible', true)")
  }),
  # observers on cell_modify btn to update cell_pop_tagged
  # it will trigger current selection computation and launch popplation modification
  observeEvent({
    input$cell_modify
  }, suspended = TRUE, {
    session$sendCustomMessage("updateSelection", list(selector = ".cell_checkbox", inputId = "cell_selected"))
    session$sendCustomMessage("modifyTaggedSelection", list(selector = ".cell_checkbox", inputId = "cell_selected")) 
  }),
  observeEvent(req(input$IFCshiny_modifyTaggedSelection_ret), suspended = TRUE,  {
    obj = sort(unique(as.integer(unlist(input$IFCshiny_modifyTaggedSelection_ret$obj))))
    if(length(obj) == 0) {
      mess_global(title = "modify tagged population", msg = "There should be at least 1 object in a tagged population", 
                  type = "error", duration = 10)
      return(NULL)
    }
    add_log("Tagged Population Modification")
    P = obj_react$obj$pops[input$cell_pop_tagged]
    if(identical(which(P[[1]]$obj) -1L, obj)) return(NULL)
    P[[1]]$obj = obj
    obj_back = obj_react$obj
    tryCatch({
      obj_react$obj = data_modify_pops(obj_react$obj, pops = P, display_progress = TRUE)
    }, error = function(e) {
      obj_react$obj = obj_back
      mess_global(title = "modify tagged population", msg = e$message, 
                  type = "error", duration = 10)
      return(NULL)
    })
    tagged = toJSON(P[[1]]$obj)
    # modification has been done we can now include selected cells as part of tagged objects
    runjs(code = "$('.cell_tagged').addClass('hidden');")
    runjs(code = sprintf("$('.cell_checkbox').each(function() { this.checked = %s.includes($(this).data('value')); } )", tagged))
    runjs(code = sprintf("Shiny.setInputValue('cell_selected', %s );", tagged))
    runjs(code = sprintf("$('.cell_tagged').filter(function(x) { return %s.includes($(this).data('value')); }).removeClass('hidden');", tagged))
  }),
  # observers on cell_create btn to update create a new tagged population
  # it will trigger current selection computation and launch population creation
  observeEvent({
    input$cell_create
  }, suspended = TRUE, {
    session$sendCustomMessage("updateSelection", list(selector = ".cell_checkbox", inputId = "cell_selected"))
    session$sendCustomMessage("getTaggedSelection", list(selector = ".cell_checkbox", inputId = "cell_selected")) 
  }),
  observeEvent(req(input$IFCshiny_getTaggedSelection_ret), suspended = TRUE,  {
    # we ensure that only cell manager is shown
    if(!check_managers(alw="cell")) return(NULL)
    # we check that there is at least one cell selected
    if(length(input$IFCshiny_getTaggedSelection_ret$obj)==0) return(NULL)
    add_log("Tagged Population Creation")
    # we close cell manager and open pop manager
    runjs("Shiny.onInputChange('cell_manager_visible', false)")
    onFlushed(once=TRUE, fun = function() {
      runjs("Shiny.onInputChange('pop_manager_visible', true)")
    })
    # we pass selection in the definition of a new tagged population
    pops_react$def = character()
    enable("pop_validate")
    enable("pop_def_name")
    hideElement("pop_plot")
    hideElement("pop_tag")
    hideElement(selector = "#pop_def_edit, .pop_def_feedback")
    hideElement("pop_remove")
    updateSelectInput(session = session, inputId = "pop_symbol", selected = "Simple Dot")
    colourpicker::updateColourInput(session = session, inputId = "pop_color_light", value = "black")
    colourpicker::updateColourInput(session = session, inputId = "pop_color_dark", value = "white")
    updateTextInput(session = session, inputId = "pop_def_name", value = "")
    obj = unique(as.integer(unlist(input$IFCshiny_getTaggedSelection_ret$obj)))
    pops_react$revert = list(name = NULL, style = 20, color = "White",
                             lightModeColor = "Black", type = "T",
                             obj = obj)
    output$pop_def_viz <- renderPrint( list("Tagged" = substr(paste0(obj, collapse = ", "), 0, 120)))
    pops_react$new = TRUE
    # population definition has been passed to Population Manager we can now reset selection
    runjs("Shiny.onInputChange('IFCshiny_getTaggedSelection_ret', { rand: null, obj:[] } )")
    runjs(code = "Shiny.setInputValue('cell_selected', [] );")
    runjs(code = "$('.cell_tagged').addClass('hidden');")
    runjs(code = "$('.cell_checkbox').each(function() { this.checked = false; } );")
  }))