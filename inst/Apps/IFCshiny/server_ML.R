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

check_pop_training <- function(obj, pops) {
  msg = character()
  if((length(pops) == 0) || all(pops == "")) {
    msg = "select at least one population"
  } else {
    if(any(grepl("^ML_", pops))) msg = c(msg, "you can not select ML population")
    g = sapply(obj$pops[pops], FUN = function(p) p$obj)
    # sub = apply(g, 1, any)
    sub = fastAny(g)
    groups = apply(g, 1, FUN = function(x) {
      foo = which(x)
      if(length(foo) == 1) return(pops[foo])
      return(NA)
    })
    groups = factor(groups[sub], levels = pops, labels = pops)
    check = table(groups)/apply(g, 2, sum)
    if(any(check < 0.8) || (length(na.omit(unique(groups))) != length(pops))) msg = c(msg, "too many cells belong to more than 1 unique population")
  }
  if(length(msg) == 0) {
    shinyFeedback::hideFeedback(inputId = "populations_training", session = session)
  } else {
    shinyFeedback::showFeedbackDanger(inputId = "populations_training", text = msg, session = session) 
  } 
  return(length(msg) == 0)
}

obs_ML <- list(
  # whether to use manual or auto
  # when manual the panel alowing feature selection is enabled
  observeEvent(input$features_used, suspended = TRUE,{
    if(input$features_used == "auto") {
      runjs(code = "$('#features_manual').addClass('disabled')")
      updateNumericInput(session = session, inputId = "features_bst", value = 5)
      feat_react$best_length = 5
    } else {
      runjs(code = "$('#features_manual').removeClass('disabled')")
      updateNumericInput(session = session, inputId = "features_bst", value = 0)
      feat_react$best_length = 0
    }
  }),
  # observer on population(s) used for ML
  # at least one is needed
  # if only one only unsupervised algorithm are proposed
  # if more that one supervised ML are shown
  observeEvent(input$populations_training, suspended = TRUE, ignoreNULL = FALSE, {
    features_inp = ""
    if(input$features_int == "yes") features_inp = try(grep(input$features_inp, names(obj_react$obj$features), perl = TRUE), silent = TRUE)
    can_start = check_pop_training(obj_react$obj, input$populations_training) && !inherits(x= features_inp,what = "try-error")
    if(can_start) {
      model_react$ratio = 0.5
      updateSliderInput(session=session, inputId="training_ratio", value=0.5)
      enable(selector = "#navbar_ML [data-value='ML_training']")
      addClass(selector = "#navbar_ML [data-value='ML_training']", class = "clickme") 
    } else {
      disable(selector = "#navbar_ML [data-value='ML_training']")
      removeClass(selector = "#navbar_ML [data-value='ML_training']", class = "clickme") 
    }
    chc = c("pca","tsne","umap","som")
    hideElement("training_unsupervised")
    if(length(input$populations_training) == 1) {
      hideElement(id = "features_best_length_ctn")
      showElement("training_unsupervised")
    } else {
      chc = c(chc, "em","svm","xgb","lda")
      showElement(id = "features_best_length_ctn")
    }
    updateSelectInput(session = session, inputId = "training_model", choices = setdiff(chc, c(msg_react$queue, msg_react$done)), selected = "pca")
  }),
  # number of features that best discriminate clusters
  observeEvent(input$features_bst, suspended = TRUE,{
    feat_react$best_length <- input$features_bst
  }),
  # whether to transform intensity
  observeEvent(input$features_int, suspended = TRUE,{
    if(input$features_int == "yes") {
      showElement(id = "features_inp")
      showElement(id = "features_inm")
      features_inp = try(grep(input$features_inp, names(obj_react$obj$features), perl = TRUE), silent = TRUE)
    } else {
      features_inp = ""
      hideElement(id = "features_inp")
      hideElement(id = "features_inm")
    }
    can_start = check_pop_training(obj_react$obj, input$populations_training) && !inherits(x= features_inp,what = "try-error")
    if(can_start) {
      enable(selector = "#navbar_ML [data-value='ML_training']")
      addClass(selector = "#navbar_ML [data-value='ML_training']", class = "clickme") 
    } else {
      disable(selector = "#navbar_ML [data-value='ML_training']")
      removeClass(selector = "#navbar_ML [data-value='ML_training']", class = "clickme") 
    }
  }),
  # text input allowing regex to be used to find features when manual selection is used
  observeEvent(input$pattern, suspended = TRUE,{
    feat_n <- names(obj_react$obj$features)
    feat <- tryCatch(grep(input$pattern,feat_n, perl=TRUE, ignore.case=FALSE, value=TRUE), error = function(e) e)
    l = length(feat)
    if("error" %in% class(feat)) {
      shinyFeedback::showFeedbackDanger(session = session, inputId = "pattern", text = feat$message)
      output$features_infos <- renderText({
        "No features with this pattern"
      })
      updateSelectInput(session, "sel_left", choices=list(), selected = NULL)
      return(NULL)
    } else {
      hideFeedback(session = session, inputId = "pattern")
    }
    if(l == 0) {
      output$features_infos <- renderText({
        "No features with this pattern"
      })
      updateSelectInput(session, "sel_left", choices=list(), selected = NULL)
    } else {
      output$features_infos <- renderText({
        paste0(l, " features found with this pattern")
      })
      updateSelectInput(session, "sel_left", choices=sort(feat), selected=feat)
    }
  }),
  # select features
  observeEvent(input$moveRight, suspended = TRUE,{
    if(length(input$sel_left) != 0) {
      feat_react$selected = isolate(sort(unique(c(feat_react$selected, input$sel_left, input$sel_right))))
      updateSelectInput(session = session, inputId = "sel_right", choices=feat_react$selected, selected=isolate(input$sel_left))
    }
  }),
  # deselect features
  observeEvent(input$moveLeft, suspended = TRUE,{
    if(length(input$sel_right) != 0) {
      feat_react$selected = isolate(sort(setdiff(feat_react$selected, input$sel_right)))
      N = feat_react$selected; if(length(N) == 0) N = list()
      updateSelectInput(session = session, inputId = "sel_right", choices=N)
    }
  }),
  # when features are moved from left to right or right to left
  # we need to update the tooltip
  observeEvent({
    list(input$sel_right,
         input$sel_left)
  }, suspended = TRUE,{
    runjs(code = sprintf("$('#sel_left,#sel_right').children().each(function(index,val) {var el=$(this);var txt=el.text();el.attr({'data-toggle':'tooltip','title':txt,'data-placement':'%s','data-delay':{'show':%i,'hide':%i},'data-trigger':'%s'})})", "top", 300, 300, "hover"))
  }),
  # observer on slected tab in ML
  # it will allow to change option and behaviour depending on the tab
  observeEvent(input$navbar_ML, suspended = TRUE,{
    hideElement("ML_side_inputs")
    hideElement("ML_side_training")
    if(input$navbar != "tab5") return(NULL)
    features_inp = ""
    if(input$features_int == "yes") features_inp = try(grep(input$features_inp, names(obj_react$obj$features), perl = TRUE), silent = TRUE)
    can_start = check_pop_training(obj_react$obj, input$populations_training) && !inherits(x= features_inp,what = "try-error")
    if(can_start) {
      enable(selector = "#navbar_ML [data-value='ML_training']")
      addClass(selector = "#navbar_ML [data-value='ML_training']", class = "clickme") 
    } else {
      disable(selector = "#navbar_ML [data-value='ML_training']")
      removeClass(selector = "#navbar_ML [data-value='ML_training']", class = "clickme") 
    }
    switch(input$navbar_ML, 
           # for input parameters for preprocessing
           "ML_inputs" = {
             showElement("ML_side_inputs")
             return(NULL)
           }, 
           # for launching the model
           "ML_training" = {
             showElement("ML_side_training")
             removeClass(selector = "#navbar_ML [data-value='ML_training']", class = "clickme")
             runjs(sprintf("Shiny.onInputChange('check_start', %i)", ifelse(length(input$check_start) == 0, 0, input$check_start + 1)))
           })
  }),
  # observer to check that regular expression for intensity is valid
  observeEvent(input$features_inp, suspended = TRUE,{
    features_inp = ""
    if(input$features_int == "yes") features_inp = try(grep(input$features_inp, names(obj_react$obj$features), perl = TRUE), silent = TRUE)
    can_start = check_pop_training(obj_react$obj, input$populations_training)
    shinyFeedback::hideFeedback(inputId = "features_inp", session = session)
    if((length(features_inp) == 0) || all("" == features_inp)) {
      can_start = TRUE && can_start
      shinyFeedback::showFeedbackWarning(inputId = "features_inp", text = "no feature found with this pattern", session = session)
    }
    if(inherits(x = features_inp, what = "try-error")) {
      shinyFeedback::showFeedbackDanger(inputId = "features_inp", text = "invalid pattern", session = session)
      can_start = FALSE
    }
    if(can_start) {
      enable(selector = "#navbar_ML [data-value='ML_training']")
      addClass(selector = "#navbar_ML [data-value='ML_training']", class = "clickme") 
    } else {
      disable(selector = "#navbar_ML [data-value='ML_training']")
      removeClass(selector = "#navbar_ML [data-value='ML_training']", class = "clickme") 
    }
  }),
  # observer that is aimed to check if we can start the training
  # it will test if any modelling was already performed
  observeEvent(input$check_start, suspended = TRUE, {
    obs_ML$retrain_ok$resume()
    obs_ML$retrain_abort$resume()
    tryCatch({
      pops = input$populations_training
      # before launching the fitting we check that at least one population is selected
      if((length(pops) == 0) || any(pops == "")) {
        mess_global(title = "data pre-processing", msg = c("Please select population(s)", "- one, for unsupervised ML", "- at least two, for supervised ML"), type = "info")
        updateTabsetPanel(session = session, "navbar_ML", selected = "ML_inputs")
        runjs(code = "$('#navbar_ML [data-value=\"ML_inputs\"]').trigger('click');")
        return(NULL)
      }
      # we remove all former ML_ pops and features computed during last modelling run
      # before running a new fitting
      # if a former modelling was performed we inform the user that regions, graphs, pops, features...
      # depending on these ML_ will be removed 
      check = grep("ML_subset", names(obj_react$obj$pops), fixed = TRUE, value = TRUE)
      if(length(check) != 0) {
        check = c(check, grep("^ML_meta_|^ML_pred_", names(obj_react$obj$pops), value = TRUE))
        to_remove = data_rm_pops(obj = obj_react$obj, pops = check, list_only = TRUE, session=session, adjust_graph = FALSE)
        check = grep("^ML_", names(obj_react$obj$features), value = TRUE)
        if(length(check) != 0) {
          to_remove2 = data_rm_features(obj = obj_react$obj, features = check, list_only = TRUE, session=session, adjust_graph = FALSE)
          to_remove = lapply(1:length(to_remove), FUN = function(i) unique(c(to_remove[[i]], to_remove2[[i]])))
          names(to_remove) = names(to_remove2)
        }
        if((length(to_remove$graphs) > 0) || (length(to_remove$pops) != 1) || (length(to_remove$regions) > 0)) {
          to_remove_msg = list(tags$p("Re-training will also induce the removal of"))
          if(length(to_remove$regions) > 0) to_remove_msg = c(to_remove_msg, list(tags$p("- region(s):"), tags$ul(lapply(to_remove$regions, FUN = function(x) tags$li(x)))))
          if(length(to_remove$pops) > 0) to_remove_msg = c(to_remove_msg, list(tags$p("- populations(s):"), tags$ul(lapply(to_remove$pops, FUN = function(x) tags$li(x)))))
          if(length(to_remove$graphs) > 0) to_remove_msg = c(to_remove_msg, list(tags$p("- graphs(s): "), paste0(to_remove$graphs, collapse = ", ")))
          showModal(modalDialog(to_remove_msg,
                                size = "s",
                                easyClose = FALSE,
                                footer = list(actionButton(inputId = "retrain_anyway",label = "Proceed"),
                                              actionButton(inputId = "retrain_abort", label = "Abort"))),
                    session = session)
        } else {
          runjs(sprintf("Shiny.onInputChange('retrain_anyway', %i)", ifelse(length(input$retrain_anyway) == 0, 0, input$retrain_anyway + 1)))
        }
      } else {
        runjs(sprintf("Shiny.onInputChange('retrain_anyway', %i)", ifelse(length(input$retrain_anyway) == 0, 0, input$retrain_anyway + 1)))
      }
    }, error = function(e) {
      updateTabsetPanel(session = session, "navbar_ML", selected = "ML_inputs")
      runjs(code = "$('#navbar_ML [data-value=\"ML_inputs\"]').trigger('click');")
      mess_global(title = "data pre-processing", msg = c("Can't pre-process data with input parameters:", e$message), type = "stop")
    })
  }),
  # observer to launch pre-processing of the data
  # before starting preprocessing we remove all former ML_
  retrain_ok = observeEvent(input$retrain_anyway, suspended = TRUE,{
    obs_ML$retrain_ok$suspend()
    obs_ML$retrain_abort$suspend()
    removeModal(session = session)
    showElement("ML_side_training")
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Pre-processing data for training", reset = FALSE)
    add_log("pre-processing data")
    
    tryCatch({
      pops = input$populations_training
      check = grep("ML_subset", names(obj_react$obj$pops), fixed = TRUE, value = TRUE)
      if(length(check) != 0) {
        check = c(check, grep("^ML_meta_|^ML_pred_", names(obj_react$obj$pops), value = TRUE))
        to_remove = data_rm_pops(obj = obj_react$obj, pops = check, list_only = TRUE, session=session, adjust_graph = FALSE)
        check = grep("^ML_", names(obj_react$obj$features), value = TRUE)
        if(length(check) != 0) {
          to_remove2 = data_rm_features(obj = obj_react$obj, features = check, list_only = TRUE, session=session, adjust_graph = FALSE)
          to_remove = lapply(1:length(to_remove), FUN = function(i) unique(c(to_remove[[i]], to_remove2[[i]])))
          names(to_remove) = names(to_remove2)
        }
        if(any(sapply(to_remove, length) > 0)) {
          obj_back = obj_react$obj
          tryCatch({
            if(length(to_remove$graphs) > 0) {
              obj_react$obj = data_rm_pops(obj = obj_react$obj, pops = grep("^ML_subset|^ML_meta_|^ML_pred_", names(obj_react$obj$pops), value = TRUE),
                                           list_only = FALSE, adjust_graph = FALSE, session=session)
              check = grep("^ML_", names(obj_react$obj$features), value = TRUE)
              obj_react$obj = data_rm_features(obj = obj_react$obj, features = check, list_only = FALSE, adjust_graph = FALSE, session=session)
              obj_react$obj = reinit_layout(obj_react$obj)
            } else {
              obj_react$obj = data_rm_pops(obj = obj_react$obj,pops = grep("^ML_subset|^ML_meta_|^ML_pred_", names(obj_react$obj$pops), value = TRUE),
                                           list_only = FALSE, session=session)
              check = grep("^ML_", names(obj_react$obj$features), value = TRUE)
              obj_react$obj = data_rm_features(obj = obj_react$obj, features = check, list_only = FALSE, session=session)
            }
          }, error = function(e) {
            obj_react$obj = obj_back
            obj_react$obj = reinit_layout(obj_react$obj)
            stop(e$message)
          })
        }
      }
      
      foo = list(remove_sys = input$features_sys == "yes",
                 remove_sat = input$features_sat == "yes",
                 remove_pix = input$features_pix == "yes",
                 remove_raw = input$features_raw == "yes",
                 remove_pat = "^ML_",
                 remove_nzv = input$features_nzv == "yes",
                 features_int = input$features_int == "yes",
                 features_inp = input$features_inp,
                 features_inm = input$features_inm,
                 features_har = input$features_har == "yes",
                 features_hap = "^H ",
                 features_ham = "yeojohnson",
                 features_cen = input$features_cen == "yes",
                 features_cem = mean,
                 features_scl = input$features_scl == "yes",
                 features_scm = sd,
                 features_cor = input$features_cor == "yes",
                 features_bst = input$features_bst,
                 features_kep = character(0))
      if(input$features_used == "manual") foo$features_kep = input$sel_right
      
      # apply pre-processing
      bar = do.call(what = preprocess, args = c(list(obj = obj_react$obj, pops = pops), foo))
      for(i in names(bar)) model_react[[i]] = bar[[i]]
      Q = checknames.IFCml(model_react)
      max_events = sum(!is.na(model_react$data[,Q$is_clust,drop=TRUE]))
      updateNumericInput(session = session, inputId = "training_sampling_dimred", label = paste0("number of events used for dimension reduction [max=",max_events,"]"), max = max_events, value = min(max_events, 4000))
      runjs(sprintf("Shiny.onInputChange('training_go', %i)", ifelse(length(input$training_go) == 0, 0, input$training_go + 1L)))
      # shinyjs::click("training_go")
    }, error = function(e) {
      updateTabsetPanel(session = session, "navbar_ML", selected = "ML_inputs")
      runjs(code = "$('#navbar_ML [data-value=\"ML_inputs\"]').trigger('click');")
      mess_global(title = "data pre-processing", msg = strsplit(paste0("Can't pre-process data with input parameters:\n", e$message), split = "\n")[[1]], type = "stop")
    }, finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "", reset = TRUE)
      return(NULL)
    })
  }),
  # abort training because user decided not to remove former ML
  retrain_abort = observeEvent(input$retrain_abort, suspended = TRUE,{
    obs_ML$retrain_ok$suspend()
    obs_ML$retrain_abort$suspend()
    removeModal(session = session)
    updateTabsetPanel(session = session, "navbar_ML", selected = "ML_inputs")
    runjs(code = "$('#navbar_ML [data-value=\"ML_inputs\"]').trigger('click');")
  }),
  # Clustering observers to control number of clusters in unsupervised ML
  observeEvent({
    list(input$kmeans_go,
         input$MetaClustering_go)
  }, suspended = TRUE,{
    if(input$navbar_ML != "ML_training") return(NULL)
    if(length(model_react$config$pops) != 0) runjs(sprintf("Shiny.onInputChange('training_go', %i)", ifelse(length(input$training_go) == 0, 0, input$training_go + 1L)))
      # shinyjs::click("training_go")
  }),
  # Control visibility of sampling parameters
  # it will be visible only when supervised ML is used
  observeEvent(input$training_sampling, suspended = TRUE,{
    toggleElement(id = "training_sampling_param", condition = input$training_sampling)
    if(length(model_react$ratio) == 0 || length(input$training_ratio) == 0) return(NULL)
    if(model_react$ratio == input$training_ratio) {
      removeClass(selector = "label[for=training_sampling]", class = "switchme")
    } else {
      addClass(selector = "label[for=training_sampling]", class = "switchme")
    }
  }),
  # Allow to re-sample the data
  # On resampling a new fitting will be triggered
  observeEvent(input$training_resample, suspended = TRUE,{
    if(input$navbar_ML != "ML_training") return(NULL)
    # TODO should we inform user that former ML will be cleared ?
    # ML_ should not have been used anywhere else because if user leave ML_training tab to use it
    # a check_start is triggered
    runjs(sprintf("Shiny.onInputChange('check_start', %i)", ifelse(length(input$check_start) == 0, 0, input$check_start + 1)))
  }),
  # Allow to re-sample the data
  # On param tweakin a new fitting will be triggered
  observeEvent(input$training_tweak, suspended = TRUE, {
    if(input$navbar_ML != "ML_training") return(NULL)
    # TODO should we inform user that former ML will be cleared ?
    # ML_ should not have been used anywhere else because if user leave ML_training tab to use it
    # a check_start is triggered
    # removeClass(id = "training_tweak", class = "clickme")
    # disable(id = "training_tweak")
    runjs(sprintf("Shiny.onInputChange('check_start', %i)", ifelse(length(input$check_start) == 0, 0, input$check_start + 1)))
  }),
  observeEvent(input$training_ratio, suspended = TRUE, {
    if(length(model_react$ratio) == 0) return(NULL)
    if(input$training_ratio == model_react$ratio) {
      removeClass(id = "training_resample", class = "clickme")
      removeClass(selector = "label[for=training_sampling]", class = "switchme")
    } else {
      addClass(id = "training_resample", class = "clickme")
      addClass(selector = "label[for=training_sampling]", class = "switchme")
    }
  }),
  observeEvent(input$training_sampling_dimred, suspended = TRUE, {
    if(!(input$training_model %in% c("umap","tsne"))) return(NULL)
    if(length(model_react$idx) == 0) return(NULL)
    if(length(na.omit(as.integer(input$training_sampling_dimred))) == 0) return(NULL)
    Q = checknames.IFCml(model_react)
    val = sum(!is.na(model_react$data[,Q$is_clust,drop=TRUE]))
    if(input$training_sampling_dimred > val) {
      updateNumericInput(session=session, inputId = "training_sampling_dimred", value=val)
      return(NULL)
    } 
    if(input$training_sampling_dimred < 1) {
      updateNumericInput(session=session, inputId = "training_sampling_dimred", value=1)
      return(NULL)
    } 
    runjs(sprintf("Shiny.onInputChange('training_go', %i)", ifelse(length(input$training_go) == 0, 0, input$training_go + 1L)))
    # shinyjs::click("training_go")
  }),
  # observer to control the visibility of current algorithm hyper-parameter
  observeEvent(input$training_param, suspended = TRUE,{
    sapply(c("pca","tsne","umap","som","em","svm","xgb","lda","bottom"), FUN = function(x) hideElement(id = paste0("training_param_",x)))
    if(input$training_param) {
      showElement(id = paste0("training_param_",input$training_model))
      showElement(id = "training_param_bottom")
    } 
  }),
  # observer on algorithm used
  # on change, training hyper-parameters, clustering and sampling menu will be reduced
  # in addition a new fitting will be triggered
  observeEvent(input$training_model, suspended = TRUE, {
    if(input$navbar_ML != "ML_training") return(NULL)
    updateMaterialSwitch(session=session, inputId = "training_param", value = FALSE)
    updateMaterialSwitch(session=session, inputId = "training_meta", value = FALSE)
    updateMaterialSwitch(session=session, inputId = "training_sampling", value = FALSE)
    hideElement(id = "training_kmeans_meta_all")
    if(input$training_model %in% c("pca","umap","tsne")) {
      hideElement(id="training_supervised")
    } else {
      showElement(id="training_supervised")
    }
    if(input$training_model %in% c("umap","tsne")) {
      showElement(id="training_dimred")
      if(input$training_model %in% "umap") showElement(id = "training_kmeans_meta_all")
    } else {
      hideElement(id="training_dimred")
    }
    # model parameters are reset to their default values every time training model selection is changed
    sapply(match_model(), FUN = function(x) reset(id = x))
    # we need to postpone going to training go to ensure that all model parameters havec been cleared
    k = input$training_go
    if(length(model_react$config$pops) != 0) onFlushed(once = TRUE, fun = function() {
      runjs(sprintf("Shiny.onInputChange('training_go', %i)", ifelse(length(k) == 0, 0L, k + 1L)))
      # shinyjs::click("training_go")
    })
  }),
  # observer on clustering algorithm used
  # for all unsupervised algorithm, it will be kmeans
  observeEvent(input$training_meta, suspended = TRUE,{
    hideElement(id = "training_kmeans_meta")
    if(input$training_meta) {
      if(input$training_model %in% c("pca", "tsne", "umap", "som")) {
        showElement("training_kmeans_meta")
      }
    }
  }),
  # observer to reset current algorithm hyper parameter to their default value
  observeEvent(input$training_param_reset, suspended = TRUE,{
    if(input$navbar_ML != "ML_training") return(NULL)
    sapply(match_model(), FUN = function(x) reset(id = x))
  }),
  # observer to inspect if current algorithm hyper parameter is different from default value
  # if so, shinyFeedback is used to display default value
  observeEvent({
    sapply(match_model(), FUN = function(x) input[[x]])
  }, suspended = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE, {
    if((length(input$navbar) == 0) || (input$navbar != "tab5")) return(NULL)
    has_changed = sapply(match_model(), FUN = function(x) (length(model_react$param[[x]]) == 0) || identical(input[[x]], model_react$param[[x]]))
    if(all(has_changed)) {
      removeClass(id = "training_tweak", class = "clickme")
      removeClass(selector = "label[for=training_param]", class = "switchme")
    } else {
      if(any(sapply(match_model(), length) == 0)) {
        removeClass(id = "training_tweak", class = "clickme")
        removeClass(selector = "label[for=training_param]", class = "switchme")
      } else {
        addClass(id = "training_tweak", class = "clickme")
        addClass(selector = "label[for=training_param]", class = "switchme")
      }
    }
  }),
  # observer to reset kmeans clustering
  observeEvent(input$kmeans_reset, suspended = TRUE,{
    N = names(input)
    to_reset = setdiff(N[grep("^kmeans_", N)], c("kmeans_reset", "kmeans_go"))
    sapply(to_reset, FUN = function(x) reset(id = x))
  }),
  # observer to show / hide relevant hyperparameter for svm 
  observeEvent(input$svm_type, suspended = TRUE,{
    if(input$svm_type %in% c("nu-classification", "nu-regression", "one-classification")) {showElement("svm_nu")} else {hideElement("svm_nu")}
  }),
  # observer to show / hide relevant hyperparameter for svm 
  observeEvent(input$svm_kernel,suspended = TRUE, {
    if(input$svm_kernel %in% c("polynomial", "sigmoid")) {showElement("svm_coef0")} else {hideElement("svm_coef0")}
    if(input$svm_kernel == "polynomial") {showElement("svm_degree")} else {hideElement("svm_degree")}
    if(input$svm_kernel == "linear") {hideElement("svm_gamma")} else {showElement("svm_gamma")}
  }),
  # observer to show / hide relevant hyperparameter for lda
  observeEvent(input$lda_method, suspended = TRUE,{
    if(input$lda_method == "t") {showElement(id = "lda_nu")} else {showElement(id = "lda_nu")}
  }),
  observeEvent(input$training_go, suspended = TRUE, {
    if(input$navbar != "tab5") return(NULL)
    if(input$navbar_ML != "ML_training") return(NULL)
    if(length(model_react$config$pops) == 0) {
      mess_global(title = "building model", msg = c("Please select population(s)", "- one, for unsupervised ML", "- at least two, for supervised ML"), type = "info")
      return(NULL)
    }
    method = input$training_model
    add_log(paste("fitting data:",method))
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Fitting data", reset = FALSE)
    obj_back = obj_react$obj
    model_ = reactiveValuesToList(model_react)
    old_l = length(obj_react$obj$graphs)
    tryCatch({
      # at first we check model
      Q = checknames.IFCml(model_)
      # we split the data into 2 sets, one for training and one for testing
      # this only applies when more than 2 populations where used 
      # in such case we use supervised ML otherwise, if only one pop is given.
      # we override ratio with 1 (meaning that we use the whole data for training only)
      # except for umap and tsne, that can be long  so we allow subseting from input population(s)
      ratio = input$training_ratio
      if(length(model_$config$pops) == 1) {
        hideElement(id="training_supervised")
        ratio = 1
      }
      if(method %in% c("pca")) ratio = 1
      if(method %in% c("tsne", "umap")) ratio = input$training_sampling_dimred / sum(!is.na(model_$data[,Q$is_clust,drop=TRUE]))
      # split data
      model_ = splitdata.IFCml(model = model_, ratio = ratio, session = session)
      # retrieve fit parameters
      model_$param <- sapply(match_model(x = method), simplify = FALSE, USE.NAMES = TRUE, FUN = function(x) input[[x]])
      # fit data
      model_ = fit.IFCml_set(model = model_, method = method, session = session)
      # set meta_args and apply model
      if(length(model_$config$pops) == 1) {
        meta_args <- list(centers = input$kmeans_centers,
                          iter.max = input$kmeans_iter_max,
                          nstart = input$kmeans_nstart,
                          algorithm = input$kmeans_algorithm)  
        newdata = input$kmeans_all == "yes"
        model_$meta_args <- meta_args
      } else {
        newdata = 0L
      }
      nv = apply.IFCml(obj = obj_react$obj, model = model_, mode = "self", 
                       newdata = c("train","all","test")[newdata + 1L],
                       self_split = FALSE, session = session, verbose = FALSE)
      # copy model
      for(i in names(nv$model)) model_react[[i]] <- nv$model[[i]]
      # copy obj
      obj_react$obj <- compare(nv$obj, obj_react$obj)
      
      # redefine plots
      h = 600; w = h
      lev = levels(model_$data[,Q$is_clust,drop=TRUE])
      switch(model_react$method, 
             "xgb" = { w = 2 * h }
      )
      if(method == "svm") {
        hideElement("training_plot_placeholder")
      } else {
        showElement("training_plot_placeholder")
        output$training_plot_placeholder <- renderUI({plotOutput(outputId = "training_plot", width = w, height = h)})
      }
      if(any(c(method == c("pca", "tsne", "umap"), length(model_react$config$pops) == 1))) {
        hideElement("training_matrix_placeholder")
      } else {
        showElement("training_matrix_placeholder")
        output$training_matrix_placeholder <- renderUI({plotOutput(outputId = "training_matrix", width = h, height = h)})
      }
      
      # remove clickme / switchme
      removeClass(id = "training_resample", class = "clickme")
      removeClass(selector = "label[for=training_sampling]", class = "switchme")
      removeClass(selector = "label[for=training_param]", class = "switchme")
      removeClass(id = "training_tweak", class = "clickme")
      
      # reinit layout
      if(old_l > length(obj_react$obj$graphs)) obj_react$obj = reinit_layout(obj_react$obj)
    }, error = function(e) {
      obj_react$obj = obj_back
      runjs(sprintf("Shiny.onInputChange('training_param_reset', %i)", ifelse(length(input$training_param_reset) == 0, 0, input$training_param_reset + 1L)))
      # shinyjs::click("training_param_reset")
      mess_global(title = "building model", msg = c("Can't fit data with input parameters:", e$message,
                                                    "Tips: Try to tweak model parameters",
                                                    "Tips: Try to include more cells",
                                                    "Tips: tweak pre-processing step"), type = "stop")
    }, finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "", reset = TRUE)
    })
  }),
  observeEvent(list(input[[paste0(model_react$method, "_seed")]],
                    input$kmeans_centers,
                    input$kmeans_iter_max,
                    input$kmeans_nstart,
                    input$kmeans_algorithm,
                    input$MetaClustering_method,
                    input$MetaClustering_max,
                    input$MetaClustering_all,
                    input$kmeans_all), suspended = TRUE, {
                      if(input$navbar != "tab5") return(NULL)
                      if(input$navbar_ML != "ML_training") return(NULL)
                      if(length(model_react$fit) == 0) return(NULL)
                      seed = NULL
                      if((length(input[[paste0(model_react$method, "_seed")]]) != 0) && !is.na(input[[paste0(model_react$method, "_seed")]])) seed = input[[paste0(model_react$method, "_seed")]]
                      set.seed(seed)
                      on.exit(set.seed(NULL), add = TRUE)
                      obj_back = obj_react$obj 
                      tryCatch({
                        model_ = reactiveValuesToList(model_react)
                        if(length(model_$config$pops) == 1) {
                          meta_args <- list(centers = input$kmeans_centers,
                                            iter.max = input$kmeans_iter_max,
                                            nstart = input$kmeans_nstart,
                                            algorithm = input$kmeans_algorithm) 
                          newdata = input$kmeans_all == "yes"
                          model_$meta_args <- meta_args
                        } else {
                          newdata = 0L
                        }
                        nv = apply.IFCml(obj = obj_react$obj, model = model_, mode = "self",
                                         newdata = c("train","all","test")[newdata + 1L],
                                         self_split = FALSE, session = session, verbose = FALSE)
                        # copy model
                        for(i in names(nv$model)) model_react[[i]] <- nv$model[[i]]
                        # copy obj
                        obj_react$obj <- compare(nv$obj, obj_react$obj)
                      }, error = function(e) {
                        obj_react$obj = obj_back
                        runjs(sprintf("Shiny.onInputChange('training_param_reset', %i)", ifelse(length(input$training_param_reset) == 0, 0, input$training_param_reset + 1L)))
                        # shinyjs::click("training_param_reset")
                        mess_global(title = "creating metaclustering", msg = c("Can't create metaclusters:", e$message), type = "stop")
                      })
                    }))

output$training_summary <- renderPrint({
  if(input$navbar != "tab5") return(NULL)
  if(input$navbar_ML != "ML_training") return(NULL)
  if(!identical(input$training_model, model_react$method)) return(NULL)
  summary(model_react$fit$fit)
})

output$training_features <- renderPrint({
  if(input$navbar != "tab5") return(NULL)
  if(input$navbar_ML != "ML_training") return(NULL)
  if(!identical(input$training_model, model_react$method)) return(NULL)
  tryCatch({
    Q = checknames.IFCml(reactiveValuesToList(model_react))
    cat(paste("Features used for the model:\n", paste0(paste("\t-",Q$is_feat,collapse = "\n"))))
  }, error = function(e) {
    return(e$message)
  })
})

output$training_matrix <- renderPlot(expr = {
  old_par=par("pty");par("pty"="s")
  on.exit(par("pty"=old_par))
  if(input$navbar != "tab5") return(NULL)
  if(input$navbar_ML != "ML_training") return(NULL)
  if(!identical(input$training_model, model_react$method)) return(NULL)
  if(length(model_react$idx) == 0) return(NULL)
  if(length(model_react$fit) == 0) return(NULL)
  if(any(c(model_react$method == c("pca", "tsne", "umap"), length(model_react$config$pops) == 1))) {
    output$training_summary <- renderPrint(summary(model_react$fit$fit))
    return(NULL)
  } else {
    tryCatch({
      model_ = predict.IFCml_fit(reactiveValuesToList(model_react), newdata = "test", session = session)
      Q = checknames.IFCml(model_)
      sub_ = model_$sub
      y_test = model_$data[sub_, Q$is_clust, drop = TRUE]
      conf = caret::confusionMatrix(data = model_$pred, reference = y_test) 
      output$training_summary <- renderPrint(conf)
      plot_conf(confusion_matrix = conf, c(table(y_test)))
    },
    error = function(e) {
      mess_global(title = "confusion matrix", msg = c("problem during plot:", e$message), type = "stop")
      output$training_summary <- renderPrint(NULL)
      return(NULL)
    })
  }
})

output$training_plot <- renderPlot(expr = {
  if(input$navbar != "tab5") return(NULL)
  if(input$navbar_ML != "ML_training") return(NULL)
  if(!identical(input$training_model, model_react$method)) return(NULL)
  if(length(model_react$fit) == 0) return(NULL)
  
  seed = NULL
  if((length(input[[paste0(model_react$method, "_seed")]]) != 0) && !is.na(input[[paste0(model_react$method, "_seed")]])) seed = input[[paste0(model_react$method, "_seed")]]
  set.seed(seed)
  on.exit(set.seed(NULL), add = TRUE)
  
  Q = checknames.IFCml(model_react)
  fit = model_react$fit$fit
  y_train = model_react$fit$y_train
  lev = levels(model_react$data[,Q$is_clust,drop=TRUE])
  h = max(600, 200*(length(lev)-1))
  
  model_ <- reactiveValuesToList(model_react)
  tryCatch({
    sub_ = model_$sub
    if(length(lev) == 1) {
      V = model_$clust 
      L = nlevels(as.factor(V))
      pal = c("CornflowerBlue", "IndianRed", "DarkTurquoise", "Gold", "Cyan4", "DarkOrchid", "Green4", "DeepPink", "Tomato")
      lightModeColor = sapply(pal, FUN = function(x) IFC::paletteIFC(col = x, "to_light")[1,2])
      pch = rep(c(20, 4, 3, 1, 5, 0, 2, 18, 15, 17), length.out = L)
      col = rep(lightModeColor, length.out = L)
      lab = sprintf("ML_meta_%02i",as.integer(levels(as.factor(V))))
    } else {
      V = y_train
      pch = sapply(obj_react$obj$pops[lev], FUN = function(p) map_style(p$style, toR = TRUE))
      col = sapply(obj_react$obj$pops[lev], FUN = function(p) map_color(p$lightModeColor, toR = TRUE))
      lab = lev
    }
    switch(model_react$method,
           "pca" = {
             proj_all = predict.IFCml_fit(reactiveValuesToList(model_react), newdata = "all", session = session)$proj
             dat = model_$proj
             ylab = "pca_2_extra"
             if(ncol(dat) == 1) {
               dat = cbind(dat, dat)
               ylab = "pca_1_extra"
             }
             rasterplot(x = dat[, 1], y = dat[, 2], main = "PCA dimension reduction",
                  xlim = range(proj_all[, 1], finite = TRUE), ylim = range(proj_all[, 2], finite = TRUE),
                  pch = pch[V],
                  col = col[V],
                  xlab = "pca_1_extra", ylab = ylab)
             legend("topleft", legend = sapply(lab, center_short), pch=pch, col=col,
                    xpd=TRUE, horiz=FALSE, inset = 0.025, 
                    cex = 0.6, bg = "#ADADAD99", 
                    pt.cex = 1, bty = "o", box.lty = 0)
           },
           "tsne" = {
             # proj_all, no projection on all dataset can be done
             dat = fit$Y[, , drop=FALSE]
             ylab = "tSNE_2"
             sub1 = !apply(model_react$pca$x, 1, anyNA)
             if(ncol(dat) == 1) { dat = cbind(dat, dat); ylab = "tSNE_1" }
             rasterplot(x = dat[,1,drop=TRUE], y = dat[,2,drop=TRUE], main = "t-SNE dimension reduction",
                  xlim = range(dat[, 1], finite = TRUE),
                  ylim = range(dat[, 2], finite = TRUE),
                  pch = pch[V[sub1]],
                  col = col[V[sub1]],
                  xlab = "tSNE_1", ylab = ylab)
             legend("topleft", legend = sapply(lab, center_short), pch=pch, col=col,
                    xpd=TRUE, horiz=FALSE, inset = 0.025, 
                    cex = 0.6, bg = "#ADADAD99", 
                    pt.cex = 1, bty = "o", box.lty = 0)
           },
           "umap" = {
             proj_all = predict.IFCml_fit(reactiveValuesToList(model_react), newdata = "all", session = session)$proj
             rasterplot(x = model_$proj[,1,drop=TRUE], y = model_$proj[,2,drop=TRUE], main = "UMAP dimension reduction",
                  xlim = range(proj_all[, 1,drop=TRUE], finite = TRUE),
                  ylim = range(proj_all[, 2,drop=TRUE], finite = TRUE),
                  pch = pch[V],
                  col = col[V],
                  xlab = "umap_1", ylab = "umap_2")
             legend("topleft", legend = sapply(lab, center_short), pch=pch, col=col,
                    xpd=TRUE, horiz=FALSE, inset = 0.025,
                    cex = 0.6, bg = "#ADADAD99",
                    pt.cex = 1, bty = "o", box.lty = 0)
           },
           "som" = { 
             proj_all = predict.IFCml_fit(reactiveValuesToList(model_react), newdata = "all", session = session)$proj
             rasterplot(x = model_$proj[,1,drop=TRUE], y = model_$proj[,2,drop=TRUE], main = "SOM MST embedding",
                        xlim = range(proj_all[, 1,drop=TRUE], finite = TRUE),
                        ylim = range(proj_all[, 2,drop=TRUE], finite = TRUE),
                        pch = pch[V],
                        col = col[V],
                        xlab = "som_1", ylab = "som_2")
             legend("topleft", legend = sapply(lab, center_short), pch=pch, col=col,
                    xpd=TRUE, horiz=FALSE, inset = 0.025,
                    cex = 1, bg = "#ADADAD99",
                    pt.cex = 1, bty = "o", box.lty = 0)
           },
           "em" = { 
             dr = mclust::MclustDR(fit, lambda = 1)
             dims = dr$dir
             if(!is.matrix(dims) || (ncol(dims) < 2)) dims = cbind(dims, dims)
             pairs(dims, lower.panel = panel_pair, upper.panel =  panel_pair,
                   label.pos = 0.5, diag.panel = function(x, ...) {
                     mfg <- par("mfg")
                     if(mfg[1] == 1 && mfg[2] == 1) {
                       legend("topleft", legend = sapply(lab, center_short), pch=pch, col=col,
                              horiz=FALSE, "xpd"=TRUE, bty="o", inset = 0.025,
                              cex = 1, bg = "#ADADAD",
                              pt.cex = 1,  box.lty = 0)
                     }
                   },
                   pch = pch[V],
                   col = col[V])
           },
           "svm" = {
             return(NULL)
           },
           "xgb" = { 
             nn = xgboost::xgb.importance(Q$is_feat, model = fit)
             maxchar = 20
             toolong = sapply(nn$Feature, FUN = function(x) ifelse(nchar(x) > maxchar, "...", ""))
             nn$Feature <- paste(substring(nn$Feature, 1, maxchar), toolong, sep = "")
             xgboost::xgb.plot.importance(importance_matrix = nn, top_n = min(length(nn$Feature), 10), left_margin = max(nchar(nn$Feature[1:min(length(nn$Feature), 10)])) / 2.5)
           },
           "lda" = {
             if(!is.matrix(fit$scaling) || (ncol(fit$scaling) <= 2)) {
               plot(fit, panel = panel_pair,
                    pch = pch[V],
                    col = col[V]
               )
               legend("topleft", legend = sapply(lab, center_short), pch=pch, col=col,
                      xpd=TRUE, horiz=FALSE, inset = 0.025,
                      cex = 1, bg = "#ADADAD99",
                      pt.cex = 1, bty = "o", box.lty = 0)
             } else{
               plot(fit, panel = panel_pair, 
                    label.pos = 0.5, diag.panel = function(x, ...) {
                      mfg <- par("mfg")
                      if(mfg[1] == 1 && mfg[2] == 1) {
                        legend("topleft", legend = sapply(lab, center_short), pch=pch, col=col,
                               horiz=FALSE, "xpd"=TRUE, bty="o", inset = 0.025,
                               cex = 1, bg = "#ADADAD",
                               pt.cex = 1,  box.lty = 0)
                      }
                    },
                    pch = pch[V],
                    col = col[V]
               )
             }
           }
    )
  }, error = function(e) {
    # mess_global(title = "training plot", msg = c("problem during plot:", e$message), type = "stop")
    return(NULL) 
  })
})