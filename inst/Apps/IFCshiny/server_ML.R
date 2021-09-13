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
    sub = apply(g, 1, any)
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
    model_react$pops <- input$populations_training
    chc = c("pca","tsne","umap","flowsom")
    hideElement("training_unsupervised")
    if(length(input$populations_training) == 1) {
      hideElement(id = "features_best_length_ctn")
      showElement("training_unsupervised")
    } else {
      chc = c(chc, "em","svm","xgb","lda")
      showElement(id = "features_best_length_ctn")
    }
    updateSelectInput(session = session, inputId = "training_model", choices = chc, selected = "pca")
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
      updateSelectInput(session, "sel_left", choices="", selected = NULL)
      return(NULL)
    } else {
      hideFeedback(session = session, inputId = "pattern")
    }
    if(l == 0) {
      output$features_infos <- renderText({
        "No features with this pattern"
      })
      updateSelectInput(session, "sel_left", choices="", selected = NULL)
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
      updateSelectInput(session = session, inputId = "sel_right", choices=feat_react$selected)
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
      model_react$pops = input$populations_training
      pops = model_react$pops
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
      check = grep("ML_subset", names(obj_react$obj$pops), value = TRUE)
      if(length(check) != 0) {
        check = c(check, grep("^ML_meta_", names(obj_react$obj$pops), value = TRUE))
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
      pops = model_react$pops
      check = grep("ML_subset", names(obj_react$obj$pops), value = TRUE)
      if(length(check) != 0) {
        check = c(check, grep("^ML_meta_", names(obj_react$obj$pops), value = TRUE))
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
              obj_react$obj = data_rm_pops(obj = obj_react$obj, pops = "ML_subset", list_only = FALSE, adjust_graph = FALSE, session=session)
              check = grep("^ML_", names(obj_react$obj$features), value = TRUE)
              obj_react$obj = data_rm_features(obj = obj_react$obj, features = check, list_only = FALSE, adjust_graph = FALSE, session=session)
              obj_react$obj = reinit_layout(obj_react$obj)
            } else {
              obj_react$obj = data_rm_pops(obj = obj_react$obj, pops = "ML_subset", list_only = FALSE, session=session)
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
      bar = do.call(what = preprocess, args = c(list(obj = obj_react$obj, pops = model_react$pops), foo))
      for(i in names(bar)) model_react[[i]] = bar[[i]]
      model_react$data$clust = as.factor(model_react$data$clust)
      click("training_go")
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
    if(length(model_react$pops) != 0) click("training_go")
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
  # observer to control the visibility of current algorithm hyper-parameter
  observeEvent(input$training_param, suspended = TRUE,{
    sapply(c("pca","tsne","umap","flowsom","em","svm","xgb","lda","bottom"), FUN = function(x) hideElement(id = paste0("training_param_",x)))
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
    toggleElement(id="training_supervised", condition= !(input$training_model %in% c("pca","umap","tsne")))
    # model parameters are reset to their default values every time training model selection is changed
    sapply(match_model(), FUN = function(x) reset(id = x))
    # we need to postpone going to training go to ensure that all model parameters havec been cleared
    if(length(model_react$pops) != 0) onFlushed(once = TRUE, fun = function() {
      click("training_go")
    })
  }),
  # observer on clustering algorithm used
  # for all unsupervised algorithm, except flowsom, it will be kmeans
  # for flowsom, it will be a kmeans customized in flowsom 
  # other flowsom algorithm are not used
  # TODO, investigate more on implementing the other flowsom metaClustering algorithm
  # but kmean is fast and I had faulty results with other method
  observeEvent(input$training_meta, suspended = TRUE,{
    hideElement(id = "training_flowsom_meta")
    hideElement(id = "training_kmeans_meta")
    if(input$training_meta) {
      if(input$training_model %in% c("pca", "tsne", "umap")) {
        showElement("training_kmeans_meta")
      } else {
        showElement("training_flowsom_meta")
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
  # observer to reset meta clustering (for flowsom)
  observeEvent(input$MetaClustering_reset,suspended = TRUE, {
    N = names(input)
    to_reset = setdiff(N[grep("^MetaClustering_", N)] , c("MetaClustering_reset", "MetaClustering_go"))
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
  # observer to trigger a new fitting on the already pre-processed data
  # it will also reactualize modelling details, confusion matrix (for supervised),
  # training plot (if available), and features used for modeling
  # training plot will reacts (without recomputing the model) on clustering modification (for unsupervised)
  # - whatever the model choosen a pca will always be computed 
  # - in addition, tsne will always use pca as input and not the raw data directly
  # for the other algorithms the raw data are always used
  # - finally, ML_ pops and features will be added to obj_react$obj
  # pops are the subset used for fitting
  # pops are also the clusters identified in unsupervised ML
  # features are the dimension projection pca lda som tsne where it applies
  observeEvent(input$training_go, suspended = TRUE, {
    if(input$navbar_ML != "ML_training") return(NULL)
    if(length(model_react$pops) == 0) {
      mess_global(title = "building model", msg = c("Please select population(s)", "- one, for unsupervised ML", "- at least two, for supervised ML"), type = "info")
      return(NULL)
    }
    add_log("fitting data")
    model_react$success <- FALSE
    mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "Fitting data", reset = FALSE)
    obj_back = obj_react$obj
    tryCatch({
      # we identify the subset as the population where the cells uniquely belongs
      # see pre-processing, as a result model_react$data$clust contains this unique population
      # or NA when the cell belong to more than one population
      idx_train = !is.na(model_react$data$clust)
      SUB = model_react$data[idx_train, ]
      
      # we split the data into 2 sets, one for training and one for testing
      # this only applies when more than 2 populations where used 
      # in such case we use supervised ML otherwise, if only one pop is given.
      # we override ratio with 1 (meaning that we use the whole data for training only)
      ratio = input$training_ratio
      if(input$training_model %in% c("pca", "tsne", "umap")) ratio = 1
      model_react$ratio = ratio
      if(input$training_model == "flowsom") {
        if(!all(c(requireNamespace("FlowSOM", quietly = TRUE), requireNamespace("flowCore", quietly = TRUE)))) {
          mess_global(title = "package required", msg = c("'flowCore' and 'FlowSOM' packages are required to build Self Organizing Map",
                                                          "Please install 'flowCore' and 'FlowSOM' from bioconductor first",
                                                          "install.packages('BiocManager')",
                                                          "BiocManager::install(c('flowCore','FlowSOM'))"), type = "info")
          updateSelectInput(session = session, inputId = "training_model", label = "model", choices = c("pca","tsne","umap","em","svm","xgb","lda"), selected = "pca")
          return(NULL)
        }
        if(length(model_react$pops) == 1) ratio = 1
      } 
      idx = createDataPartition(SUB$clust, p = ratio, list = FALSE)
      idx_train[!idx] <- FALSE
      model_react$idx = idx
      
      train = SUB[idx, ]
      train$clust = factor(train$clust, levels = levels(model_react$data$clust))
      model_react$train = train
      y_train = train$clust
      train = train[, names(train) != "clust"]
      
      test = SUB[-idx, ]
      test$clust = factor(test$clust, levels = levels(model_react$data$clust))
      model_react$test = test
      y_test = test$clust
      test = test[, names(test) != "clust"]
      
      # we init returned variables
      # in addition, the whole modelling and its input parameters will be stored in model_react
      # this allows to use it afterward to predict on other data
      pop = NULL
      conf = NULL
      pred = NULL
      
      # a pca is always performed
      pca_args = list(x = train[, -1], center = FALSE, scale. = FALSE)
      if((input$training_model == "pca") && (length(input$pca_tol) != 0) && !is.na(input$pca_tol)) pca_args = c(pca_args, list(tol = input$pca_tol))
      pca_dr = do.call(what = prcomp, args = pca_args)
      proj_pca = predict(object = pca_dr, newdata = model_react$data[, names(train[, -1])])
      model_react$pca <- pca_dr
      
      # remove former pca
      obj_react$obj = suppressWarnings(data_rm_features(obj_react$obj, features = grep("^ML_pca_", names(obj_react$obj$features), value = TRUE, invert = FALSE), list_only = FALSE, session=session))
      
      # build ML_pca features representing the 9 first new pca dimensions  
      exported_feat = lapply(1:min(9, ncol(proj_pca)), FUN = function(i) {
        buildFeature(name = paste0("ML_pca_", i, "_extra"), val = proj_pca[,i])
      })
      
      # we show training plot, it will be hide for svm
      showElement("training_plot")
      
      # placeholder for clustering and features returned
      clust_NA = rep_len(NA, length.out = nrow(obj_react$obj$features))
      feat_NA = rep_len(NA, length.out = nrow(obj_react$obj$features))
      
      # function to recompute clusters 
      # it is called when clustering hyperparameters are changed
      # it will erase former ML_meta_ populations from obj_react$obj$pops
      # and create new ones
      rebuild_meta = function(clust) {
        pop = lapply(levels(as.factor(clust)), FUN = function(i_pop) {
          buildPopulation(name = sprintf("ML_meta_%02i", as.integer(i_pop)),
                          type = "T", 
                          obj = !is.na(clust) & clust == i_pop)
        })
        isolate({
          old_l = length(obj_react$obj$graphs)
          obj_react$obj = suppressWarnings(data_rm_pops(obj_react$obj, pops = grep("^ML_meta", names(obj_react$obj$pops), value = TRUE), list_only = FALSE, adjust_graph = FALSE, session = session))
          if(old_l > length(obj_react$obj$graphs)) obj_react$obj = reinit_layout(obj_react$obj)
          obj_react$obj = data_add_pops(obj_react$obj, pops = pop, session = session)
          for(i in pop) {attr(obj_react$obj$pops[[i$name]], "reserved") <- TRUE}
        })
        return(sapply(pop, FUN = function(p) p$name))
      }
      
      # depending on algorithm choose by user we will try to fit a model
      # for supervised ML (em, svm, xgb, lda, flowsom) we will compute the model and return
      # - model details, (TODO, sometimes returned object may not be highly informative e.g. svm, should we hide it or return other thing)
      # - model plot (excepted for svm and for xgb we return features importance plot)
      # - confusion matrix of the test set (i.e. expected vs predicted classes)
      # - features used (TODO should we present a table with Rd Ratio between each ?)
      # for unsupervised ML (pca, tsne, umap, flowsom) we will compute the model and return
      # - model details, (TODO, sometimes returned object may not be highly informative e.g. flowsom, should we hide it or return other thing)
      # -model plot, a reactive plot (to clustering) with the projection of the data (whole data or only the subset) in 
      # the dimension reduction computed by the model and the clusters identified
      # - features used
      switch(input$training_model, 
             "em" = {
               fit = MclustDA(data = train[, -1], 
                              modelNames = input$MclustDA_modelNames, modelType = input$MclustDA_modelType, G = input$MclustDA_G,
                              class = y_train)
               pred = predict(object = fit, newdata = test[, -1])
               conf = confusionMatrix(data = pred$classification, reference = y_test)
               output$training_plot <- renderPlot(plot(MclustDR(fit, lambda = 1), dimens = c(1:3), what = "pairs"))
             },
             "svm" = {
               svm_args = list(x = train[, -1],
                               type = input$svm_type, kernel = input$svm_kernel, degree = input$svm_degree,
                               coef0 = input$svm_coef0, cost = input$svm_cost, nu = input$svm_nu,
                               tolerance = input$svm_tolerance, epsilon = input$svm_epsilon,
                               scale = FALSE,  y = y_train)
               if((length(input$svm_gamma) !=0) && !is.na(input$svm_gamma)) svm_args = c(svm_args, list(gamma = input$svm_gamma))
               fit = do.call(what = svm, args = svm_args)
               pred = predict(object = fit, newdata = test[, -1])
               conf = confusionMatrix(data = pred, reference = y_test)
               output$training_plot <- renderPlot(NULL)
               hideElement("training_plot")
             },
             "xgb" = {
               fit = xgboost(data = as.matrix(train[, -1]), label = as.integer(y_train)-1,
                             params = list(
                               booster = input$xgb_booster,
                               eta = input$xgb_eta,
                               gamma = input$xgb_gamma,
                               max_depth = input$xgb_max_depth,
                               subsample = input$xgb_subsample,
                               colsample_bytree = input$xgb_colsample_bytree,
                               objective = input$xgb_objective,
                               eval_metric = input$xgb_eval_metric,
                               num_class = nlevels(y_train)
                             ),
                             nrounds=input$xgb_nrounds,
                             early_stopping_rounds=input$xgb_early_stopping_rounds,
                             verbose=0)
               
               pred = predict(fit, newdata = as.matrix(test[, -1]), reshape = TRUE)
               pred = factor(levels(y_train)[apply(pred, 1, which.max)], levels = levels(model_react$data$clust))
               conf = confusionMatrix(data = pred, reference = y_test)
               output$training_plot <- renderPlot({
                 nn = xgb.importance(colnames(train[, -1]), model = fit)
                 maxchar = 20
                 toolong = sapply(nn$Feature, FUN = function(x) ifelse(nchar(x) > maxchar, "...", ""))
                 nn$Feature <- paste(substring(nn$Feature, 1, maxchar), toolong, sep = "")
                 xgb.plot.importance(importance_matrix = nn, top_n = min(length(nn$Feature), 10), left_margin = max(nchar(nn$Feature[1:min(length(nn$Feature), 10)])) / 2.5)
               })
             },
             "lda" = {
               args_lda = list(x = train[, -1], 
                               method = input$lda_method, tol = input$lda_tol,
                               grouping = y_train)
               if(length(input$lda_nu != 0) && !is.na(input$lda_nu)) args_lda = c(args_lda, input$lda_nu)
               fit = do.call(what = lda, args = args_lda)
               pred = predict(object = fit, newdata = test[, -1])
               conf = confusionMatrix(data = pred$class, reference = y_test)
               pch = sapply(obj_react$obj$pops[levels(y_train)], FUN = function(p) p$style)
               col = sapply(obj_react$obj$pops[levels(y_train)], FUN = function(p) p$lightModeColor)
               output$training_plot <- renderPlot({
                 plot(fit, panel = panel_lda, pch = pch[as.character(y_train)], col = col[as.character(y_train)], oma=c(3,3,3,15))
                 legend("bottomright", legend = sapply(levels(y_train), center_short), pch=pch, col=col,
                        xpd=TRUE, horiz=FALSE, bty="n") 
               })
               # remove former lda
               obj_react$obj = suppressWarnings(data_rm_features(obj_react$obj, features = grep("^ML_lda_", names(obj_react$obj$features), value = TRUE, invert = FALSE), list_only = FALSE, session=session))
               proj_lda = predict(object = fit, newdata = model_react$data[, names(train[, -1])])
               exported_feat = c(exported_feat, lapply(1:min(99, ncol(proj_lda$x)), FUN = function(i) {
                 buildFeature(name = sprintf("ML_lda_%02i_extra", i), val = proj_lda$x[,i])
               }))
             },
             "flowsom" = {
               G = floor(sqrt(nrow(train)))
               seed = NULL
               if((length(input$flowsom_seed) !=0) && !is.na(input$flowsom_seed)) seed = input$flowsom_seed
               set.seed(seed)
               on.exit(set.seed(NULL))
               ff = new(structure("flowFrame", package="flowCore"), exprs = as.matrix(train[, -1]))
               SOM = FlowSOM::BuildSOM(FlowSOM::ReadInput(ff, compensate = FALSE, transform = FALSE, scale = FALSE), 
                              silent = TRUE, xdim = min(G, 10), ydim = min(G, 10))
               fit = suppressMessages(FlowSOM::BuildMST(fsom = SOM, silent = TRUE))
               # projects all data in fitted SOM
               col = sapply(obj_react$obj$pops[levels(y_train)], FUN = function(p) p$lightModeColor)
               ff = as.matrix(model_react$data[, names(train[, -1])])
               sub = !apply(ff, 1, anyNA)
               ff = new(structure("flowFrame", package="flowCore"), exprs = ff[sub, ])
               proj_som = FlowSOM::NewData(fit, ff, spillover = FALSE, transform = FALSE, scale = FALSE)
               feat_som = data.frame(proj_som$MST$l[proj_som$map$mapping[,1], ],
                                     proj_som$MST$size[proj_som$map$mapping[,1]],
                                     model_react$data$clust[sub],
                                     seq_along(proj_som$map$mapping[,1]),
                                     proj_som$map$mapping[,1], stringsAsFactors = FALSE)
               # adds jittering
               ran = diff(apply(fit$MST$l, 2, range))
               ratio = ran[1]/ran[2]
               feat_som = data.frame(do.call(what = rbind, args = by(feat_som, feat_som[, 6], FUN = function(d) { 
                 cbind(x = rnorm(nrow(d), d[, 1], d[, 3]/25), y = rnorm(nrow(d), d[, 2], d[, 3]/25 / ratio), ids = d[, 5], clust = d[, 4], size = d[, 3])
               })))
               feat_som = feat_som[order(feat_som[, "ids"]), ]
               obj_react$obj = suppressWarnings(data_rm_features(obj_react$obj, features = grep("^ML_flowsom_", names(obj_react$obj$features), value = TRUE, invert = FALSE), list_only = FALSE, session=session))
               exported_feat = c(exported_feat, lapply(1:min(2, ncol(feat_som)), FUN = function(i) {
                 feat_NA[sub] <- feat_som[,i]
                 buildFeature(name = sprintf("ML_flowsom_%i_extra", i), val = feat_NA)
               }))
               if(nlevels(y_train) > 1) {
                 ff = new(structure("flowFrame", package="flowCore"), exprs = as.matrix(test[,-1]))
                 proj_test = FlowSOM::NewData(fit, ff, spillover = FALSE, transform = FALSE, scale = FALSE)
                 map = by(fit$map$mapping[, 1], y_train, FUN = function(x) {
                   proj_test$map$mapping[, 1] %in% x
                 })
                 map = sapply(map, FUN = function(x) x)
                 N = colnames(map)
                 pred = apply(map, 1, FUN = function(x) {
                   foo = which(x)
                   if(length(foo) == 1) return(N[foo])
                   return(NA)
                 })
                 pred = factor(pred, levels = levels(model_react$data$clust))
                 conf = confusionMatrix(data = pred, reference = y_test)
               }
               output$training_plot <- renderPlot({
                 if(length(seed) != 0) {
                   set.seed(1)
                   on.exit(set.seed(NULL))
                 }
                 p = NULL
                 # the tryCatch is here because some metaClustering methods failed to identify clusters
                 # these method have been removed and only kmeans is used
                 # should we re-add them ?
                 tryCatch({ 
                   if(nlevels(y_train) > 1) {
                     p = FlowSOM::PlotPies(fit, cellTypes = y_train, colorPalette = grDevices::colorRampPalette(col[levels(y_train)]), view = "MST")
                   } else {
                     clust = clust_NA
                     meta_args = list(input$MetaClustering_method, max = input$MetaClustering_max)
                     if(input$MetaClustering_method == "metaClustering_consensus") meta_args = c(meta_args, list(seed = seed))
                     if(input$MetaClustering_all == "yes") {
                       clust[sub] = do.call(what = FlowSOM::MetaClustering, args = c(list(data = proj_som$map$codes), meta_args))[proj_som$map$mapping[, 1]]-1
                     } else {
                       clust[idx_train] = do.call(what = FlowSOM::MetaClustering, args = c(list(data = fit$map$codes), meta_args))[fit$map$mapping[, 1]]-1
                     }
                     pop = rebuild_meta(clust)
                     if(input$MetaClustering_all == "yes") {
                       p = FlowSOM::PlotPies(fsom = proj_som, cellTypes = clust[sub], view = "MST")
                     } else {
                       p = FlowSOM::PlotPies(fsom = fit, cellTypes = clust[idx_train], view = "MST")
                     }
                   }
                 }, error = function(e) {}, finally = return(p))
               })
             },
             "tsne" = {
               sub1 = !apply(proj_pca, 1, anyNA)
               # TODO it would be fine to capture output so as to get progress to shiny
               fit = Rtsne(proj_pca[sub1, ], dims = 3,
                           perplexity = input$Rtsne_perplexity, theta = input$Rtsne_theta, max_iter = input$Rtsne_max_iter,
                           eta = input$Rtsne_eta, momentum = input$Rtsne_momentum, final_momentum = input$Rtsne_final_momentum,
                           exaggeration_factor = input$Rtsne_exaggeration_factor,
                           pca = FALSE, partial_pca = FALSE, normalize = FALSE, pca_center = FALSE, pca_scale = FALSE, verbose = FALSE)
               pch_l = sapply(obj_react$obj$pops[levels(y_train)], FUN = function(p) p$style)
               col_l = sapply(obj_react$obj$pops[levels(y_train)], FUN = function(p) p$lightModeColor)
               lab = levels(y_train)
               output$training_plot <- renderPlot({
                 set.seed(1)
                 on.exit(set.seed(NULL))
                 if(length(lab) == 1) {
                   clust = clust_NA
                   if(input$kmeans_all == "yes") {
                     sub = !apply(fit$Y, 1, anyNA)
                     clust[sub1[sub]] = kmeans(x = fit$Y[sub, ], centers = input$kmeans_centers, iter.max = input$kmeans_iter_max,
                                               nstart = input$kmeans_nstart, algorithm = input$kmeans_algorithm)$cluster-1
                     col = clust[sub1[sub]]
                     dat = fit$Y[sub, ]
                   } else {
                     clust[sub1 & idx_train] = kmeans(x = fit$Y[idx_train[sub1], ], centers = input$kmeans_centers, iter.max = input$kmeans_iter_max,
                                                      nstart = input$kmeans_nstart, algorithm = input$kmeans_algorithm)$cluster-1
                     col = clust[sub1 & idx_train]
                     dat = fit$Y[idx_train[sub1], ]
                   }
                   clust = factor(clust, levels = sort(unique(clust)))
                   
                   pch = rep_len(pch_l, length.out = length(col))
                   col_l = as.integer(levels(as.factor(clust)))
                   lab = c(lab, rebuild_meta(clust))
                 } else {
                   dat = fit$Y
                   pch = pch_l[y_train[sub1]]
                   col = col_l[y_train[sub1]]
                 }
                 if(ncol(dat) == 1) {
                   plot(x = dat[, 1], y = dat[, 1],
                        xlim = range(fit$Y[, 1], na.rm = TRUE), ylim = range(fit$Y[, 1], na.rm = TRUE),
                        pch = pch,
                        col = col,
                        xlab = "tSNE_1", ylab = "tSNE_1")
                   legend("topleft", legend = sapply(lab, center_short), pch=pch_l, col=col_l,
                          xpd=TRUE, horiz=FALSE, inset = 0.025, 
                          cex = 0.8, bg = "#ADADAD99", 
                          pt.cex = 1, bty = "o", box.lty = 0)
                 } else {
                   plot(x = dat[, 1], y = dat[, 2], 
                        xlim = range(fit$Y[, 1], na.rm = TRUE), ylim = range(fit$Y[, 2], na.rm = TRUE),
                        pch = pch,
                        col = col,
                        xlab = "tSNE_1", ylab = "tSNE_2")
                   legend("topleft", legend = sapply(lab, center_short), pch=pch_l, col=col_l,
                          xpd=TRUE, horiz=FALSE, inset = 0.025, 
                          cex = 0.8, bg = "#ADADAD99", 
                          pt.cex = 1, bty = "o", box.lty = 0)
                 }
               })
               # remove former tSNE
               obj_react$obj = suppressWarnings(data_rm_features(obj_react$obj, features = grep("^ML_tSNE_", names(obj_react$obj$features), value = TRUE, invert = FALSE), list_only = FALSE, session=session))
               feat_NA = rep_len(NA, length.out = nrow(obj_react$obj$features))
               exported_feat = c(exported_feat, lapply(1:min(3, ncol(fit$Y)), FUN = function(i) {
                 feat_NA[sub1] <- fit$Y[,i]
                 buildFeature(name = paste0("ML_tSNE_", i, "_extra"), val = feat_NA)
               }))
             },
             "umap" = {
               fit = umap(train[, -1], n_components = 3, method = "naive",
                          n_neighbors = input$umap_n_neighbors, metric = input$umap_metric, n_epochs = input$umap_n_epochs,
                          init = input$umap_init, min_dist = input$umap_min_dist, set_op_mix_ratio = input$umap_set_op_mix_ratio, 
                          local_connectivity = input$umap_local_connectivity, bandwidth = input$umap_bandwidth,
                          alpha = input$umap_alpha, gamma = input$umap_gamma, negative_sample_rate = input$umap_negative_sample_rate,
                          spread = input$umap_spread, knn_repeats = input$umap_knn_repeats, verbose = 2)
               sub1 = !apply(model_react$data[, names(train[, -1])], 1, anyNA)
               proj_umap = predict(object = fit, data = model_react$data[sub1, names(train[, -1])])
               pch_l = sapply(obj_react$obj$pops[levels(y_train)], FUN = function(p) p$style)
               col_l = sapply(obj_react$obj$pops[levels(y_train)], FUN = function(p) p$lightModeColor)
               lab = levels(y_train)
               output$training_plot <- renderPlot({
                 set.seed(1)
                 on.exit(set.seed(NULL))
                 if(length(lab) == 1) {
                   clust = clust_NA
                   if(input$kmeans_all == "yes") {
                     dat = proj_umap
                     sub = !apply(dat, 1, anyNA)
                     clust[sub1[sub]] = kmeans(x = dat[sub, ], centers = input$kmeans_centers, iter.max = input$kmeans_iter_max,
                                               nstart = input$kmeans_nstart, algorithm = input$kmeans_algorithm)$cluster-1
                     col = clust[sub1]
                   } else {
                     dat = fit$layout
                     clust[idx_train] = kmeans(x = dat, centers = input$kmeans_centers, iter.max = input$kmeans_iter_max,
                                               nstart = input$kmeans_nstart, algorithm = input$kmeans_algorithm)$cluster-1
                     col = clust[idx_train]
                   }
                   clust = factor(clust, levels = sort(unique(clust)))
                   pch = rep(pch_l, length.out = length(col))
                   col_l = as.integer(levels(as.factor(clust)))
                   lab = c(lab, rebuild_meta(clust))
                 } else {
                   dat = fit$layout
                   pch = pch_l[y_train]
                   col = col_l[y_train]
                 }
                 plot(x = dat[, 1], y = dat[, 2], 
                      xlim = range(proj_umap[, 1], na.rm = TRUE), ylim = range(proj_umap[, 2], na.rm = TRUE),
                      pch = pch, col = col, 
                      xlab = "umap_1", ylab = "umap_2")
                 legend("topleft", legend = sapply(lab, center_short), pch=pch_l, col=col_l,
                        xpd=TRUE, horiz=FALSE, inset = 0.025, 
                        cex = 0.8, bg = "#ADADAD99", 
                        pt.cex = 1, bty = "o", box.lty = 0)
               })
               # remove former umap
               feat_NA = rep_len(NA, length.out = nrow(obj_react$obj$features))
               obj_react$obj = suppressWarnings(data_rm_features(obj_react$obj, features = grep("^ML_umap_", names(obj_react$obj$features), value = TRUE, invert = FALSE), list_only = FALSE, session=session))
               exported_feat = c(exported_feat, lapply(1:min(9, ncol(proj_umap)), FUN = function(i) {
                 feat_NA[sub1] <- proj_umap[,i]
                 buildFeature(name = sprintf("ML_umap_%i_extra", i), val = feat_NA)
               }))
             },
             "pca" = {
               fit = pca_dr
               pch_l = sapply(obj_react$obj$pops[levels(y_train)], FUN = function(p) p$style)
               col_l = sapply(obj_react$obj$pops[levels(y_train)], FUN = function(p) p$lightModeColor)
               lab = levels(y_train)
               output$training_plot <- renderPlot({
                 set.seed(1)
                 on.exit(set.seed(NULL))
                 if(length(lab) == 1) {
                   clust = clust_NA
                   if(input$kmeans_all == "yes") {
                     dat = proj_pca
                     sub = !apply(dat, 1, anyNA)
                     clust[sub] = kmeans(x = dat[sub, ], centers = input$kmeans_centers, iter.max = input$kmeans_iter_max,
                                         nstart = input$kmeans_nstart, algorithm = input$kmeans_algorithm)$cluster-1
                     col = clust
                   } else {
                     dat = pca_dr$x
                     clust[idx_train] = kmeans(x = dat, centers = input$kmeans_centers, iter.max = input$kmeans_iter_max,
                                               nstart = input$kmeans_nstart, algorithm = input$kmeans_algorithm)$cluster-1
                     col = clust[idx_train] 
                   }
                   clust = factor(clust, levels = sort(unique(clust)))
                   
                   pch = rep(pch_l, length.out = length(col))
                   col_l = as.integer(levels(as.factor(clust)))
                   lab = c(lab, rebuild_meta(clust))
                 } else {
                   dat = fit$x
                   pch = pch_l[y_train]
                   col = col_l[y_train]
                 }
                 if(ncol(fit$x) == 1) {
                   plot(x = dat[, 1], y = dat[, 1], 
                        xlim = range(proj_pca[, 1], na.rm = TRUE), ylim = range(proj_pca[, 1], na.rm = TRUE),
                        pch = pch,
                        col = col,
                        xlab = "pca_1_extra", ylab = "pca_1_extra") 
                 } else {
                   plot(x = dat[, 1], y = dat[, 2], 
                        xlim = range(proj_pca[, 1], na.rm = TRUE), ylim = range(proj_pca[, 2], na.rm = TRUE),
                        pch = pch,
                        col = col,
                        xlab = "pca_1_extra", ylab = "pca_2_extra") 
                 }
                 legend("topleft", legend = sapply(lab, center_short), pch=pch_l, col=col_l,
                        xpd=TRUE, horiz=FALSE, inset = 0.025, 
                        cex = 0.8, bg = "#ADADAD99", 
                        pt.cex = 1, bty = "o", box.lty = 0)
               })
             })
      # former pca has already been removed
      # we can now add new features and new populations to obj_react$obj
      obj_react$obj = data_add_features(obj_react$obj, features = exported_feat, session = session)
      pop = c(list(buildPopulation(name = "ML_subset", type = "C",
                                   definition = paste0(model_react$pops, collapse = "|Or|"),
                                   color = "White", lightModeColor = "Black")))
      # we remove former ML_ # TODO, normally this has already been done ...
      old_l = length(obj_react$obj$graphs)
      obj_react$obj = suppressWarnings(data_rm_pops(obj_react$obj, pops = grep("^ML_subset", names(obj_react$obj$pops), value = TRUE), list_only = FALSE, adjust_graph = FALSE, session = session))
      if(old_l > length(obj_react$obj$graphs)) obj_react$obj = reinit_layout(obj_react$obj)
      obj_react$obj = data_add_pops(obj_react$obj, pops = pop, session = session)
      # we set ML_ population as reserved population.
      # this prevents name editiion / removal in the app
      for(i in pop) {attr(obj_react$obj$pops[[i$name]], "reserved") <- TRUE}
      
      # we actualize output and model_react
      model_react$name <- input$training_model
      model_react$fit <- fit
      if(length(conf) == 0) {
        if(any(c(input$training_model == c("tsne", "umap"), nlevels(y_train) == 1))) {
          hideElement("training_matrix")
          output$training_matrix <- isolate(renderPlot(NULL))
        } else {
          showElement("training_matrix")
          output$training_matrix <- isolate(renderPlot(plot(fit)))
        }
        output$training_summary <- isolate(renderPrint(summary(fit)))
      } else {
        showElement("training_matrix")
        output$training_matrix <- isolate(renderPlot(plot_conf(conf, c(table(y_test)))))
        output$training_summary <- isolate(renderPrint(conf))
      }
      output$training_features <- isolate(renderPrint({
        cat(paste("Features used for the model:\n", paste0(paste("\t-",names(model_react$data)[-c(1,ncol(model_react$data))]),collapse = "\n")))
      }))
      model_react$param <- sapply(match_model(), simplify = FALSE, USE.NAMES = TRUE, FUN = function(x) input[[x]])
      model_react$success <- TRUE
      removeClass(id = "training_resample", class = "clickme")
      removeClass(selector = "label[for=training_sampling]", class = "switchme")
      removeClass(selector = "label[for=training_param]", class = "switchme")
      removeClass(id = "training_tweak", class = "clickme")
    }, error = function(e) {
      obj_react$obj = obj_back
      click("training_param_reset")
      mess_global(title = "building model", msg = c("Can't fit data with input parameters:", e$message), type = "stop")
    }, finally = {
      mess_busy(id = "msg_busy", ctn = "msg_busy_ctn", msg = "", reset = TRUE)
    })
  }))