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

# function to compare 2 IFC_data object and check if names (pops, regions, features) are identical or not
# if not it will trigger reactualization of input thanks to former update_ family functions
compare = function(obj1, obj2, session = getDefaultReactiveDomain()) {
  a = sort(names(obj1$pops))
  b = sort(names(obj2$pops))
  if(!identical(sort(a), sort(b))) update_pops(session = session, obj = obj1)
  if(!identical(sort(names(obj1$regions)), sort(names(obj2$regions)))) update_regions(session = session, obj = obj1)
  if(!identical(sort(names(obj1$features)), sort(names(obj2$features)))) update_features(session = session, obj = obj1)
  p_all = union(a, b)
  p_same = intersect(a, b)
  p_diff = setdiff(p_all, p_same)
  changed = c(p_same[!sapply(p_same, FUN = function(i_pop) identical(obj1$pops[[i_pop]]$obj, obj2$pops[[i_pop]]$obj))], p_diff)
  if(length(changed) != 0) obj1$haschanged_objects <- changed
  return(obj1)
}

# function to modify IFC_data object (remove / add / redefine names)
# they are just wrapper of IFC functions but launch compare() before final return
data_rm_pops <- function(obj, pops, list_only = TRUE, compare_with = obj, session = getDefaultReactiveDomain(), ...) {
  if(list_only) return(IFC::data_rm_pops(obj = obj, pops = pops, list_only = TRUE, session = session, ...))
  ans = IFC::data_rm_pops(obj = obj, pops = pops, list_only = FALSE, session = session, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  compare(ans, compare_with)
}
data_rm_regions <- function(obj, regions, list_only = TRUE, compare_with = obj, session = getDefaultReactiveDomain(), ...) {
  if(list_only) return(IFC::data_rm_regions(obj = obj, regions = regions, list_only = TRUE, session = session, ...))
  ans = IFC::data_rm_regions(obj = obj, regions = regions, list_only = FALSE, session = session, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  compare(ans, compare_with)
}
data_rm_features <- function(obj, features, list_only = TRUE, compare_with = obj, session = getDefaultReactiveDomain(), ...) {
  if(list_only) return(IFC::data_rm_features(obj = obj, features = features, list_only = TRUE, session = session, ...))
  ans = IFC::data_rm_features(obj = obj, features = features, list_only = FALSE, session = session, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  compare(ans, compare_with)
}
data_add_pop_sample <- function(obj, pop, size, new_name, random_seed = NULL, compare_with = obj, session = getDefaultReactiveDomain(), ...) {
  ans = getFromNamespace("data_add_pop_sample", "IFC")(obj = obj, pop = pop, size = size, new_name = new_name, random_seed = random_seed, session = session, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  compare(ans, compare_with)}

data_add_pops <- function(obj, pops, compare_with = obj, session = getDefaultReactiveDomain(), ...) {
  ans = IFC::data_add_pops(obj = obj, pops = pops,  session = session, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  compare(ans, compare_with)
}
data_add_regions <- function(obj, regions, compare_with = obj, session = getDefaultReactiveDomain(), ...) {
  ans = IFC::data_add_regions(obj = obj, regions = regions, session = session, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  compare(ans, compare_with)
}
data_add_features <- function(obj, features, compare_with = obj, session = getDefaultReactiveDomain(), ...) {
  ans = IFC::data_add_features(obj = obj, features = features, session = session, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  compare(ans, compare_with)
}
data_modify_regions <- function(obj, regions, compare_with = obj, session = getDefaultReactiveDomain(), ...) {
  f = getFromNamespace("data_modify_regions", "IFC")
  ans = f(obj = obj, regions = regions, session = session, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  compare(ans, compare_with)
}
data_modify_pops <- function(obj, pops, compare_with = obj, session = getDefaultReactiveDomain(), ...) {
  f = getFromNamespace("data_modify_pops", "IFC")
  ans = f(obj = obj, pops = pops, session = session, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  compare(ans, compare_with)
}
data_redefine <- function(obj, new_feat_def, compare_with = obj, session = getDefaultReactiveDomain(), ...) {
  attr(new_feat_def, "map") = list(initial = names(obj$features_def), to = names(new_feat_def$features_def))
  ans = redefine_obj(obj = obj, new_feat_def = new_feat_def, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  compare(ans, compare_with)
}

# function to check validity of population definition and display information about element edition
pop_def = function(x = "") {
  disable("pop_validate")
  disable("pop_def_and")
  disable("pop_def_or")
  disable("pop_def_not")
  disable("pop_def_bkt1")
  disable("pop_def_bkt2")
  disable("pop_def_add")
  if((length(x) == 0) || (x == "")) {
    enable("pop_def_not")
    enable("pop_def_bkt1")
    enable("pop_def_add")
    return(FALSE)
  }
  count = 0
  valid = FALSE
  for(i in x) {
    if(i == "(") count = count + 1
    if(i == ")") count = count - 1
  }
  switch(x[length(x)],
         "And" = {
           enable("pop_def_not")
           enable("pop_def_bkt1")
           enable("pop_def_add")
         },
         "Or" = {
           enable("pop_def_not")
           enable("pop_def_bkt1")
           enable("pop_def_add")
         },
         "Not" = {
           enable("pop_def_bkt1")
           enable("pop_def_add")
         },
         "(" = {
           enable("pop_def_not")
           enable("pop_def_bkt1")
           enable("pop_def_add")
         },
         ")" = {
           enable("pop_def_and")
           enable("pop_def_or")
           if(count > 0) enable("pop_def_bkt2")
           valid = TRUE
         },
         { # pop
           enable("pop_def_and")
           enable("pop_def_or")
           if(count > 0) enable("pop_def_bkt2")
           if(length(x) > 1) {
             if(x[length(x)-1] %in% c("And", "Or", "Not")) valid = TRUE
           } else {
             valid = TRUE
           }
         })
  return((count == 0) & valid)
}

# function to check validity of region definition and display information about element edition
reg_def = function(reg, reg_back, all_names, check = "both", session = getDefaultReactiveDomain()) {
  if(length(reg$name) == 0) return(structure(rep(FALSE, 6), names = c("label", "light", "dark", "cx", "cy", "table")))
  type = check
  if(check == "info") {
    type = "edit"
    myfeedback = showFeedbackSuccess
  } else {
    myfeedback = showFeedbackWarning
  }
  same = structure(rep(TRUE, 6), names = c("label", "light", "dark", "cx", "cy", "table"))
  valid = structure(rep(TRUE, 6), names = c("label", "light", "dark", "cx", "cy", "table"))
  switch(type,
         "both" = {
           valid = reg_def(reg, reg_back, all_names, check = "valid", session)
           same = reg_def(reg, reg_back, all_names, check = "info", session)
         },
         "valid" = {
           if((length(session$input$reg_def_label) == 0) || (session$input$reg_def_label == "")) {
             valid[1] = FALSE
             hideFeedback(session=session, inputId="reg_def_label")
             showFeedbackDanger(session = session, inputId = "reg_def_label", text = "empty")
           } else {
             if(session$input$reg_def_label %in% c("And", "Or", "Not", "(", ")", "All")) {
               valid[1] = FALSE
               hideFeedback(session=session, inputId="reg_def_label")
               showFeedbackDanger(session = session, inputId = "reg_def_label", text = "invalid")
             }
             if(grepl("^ML_", session$input$reg_def_label)) {
               valid[1] = FALSE
               hideFeedback(session=session, inputId="reg_def_label")
               showFeedbackDanger(session = session, inputId = "reg_def_label", text = "not allowed")
             }
             if(session$input$reg_def_label %in% setdiff(all_names, reg$name)) {
               valid [1]= FALSE
               hideFeedback(session=session, inputId="reg_def_label")
               showFeedbackDanger(session = session, inputId = "reg_def_label", text = "already exists")
             }
           }
           if(!any(is.finite(na.omit(reg$cx)))) {
             valid[4] = FALSE
             hideFeedback(session=session, inputId="reg_def_cx")
             showFeedbackDanger(session=session, inputId="reg_def_cx", text="non-finite value")
           }
           if(!any(is.finite(na.omit(reg$cy)))) {
             valid[5]= FALSE
             hideFeedback(session=session, inputId="reg_def_cy")
             showFeedbackDanger(session=session, inputId="reg_def_cy", text="non-finite value")
           }
           if(!all(is.finite(c(reg$x, reg$y)))) {
             valid[6] = FALSE
             hideFeedback(session=session, inputId="reg_def_table_feedback")
             showFeedbackDanger(session=session, inputId="reg_def_table_feedback", text="non-finite value")
           }
           if(all(valid)) {
             enable("reg_validate")
           } else {
             disable("reg_validate")
           }
         },
         "edit" = {
           if(any(reg$label != reg_back$label)) {
             same[1] = FALSE
             if(valid[1]) myfeedback(inputId = "reg_def_label", text=reg_back$label)
           }
           if(any(reg$color != reg_back$color)) {
             same[2] = FALSE
             addClass(id = "reg_color_dark_reset", class = "modified")
           }
           if(any(reg$lightcolor != reg_back$lightcolor)) {
             same[3] = FALSE
             addClass(id = "reg_color_light_reset", class = "modified")
           }
           if(!identical(as.numeric(reg$cx), as.numeric(reg_back$cx))) {
             same[4] = FALSE
             if(valid[4]) myfeedback(inputId = "reg_def_cx", text=as.character(reg_back$cx))
           }
           if(!identical(as.numeric(reg$cy), as.numeric(reg_back$cy))) {
             same[5] = FALSE
             if(valid[5]) myfeedback(inputId = "reg_def_cy", text=as.character(reg_back$cy))
           }
           if(!(all(c(identical(as.numeric(reg$x), as.numeric(reg_back$x)),
                      identical(as.numeric(reg$y), as.numeric(reg_back$y)))))) {
             same[6] = FALSE
             if(valid[6]) myfeedback(inputId = "reg_def_table_feedback", text="edited")
           }
         })
  ans = valid & same
  if(ans[1]) hideFeedback(session=session, inputId="reg_def_label")
  if(ans[2]) removeClass(id = "reg_color_dark_reset", class = "modified")
  if(ans[3]) removeClass(id = "reg_color_light_reset", class = "modified")
  if(ans[4]) hideFeedback(session=session, inputId="reg_def_cx")
  if(ans[5]) hideFeedback(session=session, inputId="reg_def_cy")
  if(ans[6]) hideFeedback(session=session, inputId="reg_def_table_feedback")
  return(ans)
}

# function to update all input containing population name
update_pops <- function(session = getDefaultReactiveDomain(), obj, init = FALSE, ...) {
  if(length(session) != 0) {
  N = names(obj$pops); 
  TAGGED = N[sapply(obj$pops, FUN = function(p) p$type == "T")]
  if(length(N) == 0) N = list()
  if(!init && (length(session$input$population) !=0) && all(session$input$population %in% N)) {
    updateSelectInput(session=session, inputId = "population", choices = N, selected = session$input$population)
  } else {
    updateSelectInput(session=session, inputId = "population", choices = N, selected = "All")
  }
  if(!init && (length(session$input$population) !=0) && all(session$input$population %in% N)) {
    updateSelectInput(session=session, inputId = "pop_sample_name", choices = N, selected = session$input$population)
  } else {
    updateSelectInput(session=session, inputId = "pop_sample_name", choices = N, selected = "All")
  }
  if(!init && (length(session$input$cell_population) !=0) && all(session$input$cell_population %in% N)) {
    updateSelectInput(session=session, inputId = "cell_population", choices = N, selected = session$input$cell_population)
  } else {
    updateSelectInput(session=session, inputId = "cell_population", choices = N, selected = "All")
  }
  if(!init && (length(session$input$cell_pop_tagged) !=0) && all(session$input$cell_pop_tagged %in% N)) {
    updateSelectInput(session=session, inputId = "cell_pop_tagged", choices = TAGGED, selected = session$input$cell_pop_tagged)
  } else {
    updateSelectInput(session=session, inputId = "cell_pop_tagged", choices = TAGGED, selected = "")
  }
  if(!init && (length(session$input$plot_base) !=0) && all(session$input$plot_base %in% N)) {
    updateSelectInput(session=session, inputId = "plot_base", choices = N, selected = session$input$plot_base)
  } else {
    updateSelectInput(session=session, inputId = "plot_base", choices = N, selected = "All")
  }
  if(!init && (length(session$input$plot_shown) !=0) && all(session$input$plot_shown %in% N)) {
    updateSelectInput(session=session, inputId = "plot_shown", choices = N, selected = session$input$plot_shown)
  } else {
    updateSelectInput(session=session, inputId = "plot_shown", choices = N, selected = character())
  }
  if(!init && (length(session$input$populations_training) !=0) && all(session$input$populations_training %in% N)) {
    updateSelectInput(session=session, inputId = "populations_training",
                      choices = grep("^ML_", N, value = TRUE, invert = TRUE),
                      selected = grep("^ML_", session$input$populations_training, value = TRUE, invert = TRUE))
  } else {
    updateSelectInput(session=session, inputId = "populations_training", choices = N, selected = character())
  }
  if(!init && (length(session$input$plot_batch_population) !=0) && all(session$input$plot_batch_population %in% N)) {
    updateSelectInput(session=session, inputId = "plot_batch_population", choices = N, selected = session$input$plot_batch_population)
  } else {
    updateSelectInput(session=session, inputId = "plot_batch_population", choices = N, selected = "All")
  }
}
return(invisible(NULL))
}

# function to update all input containing regions name
update_regions <- function(session = getDefaultReactiveDomain(), obj, init = FALSE, ...) {
  if(length(session) != 0) {
  N = names(obj$regions); if(length(N) == 0) N = list()
  if(!init && (length(session$input$reg_selection) !=0) && all(session$input$reg_selection %in% N)) {
    updateSelectInput(session=session, inputId = "reg_selection", choices = N, selected = session$input$reg_selection)
  } else {
    updateSelectInput(session=session, inputId = "reg_selection", choices = N, selected = character())
  }
}
return(invisible(NULL))
}

# function to update all input containing features name
update_features <- function(session = getDefaultReactiveDomain(), obj, init = FALSE, ...) {
  if(length(session) != 0) {
  N = names(obj$features); if(length(N) == 0) N = list()
  session$output$features_infos <- renderText({
    paste0(length(N), " features found with this pattern")
  })
  if(!init && (length(session$input$stats_feature) !=0) && all(session$input$stats_feature %in% N)) {
    updateSelectInput(session=session, inputId = "stats_feature", choices = N, selected = session$input$stats_feature)
  } else {
    updateSelectInput(session=session, inputId = "stats_feature", choices = N, selected = "Object Number")
  }
  if(!init && (length(session$input$table_sort_feature) !=0) && all(session$input$table_sort_feature %in% N)) {
    updateSelectInput(session=session, inputId = "table_sort_feature", choices = N, selected = session$input$table_sort_feature)
  } else {
    updateSelectInput(session=session, inputId = "table_sort_feature", choices = N, selected = "Object Number")
  }
  if(!init && (length(session$input$cell_feature) !=0) && all(session$input$cell_feature %in% N)) {
    updateSelectInput(session=session, inputId = "cell_feature", choices = N, selected = session$input$cell_feature)
  } else {
    updateSelectInput(session=session, inputId = "cell_feature", choices = N, selected = "Object Number")
  }
  if(!init && (length(session$input$plot_dens_feature) !=0) && all(session$input$plot_dens_feature %in% N)) {
    updateSelectInput(session=session, inputId = "plot_dens_feature", choices = c("initial","default",N), selected = c("initial", session$input$cell_feature))
  } else {
    updateSelectInput(session=session, inputId = "plot_dens_feature", choices = c("initial","default",N), selected = "initial")
  }
  if(!init && (length(session$input$batch_population) !=0) && all(session$input$batch_population %in% N)) {
    updateSelectInput(session=session, inputId = "plot_batch_feature", choices = N, selected = session$input$batch_population)
  } else {
    updateSelectInput(session=session, inputId = "plot_batch_feature", choices = N, selected = "Object Number")
    updateTextInput(session = session, inputId = "plot_batch_feature_transform", value = "P")
  }
  if(!init && (length(session$input$plot_x_feature) !=0) && all(session$input$plot_x_feature %in% N)) {
    updateSelectInput(session=session, inputId = "plot_x_feature", choices = N, selected = session$input$plot_x_feature)
  } else {
    updateSelectInput(session=session, inputId = "plot_x_feature", choices = N, selected = "Object Number")
    updateTextInput(session = session, inputId = "plot_x_transform", value = "P")
  }
  if(!init && (length(session$input$plot_y_feature) !=0) && all(session$input$plot_y_feature %in% N)) {
    updateSelectInput(session=session, inputId = "plot_y_feature", choices = N, selected = session$input$plot_y_feature)
  } else {
    updateSelectInput(session=session, inputId = "plot_y_feature", choices = N, selected = "Object Number")
    updateTextInput(session = session, inputId = "plot_y_transform", value = "P")
  }
  if(!init && (length(session$input$plot_z_feature) !=0) && all(session$input$plot_z_feature %in% N)) {
    updateSelectInput(session=session, inputId = "plot_z_feature", choices = N, selected = session$input$plot_z_feature)
  } else {
    updateSelectInput(session=session, inputId = "plot_z_feature", choices = N, selected = "Object Number")
    updateTextInput(session = session, inputId = "plot_y_transform", value = "P")
  }
}
  return(invisible(NULL))
}