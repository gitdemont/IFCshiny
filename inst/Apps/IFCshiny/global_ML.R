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

# function to create confusion matrix plot
plot_conf <- function(confusion_matrix, class_n) {
  M = confusion_matrix$table
  if(missing(class_n)) {
    M = M[order(rownames(M)), order(colnames(M)) ]
    perc = t(100 * t(M) / colSums(M))
  } else {
    M = M[order(names(class_n)), order(names(class_n))]
    perc = 100 * M / class_n
  }
  plot(x = -1, y = -1,
       xlim = c(1, nrow(M)) + c(-0.5,0.5),
       ylim = c(1, nrow(M))  + c(-0.5,0.5),
       xaxs="i", yaxs="i", axes = FALSE, ann = FALSE)
  axis(side = 1, at = seq(1, nrow(M)), tick = FALSE, adj = 1, labels = FALSE, cex = 0.8)
  text(x = 1:nrow(M),
       y = par("usr")[3] - 0.05,
       labels = sapply(rownames(M), center_short),
       xpd = NA,
       srt = 35,
       adj = 1,
       cex = 0.8)
  axis(side = 2, at = seq(1, ncol(M)), tick = FALSE, las = 2, cex = 0.8, labels = FALSE)
  text(y = 1:nrow(M),
       x = par("usr")[1] - 0.05,
       labels =  sapply(colnames(M), center_short),
       xpd = NA,
       srt = 0,
       adj = 1,
       cex = 0.8)
  mtext(text = "Reference", side = 3, cex = 1.2)
  mtext(text = "Predicted", side = 4, cex = 1.2)
  for(i_col in 1:ncol(M)) {
    for(i_row in 1:nrow(M)) {
      col = "grey"
      val = sprintf("%03.1f", perc[i_row, i_col])
      if(i_col == i_row) col = "lightblue"
      if((as.numeric(val) != 0) || (col == "lightblue")) {
        rect(xleft = i_col - 0.5, xright = i_col + 0.5, ybottom = i_row - 0.5, ytop = i_row + 0.5, col = col, border = NA)
        text(label = val, x = i_col, y = i_row, cex = 0.8)
      }
    }
  }
  box()
}

# function to match model name
match_model <- function(x = session$input$training_model, to_exlude = "_reset", session = shiny::getDefaultReactiveDomain()) {
  if(length(x) == 0) return(NULL)
  mdl = c("pca","tsne","umap","som","em","svm","xgb","lda")
  pattern = x == mdl
  pattern = c("^pca_","^Rtsne_","^umap_","^som_","^MclustDA_","^svm","^xgb_","^lda_")[pattern]
  N = names(session$input)
  return(setdiff(N[grep(pattern, N)], paste0(mdl, to_exlude)))
}

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

################################################################################
#             functions described hereunder are experimental                   #
#              inputs and outputs may change in the future                     #
#  ~ they should facilitate the application of model from one file to other ~  #
################################################################################

#' @title IFCml Names Identifier
#' @description
#' Checks and identifies names in `IFCml` model
#' @param model an `IFCml` model
#' @value returns a list containing:
#' -is_feat the features names of the model
#' -is_num the position of the "Object Number" feature
#' -is_clust the position of the grouping feature
#' @keywords internal
checknames.IFCml <- function(model) {
  N = names(model$data)
  is_clust = ncol(model$data)
  if(substr(N[is_clust],1,5) != "clust") stop("can't find 'clust' in model$data")
  is_num = 1
  if(N[is_num] != "Object Number")  stop("can't find 'Object Number' in model$data")
  list(is_feat = N[-c(is_num,is_clust)],
       is_num = is_num,
       is_clust = is_clust)
}

#' @title Default List Setter
#' @description
#' Resets list
#' @param x a list
#' @param what name(s) of list element to set to NULL. Default is NULL.
#' @param class desired class of the returned object. Default is "list".
#' @value a list with class 'class' and element(s) 'what' set to NULL
#' @keywords internal
reset.list <- function(x, what = NULL, class = "list") {
  if(length(what) != 0) for(i in what) {
    if(i %in% names(x)) { 
      x[[i]] = NULL
    } else { 
      x = c(x, structure(.Data = list(NULL), names = i)) 
    }
  }
  return(structure(x, class = class))
}

#' @title Default IFCml Setter
#' @description
#' Resets `IFCml` object
#' @param x a list
#' @param what name(s) of list element to set to NULL. Default is NULL.
#' @param class desired class of the returned object. Default is "IFCml".
#' @value a list with class 'class' and element(s) 'what' set to NULL
#' @keywords internal
reset.IFCml <- function(model, what = NULL, class = "IFCml") {
  reset.list(x=model, what=what, class=class)
}

#' @title IFC_data Preprocessing
#' @description
#' Preprocess `IFC_data` object.
#' @param obj an `IFC_data` object extracted by ExtractFromDAF(extract_features = TRUE) or ExtractFromXIF(extract_features = TRUE).
#' @param pops name(s) of population(s) present in 'obj' that will be used to determine clusters and features that better separate each one.
#' If only one pop is provided, no best features is computed and only preprocess object is returned.
#' /!\ Please note that if multiple pops are provided, no more thant 80% of objects belonging to each pops should overlap with other pops.
#' @param remove_sys, whether to remove system features whose definition are "Time", "Flow Speed", "Camera Timer", "Camera Line Number", "Objects per mL", "Objects per sec" and "Object Number" or only "Camera Line Number" for fcs, Default is TRUE
#' @param remove_sat, whether to remove system features whose definition are "Saturation Count" and "Saturation Percent",Default is TRUE
#' @param remove_pix, whether to remove system features whose definition contains "Pixel". Default is TRUE
#' @param remove_pix, whether to remove system features whose definition contains "Raw" or "Uncompensated". Default is TRUE\
#' @param remove_pat, removes features names matching with the input pattern. Default is "^ML_"
#' @param remove_nan, whether to remove features with NA/NaN values. Default is TRUE
#' @param remove_nzv, whether to remove features zero or near from zero variance. Default is FALSE.\cr
#' -any negative value will remove nothing\cr
#' -FALSE (or any value >= 0) will remove only features with exactly zero variance\cr
#' -TRUE (or any value > 0) will use nzv() from caret package to remove features with near from zero variance.
#' @param features_int, (used for intensities features) whether to transform features matching with 'config$features_inp'. Default is TRUE
#' @param features_inp, pattern used to find intensity features to transform. Default is ifelse(length(obj$description$FCS) == 0, "^Intensity|^Bright Detail Intensity|^Uncompensated", "^(?!.*FS|.*SS|.*LOG).*$").\cr
#' It applies to features definition except for FCS where it applies to features names.
#' @param features_inm, method (a string) used to transform features matching with 'config$features_inp'. Default is ifelse(length(obj$description$FCS) == 0, "LinLog", "Arcsinh")
#' @param int_to_tra, names of intensity features (in addition of features found with 'features_inp'). Default is NULL.
#' @param features_har, (used for haralick features) whether to transform features matching with 'config$features_hap'. Default is TRUE
#' @param features_hap, pattern used to find haralick features to transform. Default is "^H ".\cr
#' It applies to features definition except for FCS where it applies to features names.
#' @param features_ham, method (a string) used to transform features matching with 'config$features_hap'. Default is "yeojohnson"
#' @param har_to_tra, names of haralick features (in addition of features found with 'features_hap'). Default is NULL.
#' @param features_cen, whether to center features, it can also be a named vector of center values. Default is TRUE
#' @param features_cem, method (expecting a function) used to center features. Default is mean
#' @param features_scl, whether to scale features, it can also be a named vector of scaling values. Default is TRUE.
#' @param features_scm, method (expecting a function) used to center features. Default is sd
#' @param features_cor, values used to determine correlation threshold above which features are kept. Default is 0 to keep all features.\cr
#' If the value is equal or superior to 1, if will be set to 0.95 automatically.\cr
#' @param features_bst, number of features that discriminate each couple of 'pops' at best. Default is 0, to use all features.\cr
#' If pops is of length one this argument will not be used.\cr
#' @param features_kep, a character vector of the only desired features to use for preprocessing the data. Default is character(0), to apply the preprocessing on all features.
#' @param check_groups, whether to check that all population mentioned in 'pops' can be found in obj and each of them is sufficiently represented. Default is TRUE. 
#' @details The preprocessing workflow applied is:
#' -undesired features among desired are first removed,\cr
#' -transformation are applied,\cr
#' -features resulting of NA values or no variance are removed,\cr
#' -features are centered and scaled\cr
#' -features with near from zero variance or highly correlated are removed\cr
#' -only best discriminant features are kept.
#' @param ... other arguments to be passed.
#' @param verbose whether to display information. Default is FALSE.
#' @return A named list, whose members are:\cr
#' -data, a preprocessed data.frame,\cr
#' -cen, the values used to center the features,\cr
#' -cem, the method used to center the features,\cr
#' -scl, the values used to scale the features,\cr
#' -scm, the method used to scale the features,\cr
#' -int, a character vector of features names where int transformation was applied,\cr
#' -inm, the method used for int transformation,\cr
#' -har, a character vector of features names where har transformation was applied,\cr
#' -ham, the method used for har transformation,\cr
#' -config, a list with the config input.\cr
#' -idx, is set to logical().
#' @keywords internal
preprocess = function(obj, pops = "All",
                      remove_sys = TRUE,
                      remove_sat = TRUE,
                      remove_pix = TRUE,
                      remove_raw = TRUE,
                      remove_pat = "^ML_",
                      remove_nan = TRUE,
                      remove_nzv = FALSE,
                      features_int = TRUE,
                      features_inp = ifelse(length(obj$description$FCS) == 0, "^Intensity|^Bright Detail Intensity|^Uncompensated", "^(?!.*FS|.*SS|.*LOG).*$"),
                      features_inm = ifelse(length(obj$description$FCS) == 0, "LinLog", "Arcsinh"),
                      int_to_tra = NULL,
                      features_har = TRUE,
                      features_hap = "^H ",
                      features_ham = "yeojohnson",
                      har_to_tra = NULL,
                      features_cen = TRUE,
                      features_cem = mean,
                      features_scl = TRUE,
                      features_scm = sd,
                      features_cor = 0,
                      features_bst = 0,
                      features_kep = character(0),
                      check_groups = TRUE,
                      ...,
                      verbose = FALSE) {
  # check every input training population will be present
  if(verbose) cat("preprocessing data\n")
  g = sapply(obj$pops[pops], FUN = function(p) p$obj)
  # sub = apply(g, 1, any)
  sub = fastAny(g)
  groups = apply(g, 1, FUN = function(x) {
    foo = which(x)
    if(length(foo) == 1) return(pops[foo])
    return(NA)
  })
  groups = factor(groups[sub], levels = pops, labels = pops)
  if(check_groups) {
    check = table(groups)/apply(g, 2, sum)
    if(any(check < 0.8)) stop("too many cells belong to more than 1 unique population. check training pops !")
    if(length(na.omit(unique(groups))) != length(pops)) stop("too many cells belong to more than 1 unique population. check training pops !")
  }
  
  # retrieve data and place Object Number 1st
  df = obj$features
  clust_n = paste0("clust", random_name(special = NULL, forbidden = names(df)))
  def_sub = which(sapply(obj$features_def, FUN = function(x) x$type=="single"))
  def_def = sapply(obj$features_def, FUN = function(x) x$def)
  def_nam = sapply(obj$features_def, FUN = function(x) x$nam)
  
  # find feat to remove / to transform
  to_rm = NULL
  to_rm_sys = NULL
  to_rm_sat = NULL
  to_rm_pix = NULL
  to_rm_raw = NULL
  to_rm_fea = NULL
  
  POPS = structure(pops,
                   "lightModeColor" = sapply(pops, FUN = function(p) map_color(obj$pops[[p]]$lightModeColor, toR = TRUE)),
                   "color" = sapply(pops, FUN = function(p) map_color(obj$pops[[p]]$color, toR = TRUE)),
                   "style" = sapply(pops, FUN = function(p) map_style(obj$pops[[p]]$style, toR = TRUE)))
  
  ans = list(data = data.frame(),
             cen = numeric(),
             cem = features_cem,
             scl = numeric(),
             scm = features_scm,
             int = character(),
             inm = character(),
             har = character(),
             ham = character(),
             config = c(list(pops = POPS,
                             remove_sys = remove_sys,
                             remove_sat = remove_sat,
                             remove_pix = remove_pix,
                             remove_raw = remove_raw,
                             remove_pat = remove_pat,
                             remove_nan = remove_nan,
                             remove_nzv = remove_nzv,
                             features_int = features_int,
                             features_inp = features_inp, #ifelse(length(obj$description$FCS) == 0, "^Intensity|^Bright Detail Intensity|^Uncompensated", "^(?!.*FS|.*SS|.*LOG).*$"),
                             features_inm = features_inm, #ifelse(length(obj$description$FCS) == 0, "LinLog", "Arcsinh"),
                             int_to_tra = int_to_tra,
                             features_har = features_har,
                             features_hap = features_hap, #"^H ",
                             features_ham = features_ham, # "yeojohnson",
                             har_to_tra = har_to_tra,
                             features_cen = features_cen,
                             features_cem = features_cem,
                             features_scl = features_scl,
                             features_scm = features_scm,
                             features_cor = features_cor,
                             features_bst = features_bst,
                             features_kep = features_kep,
                             check_groups = check_groups)),
             idx = logical()) # we reset idx
  if(verbose) cat("preprocessing data [removing features using sys,sat,pix,pat,raw inputs]\n")
  if(length(remove_pat) != 0) to_rm_fea = grep(remove_pat, names(df), value = TRUE, invert = FALSE)
  if(length(obj$description$FCS) == 0) {
    # constant values
    if(remove_sys) to_rm_sys = which(def_def %in% c("Time", "Flow Speed", "Camera Timer", "Camera Line Number", "Objects per mL", "Objects per sec", "Object Number"))
    # saturation
    if(remove_sat) to_rm_sat = grep("^Saturation [Count|Percent]", def_def, value = FALSE, invert = FALSE)
    # pixel
    if(remove_pix) to_rm_pix = grep("Pixel", def_def, value = FALSE, invert = FALSE)
    # raw and uncompensated
    if(remove_raw) to_rm_raw = grep("Raw|Uncompensated", def_def, value = FALSE, invert = FALSE)
    to_rm = unique(intersect(c(to_rm_sys, to_rm_sat, to_rm_pix, to_rm_raw), def_sub))
    to_rm = def_nam[to_rm]
    
    # find feat to transform
    # intensities
    if(length(features_inp) == 0) {
      int_to_tra2 = integer()
    } else {
      int_to_tra2 = grep(features_inp, def_def, value = FALSE, invert = FALSE, perl = TRUE)
    }
    int_to_tra = unique(c(int_to_tra, def_nam[unique(intersect(int_to_tra2, def_sub))]))
    # haralick
    if(length(features_hap) == 0) {
      har_to_tra2 = integer()
    } else {
      har_to_tra2 = grep(features_hap, def_def, value = FALSE, invert = FALSE, perl = TRUE)
    }
    har_to_tra = unique(c(har_to_tra, def_nam[unique(intersect(har_to_tra2, def_sub))]))
  } else {
    if(remove_sys) to_rm = def_nam[c(to_rm, which(def_nam == "Camera Line Number"))]
    if(length(features_inp) == 0) {
      int_to_tra2 = character()
    } else {
      int_to_tra2 = grep(features_inp, def_nam, value = TRUE, invert = FALSE, perl = TRUE)
    }
    int_to_tra = setdiff(c(int_to_tra, int_to_tra2), "Object Number")
    if(length(features_hap) == 0) {
      har_to_tra2 = character()
    } else {
      har_to_tra2 = grep(features_hap, def_nam, value = TRUE, invert = FALSE, perl = TRUE)
    }
    har_to_tra = setdiff(c(har_to_tra, har_to_tra2), "Object Number")
  }
  
  if(length(features_kep) == 0) {
    df = df[, c("Object Number", setdiff(names(df), c(to_rm, to_rm_fea, "Object Number")))]
  } else {
    df = df[, c("Object Number", setdiff(features_kep, c(to_rm, to_rm_fea, "Object Number")))]
  }
  df = df[, unique(names(df))]
  
  # remove features with no variance
  if(remove_nzv >= 0) { # e.g. TRUE or FALSE but not -1
    df_sd = apply(df[sub, -1], 2, sd, na.rm = TRUE)
    df_sd = which(is.na(df_sd) | (df_sd == 0))
    if(length(df_sd) !=0) df = df[, -(df_sd + 1)]
  }
  
  # transform intensities
  if(features_int) {
    int_to_tra = int_to_tra[int_to_tra %in% names(df)]
    if(length(int_to_tra) != 0) {
      if(verbose) cat("preprocessing data [transforming intensity features]\n")
      switch(features_inm,
             "LinLog" = {
               df[, int_to_tra] = sapply(int_to_tra, FUN = function(i_trans) smoothLinLog(x = df[, i_trans]))
               ans$inm = "LinLog"
             },
             "Arcsinh" = {
               df[, int_to_tra] = sapply(int_to_tra, FUN = function(i_trans) asinh(x = df[, i_trans]))
               ans$inm = "Arcsinh"
             },
             stop("the transformation algorithm:", features_inm," is not supported"))
      ans$int = int_to_tra
    }
  }
  
  # transform haralick
  if(features_har) {
    har_to_tra = har_to_tra[har_to_tra %in% names(df)]
    if(length(har_to_tra) != 0) {
      if(verbose) cat("preprocessing data [transforming haralick features]\n")
      switch(features_ham,
             "yeojohnson" = {
               if(requireNamespace("bestNormalize", quietly = TRUE)) {
                 df[, har_to_tra] = sapply(har_to_tra, FUN = function(i_trans) bestNormalize::yeojohnson(x = df[, i_trans], standardize = FALSE)$x.t)
                 ans$ham = "yeojohnson" 
               } else {
                 stop("'bestNormalize' package is required to perform Yeo-Johnson transformation")
               }
             },
             stop("the transformation algorithm:", features_ham," is not supported"))
      ans$har = har_to_tra
    }
  }
  
  # remove non finite values
  if(remove_nan) {
    tokeep = which(apply(apply(df[sub, -1], 2, is.finite), 2, all))
    df = df[, c("Object Number", names(df[,-1])[tokeep])]
  }
  
  do_center = FALSE
  do_scale = FALSE
  
  # center
  if((length(features_cen) == 0) || is.logical(features_cen)) {
    if((length(features_cen) != 0) && features_cen) {
      df_cen = apply(df[sub, -1], 2, features_cem, na.rm = TRUE)
      do_center = TRUE
    } else {
      df_cen = rep(0, ncol(df) - 1)
    }
    names(df_cen) = names(df)[-1]
  } else {
    df_cen = features_cen
    do_center = TRUE
  }
  ans$cen = df_cen
  
  # scale
  if((length(features_scl) == 0) || is.logical(features_scl)) {
    if((length(features_scl) != 0) && features_scl) {
      df_scl = apply(df[sub, -1], 2, features_scm, na.rm = TRUE)
      do_scale = TRUE
    } else {
      df_scl = rep(1, ncol(df) -1)
    }
    names(df_scl) = names(df)[-1]
  } else {
    df_scl = features_scl
    do_scale = TRUE
  }
  ans$scl = df_scl
  
  # center/scale data
  if(any(c(do_center, do_scale))) {
    if(verbose) cat("preprocessing data [applying normalization]\n")
    df[, 2:ncol(df)] = sapply(2:ncol(df), FUN = function(i) (df[, i] - df_cen[i-1]) / df_scl[i-1])
  }
  
  # remove features with near from zero variance
  if(remove_nzv > 0) { # e.g. TRUE but not FALSE nor -1
    to_rm_nzv = nzv(df[sub, -1], names = TRUE)
    if(length(to_rm_nzv) != 0) df = df[, setdiff(names(df), to_rm_nzv)]
  }
  
  # remove correlated features
  if(features_cor > 0) {
    if(verbose) cat("preprocessing data [checking for correlation]\n")
    cor_mat = sapply(names(df[, -1]), FUN = function(var1) {
      sapply(names(df[, -1]), FUN = function(var2) {
        cor(df[sub, var1], df[sub, var2])
      })
    })
    to_rm_cor = findCorrelation(cor_mat, cutoff = ifelse(features_cor >= 1, 0.95, features_cor), verbose = FALSE, names = TRUE, exact = TRUE)
    if(length(to_rm_cor) != 0) df =  df[, setdiff(names(df), to_rm_cor)]
  }
  
  # store the whole data
  ans$data <- df
  
  # reduce to subset
  df = df[sub, ]
  if((length(pops) != 1) && (features_bst != 0)) {
    if(verbose) cat("preprocessing data [identifying best features]\n")
    val = by(df[, -1], as.factor(groups), FUN = function(d) {
      apply(d, 2, FUN = function(x) {
        c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
      })
    })
    
    comb = combn(x = pops, m = 2)
    comb_n = sapply(1:ncol(comb), FUN = function(i_comb) { paste0(comb[, i_comb], collapse = "_") })
    names(comb) = comb_n
    
    Rd = sapply(1:ncol(comb), FUN = function(i_comb) {
      cond1 = comb[1, i_comb]
      cond2 = comb[2, i_comb]
      sapply(1:ncol(val[[1]]), FUN = function(i) abs((val[[cond1]][1,i] - val[[cond2]][1,i])/(val[[cond1]][2,i] + val[[cond2]][2,i])))
    })
    colnames(Rd) = comb_n
    
    best = sapply(colnames(Rd),FUN=function(x) {
      val = sort(Rd[rownames(Rd),x],decreasing = TRUE)[1:min(nrow(Rd), features_bst)]
      names(val)
    })
    best = unique(c(unlist(best)))
    ans$data = ans$data[, unique(c("Object Number", setdiff(best, "Object Number")))]
  } else {
    ans$data = ans$data[, c("Object Number", setdiff(colnames(df), "Object Number"))]
  }
  kept = setdiff(colnames(ans$data), "Object Number")
  ans$data = cbind.data.frame(ans$data, matrix(rep(NA, nrow(ans$data)), ncol=1, dimnames = list(NULL, clust_n)))
  ans$data[sub, clust_n] = as.character(groups)
  ans$data[, clust_n] = factor(ans$data[, clust_n, drop = TRUE], levels = pops, labels = pops)
  ans$cen = ans$cen[kept]
  ans$scl = ans$scl[kept]
  ans$har = ans$har[ans$har %in% kept]
  ans$int = ans$int[ans$int %in% kept]
  class(ans) <- c("IFCml","IFCml_pre")
  ans
}

#' @title IFCml Data Splitting
#' @description
#' Splits data into training and testing set in an `IFCml` model
#' @param model an `IFCml` model
#' @param ratio value defining the percentage of data that goes to training.
#' If missing the default, the value in model$ratio will be used.
#' @param ... other arguments to be passed.
#' @param verbose whether to display information. Default is FALSE.
#' @value an `IFCml` model of class `IFCml` and `IFCml_set`
#' @keywords internal
splitdata.IFCml <- function(model, ratio, ..., verbose = FALSE) {
  dots = list(...)
  session = dots$session
  dots = dots[names(dots) != "session"]
  if(!missing(ratio)) model$ratio <- ratio
  Q = checknames.IFCml(model)
  if(verbose) cat("splitting data\n")
  # generate unique id for split
  id = random_name(special = FALSE)
  
  # identify indices of train vs test
  SUB = !is.na(model$data[, Q$is_clust, drop = TRUE])
  lev = levels(model$data[SUB, Q$is_clust, drop = TRUE])
  model$data[, Q$is_clust] = factor(model$data[, Q$is_clust, drop = TRUE], levels = lev)
  sub_id = unlist(caret::createDataPartition(model$data[SUB, Q$is_clust, drop = TRUE], p = model$ratio, list = FALSE))
  idx = rep(FALSE, length(SUB))
  names(idx) = model$data[, "Object Number",drop = TRUE]
  idx[SUB][sub_id] <- TRUE
  model$idx = idx
  attr(model$data , "id") <- id
  attr(model$idx , "id") <- id
  
  # reset fit, pca, proj_pca, pred, feat
  return(reset.IFCml(model, what = c("fit","pca","proj_pca","sub","pred","feat"), class = c("IFCml","IFCml_set")))
}
# bar = splitdata.IFCml(model = cc)

#' @title IFCml Data Fitting
#' @description
#' Fits training data of `IFCml` model
#' @param model an `IFCml` model of class `IFCml_set`
#' @param method algorithm used for fitting the data. Allowed is either "pca","tsne","umap", "som", "em", "svm", "xgb", or "lda".
#' If missing the default, the value in model$method will be used.
#' @param ... other arguments to be passed.
#' @param verbose whether to display information. Default is FALSE.
#' @details fitting will be performed using model$param or dots, which should contain (if any) arguments to pass to (with the exception of):\cr
#' base::prcomp ('x' = train, 'center', 'scale.', and 'rank.')
#' Rtsne::Rtsne ('X', 'dims', 'pca', 'partial_pca', 'normalize', 'pca_center', 'pca_scale', and 'verbose'),\cr
#' umap::umap ('d', 'n_components', 'method', 'objective' and 'verbose'),\cr
#' EmbedSOM::SOM ('data'),\cr
#' mclust::MclustDA ('data' and 'class'),\cr
#' e1071::svm ('x', 'y', and 'scale'),\cr
#' xgboost::xgb ('data', 'label', and 'verbose'),\cr
#' MASS::lda ('x' and 'grouping').\cr
#' NOTE: extra arguments should be named in the form name_, e.g. for e1071::svm, svm_nu to be applied as 'nu' argument to svm. Or directly used as 'nu'.\cr
#' NOTE: centering and scaling is set to FALSE because it has to be performed during preprocessing.\cr
#' NOTE: a "pca" (base::prcomp) is always performed and is used as 'X' input to Rtsne::Rtsne.
#' @value an `IFCml` model of class `IFCml` and `IFCml_fit` or 'model' if model$idx is NULL
#' @keywords internal
fit.IFCml_set <- function(model, method, ..., verbose = FALSE) {
  if(length(model$idx) == 0) return(model)
  dots = list(...)
  session = dots$session
  dots = dots[names(dots) != "session"]
  if(!missing(method)) model$method <- method
  Q = checknames.IFCml(model)
  if(length(model$idx) == 0) stop("can't find model$idx")
  if(!identical(attr(model$idx, "id"), attr(model$data, "id"))) stop("'idx' was not built on 'dataset'")
  
  method = model$method
  method_alw = c("pca", "tsne", "umap", "som", "em", "svm", "xgb", "lda")
  method_alt = c("pca", "Rtsne", "umap", "som", "MclustDA", "svm", "xgb", "lda")
  method_fun = c("stats::prcomp", "Rtsne::Rtsne","umap::umap", "EmbedSOM::SOM", "mclust::MclustDA", "e1071::svm", "xgboost::xgboost", "MASS::lda")
  method_cla = c("prcomp", "list", "umap", "list", "MclustDA", "svm", "xgb.Booster", "lda")
  assert(method, len = 1, alw = method_alw)
  method_fun = method_fun[match(method, method_alw)]
  method_alt = method_alt[match(method, method_alw)]
  method_cla = method_cla[match(method, method_alw)]
  
  fun_ = strsplit(method_fun, split = "::", fixed = TRUE)[[1]]
  env_ = head(fun_, 1)
  if(!requireNamespace(env_, quietly = TRUE)) stop(env_, " is required to fit data. Please install it")
  
  # method parameters are appended with method$name_ so we need to remove it
  param = c(model$param, dots)
  param_names = names(param)
  if(length(param_names) != 0) param_names = sapply(strsplit(param_names, split = paste0(method_alt,"_"), fixed = TRUE), FUN = function(x) paste0(x[-1], collapse = ""))
  names(param) = param_names
  method_arg = formals(tail(fun_, 1), envir = asNamespace(env_))
  param = param[names(param) %in% names(method_arg)]
  
  train = model$data[model$idx, Q$is_feat, drop = FALSE]
  y_train = model$data[model$idx, Q$is_clust, drop = TRUE]
  
  pca_args = list(x = train, center = FALSE, scale. = FALSE, rank.=50) # we use the top most 50 components
  if((method == "pca") && (length(param$tol) != 0) && !is.na(param$tol)) pca_args = c(pca_args, list(tol = param$tol))
  pca_dr = do.call(what = prcomp, args = pca_args)
  proj_pca = predict(object = pca_dr, newdata = model$data[, Q$is_feat, drop=FALSE])
  attr(pca_dr, "id") = attr(model$idx, "id")
  attr(proj_pca, "id") = attr(model$idx, "id")
  model$pca <- pca_dr 
  model$proj_pca <- proj_pca
  
  fit = NULL
  if(verbose) cat("fitting data with method [\"",method,"\"]\n", sep="")
  switch(method, 
         "em" = {
           args = list(data = train, class = y_train)
         },
         "svm" = {
           args = list(x = train, y = y_train, scale = FALSE)
           if((length(param$gamma) !=0) && !is.na(param$gamma)) args = c(args, list(gamma = param$gamma))
         },
         "xgb" = {
           args = list(data = as.matrix(train), label = (as.integer(y_train) - 1L), verbose = 0,
                       objective = "multi:softprob", num_class = nlevels(y_train))
         },
         "lda" = {
           args = list(x = train, grouping = y_train)
           args = c(args, list(nu = na.omit(param$nu)))
         },
         "som" = {
           args = list(data = as.matrix(train))
           args = c(args, list(zdim = na.omit(param$zdim)))
           args = c(args, list(distf = as.integer(param$distf)))
           args = c(args, list(batch = param$batch == "yes"))
         },
         "tsne" = {
           sub = !apply(pca_dr$x, 1, anyNA) # in case pca produce some NA ?
           args = list(X = pca_dr$x[sub, ,drop=FALSE], dims = 3, 
                       pca = FALSE, partial_pca = FALSE, normalize = FALSE, pca_center = FALSE, pca_scale = FALSE, verbose = FALSE)
         },
         "umap" = {
           args = list(d = train, n_components = 3, method = "naive", verbose = 2)
         })
  if(method == "pca") {
    fit = pca_dr
  } else {
    args = c(args, param[!(names(param) %in% names(args))])
    args = args[sapply(args, length) != 0]
    sp = strsplit(method_fun, split = "::", fixed = TRUE)[[1]]
    fit = do.call(what = get(x = sp[2], envir = asNamespace(sp[1])), args = args)
  }
  
  attr(fit, "id") = attr(model$idx, "id")
  model$fit <- list(fit = fit, y_train = y_train)
  
  # reset pred, sub, feat
  return(reset.IFCml(model, what = c("pred","sub","feat"), class = c("IFCml","IFCml_fit")))
}

#' @title IFCml Data Prediction
#' @description
#' Predicts data in an `IFCml` model
#' @param model an `IFCml` model of class `IFCml_set`
#' @param newdata data on which the prediction will be performed. Allowed is either "train", "test", or "all". Default is "train".
#' @param ... other arguments to be passed.
#' @verbose whether to display information. Default is FALSE.
#' @details metaclustering will be done automatically when model$config$pops is of length 1.\cr
#' It will be performed by calling model$meta_fun with model$meta_args parameters.\cr
#' If model$meta_args are not found, default values are:\cr
#' -model$meta_args = list(centers = 10L, iter.max = 10L, nstart = 1L, algorithm = "Hartigan-Wong"), for "stats::kmeans".
#' @value an `IFCml` model of class `IFCml` and `IFCml_fit` or 'model' if model$idx is NULL
#' @keywords internal
predict.IFCml_fit <- function(model, newdata="train", ..., verbose = FALSE) {
  if(length(model$fit) == 0) return(model)
  dots = list(...)
  session = dots$session
  dots = dots[names(dots) != "session"]
  assert(newdata, len = 1, alw = c("train","test","all"))
  Q = checknames.IFCml(model)
  
  method = model$method
  fit = model$fit$fit
  y_train = model$fit$y_train
  switch (newdata,
          "all" = {
            sub_ = rep(TRUE, nrow(model$data))
            n_dat = model$data[, Q$is_feat, drop=FALSE]
            clust_ = model$data[, Q$is_clust, drop = TRUE]
          },
          "train" = {
            sub_ = model$idx
            n_dat = model$data[model$idx, Q$is_feat, drop = FALSE]
            clust_ = model$data[model$idx, Q$is_clust, drop = TRUE]
          },
          "test" = {
            sub_ = (!is.na(model$data[, Q$is_clust, drop = TRUE])) & (!model$idx)
            n_dat = model$data[sub_, Q$is_feat, drop = FALSE]
            clust_ = model$data[sub_, Q$is_clust, drop = TRUE]
          })
  lev = levels(model$data[, Q$is_clust, drop = TRUE])
  isna = apply(n_dat, 1, anyNA)
  sub_[isna] <- FALSE # almost all methods do not handle NA values
  n_dat = n_dat[!isna,,drop=FALSE]
  
  exported_feat = list()
  
  clust_NA = rep_len(NA, length.out = nrow(model$data))
  feat_NA = rep_len(NA, length.out = nrow(model$data))
  
  clust = NULL
  proj = NULL
  pred = NULL
  if(verbose) cat("applying prediction on data [\"",newdata,"\"] with method [\"",method,"\"]\n",sep="")
  if(nrow(n_dat) != 0) {
    switch(method, 
           "em" = { # pred
             pred = predict(fit, newdata = n_dat)
             pred = do.call(what = structure, c(list(.Data = pred$classification), pred[names(pred) != "classification"]))
           },
           "svm" = { # pred
             pred = predict(fit, newdata = n_dat)
           },
           "xgb" = { # pred
             pred = predict(fit, newdata = as.matrix(n_dat), reshape = FALSE)
             pred = matrix(pred, ncol=length(lev), byrow=TRUE)
             pred = factor(lev[apply(pred, 1, which.max)], levels = lev)
           },
           "lda" = { # pred + proj
             pred = predict(fit, newdata = n_dat)
             proj = pred
             pred = do.call(what = structure, c(list(.Data = pred$class), pred[names(pred) != "class"]))
             # project lda
             exported_feat = c(exported_feat, lapply(1:min(99, ncol(proj$x)), FUN = function(i) {
               feat_NA[sub_] <- proj$x[,i,drop=TRUE]
               buildFeature(name = sprintf("ML_lda_%02i_extra", i), val = feat_NA)
             }))
           },
           "som" = { # pred + proj
             # proj
             proj = EmbedSOM::EmbedSOM(data = n_dat, map = fit, coordsFn = EmbedSOM::MSTCoords())
             exported_feat = c(exported_feat, lapply(1:min(2, ncol(proj)), FUN = function(i) {
               feat_NA[sub_] <- proj[,i,drop=TRUE]
               buildFeature(name = sprintf("ML_som_%i_extra", i), val = feat_NA)
             }))
             # pred
             dcode = EmbedSOM::MapDataToCodes(fit$codes, data = n_dat)
             if(length(lev) > 1) {
               map = by(fit$mapping[, 1], y_train, FUN = function(x) {
                 dcode[, 1] %in% x
               })
               map = sapply(map, FUN = function(x) x)
               N = colnames(map)
               pred = apply(map, 1, FUN = function(x) {
                 foo = which(x)
                 if(length(foo) == 1) return(N[foo])
                 return(NA)
               })
               pred = factor(pred, levels = levels(model$data[,Q$is_clust, drop = TRUE]))
             }
           },
           "tsne" = { # proj
             # no pred possible, only projection on training set
             if(newdata == "train") if(any(attr(model$pca, "id") == attr(fit, "id"))) {
               proj = fit$Y
               sub = !apply(model$pca$x, 1, anyNA) # # in case pca produce some NA ?
               exported_feat = c(exported_feat, lapply(1:min(3, ncol(fit$Y)), FUN = function(i) {
                 feat_NA[sub_[sub]] <- fit$Y[, i, drop = TRUE]
                 buildFeature(name = sprintf("ML_tsne_%i_extra", i), val = feat_NA)
               }))
             } else {
               warning("'tsne', can't make prediction: 'id' do not match")
             }
           },
           "umap" = { # proj
             proj = suppressMessages(predict(object = fit, data = n_dat))
             exported_feat = c(exported_feat, lapply(1:min(3, ncol(proj)), FUN = function(i) {
               feat_NA[sub_] <- proj[,i, drop = TRUE]
               buildFeature(name = sprintf("ML_umap_%i_extra", i), val = feat_NA)
             }))
           },
           "pca" = { # proj, whatever happen "pca" features are exported  
             proj = model$proj_pca[sub_,,drop=FALSE]
           })
  }
  
  # pca projection is always exported  
  exported_feat = c(exported_feat, lapply(1:min(9, ncol(model$proj_pca)), FUN = function(i) {
    feat_NA[sub_] <- model$proj_pca[sub_,i,drop=TRUE]
    buildFeature(name = sprintf("ML_pca_%i_extra", i), val = feat_NA)
  }))
  
  model$proj = proj
  model$sub = sub_
  model$pred = pred
  
  model$meta_fun <- NULL
  if(length(lev) == 1) {
    if(verbose) cat("computing metaclusters\n")
    if((method %in% c("som","tsne","umap","pca"))) {
      model$meta_fun = "stats::kmeans"
      meta_fun = strsplit(model$meta_fun, split = "::", fixed = TRUE)[[1]]
      meta_env = head(meta_fun,1)
      meta_fun = tail(meta_fun,1)
      if(meta_env == meta_fun) {
        n_meta = names(formals(meta_fun)) 
      } else {
        n_meta = names(formals(meta_fun, envir = asNamespace(meta_env, base.OK = FALSE)))
      }
      N = names(dots)
      N = N[N %in% c(n_meta[-1], "seed")]
      if(length(N) != 0) for(i in names(dots)) model$meta_args <- dots[[N[i]]]
      meta_args <- model$meta_args
      clust = clust_NA
      if(length(meta_args) == 0) {
        if(meta_fun == "kmeans") {
          model$meta_args = list(centers = 10L, iter.max = 10L, nstart = 1L, algorithm = "Hartigan-Wong")
        }
        meta_args <- model$meta_args
        N = names(model$meta_args)
        warning("no \"metaclust\" args found. '",model$meta_fun, "' will use:\n-", paste0(sapply(1:length(model$meta_args), FUN = function(i) paste0(N[i]," = [",typeof(meta_args[[i]]),"] ", meta_args[[i]])), collapse="\n-"))
      }
      switch(method,
             "tsne" = {
               if(newdata == "train") if(any(attr(model$pca, "id") == attr(fit, "id"))) {
                 sub = !apply(model$pca$x, 1, anyNA) # # in case pca produce some NA ?
                 K = do.call(kmeans, c(list(x = proj), model$meta_args))
                 clust[sub_[sub]] = K$cluster
                 model$meta_args$centers = K$centers
               } else {
                 clust = NULL
                 warning("'tsne', can't make metaclust: 'id' do not match")
               }
             }, 
             "som" = { # in som clustering is done on map$codes and not on MST projection
                 K = do.call(kmeans, c(list(x = dcode), model$meta_args))
                 clust[sub_] = K$cluster
                 model$meta_args$centers = K$centers
             },
             {
               K = do.call(kmeans, c(list(x = proj), model$meta_args))
               clust[sub_] = K$cluster
               model$meta_args$centers = K$centers
             })
      if(length(clust)!=0) clust = factor(clust - 1L, levels = sort(unique(clust - 1L)))
    }
  }
  model$clust = clust
  model$feat = exported_feat
  return(reset.IFCml(model, class = c("IFCml","IFCml_pred")))
}

#' @title IFCml Preprocessing on IFC_data
#' @description
#' Preprocess `IFC_data` object according to `IFCml` model
#' @param model an `IFCml` model
#' @param obj an `IFC_data` model
#' @param mode the desired mode of action to preprocess 'obj'. Allowed are "self","predict","predict_norm", and "full".\cr
#' -"self", will do nothing,\cr
#' -"predict", will apply preprocessing on "obj" using parameters found in model$config but using exactly the features resulted from creation of model,\cr
#' -"predict_norm", will perform as "predict" except that centering and scaling (if any) will be applied with values already computed in model$config,\cr
#' -"full", will apply preprocessing using parameters found in model$config.
#' @param ... other arguments to be passed.
#' @value an `IFCml` model
#' @keywords internal
preprocess.IFCml <- function(model, obj, mode = c("self","predict","predict_norm","full")[1], ...) {
  dots = list(...)
  assert(obj, cla = "IFC_data")
  assert(mode, len = 1, alw = c("self","predict","predict_norm","full"))
  obj_count <- obj$description$ID$objcount
  Q = checknames.IFCml(model)
  
  switch(mode,
         "self" = {
           if(nrow(model$data) != obj_count) stop("'mode' is \"self\" but 'model' has not been built with 'obj'")
           return(model)
         },
         "predict" = {
           # here centering and scaling if any are done with current obj value and not
           # retrieve from former preprocessing
           # retrieve model preprocessing input parameters
           config = model$config
           
           # ensure no features will be removed due to remove_ parameters
           # force remove_ to FALSE
           for(i in c("sys","sat","pix","raw","nan")) config[[paste0("remove_",i)]] <- FALSE
           config[["remove_nzv"]] <- -1L  # forbid removal of zero AND near from zero variance
           config[["remove_pat"]] <- NULL # forbid removal due to pattern
           config[["features_cor"]] <- 0  # forbid removal due to correlation
           config[["features_bst"]] <- 0  # forbid removal due to best features selection
           
           # force use of same features as in model
           config[["features_kep"]] = Q$is_feat
           
           # force features transformation
           config$int_to_tra = model$int
           config$har_to_tra = model$har
           config$features_inp = NULL
           config$features_hap = NULL
           config = reset.IFCml(config, what = c("features_inp","features_hap","remove_pat"), cla = NULL)
           
           # disable groups checking
           config$check_groups = FALSE
           
           # build model
           n_model = do.call(preprocess, args = c(list(obj=obj), config, ...))
           
           # copy back all initial parameters
           n_model$config <- model$config
         },
         "predict_norm" = {
           # preprocess data using the same data as the ones used in model fit
           # retrieve model preprocessing input parameters
           config = model$config
           
           # ensure no features will be removed due to remove_ parameters
           # force remove_ to FALSE
           for(i in c("sys","sat","pix","raw","nan")) config[[paste0("remove_",i)]] <- FALSE
           config[["remove_nzv"]] <- -1L  # forbid removal of zero AND near from zero variance
           config[["remove_pat"]] <- NULL # forbid removal due to pattern
           config[["features_cor"]] <- 0  # forbid removal due to correlation
           config[["features_bst"]] <- 0  # forbid removal due to best features selection
           
           # force use of same features as in model
           config[["features_kep"]] = Q$is_feat
           
           # force features transformation
           config$int_to_tra = model$int
           config$har_to_tra = model$har
           config$features_inp = NULL
           config$features_hap = NULL
           config = reset.list(config, what = c("features_inp","features_hap","remove_pat"), cla = NULL)
           
           # disable groups checking
           config$check_groups = FALSE
           
           # force feature centering, scaling if any
           config$features_cen = model$cen
           config$features_scl = model$scl
           
           # build model
           n_model = do.call(preprocess, args = c(list(obj=obj), config, ...))
           
           # copy back all initial parameters
           n_model$config <- model$config
         },
         "full" = {
           n_model = do.call(what = preprocess, args = c(list(obj = obj), model$config, ...))
         })
  return(n_model)
}

#' @title Apply IFCml on IFC_data
#' @description
#' Applies an `IFCml` model on an `IFC_data` object
#' @param model an `IFCml` model
#' @param obj an `IFC_data` model
#' @param mode the desired mode of action to preprocess 'obj'. Allowed are "self","predict","predict_norm", and "full".\cr
#' -"self", will do nothing,\cr
#' -"predict", will apply preprocessing on "obj" using parameters found in model$config but using exactly the features resulted from creation of model,\cr
#' -"predict_norm", will perform as "predict" except that centering and scaling (if any) will be applied with values already computed in model$config,\cr
#' -"full", will apply preprocessing using parameters found in model$config.
#' @param newdata data on which the prediction will be performed. Allowed is either "train", "test", or "all". Default is "all".
#' @param self_split only applies when mode is self, whether to do a new data splitting . Default is FALSE.
#' If set to TRUE, this will force mode to "full" since new fitting should be performed is any.
#' @param ... other arguments to be passed.
#' @value an `IFCml` model
#' @keywords internal
apply.IFCml <- function(model, obj, mode = c("self", "predict", "predict_norm", "full")[1], newdata = "all", self_split = FALSE, ...) {
  dots = list(...)
  session = dots$session
  assert(obj, cla = "IFC_data")
  assert(mode, len = 1, alw = c("self","predict","predict_norm","full"))
  obj_count <- obj$description$ID$objcount
  Q = checknames.IFCml(model)

  if(nrow(model$data) != obj_count) {
    mode = "predict_norm"
    warning("'model' has not been built with 'obj', 'mode' has been set to \"predict_norm\"")
  }
  
  # preprocess data
  n_model = do.call(preprocess.IFCml, args = c(list(model = model, obj=obj, mode = mode), ...))
  
  # if self_split is required then a new split is done
  # and consequently a new fitting needs to be performed
  if((mode == "self") && self_split) {
    warning("'mode' has forced to \"full\" since a new splitting has been required on \"self\" ")
    mode = "full"
  }
  
  # we can't apply any prediction if model does not contain any fitting
  if(length(model$fit) != 0) {
    n_model$method <- model$method
    switch(mode,
           "predict" = {
             # use the fitting included in model
             n_model = do.call(what = splitdata.IFCml, args = c(list(model = n_model, ratio = model$ratio), ...))
             n_model$fit <- model$fit
             n_model$pca <- model$pca
             n_model$proj_pca <- predict(object = n_model$pca, newdata = n_model$data[, Q$is_feat, drop=FALSE])
             attr(n_model$proj_pca, "id") <- attr(model$fit, "id")
           },
           "predict_norm" = {
             # use the fitting included in model
             n_model = do.call(what = splitdata.IFCml, args = c(list(model = n_model, ratio = model$ratio), ...))
             n_model$fit <- model$fit
             n_model$pca <- model$pca
             n_model$proj_pca <- predict(object = n_model$pca, newdata = n_model$data[, Q$is_feat, drop=FALSE])
             attr(n_model$proj_pca, "id") <- attr(model$fit, "id")
           },
           "full" = {
             # fit the data with obj, according to model parameters
             n_model = do.call(what = splitdata.IFCml, args = c(list(model = n_model, ratio = model$ratio), ...))
             n_model$param <- model$param
             n_model = do.call(what = fit.IFCml_set, args = c(list(model = n_model, method = model$method), ...))
           })
    # apply prediction on the new obj
    if(mode != "self") {
      n_model$meta_fun <- model$meta_fun
      n_model$meta_args <- model$meta_args
    }
    n_model = do.call(what = predict.IFCml_fit, args = c(list(model = n_model, newdata = newdata), ...))
  }
  # remove former ML features / pops
  feat_to_rm = grep(paste0("^ML_pca_|^ML_",n_model$method,"_"), names(obj$features), value = TRUE, invert = FALSE)
  n_obj = suppressWarnings(IFC::data_rm_features(obj, features = feat_to_rm,
                                                 list_only = FALSE, session=session))
  n_obj = suppressWarnings(IFC::data_rm_pops(n_obj, pops = grep("^ML_subset|^ML_meta|^ML_pred", names(n_obj$pops), value = TRUE), 
                                             list_only = FALSE, adjust_graph = FALSE, session = session))
  
  lev = levels(n_model$data[, Q$is_clust, drop = TRUE])
  pred_NA = rep_len(NA, length.out = nrow(model$data))
  pred_FALSE = rep_len(FALSE, length.out = nrow(model$data))
  style = attr(model$config$pops, "style")
  lightModeColor = attr(model$config$pops, "lightModeColor")
  color = attr(model$config$pops, "color")
  style = closest_style(style)
  
  exported_pops = list(buildPopulation(name = "ML_subset", type = "C",
                                       definition = paste0(n_model$config$pops, collapse = "|Or|"),
                                       style = 20,
                                       color = "White", lightModeColor = "Black"))
  
  if(length(n_model$pred) != 0) {
    pred = n_model$pred
    sub_ = n_model$sub
    foo = !is.na(pred) # some prediction can result in NA (e.g. som)
    pred_NA[sub_][foo] <- as.character(pred[foo])
    exported_pops = c(exported_pops, lapply(lev, FUN = function(p) {
      tmp = (!is.na(pred_NA)) & (pred_NA == p) 
      if(any(tmp)) {
        buildPopulation(name = paste0("ML_pred_",p), type = "T", 
                        lightModeColor = lightModeColor[p],
                        color = color[p],
                        style = style[p],
                        obj = tmp)
      }
    }))
  }
  
  # if(mode %in% c("self", "full")) { 
  if(length(n_model$fit) != 0) {
    sub_train = n_model$idx
    y_train = n_model$data[sub_train, Q$is_clust, drop = TRUE]
    sub_test = (!is.na(n_model$data[, Q$is_clust, drop = TRUE])) & (!n_model$idx)
    y_test = n_model$data[sub_test, Q$is_clust, drop = TRUE]
    exported_pops = c(exported_pops,
                      list(buildPopulation(name = "ML_subset_train", type = "T",
                                           obj = sub_train,
                                           style = 0,
                                           color = "Chartreuse", lightModeColor = "Green4"),
                           buildPopulation(name = "ML_subset_test", type = "T",
                                           obj = sub_test,
                                           style = 2,
                                           color = "Orange", lightModeColor = "DarkOrange")))
  }
  if(length(n_model$pred != 0)) {
    if(newdata %in% c("all", "train")) {
      exported_pops = c(exported_pops, lapply(lev, FUN = function(p) {
        tmp = (!is.na(y_train)) & (is.na(pred[sub_train] == y_train) | (pred[sub_train] != y_train)) & (y_train == p)
        if(any(tmp)) {
          pred_FALSE[sub_train][tmp]  <- TRUE
          buildPopulation(name = paste0("ML_pred_train_", p, "_mismatch"), type = "T",
                          lightModeColor = lightModeColor[p],
                          color = color[p],
                          style = style[p],
                          obj = pred_FALSE)
        }
      }))
    }
    if(newdata %in% c("all", "test")) {
      sub_train = n_model$idx
      y_train = n_model$data[sub_train, Q$is_clust, drop = TRUE]
      sub_test = (!is.na(n_model$data[, Q$is_clust, drop = TRUE])) & (!n_model$idx)
      y_test = n_model$data[sub_test, Q$is_clust, drop = TRUE]
      exported_pops = c(exported_pops, lapply(lev, FUN = function(p) {
        tmp = (!is.na(y_test)) & (is.na(pred[sub_test] == y_test) | (pred[sub_test] != y_test)) & (y_test == p)
        if(any(tmp)) {
          pred_FALSE[sub_test][tmp]  <- TRUE
          # print(paste0("ML_pred_test_", p, "_mismatch"))
          # print(sum(tmp))
          # print(sum(pred_FALSE))
          buildPopulation(name = paste0("ML_pred_test_", p, "_mismatch"), type = "T",
                          lightModeColor = lightModeColor[p],
                          color = color[p],
                          style = style[p],
                          obj = pred_FALSE)
        }
      }))
    }
  }
  # }
  
  clust = n_model$clust
  if(length(clust) != 0) {
    L = length(levels(as.factor(clust)))
    pal = c("CornflowerBlue", "IndianRed", "DarkTurquoise", "Gold", "Cyan4", "DarkOrchid", "DeepPink", "Tomato")
    lightModeColor = sapply(pal, FUN = function(x) IFC::paletteIFC(col = x, "to_light")[1,2])
    color = sapply(pal, FUN = function(x) IFC::paletteIFC(col = x, "to_dark")[1,2])
    style = rep(c(20, 4, 3, 1, 5, 0, 2, 18, 15, 17), length.out = L)
    lightModeColor = rep(lightModeColor, length.out = L)
    color = rep(color, length.out = L)
    exported_pops = c(exported_pops,lapply(levels(as.factor(clust)), FUN = function(p) {
      pp = as.integer(p) + 1L
      buildPopulation(name = sprintf("ML_meta_%02i", pp - 1L),
                      lightModeColor = lightModeColor[pp],
                      color = color[pp],
                      style = style[pp],
                      type = "T", 
                      obj = !is.na(clust) & clust == p)
    })) 
  }
  
  # add new ML features / pops
  exported_feat = n_model$feat
  exported_feat = exported_feat[sapply(exported_feat, length) != 0]
  if(length(exported_feat) !=0) {
    n_obj = IFC::data_add_features(n_obj, features = n_model$feat, session = session)
  }
  exported_pops = exported_pops[sapply(exported_pops, length) != 0]
  exported_pops = exported_pops[sapply(exported_pops, FUN = function(p) ifelse(p$type == "T", sum(p$obj), 1)) != 0]
  if(length(exported_pops) !=0) {
    n_obj = IFC::data_add_pops(n_obj, pops = exported_pops, session = session)
    # we set ML_ population as reserved population.
    for(i in exported_pops) { attr(n_obj$pops[[i$name]], "reserved") <- TRUE }
  }
  return(list(obj = n_obj, model = n_model))
}
