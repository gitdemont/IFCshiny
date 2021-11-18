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

# function to create confusion matrrix plot
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
  mdl = c("pca","tsne","umap","flowsom","em","svm","xgb","lda")
  pattern = x == mdl
  pattern = c("^pca_","^Rtsne_","^umap_","^flowsom_","^MclustDA_","^svm","^xgb_","^lda_")[pattern]
  N = names(session$input)
  return(setdiff(N[grep(pattern, N)], paste0(mdl, to_exlude)))
}

#' @title preprocess
#' @description
#' Function to make tensorboard embedding.
#' @param obj an `IFC_data` object extracted by ExtractFromDAF(extract_features = TRUE) or ExtractFromXIF(extract_features = TRUE).
#' @param pops name(s) of population(s) present in 'obj' that will be used to determine clusters and features that better separate each one.
#' If only one pop is provided, no best features is computed and only preprocess object is returned.
#' /!\ Please note that if multiple pops are provided, no more thant 80% of objects belonging to each pops should overlap with other pops.
#' @param remove_sys, whether to remove system features whose definition are "Time", "Flow Speed", "Camera Timer", "Camera Line Number", "Objects per mL", "Objects per sec" and "Object Number" or only "Camera Line Number" for fcs, Default is TRUE
#' @param remove_sat, whether to remove system features whose definition are "Saturation Count" and "Saturation Percent",Default is TRUE
#' @param remove_pix, whether to remove system features whose definition contains "Pixel". Default is TRUE
#' @param remove_pix, whether to remove system features whose definition contains "Raw" or "Uncompensated". Default is TRUE\
#' @param remove_pat, removes features names matching with the input pattern. Default is "^ML_"
#' @param remove_nzv, whether to remove features with near from zero variance nzv() from caret package. Default is FALSE
#' @param features_int, (used for intensities features) whether to transform features matching with 'config$features_inp'. Default is TRUE
#' @param features_inp, pattern used to find features to transform. Default is ifelse(length(obj$description$FCS) == 0, "^Intensity|^Bright Detail Intensity|^Uncompensated", "^(?!.*FS|.*SS|.*LOG).*$").\cr
#' It applies to features definition except for FCS where it applies to features names.
#' @param features_inm, method (a string) used to transform features matching with 'config$features_inp'. Default is ifelse(length(obj$description$FCS) == 0, "LinLog", "Arcsinh")
#' @param features_har, (used for haralick features) whether to transform features matching with 'config$features_hap'. Default is TRUE
#' @param features_hap, pattern used to find features to transform. Default is "^H ".\cr
#' It applies to features definition except for FCS where it applies to features names.
#' @param features_ham, method (a string) used to transform features matching with 'config$features_hap'. Default is "yeojohnson"
#' @param features_cen, whether to center features. Default is TRUE
#' @param features_cem, method (expecting a function) used to center features. Default is mean
#' @param features_scl, whether to scale features. Default is TRUE
#' @param features_scm, method (expecting a function) used to center features. Default is sd
#' @param features_cor, values used to determine correlation threshold above which features are kept. Default is 0 to keep all features.\cr
#' If the value is equal or superior to 1, if will be set to 0.95 automatically.\cr
#' @param features_bst, number of features that discriminate each couple of 'pops' at best. Default is 0, to use all features.\cr
#' If pops is of length one this argument will not be used.\cr
#' @param features_kep, a character vector of the only desired features to use for preprocessing the data. Default is character(0), to apply the preprocessing on all features.
#' @details The preprocessing workflow applied is:
#' -undesired features among desired are first removed,\cr
#' -transformation are applied,\cr
#' -features resulting of NA values or no variance are removed,\cr
#' -features are centered and scaled\cr
#' -features with near from zero variance or highly correlated are removed\cr
#' -only best discriminant features are kept.
#' @param display_progress whether to display information about embedding. Default is TRUE.
#' @param ... other arguments to be passed.
#' @return A named list, whose members are:\cr
#' -data, a preprocessed data.frame,\cr
#' -sub, the subset used as defined by 'pops',\cr
#' -cen, the values used to center the features,\cr
#' -cem, the method used to center the features,\cr
#' -scl, the values used to scale the features,\cr
#' -scm, the method used to scale the features,\cr
#' -int, a character vector of features names where int transformation was applied,\cr
#' -inm, the method used for int transformation,\cr
#' -har, a character vector of features names where har transformation was applied,\cr
#' -ham, the method used for har transformation,\cr
#' -config, a list with the config input.
#' @export
preprocess = function(obj, pops = "All",
                      remove_sys = TRUE,
                      remove_sat = TRUE,
                      remove_pix = TRUE,
                      remove_raw = TRUE,
                      remove_pat = "^ML_",
                      remove_nzv = FALSE,
                      features_int = TRUE,
                      features_inp = ifelse(length(obj$description$FCS) == 0, "^Intensity|^Bright Detail Intensity|^Uncompensated", "^(?!.*FS|.*SS|.*LOG).*$"),
                      features_inm = ifelse(length(obj$description$FCS) == 0, "LinLog", "Arcsinh"),
                      features_har = TRUE,
                      features_hap = "^H ",
                      features_ham = "yeojohnson",
                      features_cen = TRUE,
                      features_cem = mean,
                      features_scl = TRUE,
                      features_scm = sd,
                      features_cor = 0,
                      features_bst = 0,
                      features_kep = character(0), ...) {
  dots = list(...)
  
  # check every input training population will be present
  g = sapply(obj$pops[pops], FUN = function(p) p$obj)
  sub = apply(g, 1, any)
  groups = apply(g, 1, FUN = function(x) {
    foo = which(x)
    if(length(foo) == 1) return(pops[foo])
    return(NA)
  })
  groups = factor(groups[sub], levels = pops, labels = pops)
  check = table(groups)/apply(g, 2, sum)
  if(any(check < 0.8)) stop("too many cells belong to more than 1 unique population. check training pops !")
  if(length(na.omit(unique(groups))) != length(pops)) stop("too many cells belong to more than 1 unique population. check training pops !")
  
  # retrieve data and place Object Number 1st
  df = obj$features
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
  int_to_tra = NULL
  har_to_tra = NULL
  
  ans = list(data = data.frame(),
             sub = logical(),
             cen = numeric(),
             cem = features_cem,
             scl = numeric(),
             scm = features_scm,
             int = character(),
             inm = character(),
             har = character(),
             ham = character(),
             config = c(list(pops = pops,
                             remove_sys = remove_sys,
                             remove_sat = remove_sat,
                             remove_pix = remove_pix,
                             remove_raw = remove_raw,
                             remove_pat = remove_pat,
                             remove_nzv = remove_nzv,
                             features_int = features_int,
                             features_inp = features_inp, #ifelse(length(obj$description$FCS) == 0, "^Intensity|^Bright Detail Intensity|^Uncompensated", "^(?!.*FS|.*SS|.*LOG).*$"),
                             features_inm = features_inm, #ifelse(length(obj$description$FCS) == 0, "LinLog", "Arcsinh"),
                             features_har = features_har,
                             features_hap = features_hap, #"^H ",
                             features_ham = features_ham, # "yeojohnson",
                             features_cen = features_cen,
                             features_cem = features_cem,
                             features_scl = features_scl,
                             features_scm = features_scm,
                             features_cor = features_cor,
                             features_bst = features_bst,
                             features_kep = features_kep), dots))
  
  if(length(remove_pat) ==0) to_rm_fea = grep(remove_pat, names(df), value = TRUE, invert = FALSE)
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
    int_to_tra = grep(features_inp, def_def, value = FALSE, invert = FALSE, perl = TRUE)
    int_to_tra = def_nam[unique(intersect(int_to_tra, def_sub))]
    # haralick
    har_to_tra = grep(features_hap, def_def, value = FALSE, invert = FALSE, perl = TRUE)
    har_to_tra = def_nam[unique(intersect(har_to_tra, def_sub))]
  } else {
    if(remove_sys) to_rm = def_nam[c(to_rm, which(def_nam == "Camera Line Number"))]
    int_to_tra = setdiff(grep(features_inp, def_nam, value = TRUE, invert = FALSE, perl = TRUE), "Object Number")
    har_to_tra = setdiff(grep(features_hap, def_nam, value = TRUE, invert = FALSE, perl = TRUE), "Object Number")
  }
  
  if(length(features_kep) == 0) {
    df = df[, c("Object Number", setdiff(names(df), c(to_rm, to_rm_fea, c("Object Number", "clust"))))]
  } else {
    df = df[, c("Object Number", setdiff(features_kep, c(to_rm, to_rm_fea, c("Object Number", "clust"))))]
  }
  df = df[, unique(names(df))]
  
  # remove features with no variance
  df_sd = apply(df[sub, -1], 2, sd, na.rm = TRUE)
  df_sd = which(is.na(df_sd) | (df_sd == 0))
  if(length(df_sd) !=0) df = df[, -(df_sd + 1)]
  
  # transform intensities
  if(features_int) {
    int_to_tra = int_to_tra[int_to_tra %in% names(df)]
    if(length(int_to_tra) != 0) {
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
      switch(features_ham,
             "yeojohnson" = {
               df[, har_to_tra] = sapply(har_to_tra, FUN = function(i_trans) yeojohnson(x = df[, i_trans], standardize = FALSE)$x.t)
               ans$ham = "yeojohnson"
             },
             stop("the transformation algorithm:", features_ham," is not supported"))
      ans$har = har_to_tra
    }
  }
  
  # remove non finite values
  tokeep = which(apply(apply(df[sub, -1], 2, is.finite), 2, all))
  df = df[, c("Object Number", names(df[,-1])[tokeep])]
  
  # center
  if(features_cen) {
    df_cen = apply(df[sub, -1], 2, features_cem, na.rm = TRUE)
  } else {
    df_cen = rep(0, ncol(df) - 1)
  }
  names(df_cen) = names(df)[-1]
  ans$cen = df_cen
  
  # scale
  if(features_scl) {
    df_scl = apply(df[sub, -1], 2, features_scm, na.rm = TRUE)
  } else {
    df_scl = rep(1, ncol(df) -1)
  }
  names(df_scl) = names(df)[-1]
  ans$scl = df_scl
  
  # center/scale data
  if(any(c(features_cen, features_scl)))
    df[, 2:ncol(df)] = sapply(2:ncol(df), FUN = function(i) (df[, i] - df_cen[i-1]) / df_scl[i-1])
  
  # remove nzv
  if(remove_nzv) {
    to_rm_nzv = nzv(df[sub, -1], names = TRUE)
    if(length(to_rm_nzv) != 0) df = df[, setdiff(names(df), to_rm_nzv)]
  }
  
  # remove correlated features
  if(features_cor > 0) {
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
  ans$sub <- sub
  
  # reduce to subset
  df = df[sub, ]
  if((length(pops) != 1) && (features_bst != 0)) {
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
    ans$data = ans$data[, unique(c("Object Number", setdiff(best, c("Object Number", "clust"))))]
  } else {
    ans$data = ans$data[, c("Object Number", setdiff(colnames(df), c("Object Number", "clust")))]
  }
  kept = setdiff(colnames(ans$data), c("Object Number", "clust"))
  ans$data$clust <- NA
  ans$data$clust[sub] <- as.character(groups)
  ans$cen = ans$cen[kept]
  ans$scl = ans$scl[kept]
  ans$har = ans$har[ans$har %in% kept]
  ans$int = ans$int[ans$int %in% kept]
  return(ans)
}