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

## define cleanup to recover options and environment
.cleanup = list(ls = setdiff(ls(all.names = TRUE),"..."), env = environment(), options = options())
shiny::onStop(fun = function() {
  # options(.cleanup$options)
  # if(!("shiny.maxRequestSize" %in% names(.cleanup$options))) options("shiny.maxRequestSize"=NULL)
  # for(i in .cleanup$ls) assign(x = i, value = get(x = i, envir = .cleanup$env), envir = .cleanup$env)
  # rm(list = setdiff(ls(all.names = TRUE, envir = .cleanup$env), .cleanup$ls), envir = .cleanup$env)
  return(invisible(NULL))
})

## set options for rgl
options(rgl.printRglwidget = TRUE)
options(rgl.inShiny = TRUE)
options(rgl.useNULL = TRUE)

## extract current running dir when file is sourced
.runfiles <- lapply(sys.frames(), function(x) x$ofile)
.runfiles <- .runfiles[sapply(.runfiles, length) != 0]
.rundir = getwd()
if(length(.runfiles) != 0) .rundir = dirname(.runfiles[[length(.runfiles)]])

## setup authentication or not + python used
# For authentication
# authentication will be used if any of .path_to_db and .passphrase variables exists
# if .passphrase or .path_to_db are not found in evaluating environment values from
# Sys.getenv("PASSPHRASE") and Sys.getenv("PATH_TO_DB") are used
# if values different from "" are found they will be used to open the database
# in such case, if the database can't be opened the app will stop
if(!exists(".path_to_db") || !exists(".passphrase") || (.passphrase == "") || (.path_to_db == "")) {
  .passphrase = Sys.getenv("PASSPHRASE", "")
  .path_to_db = Sys.getenv("PATH_TO_DB", "")
}
# In addition, if app is run from installed IFCshiny package, from 
# IFCshiny::runIFCshinyApp no authentification will be required
if(.libPaths() %in% dirname(dirname(dirname(.rundir)))) .passphrase = ""
if(!exists(".path_to_python")) {
  .path_to_python = Sys.getenv("PATH_TO_PYTHON", "")
}

# For python
# Python path can be provided through .path_to_python variable
# if this variable is not found in evaluating environment value from Sys.getenv("PATH_TO_PYTHON", "") will be used
# if this results in something different from "", it will be passed to reticulate::use_python(required = TRUE)
# Otherwise, reticulate::py_discover_config(required_module = "tensorflow", use_environment = "r-reticulate") will be used
# if(!exists(".path_to_python")) {
#   .path_to_python = Sys.getenv("PATH_TO_PYTHON", "")
# }
# if(.path_to_python == "") {
#   reticulate::py_discover_config(required_module = "tensorflow")
# } else {
#   reticulate::use_python(.path_to_python, required = TRUE)
# }
# 
# .tensorflow_avl = try(reticulate::py_module_available("tensorflow"), silent = TRUE)
# if(inherits(what = "try-error", x = .tensorflow_avl)) .tensorflow_avl = FALSE

# ###### specific part for shinyapps.io
# reticulate::py_discover_config("numpy")
# .tensorflow_avl = FALSE
# ###### specific part for shinyapps.io END

suppressMessages(suppressWarnings({
  #' IFC package is mandatory for reading / writing conventional and imaging flow cytometry files
  require(IFC, quietly = TRUE, warn.conflicts = FALSE)
  
  #' for interaction
  require(shiny, quietly = TRUE, warn.conflicts = FALSE)
  require(shinyjs, quietly = TRUE, warn.conflicts = FALSE)
  require(shinymanager, quietly = TRUE, warn.conflicts = FALSE)
  require(shinyFeedback, quietly = TRUE, warn.conflicts = FALSE)
  require(shinyWidgets, quietly = TRUE, warn.conflicts = FALSE)
  require(jsonlite, quietly = TRUE, warn.conflicts = FALSE)
  require(visNetwork, quietly = TRUE, warn.conflicts = FALSE)
  require(colourpicker, quietly = TRUE, warn.conflicts = FALSE)
  require(DT, quietly = TRUE, warn.conflicts = FALSE)
  
  #' for export
  require(zip, quietly = TRUE, warn.conflicts = FALSE)
  require(openxlsx, quietly = TRUE, warn.conflicts = FALSE)
  require(htmlwidgets, quietly = TRUE, warn.conflicts = FALSE)
  
  #' IFCshiny has soft dependency on flowCore and FlowSOM, it will work without these packages
  # this allows IFCshiny not to be dependent on bioconductor
  # if(requireNamespace("flowCore", quietly = TRUE)) require(flowCore, quietly = TRUE, warn.conflicts = FALSE)
  # if(requireNamespace("FlowSOM", quietly = TRUE)) require(FlowSOM, quietly = TRUE, warn.conflicts = FALSE)
  
  #' model & dim reduction
  require(caret, quietly = TRUE, warn.conflicts = FALSE)
  require(MASS, quietly = TRUE, warn.conflicts = FALSE)
  require(mclust, quietly = TRUE, warn.conflicts = FALSE)
  require(e1071, quietly = TRUE, warn.conflicts = FALSE)
  require(xgboost, quietly = TRUE, warn.conflicts = FALSE)
  require(bestNormalize, quietly = TRUE, warn.conflicts = FALSE)
  require(Rtsne, quietly = TRUE, warn.conflicts = FALSE)
  require(umap, quietly = TRUE, warn.conflicts = FALSE)
  
  #' representation
  require(RColorBrewer, quietly = TRUE, warn.conflicts = FALSE)
  require(viridisLite, quietly = TRUE, warn.conflicts = FALSE)
  require(rmarkdown, quietly = TRUE, warn.conflicts = FALSE)
  require(rgl, quietly = TRUE, warn.conflicts = FALSE)
  require(grDevices, quietly = TRUE, warn.conflicts = FALSE)
  require(grid, quietly = TRUE, warn.conflicts = FALSE)
  require(gridExtra, quietly = TRUE, warn.conflicts = FALSE)
  require(plotly, quietly = TRUE, warn.conflicts = FALSE)
  
  #' parallelization
  require(utils, quietly = TRUE, warn.conflicts = FALSE)
  require(parallel, quietly = TRUE, warn.conflicts = FALSE)
  require(doParallel, quietly = TRUE, warn.conflicts = FALSE)
}))

## all the following are imports of non-exported function from IFC package
# I am the maintainer of IFC so this should not be an issue to get it on CRAN
# in addition, no function is imported through :::
# imports from IFC
#' @name assert
#' @keywords internal
assert <- getFromNamespace("assert", "IFC")

#' @name addText
#' @keywords internal
addText <- getFromNamespace("addText", "IFC")

#' @name convert_to_baseplot
#' @keywords internal
convert_to_baseplot <- getFromNamespace("convert_to_baseplot", "IFC")

#' @name specialr
#' @keywords internal
specialr <- getFromNamespace("specialr", "IFC")

#' @name getFileExt
#' @keywords internal
getFileExt <- getFromNamespace("getFileExt", "IFC")

#' @name remove_ext
#' @keywords internal
remove_ext <- getFromNamespace("remove_ext", "IFC")

#' @name cpp_base64_encode
#' @keywords internal
cpp_base64_encode <- getFromNamespace("cpp_base64_encode", "IFC")

#' @name cpp_writeBMP
#' @keywords internal
cpp_writeBMP <- getFromNamespace("cpp_writeBMP", "IFC")

#' @name cpp_pnt_in_gate
#' @keywords internal
cpp_pnt_in_gate <- getFromNamespace("cpp_pnt_in_gate", "IFC")

#' @name objectTransform
#' @keywords internal
objectTransform <- getFromNamespace("objectTransform", "IFC")

#' @name getImagesValues
#' @keywords internal
getImagesValues <- getFromNamespace("getImagesValues", "IFC")

#' @name splitn
#' @keywords internal
splitn <- getFromNamespace("splitn", "IFC")

#' @name popsWithin
#' @keywords internal
popsWithin <- getFromNamespace("popsWithin", "IFC")

#' @name popsRetrieveGraph
#' @keywords internal
popsRetrieveGraph <- getFromNamespace("popsRetrieveGraph", "IFC")

#' @name popsGetSiblings
#' @keywords internal
popsGetSiblings <- getFromNamespace("popsGetSiblings", "IFC")

#' @name extractStats
#' @keywords internal
extractStats <- getFromNamespace("extractStats", "IFC")

#' @name adjustGraph
#' @keywords internal
adjustGraph <- getFromNamespace("adjustGraph", "IFC")

#' @name computeGamma
#' @keywords internal
computeGamma <- getFromNamespace("computeGamma", "IFC")

#' @name trunc_string
#' @keywords internal
trunc_string <- getFromNamespace("trunc_string", "IFC")

#' @name parseTrans
#' @keywords internal
parseTrans <- getFromNamespace("parseTrans", "IFC")

#' @name applyTrans
#' @keywords internal
applyTrans <- getFromNamespace("applyTrans", "IFC")

#' @name map_color
#' @keywords internal
map_color <- getFromNamespace("map_color", "IFC")

#' @name map_color
#' @keywords internal
map_style <- getFromNamespace("map_style", "IFC")

#' @name colConv
#' @keywords internal
colConv <- getFromNamespace("colConv", "IFC")

#' @name inv_colConv
#' @keywords internal
inv_colConv <- getFromNamespace("inv_colConv", "IFC")

#' @name newPB
#' @keywords internal
newPB <- getFromNamespace("newPB", "IFC")

#' @name setPB
#' @keywords internal
setPB <- getFromNamespace("setPB", "IFC")

#' @name endPB
#' @keywords internal
endPB <- getFromNamespace("endPB", "IFC")

#' @name readGatingStrategy
#' @keywords internal
readGatingStrategy <- getFromNamespace("readGatingStrategy", "IFC")

#' @name writeGatingStrategy
#' @keywords internal
writeGatingStrategy <- getFromNamespace("writeGatingStrategy", "IFC")

#' @name applyGatingStrategy
#' @keywords internal
applyGatingStrategy <- getFromNamespace("applyGatingStrategy", "IFC")

#' @name redefine_features_def
#' @keywords internal
redefine_features_def <- getFromNamespace("redefine_features_def", "IFC")

#' @name redefine_obj
#' @keywords internal
redefine_obj <- getFromNamespace("redefine_obj", "IFC")

#' @name checkObj
#' @keywords internal
checkObj <- getFromNamespace("checkObj", "IFC")

#' @name smoothAsinh
#' @keywords internal
smoothAsinh <- getFromNamespace("smoothAsinh", "IFC")

#' @name inv_smoothAsinh
#' @keywords internal
inv_smoothAsinh <- getFromNamespace("inv_smoothAsinh", "IFC")

#' @name base_axis_constr
#' @keywords internal
base_axis_constr <- getFromNamespace("base_axis_constr", "IFC")

#' @name xml_new_node
#' @keywords internal
xml_new_node <- getFromNamespace("xml_new_node", "IFC")

#' @name parseFCSname
#' @keywords internal
parseFCSname <- getFromNamespace("parseFCSname", "IFC")

#' @name convert_spillover
#' @keywords internal
convert_spillover <- getFromNamespace("convert_spillover", "IFC")

# in dev functions for compensation
decompensate <- function(comp, spillover) {
  ans = try(round(t(solve(a = solve(spillover), b = t(as.matrix(comp)))),10), silent = TRUE)
  try({colnames(ans) <- colnames(comp)}, silent = TRUE)
  return(ans)
}

compensate <- function(raw, spillover) {
  ans = try(round(as.matrix(raw) %*% solve(t(spillover)),10), silent = TRUE)
  try({colnames(ans) <- colnames(raw)}, silent = TRUE)
  return(ans)
}

recompensate <- function(val, spill_dec, spill_comp) {
  ans = try(round(compensate(as.matrix(val), solve(spill_dec, spill_comp)),10), silent = TRUE)
  try({colnames(ans) <- colnames(val)}, silent = TRUE)
  return(ans)
}

# function to update all input containing population name
update_pops <- function(session = getDefaultReactiveDomain(), obj, init = FALSE, ...) {
  N = names(obj$pops); 
  TAGGED = N[sapply(obj$pops, FUN = function(p) p$type == "T")]
  if(length(N) == 0) N = list()
  if(!init && (length(session$input$population) !=0) && all(session$input$population %in% N)) {
    updateSelectInput(session=session, inputId = "population", choices = N, selected = session$input$population)
  } else {
    updateSelectInput(session=session, inputId = "population", choices = N, selected = "All")
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

# function to update all input containing regions name
update_regions <- function(session = getDefaultReactiveDomain(), obj, init = FALSE, ...) {
  N = names(obj$regions); if(length(N) == 0) N = list()
  if(!init && (length(session$input$reg_selection) !=0) && all(session$input$reg_selection %in% N)) {
    updateSelectInput(session=session, inputId = "reg_selection", choices = N, selected = session$input$reg_selection)
  } else {
    updateSelectInput(session=session, inputId = "reg_selection", choices = N, selected = character())
  }
}

# function to update all input containing features name
update_features <- function(session = getDefaultReactiveDomain(), obj, init = FALSE, ...) {
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
data_modify_regions <- function(obj, regions, display_progress = TRUE, compare_with = obj, session = getDefaultReactiveDomain(), ...){
  assert(obj, cla = "IFC_data")
  mutation = names(regions)
  if(!all(mutation %in% names(obj$regions))) stop("can't find regions to modify in 'obj'", call. = FALSE)
  R = lapply(regions, FUN = function(x) {
    reg = do.call(what = buildRegion, args = x)
    reg$color = map_color(reg$color)
    reg$lightcolor = map_color(reg$lightcolor)
    reg
  })
  names(R) = sapply(R, FUN = function(x) x$label)
  tmp = duplicated(names(R))
  if(any(tmp)) stop(paste0("duplicated regions found: ", names(regions)[tmp]), call. = FALSE)
  names(mutation) = names(R)
  type1 = sapply(obj$regions[mutation], FUN = function(r) r$type)
  type2 = sapply(R, FUN = function(r) r$type)
  tmp = type1 == type2
  if(!all(tmp)) stop(paste0("'type' modification is not allowed:\n\t- ",
                            paste0(paste(paste0(mutation[!tmp], " [", type1[!tmp], "]"),
                                         paste0(names(mutation)[!tmp], " [", type2[!tmp], "]"), sep = " -> "),
                                   collapse = "\n\t- ")), call. = FALSE)
  
  ans = obj
  # regions modification, it can be everything
  N = names(ans$regions)
  K = class(ans$regions)
  ans$regions[mutation] = R
  names(ans$regions) = sapply(ans$regions, FUN = function(r) r$label)
  class(ans$regions) <- K
  
  # now we can only modify names within pops and regions
  # so we only keep mutation with names change
  mutation = mutation[mutation != names(mutation)]
  
  if(length(mutation) != 0) {
    # populations modification, only region names within graphical population definition can be changed
    N = names(ans$pops)
    K = class(ans$pops)
    ans$pops = lapply(ans$pops, FUN = function(p) {
      if(length(p$region) ==0) return(p)
      found = mutation %in% p$region
      if(any(found)) p$region <- names(mutation[found])
      return(p)
    })
    names(ans$pops) = N
    class(ans$pops) <- K
    
    # graphs modification
    N = names(ans$graphs)
    K = class(ans$graphs)
    ans$graphs = lapply(ans$graphs, FUN = function(g) {
      if(length(g$GraphRegion) == 0) return(g)
      # check if any GraphRegion is part of modified regions new names
      found = mutation %in% sapply(g$GraphRegion, FUN = function(r) r$name)
      # if not we leave
      if(!any(found)) return(g)
      # else we need to modify GraphRegion to apply new name
      g$GraphRegion = sapply(g$GraphRegion, FUN = function(r) {
        found = mutation %in% r$name
        if(any(found)) r$name <- names(mutation[found])
        foo = sapply(ans$pops,
                     FUN = function(p) {
                       bar = (p$type == "G") &&
                         (p$region == r$name) &&
                         (p$base %in% unique(unlist(lapply(g$BasePop, FUN = function(b) b$name)))) &&
                         (g$f1 == p$fx)
                       if(ans$regions[[r$name]]$type != "line") bar = bar && (g$f2 == p$fy)
                       return(bar)
                     })
        return(list(c(r, list(def = names(which(foo))))))
      })
      # we need to recompute order
      operators = c("And","Or","Not","(",")")
      b_names = unlist(lapply(g$BasePop, FUN=function(x) x$name))
      g_names = unlist(lapply(g$GraphRegion, FUN=function(x) x$def))
      s_names = unlist(lapply(g$ShownPop, FUN=function(x) x$name))
      
      g$order = splitn(definition = g$order, all_names = c(b_names, g_names, s_names, "Selected Bin"), operators = operators)
      g$order = paste0(setdiff(g$order, "Selected Bin"), collapse = "|")
      
      g$xstatsorder = splitn(definition = g$xstatsorder, all_names = c(b_names, g_names, s_names, "Selected Bin"), operators = operators)
      g$xstatsorder = paste0(setdiff(g$xstatsorder, "Selected Bin"), collapse = "|")
      return(g)
    })
    names(ans$graphs) = N
    class(ans$graphs) <- K
  }
  
  # since regions have been changed we need to recompute objects
  ans$pops <- popsWithin(pops = ans$pops,
                         regions = ans$regions,
                         features = ans$features,
                         session = session,
                         display_progress = display_progress, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  
  # if compare was provided, we compare
  if(any(names(ans$regions) != names(compare_with$regions))) update_regions(session = session, obj = ans)
  ans$haschanged_objects <- names(ans$pops)[!sapply(names(ans$pops), FUN = function(i_pop) identical(ans$pops[[i_pop]]$obj, compare_with$pops[[i_pop]]$obj))]
  return(ans)
}
data_modify_pops <- function(obj, pops, display_progress = TRUE, compare_with = obj, session = getDefaultReactiveDomain(), ...){
  assert(obj, cla = "IFC_data")
  mutation = names(pops)
  if(!all(mutation %in% names(obj$pops))) stop("can't find pops to modify in 'obj'", call. = FALSE)
  P = lapply(pops, FUN = function(x) do.call(what = buildPopulation, args = x))
  names(P) = sapply(P, FUN = function(x) x$name)
  tmp = duplicated(names(P))
  if(any(tmp)) stop(paste0("duplicated pops found: ", names(pops)[tmp]), call. = FALSE)
  names(mutation) = names(P)
  type1 = sapply(obj$pops[mutation], FUN = function(p) p$type)
  type2 = sapply(P, FUN = function(p) p$type)
  tmp = type1 == type2
  if(!all(tmp)) stop(paste0("'type' modification is not allowed:\n\t- ",
                            paste0(paste(paste0(mutation[!tmp], " [", type1[!tmp], "]"),
                                         paste0(names(mutation)[!tmp], " [", type2[!tmp], "]"), sep = " -> "),
                                   collapse = "\n\t- ")), call. = FALSE)
  
  ans = obj
  # pops modification
  K = class(ans$pops)
  ans$pops = lapply(ans$pops, FUN = function(p) {
    found = mutation %in% p$name
    if(any(found)) {
      # it should be unique !
      if(sum(found) > 1) stop(paste0("duplicated pops found: ", names(mutation[found]), call. = FALSE))
      p$name <- names(mutation[found])
      p$color <- P[[p$name]]$color
      p$lightModeColor <- P[[p$name]]$lightModeColor
      p$style <- P[[p$name]]$style
      if(p$type == "T") p$obj = P[[p$name]]$obj
    }
    found = mutation %in% p$names
    if(any(found)) p$names[p$names %in% mutation[found]] <- names(mutation[found])
    found = mutation %in% p$split
    if(any(found)) {
      p$split[p$split %in% mutation[found]] <- names(mutation[found])
      p$definition = paste0(p$split, collapse = "|")
    }
    return(p)
  })
  names(ans$pops) = sapply(ans$pops, FUN = function(p) p$name)
  class(ans$pops) <- K
  
  # graphs modification
  N = names(ans$graphs)
  K = class(ans$graphs)
  G = lapply(ans$graphs, FUN = function(g) {
    g$BasePop = lapply(g$BasePop, FUN = function(p) {
      found = mutation %in% p$name
      if(any(found)) p$name <- names(mutation[found])
      return(p)
    })
    g$GraphRegion = sapply(g$GraphRegion, FUN = function(r) {
      foo = sapply(ans$pops,
                   FUN = function(p) {
                     bar = (p$type == "G") &&
                       (p$region == r$name) &&
                       (p$base %in% unique(unlist(lapply(g$BasePop, FUN = function(b) b$name)))) &&
                       (g$f1 == p$fx)
                     if(ans$regions[[r$name]]$type != "line") bar = bar && (g$f2 == p$fy)
                     return(bar)
                   })
      return(list(c(r, list(def = names(which(foo))))))
    })
    g$ShownPop = lapply(g$ShownPop, FUN = function(p) {
      found = mutation %in% p$name
      if(any(found)) p$name <- names(mutation[found])
      return(p)
    })
    
    b_names = unlist(lapply(g$BasePop, FUN=function(x) x$name))
    g_names = unlist(lapply(g$GraphRegion, FUN=function(x) x$def))
    s_names = unlist(lapply(g$ShownPop, FUN=function(x) x$name))
    
    operators = c("And","Or","Not","(",")")
    g$order = splitn(definition = g$order, all_names = c(b_names, g_names, s_names, "Selected Bin"), operators = operators)
    g$order = paste0(setdiff(g$order, "Selected Bin"), collapse = "|")
    
    g$xstatsorder = splitn(definition = g$xstatsorder, all_names = c(b_names, g_names, s_names, "Selected Bin"), operators = operators)
    g$xstatsorder = paste0(setdiff(g$xstatsorder, "Selected Bin"), collapse = "|")
    return(g)
  })
  names(ans$graphs) = N
  class(ans$graphs) <- K
  
  ans$pops <- popsCompute(pops = ans$pops,
                          regions = ans$regions,
                          features = ans$features,
                          session = session,
                          display_progress = display_progress,
                          ...)
  for(i in names(mutation)) attributes(ans$pops[[i]]) <- attributes(pops[[mutation[i]]])
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  
  # if compare was provided, we compare
  if(any(names(ans$pops) != names(compare_with$pops))) update_pops(session = session, obj = ans)
  ans$haschanged_objects <- names(ans$pops)[!sapply(names(ans$pops), FUN = function(i_pop) identical(ans$pops[[i_pop]]$obj, compare_with$pops[[i_pop]]$obj))]
  return(ans)
}
data_redefine <- function(obj, new_feat_def, compare_with = obj, session = getDefaultReactiveDomain(), ...) {
  ans = redefine_obj(obj = obj, new_feat_def = new_feat_def, ...)
  if(!inherits(x = compare_with, what = "IFC_data")) return(ans)
  compare(ans, compare_with)
}

#### controls for app capabilities
# create shiny variable to handle user connection
shinyOptions("connected_users" = NULL)
# define the max number of users that can use app simultaneously
if(!exists(".max_users")) {
  .max_users = as.integer(Sys.getenv("MAX_USERS", 5))
}
# define the max number of cpu to use
if(!exists(".max_cores")) {
  .max_cores = as.integer(Sys.getenv("MAX_CORES", max(1, detectCores())))
}
# define the max time allowed for a session in seconds
if(!exists(".max_time")) {
  .max_time = as.integer(Sys.getenv("MAX_TIME", 600))
}
# define the max file size allowed
if(!exists(".max_size") || (length(.max_size) == 0)) {
  foo = unlist(options("shiny.maxRequestSize"))
  if(length(foo) == 0) {
    foo = Sys.getenv("MAX_SIZE", 4*1024^3)
  } else {
    foo = Sys.getenv("MAX_SIZE", options("shiny.maxRequestSize"))
  }
  .max_size = na.omit(as.numeric(foo))
}
.max_size = na.omit(as.numeric(.max_size))
if(length(.max_size) == 0) .max_size = NULL
options(shiny.maxRequestSize = .max_size)

# Create a rgl toggle checkbox
# I used the same structure as in shiny
# except that I did not include width param but it can be passed thanks to ...
Toggle3D <- function(inputId,
                     label,
                     value = FALSE,
                     widgetId = NULL,
                     objIds = NULL,
                     ...) {
  dots = list(...)
  onchg = HTML(paste(sep = ";\n",
                     "{",
                     "var wdg = document.getElementById(this.getAttribute('widgetId'))",
                     "if(wdg == null) return null",
                     "var rgl = wdg.rglinstance",
                     "if(rgl == null) return null",
                     "var ids = this.getAttribute('objIds')",
                     "if(ids == null) return null",
                     "ids = ids.split(',')",
                     "if(ids[0] === '') return null", # should handle null value but remains to be checked
                     "for(var obj, id, i = 0; i < ids.length; i++) {",
                     "id = parseInt(ids[i])",
                     "obj = rgl.scene.objects[id]",
                     "if(obj == null) continue",
                     "if(this.checked) {",
                     "rgl.addToSubscene(id, rgl.subsceneid)",
                     "} else {",
                     "rgl.delFromSubscene(id, rgl.subsceneid)",
                     "}",
                     "}",
                     "return rgl.drawScene()",
                     "}"))
  chk = tags$input(id = inputId, type = "checkbox", onchange = onchg, widgetId = widgetId, objIds = objIds, class = "rgl_toggle")
  if(!is.null(value) && value) chk$attribs$checked <- "checked"
  args = c(dots, list(tags$div(class = "checkbox",tags$label(chk,tags$span(label)))))
  do.call(what = tags$div, args = args)
}

# Update a rgl toggle checkbox
updateToggle3D <- function(session,
                           inputId,
                           label = NULL,
                           value = NULL,
                           widgetId = NULL,
                           objIds = NULL) {
  message <- shiny:::dropNulls(list(eleId = inputId, label = label, value = value, widgetId = widgetId, objIds = unname(unlist(objIds))))
  session$sendCustomMessage("updateToggle3D", message)
}

# function to clear plot3D; it removes javascript from scene and then run clear3d()
my_clear3d <- function() {
  shinyjs::runjs(code = "var wdg = document.getElementById('plot_3D');
                  if(wdg != null) {
                    var rgl = wdg.rglinstance;
                    if(rgl != null) {
                      delete rgl.scene.javascript;
                    }
                  }")
  rgl::clear3d()
}

#' @title create_obj3D
#' @description
#' Function to create a 3 dimensional object for further plotting with plot_obj3D()
#' @param data a data.frame
#' @param dims columns from 'data' used to generate 3D object.
#' Integer or Character vector of columns names from 'data'. Default is 1:3.
#' if length(dims) < 3, 1st element will be repeated to fill missing dimension(s).
#' @param xlab,ylab,zlab names of the x, y , z axis respectively. Default is "x", "y" and "z".
#' @param clust a vector specifying clusterization of 'data'.
#' Default is missing, in such a case if 'color' is also missing "Gray".
#' @param symbol symbol used for pch for each unique value of 'clust'. Default is  c(20, 4, 3, 1, 5, 0, 2, 18, 15, 17).
#' @param color a vector of color for each unique value of 'clust'.
#' Default is to use, IFC::paletteIFC(x = "palette_R") will be used.
#' @param level parameter to pass to rgl::ellipse3d(). Default is 0.68.
#' @expand coeeficient to apply to expand the bounding box of the data. Default is 1.1
#' @param pt_size desired point size. Default is 10.
#' @param ... other arguments to be passed.
#' @export
create_obj3D = function(data, dims = 1:3,
                        xlab ="x", ylab = "y", zlab = "z",
                        clust, symbol = c(20, 4, 3, 1, 5, 0, 2, 18, 15, 17), color = IFC::paletteIFC(x = "palette_R"),
                        level = 0.68, expand = 1.1, pt_size = 10, ...) {
  if(missing(data)) stop("'data' can't be missing")
  data = as.data.frame(data)
  ids = data[, "Object Number"]
  if(missing(clust)) clust = factor(rep("unk", nrow(data), label = "unk", levels = "unk"))
  if(!is.factor(clust)) clust = as.factor(clust)
  col = as.character(clust)
  pch = as.character(clust)
  
  if(!identical(sort(levels(clust)), sort(unique(names(color))))) {
    if(nlevels(clust) == length(unique(color))) {
      names(color) <- levels(clust)
    } else {
      color = rep_len(color, length.out = nlevels(clust))
      names(color) <- levels(clust)
    }
  }
  for(i in levels(clust)) {
    col[clust == i] <- color[i]
  }
  col[is.na(col)] <- "Grey"
  
  if(!identical(sort(levels(clust)), sort(unique(names(symbol))))) {
    if(nlevels(clust) == length(unique(symbol))) {
      names(symbol) <- levels(clust)
    } else {
      symbol = rep_len(symbol, length.out = nlevels(clust))
      names(symbol) <- levels(clust)
    }
  }
  for(i in levels(clust)) {
    pch[clust == i] <- symbol[i]
  }
  pch[is.na(pch)] <- 20
  
  SUB <- !is.na(clust)
  dims = as.character(dims)
  if(anyNA(suppressWarnings(as.integer(dims)))) {
    dims_in = lapply(dims, FUN=function(x) grep(paste0("^",x,"$"), names(data)))
    tmp = sapply(dims_in, length) == 0
    if(any(tmp)) stop(paste0("Can't find 'dims' [",paste0(dims[tmp], collapse = ","),"] in 'data' names"))
    dims = sapply(dims_in, FUN=function(x) x[1])
  } else {
    dims = as.integer(dims)
    tmp = dims %in% (1:ncol(data))
    if(!all(tmp)) stop(paste0("Can't find 'dims' [",paste0(dims[!tmp], collapse = ","),"] in 'data' names"))
  }
  while(length(dims) < 3) {
    dims = c(dims, dims[1])
  }
  data = cbind(data[, dims], "Object Number" = ids)
  
  xlim3d = range(data[,1], na.rm = TRUE)
  ylim3d = range(data[,2], na.rm = TRUE)
  zlim3d = range(data[,3], na.rm = TRUE)
  ran3d = list(x=xlim3d, y=ylim3d, z=zlim3d)
  radius = max(sapply(ran3d, diff))
  ran3d = c(ran3d, list(radius=radius / 100, size=0, col = "white", lit=FALSE))
  pts3d = by(cbind(data[SUB, 1:4], clust[SUB]), clust[SUB], FUN=function(d)
    list(x=d[,1], y=d[,2], z=d[,3], id=d[,4], pch = symbol[levels(clust)==unique(d[, 5])], color=color[levels(clust)==unique(d[, 5])], lit=FALSE))
  dim_cov = ((by(data[SUB, 1:3], clust[SUB], FUN=function(x) cov(x))))
  dim_centers = do.call(what = "cbind", args = lapply(1:3, FUN=function(i) {by(data[SUB,i], clust[SUB], FUN=function(x) {median(x, na.rm = TRUE)})}))
  ell3d = suppressWarnings(lapply(names(dim_cov), FUN=function(i) return(list(x = dim_cov[[i]], centre=dim_centers[i,], smooth=FALSE, level=level, col=color[levels(clust)==i]))))
  txt3d = lapply(names(dim_cov), FUN=function(i) return(list(x=dim_centers[i,1],y=dim_centers[i,2],z=dim_centers[i,3], texts=i, col=color[levels(clust)==i], cex = 1, adj=c(0.5,0.5), lit=FALSE, forceClipregion=FALSE)))
  ans = list(data = data, color = color, clust = clust, radius = radius, pt_size = pt_size,
             pts3d = pts3d, ell3d = ell3d, txt3d = txt3d, ran3d = ran3d,
             xlim3d = xlim3d, ylim3d = ylim3d, zlim3d = zlim3d,
             xlab=xlab, ylab=ylab, zlab=zlab,
             expand = expand)
  attr(ans, "class") <- "obj3D"
  return(ans)
}

#' @title plot_axis3D
#' @description
#' Function to create axis in a 3D graph
#' @param obj3D a 3D object as created by create_obj3D(). Default is missing.
#' @param which which axis to create. Default is c("x", "y", "z").
#' @param box whether to create a box or not. Default is TRUE.
#' @param label whether to draw axis name
#' @param line_color color of axes / box lines. Default is "black".
#' @param label_color color of axes labels. Default is "black".
#' @param tick_color color of axes ticks. Default is "black".
#' @param ... other arguments to be passed.
#' @export
plot_axis3D <- function(obj3D, which = c("x", "y", "z"), box = TRUE,
                        label = TRUE,  expand = TRUE,
                        line_color = "black", label_color = "black", tick_color = "black", ...) {
  if(missing(obj3D)) stop("'obj3D' can't be missing")
  if(class(obj3D) != "obj3D") stop("'obj3D' is not of class 'obj3D'")
  lim = list(x = obj3D$xlim3d,
             y = obj3D$ylim3d,
             z = obj3D$zlim3d)
  if(expand) lim = sapply(lim, simplify = FALSE, USE.NAMES = TRUE, FUN = function(x) x + (obj3D$expand - 1) * diff(x) / 2 * c(-1,1))
  pos = c(lim$x[1], lim$y[1], lim$z[1])
  foo = sapply(which, simplify = FALSE, USE.NAMES = TRUE, FUN = function(axe) {
    tck = axis3d(edge = axe, pos = pos, col = tick_color, lit = FALSE)
    args = list(x = lim$x[1], y = lim$y[1], z = lim$z[1], col = line_color, lit = FALSE)
    args[[axe]] = c(args[[axe]], lim[[axe]][2])
    line_in = do.call(what = rgl.lines, args = args)
    edges = list(c(2,2,2),
                 c(1,2,2),
                 c(2,1,2),
                 c(2,2,1))
    if(box) {
      line_out = lapply(edges, FUN = function(edg) {
        args = list(x = lim$x[edg[1]],
                    y = lim$y[edg[2]],
                    z = lim$z[edg[3]], col = line_color, lit = FALSE)
        args[[axe]] = lim[[axe]]
        return(do.call(what = rgl.lines, args = args))
      })
    } else {
      line_out = list()
    }
    if(label) {
      lab = mtext3d(text = obj3D[[paste0(axe, "lab")]], edge = axe, pos = pos,
                    col = label_color, line = 1.5, lit = FALSE)
    } else {
      lab = list()
    }
    return(list(tck = tck, line_in = line_in, line_out = line_out, lab =lab))
  })
  return(list(tck = sapply(foo, simplify = FALSE, USE.NAMES = TRUE, FUN = function(x) x$tck),
              lin = unlist(c(lapply(foo, FUN = function(x) x$line_in), lapply(foo, FUN = function(x) x$line_out))),
              lab = sapply(foo, simplify = FALSE, USE.NAMES = TRUE, FUN = function(x) x$lab)))
}

#' @title plot_obj3D
#' @description
#' Function to visualize a 3D object constructed by create_obj3D() using \pkg{rgl}.
#' @param obj3D a 3D object as created by create_obj3D(). Default is missing.
#' @param scaling scaling factor. Default is 1.7
#' @param draw_pts whether to draw axis. Default is TRUE.
#' @param draw_pts whether to draw points. Default is TRUE.
#' @param draw_ell whether to draw ellipses. Default is FALSE.
#' @param draw_txt whether to draw text labels. Default is FALSE.
#' @param useNULL whether to use the null graphics device. Default is TRUE.
#' @param session the session object passed to function given to shinyServer. Default is shiny::getDefaultReactiveDomain().
#' @param ... other arguments to be passed.
#' @export
plot_obj3D = function(obj3D, scaling = 1.7,
                      draw_axs = TRUE, draw_pts = TRUE, draw_ell = FALSE, draw_txt = FALSE,
                      useNULL = TRUE, session = shiny::getDefaultReactiveDomain(), ...) {
  if(missing(obj3D)) stop("'obj3D' can't be missing")
  if(class(obj3D) != "obj3D") stop("'obj3D' is not of class 'obj3D'")
  
  obj3D$scaling = scaling
  dots = list(...)
  devs = rgl.dev.list()
  dev = integer()
  if(useNULL) {
    dev = devs[names(devs) == "null"]
  } else {
    dev = devs[names(devs) == "wgl"]
  }
  if(length(dev) == 0) open3d(useNULL = useNULL)
  dev = dev[length(dev)]
  rgl.set(dev, silent = TRUE)
  clear3d()
  
  # create almost invisible spheres at each vertex of the 3D plot
  do.call(what = points3d, args = obj3D$ran3d)
  # set plot a cube
  asp = aspect3d(1,1,1)[[1]]
  names(asp) = c("x", "y", "z")
  view3d(theta = 0, phi = 0, fov = 60, zoom = 1/1.7)
  
  # change and record observer
  # INFO
  viewdepth = sum(sort(sapply(obj3D$ran3d[c("x", "y", "z")], diff), decreasing = TRUE) * c(1, 0.5, 0.25))
  viewdepth_max = scaling * sum(rep(obj3D$radius, 3) * c(1, 0.5, 0.25))
  obj3D$zoom = viewdepth/viewdepth_max
  obj3D$pt_ratio = viewdepth/1000
  
  # create axes
  axes = list()
  if(draw_axs) axes = do.call(what = plot_axis3D, args = list(obj3D = obj3D, box = TRUE))
  
  # create points
  pts = list()
  if(draw_pts) pts = lapply(obj3D$pts3d, FUN=function(i) { try(do.call(what = pch3d, args = c(list(radius = 4 * obj3D$pt_size * obj3D$pt_ratio), i[setdiff(names(i), "id")])), silent = TRUE) })
  
  # create ellipse
  ell = list()
  if(draw_ell) ell = suppressWarnings(lapply(obj3D$ell3d, FUN=function(i) {
    foo = try(do.call(what = ellipse3d, args = i), silent = TRUE)
    if(inherits(x = foo, what = "try-error")) return(NULL)
    return(shade3d(do.call(what = ellipse3d, args = i), alpha = 0.3, lit = FALSE))
  }))
  
  # create txt
  txt = list()
  if(draw_txt) txt = lapply(obj3D$txt3d, FUN=function(i) { do.call(what = text3d, args = i) })
  
  xx = obj3D$ran3d$x[1] + diff(obj3D$ran3d$x)/2
  yy = obj3D$ran3d$y[1] + diff(obj3D$ran3d$y)/2
  zz = obj3D$ran3d$z[1] + diff(obj3D$ran3d$z)/2
  hid = list()
  hid = text3d(x = xx, y = yy, z = zz, texts = "Creating 3D", alpha = 0, lit = FALSE)
  
  # creates hover for use with setUserCallbacks
  hov = list()
  if(packageVersion("rgl") >= '0.106.19') {
    hov$pts = pch3d(x=xx, y=yy, z=zz,
                    polygon_offset = 0, radius=4 * obj3D$pt_size * obj3D$pt_ratio,
                    pch = 13, color = "#F5AA2200", alpha=1, lit=FALSE)
    hov$txt = text3d(x=xx, y=yy, z=zz, texts = "-1",
                     offset = 1, pos = 4,
                     alpha = 0, lit = FALSE)
    hov$img = sprites3d(x=xx, y=yy, z=zz,
                        polygon_offset = 1, fixedSize = TRUE, radius = scaling,
                        color = "grey", alpha = 0, lit = FALSE,
                        textype = "rgb", texture = system.file(package = "rgl", "textures", "rgl2.png"))
    
    rglbegin <- local({function(x, y) {}})
    rglupdate <- local({function(x, y) {}})
    rglend <- local({function(x, y) {}})
    
    js <- sprintf(' var idverts = %s, idtext = %i, idimg = %i, idpts = %i, closest = ["",""], subid = %i;
     window.rglbegin = function(x, y) {
       return; 
     }
     window.rglupdate = function(x, y) {
       var obj, i, newdist, dist = Infinity, pt, pt_adj, newclosest, objid = - 1;
       var subscene = this.getObj(subid);
       if(Object.values(subscene.par3d.mouseMode).indexOf("selecting") < 0) {
       x = x/this.canvas.width;
       y = y/this.canvas.height;
       for(var [key, value] of Object.entries(idverts)) {
         obj = this.getObj(parseInt(value));
         //if(obj != null)
         for(i = 0; i < obj.vertices.length; i++) {
           pt = obj.vertices[i].concat(1);
           pt = this.user2window(pt, subid);
           pt[0] = x - pt[0];
           pt[1] = y - pt[1];
           pt[2] = 0;
           newdist = this.vlen(pt);
           if (newdist < dist) {
             dist = newdist;
             newclosest = [key, i + ""];
             objid = parseInt(value);
           }
         }
       }
       if((objid > 0) && ((newclosest[0] !== closest[0]) || (newclosest[1] !== closest[1]))) {
         closest = newclosest
         var txt = this.getObj(idtext);
         var img = this.getObj(idimg);
         var pts = this.getObj(idpts);
         obj = this.getObj(objid);
         pt = obj.vertices[closest[1]];

         // convert to normalized coords
         var v = pt.concat(1);
         var w = this.multVM(v, this.prmvMatrix);
         var xx = w[0]/w[3], yy = w[1]/w[3]
         //var zz = w[2]/w[3];

         // convert to pixel unit
         xx = 0.5 * this.vp.width  * (xx + 1) + this.vp.x;
         yy = 0.5 * this.vp.height * (1 - yy) - this.vp.y;
         //zz = (1 + zz) * 0.5;

         // offset x and y
         xx = xx + 25;
         yy = yy + 30;
         //zz = zz;

         // convert back to normalized coords
         xx = (xx - this.vp.x) * 2 / this.vp.width  - 1;
         yy = 1 - (yy + this.vp.y) * 2 / this.vp.height;
         //zz = zz * 2 - 1;

         xx = xx * w[3];
         yy = yy * w[3];
         //zz = zz * w[3];

         // convert to data unit
         this.prmvMatrix.invert();
         pt_adj = this.multVM([xx, yy, w[2], w[3]], this.prmvMatrix);

         pts.vertices[0] = txt.vertices[0] = pt;
         img.vertices[0] = pt_adj.slice(0, 3);
         this.prmvMatrix.invert();
         pts.colors[0][3] = img.colors[0][3] = txt.colors[0][3] = 1;
         pts.initialized = false;
         
         Shiny.onInputChange("rgl_3D_hover", { closest:newclosest, x:x, y:y, coords: { ori:pts.vertices[0], translated:pt_adj }, hover:{text:idtext, image:idimg, point:idpts }} );
       }
       }
     };
     window.rglend = function() {
       var txt = this.getObj(idtext);
       var img = this.getObj(idimg);
       var pts = this.getObj(idpts);
       pts.colors[0][3] = img.colors[0][3] = txt.colors[0][3] = 0;
       pts.initialized = img.initialized = txt.initialized = false;
       this.drawScene();
     }', toJSON(pts), hov$txt, hov$img, hov$pts, subsceneInfo()$id)
    
    scene <- scene3d(minimal = FALSE)
    if("javascript" %in% names(scene)) scene$javascript <- ""
    setUserCallbacks(0, begin = "rglbegin", update = "rglupdate", end = "rglend", javascript = js, scene = scene, applyToDev = TRUE)
  }
  view3d(theta = 0, phi = 0, fov = 60, zoom = obj3D$zoom)
  observer3d(0, 0, viewdepth_max, auto = FALSE)
  clear3d(type = c("lights","bboxdeco","background","material"))
  return(invisible(list(obj3D = obj3D, axes = axes, pts = pts, ell = ell, txt = txt, hov = hov, hid = hid, dev = dev)))
}

#' @title plotly_obj3D
#' @description
#' Function to visualize a 3D object constructed by create_obj3D() using \pkg{plotly}.
#' @param obj3D an obj3D object created by create_obj3D().
#' @param xlab,ylab,zlab names of the x, y , z axis respectively. Default is missing.
#' If not missing it will not use value stored in obj3D$xlab, obj3D$ylab, obj3D$zlab.
#' @param fileName path to file containing images. Default is NULL.
#' @param selection channels to embed when 'fileName' is provided. Default is 4.
#' @param force_width force_width parameter of IFC::objectParam(). Default is FALSE.
#' @param size size parameter of IFC::objectParam(). Default is c(0,0)
#' @param write_to write_to parameter of IFC::objectParam(). Default is "image.png",
#' @param session the session object passed to function given to shinyServer. Default is shiny::getDefaultReactiveDomain().
#' @param ... other arguments to be passed to IFC::objectParam().
#' @export
plotly_obj3D <- function(obj3D, xlab, ylab, zlab, fileName = NULL,
                         selection = 4, force_width = FALSE, size = c(0,0), write_to = "image.png",
                         session = shiny::getDefaultReactiveDomain(), ...) {
  dots = list(...)
  dat = do.call(what = rbind, args = c(list(make.row.names = FALSE), lapply(names(obj3D$pts3d), FUN = function(pop) {
    p = obj3D$pts3d[[pop]]
    data.frame(x = p$x, y = p$y, z = p$z, id = p$id, text = rep(pop, length(p$x)))
  })))
  col = unlist(lapply(obj3D$pts3d, FUN = function(p) p$color))
  names(col) = names(obj3D$pts3d)
  pch = unlist(lapply(obj3D$pts3d, FUN = function(p) p$pch))
  names(pch) = names(obj3D$pts3d)
  plotly_args = list(data = dat, x = ~x, y = ~y, z = ~z,
                     customdata = ~id, text = ~text,
                     color = ~text, colors = col, 
                     symbol = ~text, symbols = pch)
  if(length(fileName) != 0) {
    param_extra = names(dots) %in% c("objects", "base64_att")
    if("objects" %in% names(dots)) warning("'objects' from input will not be used", call. = FALSE, immediate. = TRUE)
    if("base64_att" %in% names(dots)) warning("'base64_att' from input will not be used", call. = FALSE, immediate. = TRUE)
    dots = dots[!param_extra]
    if(selection %in% c("all", "none")) stop("'selection' can not be set to \"all\" nor \"none\"")
    if(length(selection) != 1) stop("'selection' should be of length 1")
    dat$meta = unlist(do.call(what = ExtractImages_toBase64,
                              args = c(dots, list(fileName = fileName, objects = as.integer(dat$id),
                                                  selection = selection, force_width = FALSE, size = c(0,0), write_to = "image.png",
                                                  base64_att = "style='position:absolute; top:5px; left:5px; z-index:100;' class='IFC_displayed_cell'",
                                                  session = session))))
    plotly_args = c(plotly_args, list(meta = dat$meta))
  }
  fig <- do.call(what = plotly::plot_ly, args = plotly_args)
  fig <- fig %>% plotly::add_markers(hovertemplate = "%{text}<extra>%{customdata}</extra>")
  xlab <- ifelse(missing(xlab), obj3D$xlab, xlab)
  ylab <- ifelse(missing(ylab), obj3D$ylab, ylab)
  zlab <- ifelse(missing(zlab), obj3D$zlab, zlab)
  fig <- fig %>% plotly::layout(scene = list(xaxis = list(title = xlab),
                                             yaxis = list(title = ylab),
                                             zaxis = list(title = zlab)))
  if("meta" %in% names(dat)) {
    fig <- fig %>% htmlwidgets::onRender("
    function(el, x) {
      el.on('plotly_hover', function(d) {
        var to_remove = el.querySelector('.IFC_displayed_cell');
        if(to_remove != null) to_remove.remove();
        el.insertAdjacentHTML('beforeend', d.points[0].meta);
      });
      el.on('plotly_unhover', function(d) {
        var to_remove = el.querySelector('.IFC_displayed_cell');
        if(to_remove != null) to_remove.remove();
      });
    }")
  }
  suppressWarnings(fig)
}

batch_stats <- function(batch, pop = "All", feat, method = c("wilcox","t","none")[3]) {
  if(missing(pop)) pop = "All"
  if(missing(feat)) feat = names(batch[[1]]$features)
  L = length(batch)
  N = names(batch)
  if(length(N) == 0) {
    N = as.character(seq_len(L))
    names(batch) = N
  }
  feat_1 = feat
  if(L > 1) for(i in 2:L) { feat = intersect(feat, names(batch[[i]]$features)) }
  dat <- lapply(1:L, FUN = function(i_batch) {
    k = 1 + (length(batch[[i_batch]]$description$FCS) == 0)
    pat = c("^(?!.*FS|.*SS|.*LOG).*$", "^Intensity|^Bright Detail Intensity|^Uncompensated")
    fun = c("asinh","smoothLinLog")
    d = batch[[i_batch]]$features[batch[[i_batch]]$pops[[pop]]$obj, feat, drop = FALSE]
    if(nrow(d) == 0) return(matrix(NA, nrow=1, ncol=1+length(feat), dimnames = list(NULL, c(feat,""))))
    int_to_tra = colnames(d)[grepl(pat[k], colnames(d))]
    if(length(int_to_tra) != 0) d[, int_to_tra] <- sapply(int_to_tra, FUN = function(i_trans) do.call(what = fun[k], args = list(x = d[,i_trans])))
    cbind(d, rep(i_batch, nrow(d)))
  })
  dat2 <- do.call(rbind.data.frame, args = dat)
  
  med = apply(dat2, 2, median, na.rm = TRUE)
  avg = apply(dat2, 2, mean, na.rm = TRUE)
  std = apply(dat2, 2, sd, na.rm = TRUE)
  if(ncol(dat2) <= 1) return(matrix(numeric(), nrow=length(N), ncol=0, dimnames=list(N,NULL)))
  
  X <- unlist(lapply(dat,FUN = function(d) {
    sapply(colnames(d)[-ncol(d)], FUN = function(i_col) {
      mean(d[, i_col], na.rm=TRUE)
    })
  }))
  
  zscore <- unlist(lapply(dat,FUN = function(d) {
    sapply(colnames(d)[-ncol(d)], FUN = function(i_col) {
      (mean(d[, i_col], na.rm=TRUE) - avg[i_col]) / std[i_col]
    })
  }))
  
  fold_avg <- unlist(lapply(dat,FUN = function(d) {
    sapply(colnames(d)[-ncol(d)], FUN = function(i_col) {
      mean(d[, i_col], na.rm=TRUE) / avg[i_col]
    })
  }))
  
  fold_med <- unlist(lapply(dat,FUN = function(d) {
    sapply(colnames(d)[-ncol(d)], FUN = function(i_col) {
      median(d[, i_col], na.rm=TRUE) / med[i_col]
    })
  }))
  
  p = rep(NA_real_, times=length(feat)*L)
  if(method != "none") {
    test = paste0(method,".test")
    p <- as.vector(t(sapply(1:L, FUN = function(i_batch) {
      d = dat[[i_batch]]
      compare = dat2[,ncol(dat2)] != i_batch
      sapply(colnames(d)[-ncol(d)], FUN = function(i_col) {
        foo = try(suppressWarnings(do.call(what = test, args = list(x=d[, i_col,drop=TRUE], y=dat2[compare, i_col,drop=TRUE]))$p.value), silent = TRUE)
        if(inherits(foo, "try-error")) return(NA_real_)
        foo
      })
    })))
  }
  return(structure(array(c(X,zscore,fold_avg,fold_med,p),
                         dim=c(length(feat),L,5),
                         dimnames=list(feat,N,c("X","zscore","fold_avg","fold_med","p"))),
                   "method"=method,
                   "pop"=pop))
}

# function to create volcano plot with plotly
# so as to visualize difference best discriminating feature for a single same pop of each members of a batch
plotly_batch_volcano <- function(a, fold="mean", height=NULL) {
  Log2 = function(x) {
    sapply(x, FUN = function(i) {
      if(is.na(i)) return(i)
      if(i < 0) return(-1*log2(-i))
      return(log2(i))
    })
  }
  w = switch(fold,"mean"="fold_avg","median"="fold_med",NA_character_)
  d = dim(a)
  dn = dimnames(a)
  xori = as.vector(a[,,w])
  yori = as.vector(a[,,"p"])
  ss = !is.na(xori) & !is.na(yori)
  xori = xori[ss]
  yori = yori[ss]
  xx = Log2(xori)
  yy = -log10(yori)
  sp = rep(dn[[2]], times = d[1])[ss]
  text = rep(dn[[1]], times = d[2])[ss]
  xran = range(xx, na.rm = TRUE, finite = TRUE)
  yran = range(c(yy, -log10(0.05)), na.rm = TRUE, finite = TRUE)
  p <- plotly::plot_ly(x = xx, y = yy, split = sp, text = text, 
                       customdata = paste0("<br>x:",xori,",<br>y:",yori),
                       type="scatter", mode="markers", height=height,
                       marker = list(size = 5, width = 2),
                       hovertemplate = "<i><b>%{text}</b></i>%{customdata}<extra></extra>")
  p <- p %>% plotly::layout(title=list(text="Volcano plot",x = 0),
                            xaxis = list(title = "log2 fold change", type="linear", range = c(c(-1.07,1.07)*max(abs(xran))),zeroline=TRUE),
                            yaxis = list(title= "-log10 p value", type="linear", range = c(0,1.07*max(yran))),
                            shapes = list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = -log10(0.05), y1 = -log10(0.05),
                                               line = list(dash="dash"), text="p value = 0.05"),
                            annotations = list(#list(text="pval = 0.001", xref="paper", x=0.05, y=-log10(0.001), yshift = 10, showarrow=FALSE),
                                               #list(text="pval = 0.01", xref="paper", x=0.05, y=-log10(0.01), yshift = 10, showarrow=FALSE),
                                               list(text="pval = 0.05", xref="paper", x=0.05, y=-log10(0.05), yshift = 10, showarrow=FALSE)))
  spp <- unique(sp) # check for empty category
  if(length(spp) != length(dn[[2]])) {
    for(i in 1:length(dn[[2]])) {
      if(!any(dn[[2]][i] %in% spp)) {
        p$x$attrs[[p$x$cur_data]]$x = c(p$x$attrs[[p$x$cur_data]]$x, 'null')
        p$x$attrs[[p$x$cur_data]]$y = c(p$x$attrs[[p$x$cur_data]]$y, 'null')
        p$x$attrs[[p$x$cur_data]]$text = c(p$x$attrs[[p$x$cur_data]]$text, "")
        p$x$attrs[[p$x$cur_data]]$customdata = c(p$x$attrs[[p$x$cur_data]]$customdata, "")
        p$x$attrs[[p$x$cur_data]]$split = c(p$x$attrs[[p$x$cur_data]]$split, dn[[2]][i])
      }
    }
  }
  p
}

# function to create heatmap with plotly
# so as to visualize difference between every (selected) features for a single same pop of each members of a batch
plotly_batch_heatmap <- function(a, what="zscore", dendro = FALSE, height=NULL, ...) {
  dots = list(...)
  dots = dots[!names(dots) %in% c("x", "row_dend_left", "plot_method", "main", "height")]
  m = a[,,what]
  if(all(is.na(m))) return(NULL)
  if((length(m) != 0) && dendro && requireNamespace("heatmaply", quietly = TRUE)) {
    do.call(what = heatmaply::heatmaply,
            args = c(dots, list(x=m, height=height,
                                row_dend_left=TRUE, 
                                plot_method="plotly",
                                main="Batch Heatmap")))
  } else {
    plotly::plot_ly(x=colnames(m), y=rownames(a), z=m, height=height, type="heatmap") %>% layout(title=list(text="Batch Heatmap",x = 0))
  }
}

# function to create violin or ridge plot with plotly
# so as to visualize difference between one features for a single same pop of each members of a batch
plotly_batch_violin <- function(batch, pop = "All", feat = "Object Number", trans = "P",
                              space = 2*length(batch)-1, height = NULL,
                              type = c("violin", "ridge")[1],
                              points = c("all","outliers","none")[2]) {
  if(missing(pop)) pop = "All"
  if(missing(feat)) feat = "Object Number"
  L = length(batch)
  N = names(batch)
  N = sort(names(batch))
  if(length(N) == 0) {
    N = as.character(seq_len(L))
    names(batch) = N
  }
  batch = batch[N]
  S = seq_len(L)
  trans_ = parseTrans(trans)
  dat <- lapply(1:L, FUN = function(i_batch) {
    V = applyTrans(batch[[i_batch]]$features[batch[[i_batch]]$pops[[pop]]$obj, feat], trans_)
    V = V[is.finite(V)]
    cbind.data.frame("V1"=V, "V2"=rep(N[i_batch], length(V)))
  })
  is_empty = sapply(dat, FUN = function(d) length(d[, 1])) == 0
  dat <- as.data.frame(do.call(rbind, dat), stringsAsFactors = FALSE, check.names = FALSE)
  if(any(is_empty)) dat = rbind(dat, cbind(V1=rep(0, times = sum(is_empty)), V2=N[is_empty]))
  switch(type,
         "violin" = {
           p <- plotly::plot_ly(x = dat[,2], y = dat[, 1], split = dat[,2],
                                height=height,
                                points = points,
                                box = list(visible = TRUE),
                                meanline = list(visible = TRUE),
                                type = "violin")
           p <- p %>% plotly::style(pointpos=0) %>%
             plotly::layout(title=list(text=feat,x = 0),
                            yaxis = list(title = "", type="linear", fixedrange =TRUE, autorange = TRUE, zeroline=FALSE),
                            xaxis = list(type="-", autorange = TRUE, ticktext=S, tickvals=N))
         }, ridge = {
           p <- plotly::plot_ly(y = dat[,2], x = dat[, 1], split = dat[,2], height = height, type = "violin")
           p <- p %>% plotly::style(orientation="h", side="positive", width=space, points=FALSE) %>%
             plotly::layout(title=list(text=feat,x = 0),
                            xaxis = list(title = "", type="linear", autorange = TRUE, zeroline=FALSE),
                            yaxis = list(type="-", fixedrange =TRUE, autorange = TRUE, ticktext=S, tickvals=N))
         })
  if(any(is_empty)) { # check for empty category
    for(i in 1:L) {
      if(is_empty[i]) {
        p$x$data[[i]]$visible="legendonly"
        p$x$data[[i]][[c("x","y")[1 + (type=="violin")]]] = 'null'
      }
    }
  }
  p
}

# function to remove starting elements of a path (with extension) of a file
# it will return relative path of files to pattern argument
short_path = function(files, pattern) {
  f = gsub(normalizePath(pattern, mustWork = FALSE, winslash = "/"), "", normalizePath(files, mustWork = FALSE, winslash = "/"), fixed = TRUE)
  f = gsub("^\\\\(.*)$", "\\1", f)
  f = gsub("^\\/(.*)$", "\\1", f)
  return(f)
}

# function to get basename of file without extension
short_name = function(file) {
  foo = basename(file)
  ext = getFileExt(foo)
  return(sub(paste0("\\.",ext,"$"), "", foo))
} 

# function to shorten name from the center
center_short = function(x, max = 12) {
  delta = floor(max/2)
  if(nchar(x) > 2 * delta) {
    a = substr(x, 0, min(delta, nchar(x) - delta + 1))
    b = substr(x, max(0, nchar(x) - delta + 1), nchar(x))
    return(paste0(a,"...",b))
  }
  return(x)
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
reg_def = function(reg, reg_back, all_names, check = "valid", session = getDefaultReactiveDomain()) {
  if(length(reg$name) == 0) return(structure(rep(FALSE, 6), names = c("label", "light", "dark", "cx", "cy", "table")))
  hideFeedback(session=session, inputId="reg_def_label")
  hideFeedback(session=session, inputId="reg_def_cx")
  hideFeedback(session=session, inputId="reg_def_cy")
  hideFeedback(session=session, inputId="reg_def_table_feedback")
  type = check
  if(check == "info") {
    type = "edit"
    myfeedback = showFeedbackSuccess
  } else {
    myfeedback = showFeedbackWarning
  }
  switch(type,
         "valid" = {
           valid = structure(rep(TRUE, 6), names = c("label", "light", "dark", "cx", "cy", "table"))
           if((length(session$input$reg_def_label) == 0) || (session$input$reg_def_label == "")) {
             valid[1] = FALSE
             showFeedbackDanger(session = session, inputId = "reg_def_label", text = "empty")
           } else {
             if(session$input$reg_def_label %in% c("And", "Or", "Not", "(", ")", "All")) {
               valid[1] = FALSE
               showFeedbackDanger(session = session, inputId = "reg_def_label", text = "invalid")
             }
             if(grepl("^ML_", session$input$reg_def_label)) {
               valid[1] = FALSE
               showFeedbackDanger(session = session, inputId = "reg_def_label", text = "not allowed")
             }
             if(session$input$reg_def_label %in% setdiff(all_names, reg$name)) {
               valid [1]= FALSE
               showFeedbackDanger(session = session, inputId = "reg_def_label", text = "already exists")
             }
           }
           if(!is.finite(reg$cx) ) {
             valid[4] = FALSE
             showFeedbackDanger(session=session, inputId="reg_def_cx", text="non-finite value")
           }
           if(!is.finite(reg$cy)) {
             valid[5]= FALSE
             showFeedbackDanger(session=session, inputId="reg_def_cy", text="non-finite value")
           }
           if(!all(is.finite(c(reg$x, reg$y)))) {
             valid[6] = FALSE
             showFeedbackDanger(session=session, inputId="reg_def_table_feedback", text="non-finite value")
           }
           if(all(valid)) {
             enable("reg_validate")
           } else {
             disable("reg_validate")
           }
           return(valid)
         },
         "edit" = {
           removeClass(id = "reg_color_light_reset", class = "modified")
           removeClass(id = "reg_color_dark_reset", class = "modified")
           same = structure(rep(TRUE, 6), names = c("label", "light", "dark", "cx", "cy", "table"))
           if(reg$label != reg_back$label) {
             same[1] = FALSE
             myfeedback(inputId = "reg_def_label", text=reg_back$label)
           }
           if(reg$color != reg_back$color) {
             same[2] = FALSE
             addClass(id = "reg_color_dark_reset", class = "modified")
           }
           if(reg$lightcolor != reg_back$lightcolor) {
             same[3] = FALSE
             addClass(id = "reg_color_light_reset", class = "modified")
           }
           if(reg$cx != reg_back$cx) {
             same[4] = FALSE
             myfeedback(inputId = "reg_def_cx", text=as.character(reg_back$cx))
           }
           if(reg$cy != reg_back$cy) {
             same[5] = FALSE
             myfeedback(inputId = "reg_def_cy", text=as.character(reg_back$cy))
           }
           if(!(all(c(identical(reg$x, reg_back$x), identical(reg$y, reg_back$y))))) {
             same[6] = FALSE
             myfeedback(inputId = "reg_def_table_feedback", text="edited")
           }
           return(same)
         })
}

# function used in pair plot
panel_lda = function(x, y, ...) {
  old_par=par("pty");par("pty"="s")
  on.exit(par("pty"=old_par))
  points(x, y, ...)
}

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

# function to match IFC color
match_col <- function(x) {
  alw = unique(unlist(paletteIFC()[,  c("color_R",  "lightModeColor_R")]))
  return(alw[tolower(x) == tolower(alw)])
}

# function to match IFC style (pch)
match_style <- function(x) {
  tmp_style = c(20, 4, 3, 1, 5, 0, 2, 18, 15, 17)
  names(tmp_style) = c("Simple Dot", "Cross", "Plus",
                       "Empty Circle", "Empty Diamond", "Empty Square",
                       "Empty Triangle", "Solid Diamond", "Solid Square",
                       "Solid Triangle")
  if(x %in% tmp_style) x = names(tmp_style[which(x == tmp_style)][1])
  if(x %in% names(tmp_style)) x = names(tmp_style[which(x == names(tmp_style))][1])
  return(x)
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

# function to map plot coordinates to px
map_coord_to_css <- function(coord = c(x = 0, y = 0), map) {
  ran_x = c(map$domain$left,map$domain$right)
  dx = diff(ran_x)
  ran_y = c(map$domain$bottom,map$domain$top)
  dy = diff(ran_y)
  ran_img_width = c(map$range$left,map$range$right)
  width = diff(ran_img_width)
  ran_img_height = c(map$range$top,map$range$bottom)
  height = diff(ran_img_height)
  tmp_x = (coord[1]-ran_x[1]) / dx * width
  tmp_y = (coord[2]-ran_y[1]) / dy * height
  return(list(coords_css = list(x = as.integer((tmp_x + ran_img_width[1]) / map$img_css_ratio$x),
                                y = as.integer((ran_img_height[2] - tmp_y) / map$img_css_ratio$y)),
              coords_img = list(x = as.integer(tmp_x + ran_img_width[1]),
                                y = as.integer(ran_img_height[2] - tmp_y))))
}

# function to map px to plot coordinates
map_css_to_coord <- function(css = c(x = 0, y = 0), map) {
  ran_x = c(map$domain$left,map$domain$right)
  dx = diff(ran_x)
  ran_y = c(map$domain$bottom,map$domain$top)
  dy = diff(ran_y)
  ran_img_width = c(map$range$left,map$range$right)
  width = diff(ran_img_width)
  ran_img_height = c(map$range$top,map$range$bottom)
  height = diff(ran_img_height)
  tmp_x = css[1] * map$img_css_ratio$x - ran_img_width[1]
  tmp_y = ran_img_height[2] - css[2] * map$img_css_ratio$y 
  return(list(x = tmp_x * dx / width + ran_x[1],
              y = tmp_y * dy / height + ran_y[1]))
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
