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
.cleanup = list(ls = setdiff(ls(all.names = TRUE),"..."), env = environment(), back_env = new.env(), options = options())
for(i in .cleanup$ls) assign(x = i, value = get(x = i, envir = .cleanup$env), envir = .cleanup$back_env)
shiny::onStop(fun = function() {
  options(.cleanup$options)
  if(!("shiny.maxRequestSize" %in% names(.cleanup$options))) options("shiny.maxRequestSize"=NULL)
  for(i in .cleanup$ls) assign(x = i, value = get(x = i, envir = .cleanup$back_env), envir = .cleanup$env)
  rm(list = setdiff(ls(all.names = TRUE, envir = .cleanup$env), .cleanup$ls), envir = .cleanup$env)
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

#' @name densCols
#' @keywords internal
densCols <- getFromNamespace("densCols", "IFC")

#' @name plot_raster
#' @keywords internal
plot_raster <- getFromNamespace("plot_raster", "IFC")

#' @name plot_base
#' @keywords internal
plot_base <- getFromNamespace("plot_base", "IFC")

#' @name plot_lattice
#' @keywords internal
plot_lattice <- getFromNamespace("plot_lattice", "IFC")

#' @name CreateGraphReport
#' @keywords internal
CreateGraphReport <- getFromNamespace("CreateGraphReport", "IFC")

#' @name plot_stats
#' @keywords internal
plot_stats <- getFromNamespace("plot_stats", "IFC")

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

#' @name random_name
#' @keywords internal
random_name <- getFromNamespace("random_name", "IFC")

#' @name parseTrans
#' @keywords internal
parseTrans <- getFromNamespace("parseTrans", "IFC")

#' @name applyTrans
#' @keywords internal
applyTrans <- getFromNamespace("applyTrans", "IFC")

#' @name map_color
#' @keywords internal
map_color <- getFromNamespace("map_color", "IFC")

#' @name map_style
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

#' @name hist_constr
#' @keywords internal
hist_constr <- getFromNamespace("hist_constr", "IFC")

#' @name val_constr
#' @keywords internal
val_constr <- getFromNamespace("val_constr", "IFC")

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

#' @name base64_encode
#' @keywords internal
base64_encode <- getFromNamespace("base64_encode", "IFC")

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
  sapply(x, FUN = function(xx) {
    delta = floor(max/2)
    n = nchar(xx)
    if(n > 2 * delta) {
      a = substr(xx, 0, min(delta, n - delta + 1))
      b = substr(xx, max(0, n - delta + 1), n)
      return(paste0(a,"...",b))
    }
    return(xx)
  })
}

# function exactly match the begining
exact_start <- function(pattern, x, value = TRUE) {
  if(length(pattern) != 1) warning("'pattern' should be of length 1")
  n = nchar(pattern[1])
  xx = substr(x, 1, n)
  if(value) {
    if(length(x) == 0) return(character())
    return(x[xx == pattern[1]])
  }
  which(xx == pattern)
}

# function used in pair plot
panel_lda = function(x, y, ...) {
  old_par=par("pty");par("pty"="s")
  on.exit(par("pty"=old_par))
  points(x, y, ...)
}

# function to match IFC color
match_col <- function(x) {
  alw = unique(unlist(paletteIFC()[,  c("color_R",  "lightModeColor_R")]))
  return(alw[tolower(x) == tolower(alw)])
}

# function to find the closest style
closest_style <- function(x) {
  set1 = c(20, 4, 3, 1, 5, 0, 2,18,15,17)
  set2 = c( 1, 3, 4,20,18,15,17, 5, 0, 2)
  N = names(x)
  structure(sapply(x, FUN = function(xx) {
    if(is.na(xx)) return(xx)
    tmp = xx == set1
    if(any(tmp)) return(set2[tmp])
    return(xx)
  }), names = N)
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
  return(list(coords_css = list(x = as.numeric((tmp_x + ran_img_width[1]) / map$img_css_ratio$x),
                                y = as.numeric((ran_img_height[2] - tmp_y) / map$img_css_ratio$y)),
              coords_img = list(x = as.numeric(tmp_x + ran_img_width[1]),
                                y = as.numeric(ran_img_height[2] - tmp_y))))
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

##### global funs
source(file.path(.rundir, "global_compensation.R"), local = TRUE, echo = FALSE, verbose = FALSE)

source(file.path(.rundir, "global_data.R"), local = TRUE, echo = FALSE, verbose = FALSE)

source(file.path(.rundir, "global_ML.R"), local = TRUE, echo = FALSE, verbose = FALSE)

source(file.path(.rundir, "global_batch.R"), local = TRUE, echo = FALSE, verbose = FALSE)

source(file.path(.rundir, "global_plot3D.R"), local = TRUE, echo = FALSE, verbose = FALSE)
