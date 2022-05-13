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

reinit_default <- function(reset = FALSE, x, not = NULL, env = shinyEnv) {
  foo = list(obj_react = list(obj = list(haschanged_objects = character()), back = list(), syncbatch = FALSE,
                              batch = list(), curr = 1, stats = array(numeric(), dim=c(0,0,4))),
             model_react =   list(data = data.frame(), 
                                  cen = numeric(),cem = mean,
                                  scl = numeric(),scm = sd,
                                  int = character(),inm = character(),
                                  har = character(),ham = character(),
                                  config = list(),
                                  ratio = numeric(), idx = logical(), method = "pca",
                                  pca = list(), proj_pca = matrix(numeric()),
                                  fit = list(), param = list(), sub = logical(),
                                  pred = factor(), proj = matrix(numeric()), feat = list(),
                                  clust = factor(), meta_args = list(), meta_fun = "kmeans"),
             pops_react = list(def = character(), new = FALSE, revert = list()),
             plot_react = list(plot = list(),  g = buildGraph(type = "histogram"), stack = buildGraph(type = "histogram"),
                               shown = NULL, order = NULL,
                               uri = NULL, param = list(), shared = NULL,
                               x_feat = "Object Number", y_feat = "Object Number", z_feat = "Object Number",
                               x_trans = "P", y_trans = "P", z_trans = "P",
                               density = 100, subset = NULL, symbol = NULL, color = NULL, click = NULL,
                               current = "#plot_1or2D", tool = "init", action = "none", hover = "init",
                               xmin = -1, xmax = 1, ymin = 0, ymax = 1,
                               zoomed = FALSE, closest = -1, id = 1000*as.numeric(Sys.time()),
                               param_ready = FALSE, densitytrans = NULL, densitycolorslightmode = "-16776961|-13447886|-256|-23296|-65536|",
                               densitytrans_selected = "initial", densitycolorslightmode_selected = "initial",
                               allowed_regions = NULL, allowed_siblings = NULL,
                               layout = matrix(integer(), ncol=0, nrow=0),
                               region = data.frame(matrix(NA, ncol = 4, nrow = 0, dimnames = list(NULL, c("x", "y", "css_x", "css_y"))))),
             regions_react = list(pre = list(), back = FALSE),
             feat_react = list(selected = NULL, best_length = 5),
             cell_react = list(objects=data.frame(), pop_tagged = "", propagation = TRUE),
             DT_react = list(length="10", order=0, sort="Object Number", type="img", temp=data.frame(), loaded=TRUE),
             param_react = list(info = list(), param = list(), back = list()),
             comp_react = list(spillover = as.matrix(data.frame()),
                               pre = as.matrix(data.frame()),
                               back = as.matrix(data.frame()),
                               last = as.matrix(data.frame()),
                               sub1 = integer(),
                               sub2 = integer()),
             msg_react = list(queue = c(), done = c()),
             file_react = list(input = list(), id = character()))
  if(missing(x)) x = setdiff(names(foo), not)
  if(reset) {
    lapply(x, FUN = function(i) {
      if(!(i %in% names(foo))) {
        warning("no default value for: ", x)
      } else {
        lapply(names(foo[[i]]), FUN = function(j) env[[i]][[j]] <- foo[[i]][[j]])
      }
    })
  } else {
    lapply(x, FUN = function(i) {
      if(!(i %in% names(foo))) warning("no default value for: ", x)
      assign(x = i, value = do.call(what = reactiveValues, args = foo[[i]]), envir = env)
    })
  }
}
