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

if(requireNamespace("FlowSOM", quietly = TRUE) && requireNamespace("flowCore", quietly = TRUE)) {
  flowsom_meta = hidden(tags$div(id = "training_flowsom_meta",
                                 radioButtons(inputId = "MetaClustering_all", label = "Do meta cluster on the projection of 'All' dataset", choices = c("yes", "no"), selected = "no"),
                                 numericInput(inputId = "MetaClustering_max", label = "max", value = 20, min = 1, max = 99, step = 1),
                                 selectInput(inputId = "MetaClustering_method", label = "method", 
                                             # choices = c("metaClustering_consensus", "metaClustering_hclust", "metaClustering_kmeans", "metaClustering_som"), 
                                             choices = "metaClustering_kmeans", 
                                             selected = "metaClustering_kmeans", multiple = FALSE),
                                 tags$div(tags$div(style = "display:inline-block; width:49%; text-align:left",
                                                   actionButton(inputId = "MetaClustering_reset", label = "Reset")),
                                 )))
  model_avl = 1:8
} else {
  flowsom_meta = list()
  model_avl = (1:8)[-4]
}
list(hidden(tags$div(id="ML_side_training",
                     hidden(actionButton(inputId = "training_go", label = "")),
                     tags$div(tags$div("data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"=paste0(c("Algorithms used for data modelling",
                                                                                                                             c("pca: principal component analysis",
                                                                                                                             "tsne: t-Distributed Stochastic Neighbor Embedding",
                                                                                                                             "umap: Uniform Manifold Approximation and Projection",
                                                                                                                             "flowsom: self-organizing map for flow cytometry",
                                                                                                                             "em: gaussian mixture modelling",
                                                                                                                             "svm: support vector machines",
                                                                                                                             "xgb: extreme gradient boosting",
                                                                                                                             "lda: linear discriminant analysis")[model_avl]), collapse="\n\u00A0\u2022"),
                                       selectInput(inputId = "training_model", label = "model", choices = c("pca","tsne","umap", "floswom","em","svm","xgb","lda")[model_avl], multiple = FALSE))),
                     hidden(tags$div(id="training_dimred",
                                     tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Quantity of data used dimension reduction. tSNE and UMAP computation can take some time. This setting allows to reduce number of events used. Default is at most 4000 (or at least the total number of input population(s)) events ",
                                              numericInput(inputId = "training_sampling_dimred", label = "number of events used for dimension reduction", value = 4000, min = 1)))),
                hidden(tags$div(id = "training_supervised",
                                tags$hr(),
                                tags$div(class = "change_align",
                                         materialSwitch(inputId = "training_sampling", "Sampling", value = FALSE, status = "info")),
                                hidden(tags$div(id = "training_sampling_param",
                                                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Quantity of data used for training and prediction",
                                                         sliderInput(inputId = "training_ratio", label = "ratio", min = 0.1, max = 0.9, value = 0.5, step = 0.1)),
                                                tags$div(style = "text-align:right", 
                                                         "data-toggle"="tooltip", "data-placement"="top", "title"="Resample",
                                                         actionButton(inputId = "training_resample", label = "Apply"))))
                )),
                tags$hr(),
                tags$div(class = "change_align",
                         materialSwitch(inputId = "training_param", "Model parameters", value = FALSE, status = "info")),
                hidden(tags$div(id = "training_param_pca",
                                numericInput(inputId = "pca_tol", label = "tol", value = numeric(), min = 0, step = 0.001)
                )),
                hidden(tags$div(id = "training_param_tsne",
                                numericInput(inputId = "Rtsne_perplexity", label = "perplexity", value = 30),
                                numericInput(inputId = "Rtsne_theta", label = "theta", value = 0.5),
                                numericInput(inputId = "Rtsne_max_iter", label = "max_iter", value = 200),
                                numericInput(inputId = "Rtsne_eta", label = "eta", value = 1000),
                                numericInput(inputId = "Rtsne_momentum", label = "momentum", value = 0.5),
                                numericInput(inputId = "Rtsne_final_momentum", label = "final_momentum", value = 0.8),
                                numericInput(inputId = "Rtsne_exaggeration_factor", label = "exaggeration_factor", value = 12)
                )),
                hidden(tags$div(id = "training_param_umap",
                                numericInput(inputId = "umap_n_neighbors", label = "n_neighbors", value = 15),
                                selectInput(inputId = "umap_metric", label = "metric",
                                            choices = c("euclidean", "manhattan","chebyshev","minkowski", # minkowski
                                                        "canberra","braycurtis", "haversine", # Misc.
                                                        "mahalanobis","wminkowski","seuclidean", # Normalized
                                                        "cosine","correlation", # angular
                                                        "hamming","jaccard","dice","russelrao", # Binary
                                                        "kulsinski","ll_dirichlet","hellinger", # Binary
                                                        "rogerstanimoto","sokalmichener","sokalsneath", # Binary
                                                        "yule"),  # Binary
                                            selected = "euclidean", multiple = FALSE),
                                numericInput(inputId = "umap_n_epochs", label = "n_epochs", value = 200),
                                selectInput(inputId = "umap_init", label = "init", choices = c("spectral", "random"), selected = "spectral", multiple = FALSE),
                                numericInput(inputId = "umap_min_dist", label = "min_dist", value = 0.1),
                                numericInput(inputId = "umap_set_op_mix_ratio", label = "set_op_mix_ratio", value = 1),
                                numericInput(inputId = "umap_local_connectivity", label = "local_connectivity", value = 1),
                                numericInput(inputId = "umap_bandwidth", label = "bandwidth", value = 1),
                                numericInput(inputId = "umap_alpha", label = "alpha", value = 1),
                                numericInput(inputId = "umap_gamma", label = "gamma", value = 1),
                                numericInput(inputId = "umap_negative_sample_rate", label = "negative_sample_rate", value = 5),
                                numericInput(inputId = "umap_spread", label = "spread", value = 1),
                                numericInput(inputId = "umap_knn_repeats", label = "knn_repeats", value = 1)
                )),
                hidden(tags$div(id = "training_param_flowsom",
                                numericInput(inputId = "flowsom_seed", label = "seed", value = integer(), step = 1, min = 0)
                )),
                hidden(tags$div(id = "training_param_em",
                                selectInput(inputId = "MclustDA_G", label = "G", choices = 1:5,
                                            selected = 1, multiple = TRUE),
                                selectInput(inputId = "MclustDA_modelType", label = "modelType", choices = c("MclustDA", "EDDA"),
                                            selected = "MclustDA", multiple = FALSE),
                                selectInput(inputId = "MclustDA_modelNames", label = "modelNames",
                                            choices = c("E", "V", "EII", "VII", "EEI", "VEI", "EVI", "VVI",  "EEE",
                                                        "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV","X", "XII",
                                                        "XXI", "XXX"),
                                            selected = c("EEV","VEV"), multiple = TRUE)
                )),
                hidden(tags$div(id = "training_param_svm",
                                selectInput(inputId = "svm_type", label = "type",
                                            choices = c("C-classification", 
                                                        # "one-classification","eps-regression", "nu-regression",
                                                        "nu-classification"),
                                            selected = "C-classification", multiple = FALSE),
                                selectInput(inputId = "svm_kernel", label = "kernel",
                                            choices = c("linear", "polynomial",
                                                        "radial","sigmoid"),
                                            selected = "polynomial", multiple = FALSE),
                                numericInput(inputId = "svm_degree", label = "degree", value=2, step=1, min=1),
                                numericInput(inputId = "svm_gamma", label = "gamma", value = numeric(), step = 0.001),
                                numericInput(inputId = "svm_coef0", label = "coef0", value = 0, step=1, min=0),
                                numericInput(inputId = "svm_cost", label = "cost", value = 1, step=1, min=0),
                                numericInput(inputId = "svm_nu", label = "nu", value = 0.5, step = 0.1, min=0),
                                numericInput(inputId = "svm_tolerance", label = "tolerance", value = 0.001, step = 0.001, min=0),
                                numericInput(inputId = "svm_epsilon", label = "epsilon", value = 0.1, step = 0.1, min=0)
                )),
                hidden(tags$div(id = "training_param_xgb",
                                disabled(selectInput(inputId = "xgb_booster", label = "booster",
                                                     choices = c("gbtree"),
                                                     selected = "gbtree", multiple = FALSE)),
                                numericInput(inputId = "xgb_eta", label = "eta", value=0.001, step=0.01, min=0, max=1),
                                numericInput(inputId = "xgb_gamma", label = "gamma", value=3, step=1, min=0, max=+Inf),
                                numericInput(inputId = "xgb_max_depth", label = "max_depth", value=5, step=1, min=0, max=+Inf),
                                numericInput(inputId = "xgb_subsample", label = "subsample", value=0.75, step=0.01, min=0, max=1),
                                numericInput(inputId = "xgb_colsample_bytree", label = "colsample_bytree", value=1, step=0.01, min=0, max=1),
                                disabled(selectInput(inputId = "xgb_objective", label = "objective",
                                                     choices = c("multi:softprob"),
                                                     selected = "multi:softprob", multiple = FALSE)),
                                disabled(selectInput(inputId = "xgb_eval_metric", label = "eval_metric",
                                                     choices = c("mlogloss"),
                                                     selected = "mlogloss", multiple = FALSE)),
                                numericInput(inputId = "xgb_nrounds", label = "nrounds", value=1000, step=100, min=1),
                                numericInput(inputId = "xgb_early_stopping_rounds", label = "early_stopping_rounds", value=10, step=1, min=1)
                )),
                hidden(tags$div(id = "training_param_lda",
                                numericInput(inputId = "lda_tol", label = "tol", value = 0.0001, step = 0.001, min=0),
                                selectInput(inputId = "lda_method", label = "method",
                                            choices = c("moment", "mle", "mve","t"),
                                            selected = "moment", multiple = FALSE),
                                numericInput(inputId = "lda_nu", label = "nu", value = integer(), step = 1, min=0)
                )),
                hidden(tags$div(id = "training_param_bottom",
                                tags$div(style = "display:inline-block; width:49%; text-align:left",
                                         actionButton(inputId = "training_param_reset", label = "Reset")),
                                tags$div(style = "display:inline-block; width:49%; text-align:right;",
                                         actionButton(inputId = "training_tweak", label = "Apply"))
                                # tags$div(id = "training_param_msg",
                                #          style = "text-align:right; font-weight: bold; color:#337ab7;",
                                #          tags$p("click to apply change(s)"))
                )),
                hidden(tags$div(id = "training_unsupervised",
                                tags$hr(),
                                tags$div(class = "change_align",
                                         materialSwitch(inputId = "training_meta", "Clustering", value = FALSE, status = "info")),
                                hidden(tags$div(id = "training_kmeans_meta",
                                                tags$div(id = "training_kmeans_meta_all", radioButtons(inputId = "kmeans_all", label = "Do meta cluster on the projection of 'All' dataset", choices = c("yes", "no"), selected = "no")),
                                                numericInput(inputId = "kmeans_centers", label = "centers", value = 10, min = 1, max = 99, step = 1),
                                                numericInput(inputId = "kmeans_iter_max", label = "iter.max", value = 10, step = 1),
                                                numericInput(inputId = "kmeans_nstart", label = "nstart", value = 1, step = 1),
                                                selectInput(inputId = "kmeans_algorithm", label = "algorithm", 
                                                            choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), selected = "Hartigan-Wong", multiple = FALSE),
                                                tags$div(tags$div(style = "display:inline-block; width:49%; text-align:left",
                                                                  actionButton(inputId = "kmeans_reset", label = "Reset")),
                                                         # tags$div(style = "display:inline-block; width:49%; text-align:right;",
                                                         #          actionButton(inputId = "kmeans_go", label = "Apply"))
                                                )
                                )),
                                flowsom_meta
                                )),
                tags$hr(),
                tags$div(tags$h4("Download ML features"),
                         tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Download model. It is advisable to download both daf and Rdata file if you want to apply model to predict another daf file",
                                  radioButtons(inputId = "ML_save_type", label = "download as:", choiceNames = c("daf only", "daf + R"), choiceValues = c("daf", "zip") , selected = "zip", inline = TRUE)),
                         downloadButton("ML_save_btn", "Save"))
)))