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

list(hidden(tags$div(id="ML_side_inputs",
                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Select only one population for unsupervised or multiple for supervised training",
                         selectInput(inputId = "populations_training", label = "Populations for training", choices = NULL, multiple = TRUE, selectize = TRUE)),
                tags$div('data-toggle'="tooltip", 'data-placement'="auto", title="Select whether to determine best feature automatically or to use manual selection",
                         radioButtons(inputId = "features_used", label = "Features used for training", choices = c("auto", "manual"), selected = "auto", inline = TRUE)),
                hidden(tags$div(id = "features_best_length_ctn", "data-toggle"="tooltip", "data-placement"="top", "title"="Max number of features that best discriminate (by Rd Fisher ratio) each couple of input training populations to use fo the model. 0 means to take all remaining features after the filtering below.",
                                numericInput(inputId = "features_bst", label = "Number of best features to keep", value = 5, min = 0))),
                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Select whether to tansform intensities",
                         radioButtons(inputId = "features_int", label = "Transform Intensities", choices = c("yes", "no"), selected = "yes", inline = TRUE)),
                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Select whether the transformation to apply",
                         radioButtons(inputId = "features_inm", label = "Transformation Algorithm", choices = c("LinLog", "Arcsinh"), selected = "LinLog", inline = TRUE)),
                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="regex used to select intensities values to transform",
                         textInput(inputId = "features_inp", label = "Regex selection", value = "^Intensity|^Bright Detail Intensity|^Uncompensated")),
                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="It is advisable to scale features (i.e. dived by standard deviation for each value)",
                         radioButtons(inputId = "features_scl", label = "Scale features", choices = c("yes", "no"), selected = "yes", inline = TRUE)),
                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="It is advisable to center features. (i.e. substract mean for each value)",
                         radioButtons(inputId = "features_cen", label = "Center features", choices = c("yes", "no"), selected = "yes", inline = TRUE)),
                hidden(tags$div(id = "features_pre_daf",
                                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Select whether to apply Yeo-Jonhson transformation on Haralick features",
                                         radioButtons(inputId = "features_har", label = "Normalize Haralick", choices = c("yes", "no"), selected = "yes", inline = TRUE)),
                                tags$div("data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"=paste0("Select whether to remove system features (Object Number will always be removed):", 
                                                                                                                             "Camera Line Number",
                                                                                                                             "Camera Timer",
                                                                                                                             "Flow Speed",
                                                                                                                             "Objects/ml",
                                                                                                                             "Objects/sec Line",
                                                                                                                             "Time", collapse="\n\u00A0\u2022"),
                                         radioButtons(inputId = "features_sys", label = "Remove system features", choices = c("yes", "no"), selected = "yes", inline = TRUE)),
                                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Select whether to remove saturation features (in count or percent).",
                                         radioButtons(inputId = "features_sat", label = "Remove saturation features", choices = c("yes", "no"), selected = "yes", inline = TRUE)),
                                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Select whether to remove pixel features.",
                                         radioButtons(inputId = "features_pix", label = "Remove pixel features", choices = c("yes", "no"), selected = "yes", inline = TRUE)),
                                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Select whether to remove raw and uncompensated features.",
                                         radioButtons(inputId = "features_raw", label = "Remove raw and uncomp features", choices = c("yes", "no"), selected = "yes", inline = TRUE)))),
                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Select whether to remove features with near from zero variance. This can be long and can result is loss of important features with low representation. Note that in each case features with zero variance will be removed.",
                         radioButtons(inputId = "features_nzv", label = "Remove zero-var features", choices = c("yes", "no"), selected = "no", inline = TRUE)),
                tags$div("data-toggle"="tooltip", "data-placement"="top", "title"="Select whether to remove highly correlated features (>0.95). This can be long.",
                         radioButtons(inputId = "features_cor", label = "Remove correlated features", choices = c("yes", "no"), selected = "no", inline = TRUE))))
)