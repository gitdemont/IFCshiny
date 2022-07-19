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

list(hidden(tags$section(id="compute_features", class = "well",
                         # style = "background-color: rgb(191, 182, 172); border: 2px solid rgb(72, 54, 54); border-radius: 6px; padding: 9.5px; margin: 0 0 10px;",
                         tags$h4(style="color: #524b4b;","Compute extra features"),
                         tags$div(style = "display: inline-block; vertical-align: text-top; width:24%;",
                                  "data-toggle"="tooltip", "data-placement"="text-bottom", "data-html"="true", "title"="The maximal order of Zernike polynomials to be computed. Be aware that computation of Zernike's moments can be quite long when this value is high.",
                                  materialSwitch(
                                    inputId = "extra_zernike",
                                    label = "zernike", 
                                    value = FALSE,
                                    status = "primary"
                                  ),
                                  numericInput(inputId = "zernike", label = "Zernike", value = 6, min = -1, max = 99, step = 1)),
                         tags$div(style = "display: inline-block; vertical-align: text-top; width:24%;",
                                  "data-toggle"="tooltip", "data-placement"="text-bottom", "data-html"="true", "title"="Controls the grain of the texture. For very fine textures, this value is small (1-3 pixels), while for very coarse textures, it is large (>10).",
                                  materialSwitch(
                                    inputId = "extra_haralick",
                                    label = "haralick", 
                                    value = FALSE,
                                    status = "primary"
                                  ),
                                  selectInput(inputId = "haralick", label = "Haralick", choices = c(1:20), selected = 3, multiple = TRUE)),
                         hidden(tags$div(style = "display: inline-block; vertical-align: text-top; width:19%;",
                                         "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="Controls the number of objects to process at the same time. Don't use values too high to avoid out-of-memory issues.",
                                         numericInput(inputId = "batch", label = "batch", value = ifelse(Sys.info()[["user"]] == "shiny", 20, 20)))),
                         tags$div(style = "display: inline-block; vertical-align: -29px; width:24%;",
                                  "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="Use parallelization for faster computing. It allows to use paralellization if multi cores are available. Progress bar will not be available when applied. This option is disabled on shinyapps.io",
                                  materialSwitch(inputId = "use_parallelization", label = "multi-CPU", value = (parallel::detectCores() > 1) && (Sys.info()[["user"]] != "shiny"), right = FALSE, status = "primary")),
                         tags$div(style = "display: inline-block; vertical-align: -29px; width:24%; right:0px; z-index:0;",
                                  "data-toggle"="tooltip", "data-placement"="top", "data-html"="true", "title"="Start features computation. Even if Zernike and Haralick are not computed Hu moments will be extracted. Features computation can be long the more as file is large and Zernike is high.",
                                  actionButton(inputId = "compute_go", label = "compute")),
                         verbatimTextOutput(outputId = "compute_features_msg"))
))