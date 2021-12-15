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

list(hidden(tags$div(id = "plot_2D_options_main",
                     style = "z-index:2;",
                     tags$div(style = "position:relative; display: inline-block; vertical-align: baseline; width:100px;",
                              actionButton(inputId = "plot_type_2D_main_option01", label = "Resample")),
                     tags$div(style = "position:relative; display: inline-block; vertical-align: middle; width:200px;",
                              sliderInput(inputId = "plot_type_2D_main_option03", label = "Density %", min = 1, max = 100, value = 100)))),
     uiOutput(outputId = "plot_1or2D_placeholder", style = "width:600px; height:600px;"),
     tags$div(id = "plot_stats_placeholder", verbatimTextOutput(outputId = "plot_stats")),
     hidden(tags$div(id = "plot_image_placeholder"))
)
