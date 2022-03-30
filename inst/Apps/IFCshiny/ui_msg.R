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

list(hidden(selectInput(inputId = "msg_once", label = "hidden msg once", choices = "", selected = "", multiple = TRUE)),
     hidden(div( 
      id = "msg_busy_ctn",
      style="width: 100%; height: 100%; top: 0; left: 0; position: fixed; display: block; opacity: 0.9; background-color: #fff; z-index: 99; text-align: center;",
      tags$div(id = "msg_busy")
    )),
    div( 
      id = "msg_busy_ctn2",
      style="width: 100%; height: 100%; top: 0; left: 0; position: fixed; display: block; opacity: 0.9; background-color: #fff; z-index: 98; text-align: center;",
      tags$div(id = "msg_busy2",
               style='position: absolute; top: 50%; left: 0; bottom: 0; right: 0; display: block; align-items: center; justify-content: center; text-align: center; margin: auto; vertical-align: middle;',
               tags$h2(id = "msg_busy_txt2", "launching the app"),
               tags$div(style = "display: inline-block; vertical-align: super; color: black;",
                        icon("fas fa-sync-alt fa-spin fa-3x fa-fw", verify_fa = FALSE)))
))