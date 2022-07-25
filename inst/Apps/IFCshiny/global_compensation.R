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

get_intensity_channels <- function(obj, info) {
  foo = unlist(sapply(obj$features_def, FUN = function(i_feat) {
    if(i_feat$type == "single" && i_feat$userfeaturetype == "Mask and Image") {
      foo = strsplit(i_feat$def, split = "|", fixed = TRUE)[[1]]
      if((foo[1] == "Intensity") && (foo[2] == "MC")) {
        bar = info$Images$physicalChannel[paste0(foo[-c(1,2)],collapse="") == info$Images$name]
        if(length(bar) == 0) {
          return(info$Images$physicalChannel[paste0(foo[-c(1,2)],collapse="") == sprintf("Ch%02i", info$Images$physicalChannel)])
        } else {
          return(bar)
        }
      } else {
        return(-1)
      }
    } else {
      return(-1)
    }
  }))
  if(length(foo) == 0) return(foo)
  sort(foo[foo >= 0])
}