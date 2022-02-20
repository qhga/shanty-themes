;;; shanty-light-theme.el --- The theme for digital workers (light)

;; Copyright (C) 2022 Philip Gaber

;; Author: Philip Gaber <phga@posteo.de>
;; Maintainer: Philip Gaber <phga@posteo.de>
;; Created: 2022-02-07
;; URL: https://github.com/qhga/shanty-theme
;; Version: 0.3
;; Package-Requires: ((emacs "27.2"))
;; Keywords: faces, theme, blue, yellow, gold, light

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defconst shanty-theme-colors
  #s(hash-table
     test eq
     data(
          bg-2       "#E1E6FC"
          bg-1       "#EDF2FF"
          bg         "#f7f8fd"
          bg+1       "#fbfbfe"
          bg+2       "#fefefe"
          fg-2       "#BBC1DC"
          fg-1       "#7d8eae"
          fg         "#122B3F"
          fg+1       "#020e18"
          white      "#0d1f2d"
          black      "#f3f4fc"
          yellow-1   "#FFB455"
          yellow     "#F99416"
          yellow+1   "#BF6B00"
          orange-1   "#ffa251"
          orange     "#ff7f11"
          orange+1   "#c45a00"
          ;; red-2      "#960004"
          red-1      "#fd7e81"
          red        "#ed474a"
          red+1      "#cd1419"
          green-1    "#a5cc69"
          green      "#739f2f"
          green+1    "#5E8520"
          green+2    "#4E7118"
          ;; blue-2     "#0683a0"
          blue-1     "#64bfd6"
          blue       "#36a1bb"
          blue+1     "#0683a0"
          ;; magenta-2  "#690635"
          magenta-1  "#b95f8a"
          magenta    "#902b5b"
          magenta+1  "#690635"
          ;; purple-2   "#630dae"
          purple-1   "#cea7f0"
          purple     "#a86cdc"
          purple+1   "#863fc4")))

(deftheme shanty-light
  "The shanty emacs theme is meant for us, you and me - the workers - who may
not get dirty hands very often but love to code and tinker while looking at
a screen full of pleasant colors on a light background.")

(defgroup shanty-light nil
  "Shanty theme options.
The theme has to be reloaded after changing anything in the faces group."
  :group 'faces)

(when load-file-name
  (load-file (concat (file-name-directory load-file-name) "shanty-themes.el")))

(shanty--activate-theme 'shanty-light)

(provide-theme 'shanty-light)

;;; shanty-light-theme.el ends here