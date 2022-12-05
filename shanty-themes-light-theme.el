;;; shanty-light-theme.el --- The theme for digital workers (light) -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Philip Gaber

;; Author: Philip Gaber <phga@posteo.de>
;; Maintainer: Philip Gaber <phga@posteo.de>
;; Created: 2022-02-07
;; URL: https://github.com/qhga/shanty-themes
;; Version: 1.0
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


;;; Commentary:
;; Shanty themes provide a light & dark theme for Emacs

;;; Code:

(declare-function shanty-themes--activate-theme "shanty-themes")

(defconst shanty-themes-colors
  #s(hash-table
     test eq
     data(bg-2       "#E1E6FC"
          bg-1       "#EDF2FF"
          bg         "#F7F8FD"
          bg+1       "#FBFBFE"
          bg+2       "#FEFEFE"
          fg-2       "#BBC1DC"
          fg-1       "#7D8EAE"
          fg         "#122B3F"
          fg+1       "#020E18"
          white      "#0D1F2D"
          black      "#F3F4FC"
          yellow-1   "#FFB455"
          yellow     "#F99416"
          yellow+1   "#BF6B00"
          orange-1   "#FFA251"
          orange     "#FF7F11"
          orange+1   "#C45A00"
          ;; red-2      "#960004"
          red-1      "#FD7E81"
          red        "#ED474A"
          red+1      "#CD1419"
          green-1    "#A5CC69"
          green      "#739F2F"
          green+1    "#5E8520"
          ;; green+2    "#4E7118"
          ;; blue-2     "#0683A0"
          blue-1     "#64BFD6"
          blue       "#36A1BB"
          blue+1     "#0683A0"
          ;; magenta-2  "#690635"
          magenta-1  "#B95F8A"
          magenta    "#902B5B"
          magenta+1  "#690635"
          ;; purple-2   "#630DAE"
          purple-1   "#CEA7F0"
          purple     "#A86CDC"
          purple+1   "#863FC4")))

(deftheme shanty-themes-light
  "The shanty emacs theme is meant for us, you and me - the workers - who may
not get dirty hands very often but love to code and tinker while looking at
a screen full of pleasant colors on a light background.")

(defgroup shanty-themes-light nil
  "Shanty theme options.
The theme has to be reloaded after changing anything in the faces group."
  :group 'faces)

(require 'shanty-themes)

(shanty-themes--activate-theme 'shanty-themes-light shanty-themes-colors)

(provide-theme 'shanty-themes-light)
(provide 'shanty-themes-light-theme)

;; Local variables:
;; package-lint-main-file: "shanty-themes.el"
;; end:
;;; shanty-themes-light-theme.el ends here