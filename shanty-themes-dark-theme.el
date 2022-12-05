;;; shanty-dark-theme.el --- The theme for digital workers (dark) -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Philip Gaber

;; Author: Philip Gaber <phga@posteo.de>
;; Maintainer: Philip Gaber <phga@posteo.de>
;; Created: 2022-02-07
;; URL: https://github.com/qhga/shanty-themes
;; Version: 1.0
;; Keywords: faces, theme, blue, yellow, gold, dark

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
     data(bg-2       "#000306"
          bg-1       "#020E18"
          bg         "#0D1F2D"
          bg+1       "#1E3141"
          bg+2       "#3C5161"
          fg-2       "#546A7B"
          fg-1       "#9EA3B0"
          fg         "#C3C9E9"
          fg+1       "#BDD3DD"
          white      "#FEFEFE"
          black      "#000306"
          yellow-1   "#DA7F05"
          yellow     "#FDAA3A"
          yellow+1   "#FFC16E"
          orange-1   "#C45A00"
          orange     "#FF7F11"
          orange+1   "#FFA251"
          ;; red-2      "#960004"
          red-1      "#CD1419"
          red        "#ED474A"
          red+1      "#FD7E81"
          green-1    "#739F2F"
          green      "#A5CC69"
          green+1    "#D5EEAE"
          ;; blue-2     "#0683A0"
          blue-1     "#36A1BB"
          blue       "#64BFD6"
          blue+1     "#A2DFED"
          ;; magenta-2  "#690635"
          magenta-1  "#902B5B"
          magenta    "#B95F8A"
          magenta+1  "#DBA1BC"
          ;; purple-2   "#630DAE"
          purple-1   "#863FC4"
          purple     "#A86CDC"
          purple+1   "#CEA7F0")))

(deftheme shanty-themes-dark
  "The shanty emacs theme is meant for us, you and me - the workers - who may
not get dirty hands very often but love to code and tinker while looking at
a screen full of pleasant colors on a dark background.")

(defgroup shanty-themes-dark nil
  "Shanty theme options.
The theme has to be reloaded after changing anything in the faces group."
  :group 'faces)

(require 'shanty-themes)

(shanty-themes--activate-theme 'shanty-themes-dark shanty-themes-colors)

(provide-theme 'shanty-themes-dark)
(provide 'shanty-themes-dark-theme)

;; Local variables:
;; package-lint-main-file: "shanty-themes.el"
;; end:
;;; shanty-themes-dark-theme.el ends here