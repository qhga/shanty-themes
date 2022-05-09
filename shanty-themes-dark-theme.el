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
          bg-1       "#020e18"
          bg         "#0d1f2d"
          bg+1       "#1e3141"
          bg+2       "#3c5161"
          fg-2       "#546a7b"
          fg-1       "#9ea3b0"
          fg         "#c3c9e9"
          fg+1       "#bdd3dd"
          white      "#fefefe"
          black      "#000306"
          yellow-1   "#da7f05"
          yellow     "#fdaa3a"
          yellow+1   "#ffc16e"
          orange-1   "#c45a00"
          orange     "#ff7f11"
          orange+1   "#ffa251"
          ;; red-2      "#960004"
          red-1      "#cd1419"
          red        "#ed474a"
          red+1      "#fd7e81"
          green-1    "#739f2f"
          green      "#a5cc69"
          green+1    "#d5eeae"
          green+2    "#a2dfed"
          ;; blue-2     "#0683a0"
          blue-1     "#36a1bb"
          blue       "#64bfd6"
          blue+1     "#a2dfed"
          ;; magenta-2  "#690635"
          magenta-1  "#902b5b"
          magenta    "#b95f8a"
          magenta+1  "#dba1bc"
          ;; purple-2   "#630dae"
          purple-1   "#863fc4"
          purple     "#a86cdc"
          purple+1   "#cea7f0")))

(deftheme shanty-themes-dark
  "The shanty emacs theme is meant for us, you and me - the workers - who may
not get dirty hands very often but love to code and tinker while looking at
a screen full of pleasant colors on a dark background.")

(defgroup shanty-themes-dark nil
  "Shanty theme options.
The theme has to be reloaded after changing anything in the faces group."
  :group 'faces)

(eval-and-compile
  (unless (and (fboundp 'require-theme)
               load-file-name
               (equal (file-name-directory load-file-name)
                      (expand-file-name "themes/" data-directory))
               (require-theme 'shanty-themes t))
    (require 'shanty-themes)))

(shanty-themes--activate-theme 'shanty-themes-dark shanty-themes-colors)

(provide-theme 'shanty-themes-dark)
(provide 'shanty-themes-dark-theme)

;; Local variables:
;; package-lint-main-file: "shanty-themes.el"
;; end:
;;; shanty-themes-dark-theme.el ends here