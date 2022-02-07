;;; shanty-theme.el --- The theme for digital workers

;; Copyright (C) 2022 Philip Gaber

;; Author: Philip Gaber <phga@posteo.de>
;; Maintainer: Philip Gaber <phga@posteo.de>
;; Created: 2022-02-07
;; URL: https://github.com/qhga/shanty-theme
;; Version: 0.1
;; Package-Requires: ((emacs "27.2"))
;; Keywords: faces, theme, blue, yellow, gold

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

;; Resources I found useful
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html

(deftheme shanty
  "The shanty emacs theme is meant for us, you and me - the workers - who may
not get dirty hands very often but love to code and tinker while looking at
a screen full of pleasant colors.")

(defgroup shanty nil
  "Shanty theme options.
The theme has to be reloaded after changing anything in the faces group."
  :group 'faces)


(let* ((bg-2       "#000306")
       (bg-1       "#020e18")
       (bg         "#0d1f2d")
       (bg+1       "#1e3141")
       (bg+2       "#3c5161")
       (fg-2       "#546a7b")
       (fg-1       "#9ea3b0")
       (fg         "#c3c9e9")
       (fg+1       "#bdd3dd")
       (white      "#fefefe")
       (yellow-1   "#da7f05")
       (yellow     "#fdaa3a")
       (yellow+1   "#ffc16e")
       (orange-1   "#c45a00")
       (orange     "#ff7f11")
       (orange+1   "#ffa251")
       (red-2      "#960004")
       (red-1      "#cd1419")
       (red        "#ed474a")
       (red+1      "#fd7e81")
       (green-1    "#739f2f")
       (green      "#a5cc69")
       (green+1    "#d5eeae")
       (green+2    "#a2dfed")
       (blue-2     "#0683a0")
       (blue-1     "#36a1bb")
       (blue       "#64bfd6")
       (blue+1     "#0683a0")
       (magenta-2  "#690635")
       (magenta-1  "#902b5b")
       (magenta    "#b95f8a")
       (magenta+1  "#dba1bc")
       (purple-1   "#863fc4")
       (purple-2   "#630dae")
       (purple     "#a86cdc")
       (purple+1   "#cea7f0")

       ;; MAYB: Alternatives I already tried
       ;; (magenta+1 "#EF476F")
       ;; (green "#8BBF9F")
       ;; (green+1 "#61c9a8")
       ;; (green+2 "#A2DFED")
       ;; (bg+1 "#193346")
       ;; (bg+2 "#2B465B")
       ;; (fg "#C4E0F9")
       (box '(:box (:line-width -1))))

  (custom-theme-set-faces
   'shanty

   ;;;; Global
   `(default ((t (:background ,bg :foreground ,fg))))
   ;; TODO: Add support to disable
   `(fringe ((t (:background ,bg+1 ))))
   ;; TODO: Add support to disable
   `(hl-line ((t (:background ,bg+1))))
   `(region ((t (:background ,bg+2))))
   `(link ((t (:foreground ,green :underline t))))
   ;; `(button ((t (:foreground ,fg :background ,green))))
   `(isearch ((t (:background ,magenta+1 :foreground ,bg-2))))
   `(lazy-highlight ((t (:background ,green+2 :foreground ,bg))))
   `(query-replace ((t (:background ,magenta+1 :foreground ,bg-2))))

   ;;;; General Programming
   ;; TODO: Not working properly -> mayb rainbow-delimiter
   `(show-paren-match ((t (:background ,fg-2))))
   ;; TODO: Not working properly -> mayb rainbow-delimiter
   `(show-paren-mismatch ((t (:background ,red))))
   `(font-lock-comment-face ((t (:foreground ,fg-2))))
   `(font-lock-doc-face ((t (:foreground ,magenta))))
   `(font-lock-keyword-face ((t (:foreground ,yellow :weight bold))))
   `(font-lock-builtin-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,blue))))
   `(font-lock-constant-face ((t (:foreground ,blue))))
   `(font-lock-function-name-face ((t (:foreground ,blue ,@box))))
   `(font-lock-type-face ((t (:foreground ,blue))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(whitespace-line ((t (:foreground ,orange))))

   ;;;; evil-mode
   `(evil-ex-search ((t (:inherit isearch))))
   `(evil-ex-lazy-highlight ((t (:inherit lazy-highlight))))
   `(evil-ex-substitute-matches ((t (:foreground ,green+1 ,@box))))
   `(evil-ex-substitute-replacement ((t (:foreground ,red ,@box))))

   ;;;; dired
   `(dired-header ((t (:foreground ,yellow :slant italic))))
   `(dired-directory ((t (:foreground ,blue :weight bold))))
   `(dired-symlink ((t (:foreground ,fg-1 :slant italic))))
   `(dired-broken-symlink ((t (:foreground ,red :strike-through t)))) ;; Emacs 28.1
   `(dired-flagged ((t (:foreground ,red :strike-through t))))
   `(dired-marked ((t (:foreground ,green :underline t))))
   `(dired-mark ((t (:foreground ,green :weight bold))))
   `(dired-ignored ((t (:foreground ,fg-1 :slant italic)))) ;; git ignored files
   `(dired-perm-write ((t (:foreground ,green :weight bold))))
   `(dired-warning ((t (:foreground ,orange :weight bold))))

   ;;;; mode-line
   `(mode-line ((t (:background ,bg-1 :foreground ,fg
                                :distant-foreground ,fg-2
                                :overline ,fg-2))))
   `(mode-line-active ((t (:inherit mode-line))))
   `(mode-line-inactive ((t (:inherit mode-line :background ,bg :foreground ,fg-2))))
   ;; TODO: I set this in my modeline config -> Otherwise it has no effect
   `(mode-line-buffer-id ((t (:foreground ,yellow :weight bold))))
   `(mode-line-emphasis ((t (:foreground ,yellow :weight bold))))
   `(mode-line-highlight ((t (:foreground ,yellow :weight bold))))

   ;;;; telephone-line

   ;;;; org-mode

   ))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'shanty)

;;; shanty-theme.el ends here