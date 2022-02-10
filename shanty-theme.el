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
not get dirty hands very often but loves to code and tinker while looking at
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
       (black      "#000306")
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
       (blue+1     "#a2dfed")
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
       (shanty-heading '(:weight bold))
       (shanty-box '(:box (:line-width -1))))

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
   `(highlight ((t (:background ,purple+1 :foreground ,bg-2))))
   `(trailing-whitespace ((t (:background ,red :foreground ,bg-2))))

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
   `(font-lock-function-name-face ((t (:foreground ,blue ,@shanty-box))))
   `(font-lock-type-face ((t (:foreground ,blue))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(whitespace-line ((t (:foreground ,orange))))

   ;;;; evil-mode
   `(evil-ex-search ((t (:inherit isearch))))
   `(evil-ex-lazy-highlight ((t (:inherit lazy-highlight))))
   `(evil-ex-substitute-matches ((t (:foreground ,green+1 ,@shanty-box))))
   `(evil-ex-substitute-replacement ((t (:foreground ,red ,@shanty-box))))

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

   ;;;; outline
   `(outline-1 ((t (:foreground ,yellow ,@shanty-heading))))
   `(outline-2 ((t (:foreground ,blue ,@shanty-heading))))
   `(outline-3 ((t (:foreground ,magenta ,@shanty-heading))))
   `(outline-4 ((t (:foreground ,green ,@shanty-heading))))
   `(outline-5 ((t (:foreground ,purple+1 ,@shanty-heading))))
   `(outline-6 ((t (:foreground ,orange+1 ,@shanty-heading))))
   `(outline-7 ((t (:foreground ,red+1 ,@shanty-heading))))
   `(outline-8 ((t (:foreground ,blue+1 ,@shanty-heading))))
   `(outline-9 ((t (:foreground ,magenta+1 ,@shanty-heading))))

   ;;;; org-mode
   `(org-level-1 ((t (:foreground ,yellow ,@shanty-heading))))
   `(org-level-2 ((t (:foreground ,blue ,@shanty-heading))))
   `(org-level-3 ((t (:foreground ,magenta ,@shanty-heading))))
   `(org-level-4 ((t (:foreground ,green ,@shanty-heading))))
   `(org-level-5 ((t (:foreground ,purple+1 ,@shanty-heading))))
   `(org-level-6 ((t (:foreground ,orange+1 ,@shanty-heading))))
   `(org-level-7 ((t (:foreground ,red+1 ,@shanty-heading))))
   `(org-level-8 ((t (:foreground ,blue+1 ,@shanty-heading))))

   `(org-link ((t (:inherit button))))
   `(org-list-dt ((t (:weight bold))))
   `(org-block-begin-line ((t (:background ,bg+1
                                           :foreground ,fg-1
                                           :underline (:color ,fg-1) :extend t
                                           :height 0.80))))
   `(org-block-end-line ((t (:inherit org-block-begin-line
                                      :underline nil :overline ,fg-1))))
   `(org-block ((t (:background ,bg-1))))
   `(org-quote ((t (:inherit org-block))))
   `(org-verse ((t (:inherit org-block))))
   `(org-hide ((t (:foreground ,bg))))

   `(org-code ((t (:background ,bg :foreground ,purple+1))))
   `(org-verbatim ((t (:inherit org-code))))
   `(org-drawer ((t (:foreground ,fg-2))))
   `(org-latex-and-related ((t (:background ,bg-1 ,@shanty-box))))

   `(org-todo ((t (:foreground ,red))))
   `(org-done ((t (:foreground ,green))))
   `(org-headline-todo ((t (:foreground ,red))))
   `(org-headline-done ((t (:foreground ,green))))
   `(org-warning ((t (:foreground ,orange))))
   `(org-special-keyword ((t (:foreground ,purple+1 :weight bold))))
   `(org-tag ((t (:foreground ,magenta))))
   `(org-tag-group ((t (:foreground ,magenta))))
   `(org-date ((t (:foreground ,purple+1))))
   `(org-date-selected ((t (:foreground ,purple))))

   `(org-table ((t (:foreground ,purple+1))))
   `(org-table-header ((t (:foreground ,purple+1 :weight bold))))
   `(org-column ((t (:foreground ,purple+1))))
   `(org-column-title ((t (:foreground ,purple+1 :weight bold))))

   `(org-checkbox ((t (:foreground ,purple+1 :weight bold))))
   `(org-checkbox-statistics-todo ((t (:foreground ,red :weight bold))))
   `(org-checkbox-statistics-done ((t (:foreground ,green :weight bold))))

   ;;;; term-color
   `(term-color-black ((t (:foreground ,black))))
   `(term-color-red ((t (:foreground ,red))))
   `(term-color-green ((t (:foreground ,green))))
   `(term-color-yellow ((t (:foreground ,yellow))))
   `(term-color-blue ((t (:foreground ,blue))))
   `(term-color-magenta ((t (:foreground ,magenta))))
   `(term-color-cyan ((t (:foreground ,blue+1))))
   `(term-color-white ((t (:foreground ,white))))

   ;;;; ansi-colors (Have to be set by the user to highlight *shell* buffers)
   `(ansi-color-black ((t (:foreground ,black))))
   `(ansi-color-red ((t (:foreground ,red))))
   `(ansi-color-green ((t (:foreground ,green))))
   `(ansi-color-yellow ((t (:foreground ,yellow))))
   `(ansi-color-blue ((t (:foreground ,blue))))
   `(ansi-color-magenta ((t (:foreground ,magenta))))
   `(ansi-color-cyan ((t (:foreground ,blue+1))))
   `(ansi-color-white ((t (:foreground ,white))))

   `(ansi-bright-color-black ((t (:foreground ,black))))
   `(ansi-bright-color-red ((t (:foreground ,red+1))))
   `(ansi-bright-color-green ((t (:foreground ,green+1))))
   `(ansi-bright-color-yellow ((t (:foreground ,yellow+1))))
   `(ansi-bright-color-blue ((t (:foreground ,blue+1))))
   `(ansi-bright-color-magenta ((t (:foreground ,magenta+1))))
   `(ansi-bright-color-cyan ((t (:foreground ,blue+1))))
   `(ansi-bright-color-white ((t (:foreground ,white))))

   ;;;; avy: works like face, face-0, face-2 (face-1 is not the 3rd column)
   `(avy-background-face ((t (:background ,bg+1 :foreground ,fg-1 :extend t))))
   `(avy-lead-face ((t (:background ,magenta :foreground ,bg-1
                                    ;; ,@shanty-box
                                    :slant normal :weight bold
                                    :box nil :underline nil :overline nil))))
   `(avy-lead-face-0 ((t (:inherit avy-lead-face :background ,purple+1))))
   `(avy-lead-face-2 ((t (:inherit avy-lead-face :background ,yellow+1))))
   `(avy-lead-face-1 ((t (:inherit avy-lead-face :background ,red+1))))
   `(avy-goto-char-timer-face ((t (:inherit avy-lead-face :background ,red))))

   ;;;; company-mode
   ;; Whole row
   `(company-tooltip ((t (:background ,bg+2 :foreground ,fg+1))))
   `(company-tooltip-selection ((t (:background ,bg+1 :foreground ,fg+1 :weight bold))))
   ;; First part (function/variable name)
   `(company-tooltip-common ((t (:inherit company-tooltip))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection))))
   ;; Second part (function/variable)
   `(company-tooltip-annotation ((t (:inherit company-tooltip))))
   `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-selection))))
   ;; Special candidates
   `(company-tooltip-deprecated ((t (:strike-through t))))
   ;; Scrollbar
   `(company-tooltip-scrollbar-track ((t (:inherit company-tooltip))))
   `(company-tooltip-scrollbar-thumb ((t (:background ,fg+1))))
   ;; Template
   `(company-template-field ((t (:foreground ,magenta+1))))

   ;;;; flycheck
   `(flycheck-error ((t (:underline (:color ,red :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,orange :style wave)))))

   ;;;; magit
   `(magit-section-heading ((t (:foreground ,yellow))))

   ;;;; lsp-mode / lsp-ui
   ;; connected etc
   `(lsp-headerline-breadcrumb-path-face ((t (:foreground ,green))))
   `(lsp-lsp-flycheck-warning-unnecessary-face ((t (:inherit flycheck-warning))))

   ;;;; rainbow-delimiters
   `(rainbow-delimiters-base-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-base-error-face ((t (:background ,red :foreground ,fg :weight bold))))
   `(rainbow-delimiters-mismatched-face ((t (:background ,red :foreground ,fg :weight bold))))
   `(rainbow-delimiters-unmatched-face ((t (:background  ,red :foreground ,fg :weight bold))))

   ))


;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'shanty)

;;; shanty-theme.el ends here