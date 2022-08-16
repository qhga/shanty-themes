;;; shanty-themes.el --- The themes for digital workers -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Philip Gaber

;; Author: Philip Gaber <phga@posteo.de>
;; Maintainer: Philip Gaber <phga@posteo.de>
;; Created: 2022-02-07
;; URL: https://github.com/qhga/shanty-themes
;; Version: 1.0
;; Package-Requires: ((emacs "24.5.1"))
;; Keywords: faces, theme, blue, yellow, gold, dark, light

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


;;; Commentary:
;; Shanty themes provide a light & dark theme for Emacs

;;; Code:

(defcustom shanty-themes-function-box t
  "Draw a box around function definitions and in some cases calls.
If non nil, draws the box."
  :type 'boolean
  :group 'shanty-themes)

;; So the doc can contain the quoted version
(declare-function shanty-themes--li-da "shanty-themes")

(defun shanty-themes--activate-theme(theme colors)
  "Set the faces according to the activated theme.

Populates the color variables according to the provided COLORS
hashmap which are then used to define the faces.  THEME is later
used in the closure `shanty-themes--li-da' to make distinct
changes to some faces that have to be different in the light vs
the dark theme.  Possible values for THEME could be 'shanty-light
or 'shanty-dark"
  (let* ((bg-2       (gethash 'bg-2      colors))
         (bg-1       (gethash 'bg-1      colors))
         (bg         (gethash 'bg        colors))
         (bg+1       (gethash 'bg+1      colors))
         (bg+2       (gethash 'bg+2      colors))
         (fg-2       (gethash 'fg-2      colors))
         (fg-1       (gethash 'fg-1      colors))
         (fg         (gethash 'fg        colors))
         (fg+1       (gethash 'fg+1      colors))
         (white      (gethash 'white     colors))
         (black      (gethash 'black     colors))
         (yellow-1   (gethash 'yellow-1  colors))
         (yellow     (gethash 'yellow    colors))
         (yellow+1   (gethash 'yellow+1  colors))
         ;; (orange-1   (gethash 'orange-1  colors))
         (orange     (gethash 'orange    colors))
         (orange+1   (gethash 'orange+1  colors))
         ;; (red-2      (gethash 'red-2     colors))
         ;; (red-1      (gethash 'red-1     colors))
         (red        (gethash 'red       colors))
         (red+1      (gethash 'red+1     colors))
         ;; (green-1    (gethash 'green-1   colors))
         (green      (gethash 'green     colors))
         (green+1    (gethash 'green+1   colors))
         (green+2    (gethash 'green+2   colors))
         ;; (blue-2     (gethash 'blue-2    colors))
         (blue-1     (gethash 'blue-1    colors))
         (blue       (gethash 'blue      colors))
         (blue+1     (gethash 'blue+1    colors))
         ;; (magenta-2  (gethash 'magenta-2 colors))
         ;; (magenta-1  (gethash 'magenta-1 colors))
         (magenta    (gethash 'magenta   colors))
         (magenta+1  (gethash 'magenta+1 colors))
         ;; (purple-1   (gethash 'purple-1  colors))
         ;; (purple-2   (gethash 'purple-2  colors))
         (purple     (gethash 'purple    colors))
         (purple+1   (gethash 'purple+1  colors))

         (shanty-heading '(:weight bold))
         (shanty-box '(:box (:line-width -1))))

    (defun shanty-themes--li-da(light-color dark-color)
      "Depending on the requested theme returns the appropriate color"
      (if (eq theme 'shanty-themes-light) light-color dark-color))

    (custom-theme-set-faces
     theme

     ;;;; Global
     `(default ((t (:background ,bg :foreground ,fg))))
     ;; `(bold ((t (:weight bold))))
     ;; TODO: Add support to disable
     `(fringe ((t (:background ,bg+1))))
     `(vertical-border ((t (:foreground ,fg+1))))
     `(cursor ((t (:background ,white))))
     ;; TODO: Add support to disable
     `(hl-line ((t (:background ,bg+1))))
     `(region ((t (:background ,(shanty-themes--li-da bg-2 bg+2)))))
     `(link ((t (:foreground ,green :underline t))))
     `(link-visited ((t (:foreground ,magenta+1 :underline t))))
     ;; `(button ((t (:foreground ,fg :background ,green))))
     `(isearch ((t (:background ,magenta+1 :foreground ,bg-2))))
     `(isearch-fail ((t (:background ,red :foreground ,bg-2))))
     `(lazy-highlight ((t (:background ,green+2 :foreground ,bg))))
     `(query-replace ((t (:background ,magenta+1 :foreground ,bg-2))))
     `(highlight ((t (:foreground ,magenta :underline t))))
     `(completions-common-part ((t (:background ,blue+1 :foreground ,bg :weight bold))))
     `(success ((t (:foreground ,green :weight bold))))
     `(error ((t (:foreground ,red :weight bold))))
     `(diary ((t (:foreground ,yellow))))

     ;;;; General Programming
     ;; TODO: Not working properly -> mayb rainbow-delimiter
     `(show-paren-match ((t (:background ,fg-2))))
     ;; TODO: Not working properly -> mayb rainbow-delimiter
     `(show-paren-mismatch ((t (:background ,red))))
     `(shadow ((t (:foreground ,(shanty-themes--li-da fg-1 fg-2)))))
     `(font-lock-comment-face ((t (:inherit shadow))))
     `(font-lock-doc-face ((t (:foreground ,magenta))))
     `(font-lock-keyword-face ((t (:foreground ,yellow :weight bold))))
     `(font-lock-builtin-face ((t (:foreground ,yellow))))
     `(font-lock-variable-name-face ((t (:foreground ,purple+1))))
     `(font-lock-constant-face ((t (:foreground ,blue))))
     `(font-lock-function-name-face ((t (:foreground ,blue
                                         ,@(when shanty-themes-function-box
                                            shanty-box)))))
     `(font-lock-type-face ((t (:foreground ,blue))))
     `(font-lock-string-face ((t (:foreground ,green))))
     ;; `(font-negation-char-face ((t (:foreground ,green))))
     `(trailing-whitespace ((t (:background ,red :foreground ,bg))))
     `(whitespace-trailing ((t (:inherit trailing-whitespace))))
     `(whitespace-line ((t (:foreground ,orange))))
     `(whitespace-indentation ((t (:foreground ,orange))))
     `(whitespace-empty ((t (:foreground ,orange))))
     `(whitespace-space-after-tab ((t (:foreground ,orange))))
     `(whitespace-space-before-tab ((t (:foreground ,orange))))
     `(whitespace-tab ((t (:foreground ,fg-1))))
     `(whitespace-newline ((t (:foreground ,fg-1))))
     `(whitespace-space ((t (:foreground ,fg-1))))
     `(whitespace-hspace ((t (:foreground ,orange))))
     `(whitespace-big-indent ((t (:foreground ,orange))))
     `(whitespace-missing-newline-at-eof ((t (:background ,yellow :foreground ,bg))))
     `(sh-heredoc ((t (:foreground ,fg))))

     ;;;; compilation
     `(compilation-info ((t (:foreground ,blue+1))))
     `(compilation-error ((t (:foreground ,red :weight bold))))
     `(compilation-column-number ((t (:foreground ,magenta))))
     `(compilation-line-number ((t (:foreground ,yellow :weight bold))))
     `(compilation-mode-line-exit ((t (:foreground ,green :weight bold))))
     `(compilation-mode-line-fail ((t (:foreground ,red :weight bold))))
     `(compilation-mode-line-run ((t (:foreground ,yellow :weight bold))))
     `(compilation-mode-line-warning ((t (:foreground ,orange :weight bold))))

     ;;;; evil-mode
     `(evil-ex-search ((t (:inherit isearch))))
     `(evil-ex-lazy-highlight ((t (:inherit lazy-highlight))))
     `(evil-ex-substitute-matches ((t (:foreground ,green+1 ,@shanty-box))))
     `(evil-ex-substitute-replacement ((t (:foreground ,red ,@shanty-box))))
     `(evil-ex-info ((t (:foreground ,orange))))

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
     `(dired-async-failures ((t (:foreground ,red :weight bold))))
     `(dired-async-message ((t (:foreground ,yellow))))
     `(dired-async-mode-message ((t (:foreground ,yellow))))

     ;;;; mode-line
     `(mode-line ((t (:background ,(shanty-themes--li-da bg-2 bg-1) :foreground ,fg
                      :distant-foreground ,fg-2
                      :overline ,fg-2))))
     `(mode-line-active ((t (:inherit mode-line))))
     `(mode-line-inactive ((t (:inherit mode-line :background ,bg :foreground ,fg-2))))
     ;; TODO: I set this in my modeline config -> Otherwise it has no effect
     `(mode-line-buffer-id ((t (:foreground ,yellow :weight bold))))
     `(mode-line-emphasis ((t (:foreground ,yellow :weight bold))))
     `(mode-line-highlight ((t (:foreground ,yellow :weight bold))))

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

     `(org-ellipsis ((t (:inherit t :weight normal))))

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

     `(org-mode-line-clock ((t (:background ,bg-1 :foreground ,fg :weight bold))))
     `(org-mode-line-clock-overrun ((t (:background ,red :foreground ,bg :weight bold))))

     `(org-dispatcher-highlight ((t (:foreground ,yellow :weight bold))))

     ;;;; org-agenda
     `(org-agenda-done ((t (:foreground ,green :weight bold))))

     ;;;; term-color
     `(term-color-black ((t (:foreground ,black))))
     `(term-color-red ((t (:foreground ,red))))
     `(term-color-green ((t (:foreground ,green))))
     `(term-color-yellow ((t (:foreground ,yellow))))
     `(term-color-blue ((t (:foreground ,blue))))
     `(term-color-magenta ((t (:foreground ,magenta))))
     `(term-color-cyan ((t (:foreground ,blue+1))))
     `(term-color-white ((t (:foreground ,white))))

     ;;;; tty-menu
     `(tty-menu-disabled-face ((t (:inherit shadow))))
     `(tty-menu-enabled-face ((t (:foreground ,green))))
     `(tty-menu-selected-face ((t (:foreground ,blue))))

     ;;;; ansi-colors (Have to be set by the user to highlight *shell* buffers)
     `(ansi-color-black ((t (:foreground ,black))))
     `(ansi-color-red ((t (:foreground ,red))))
     `(ansi-color-green ((t (:foreground ,green))))
     `(ansi-color-yellow ((t (:foreground ,yellow))))
     `(ansi-color-blue ((t (:foreground ,blue))))
     `(ansi-color-magenta ((t (:foreground ,magenta))))
     `(ansi-color-cyan ((t (:foreground ,blue+1))))
     `(ansi-color-white ((t (:foreground ,white))))

     `(ansi-color-bright-black ((t (:foreground ,black))))
     `(ansi-color-bright-red ((t (:foreground ,red+1))))
     `(ansi-color-bright-green ((t (:foreground ,green+1))))
     `(ansi-color-bright-yellow ((t (:foreground ,yellow+1))))
     `(ansi-color-bright-blue ((t (:foreground ,blue+1))))
     `(ansi-color-bright-magenta ((t (:foreground ,magenta+1))))
     `(ansi-color-bright-cyan ((t (:foreground ,blue+1))))
     `(ansi-color-bright-white ((t (:foreground ,white))))

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
     `(header-line ((t (:foreground ,fg :background ,bg-1))))
     `(lsp-headerline-breadcrumb-path-face ((t (:foreground ,fg :background ,bg-1))))
     `(lsp-headerline-breadcrumb-symbol-face ((t (:background ,bg-1))))
     `(lsp-headerline-breadcrumb-symbols-face ((t (:background ,bg-1))))
     `(lsp-headerline-breadcrumb-segments-face ((t (:background ,bg-1))))
     `(lsp-headerline-breadcrumb-separator-face ((t (:foreground ,fg :background ,bg-1))))
     `(lsp-lsp-flycheck-warning-unnecessary-face ((t (:inherit flycheck-warning))))
     `(lsp-treemacs-file-hint ((t (:foreground ,green :weight bold))))
     `(lsp-treemacs-file-info ((t (:foreground ,green :weight bold))))
     `(lsp-installation-buffer-face ((t (:foreground ,green))))
     `(lsp-face-highlight-write ((t   (:inherit highlight))))
     `(lsp-face-highlight-read ((t    (:inherit highlight))))
     `(lsp-face-highlight-textual ((t (:inherit highlight))))

     `(lsp-ui-doc-header ((t (:background ,bg+1 :weight bold))))
     `(lsp-ui-doc-background ((t (:background ,bg-1))))
     `(lsp-ui-peek-filename ((t (:foreground ,yellow))))
     `(lsp-ui-peek-footer ((t (:background ,bg+1 :weight bold))))
     `(lsp-ui-peek-header ((t (:inherit lsp-ui-peek-footer))))
     `(lsp-ui-peek-highlight ((t (:inherit lsp-ui-peek-footer ,@shanty-box))))
     `(lsp-ui-peek-line-number ((t (:inherit linum))))
     `(lsp-ui-peek-list ((t (:background ,bg-1))))
     `(lsp-ui-peek-selection ((t (:background ,bg+1 :foreground ,fg+1 :weight bold))))

     `(lsp-signature-posframe ((t (:background ,bg-1 :foreground ,fg :weight bold))))
     `(lsp-ui-sideline-code-action ((t (:foreground ,yellow))))
     `(lsp-ui-sideline-current-symbol ((t (:foreground ,fg+1 :weight bold ,@shanty-box))))
     `(lsp-ui-sideline-symbol ((t (:foreground ,fg ,@shanty-box))))

     ;;;; symbol-overlay
     `(symbol-overlay-default-face ((t (:inherit highlight))))

     ;;;; rainbow-delimiters
     `(rainbow-delimiters-base-face ((t (:foreground ,fg))))
     `(rainbow-delimiters-base-error-face ((t (:background ,red :foreground ,fg :weight bold))))
     `(rainbow-delimiters-mismatched-face ((t (:background ,red :foreground ,fg :weight bold))))
     `(rainbow-delimiters-unmatched-face ((t (:background  ,red :foreground ,fg :weight bold))))

     ;;;; minibuffer
     `(minibuffer-prompt ((t (:foreground ,blue))))

     ;;;; ivy
     `(ivy-current-match ((t (:background ,yellow+1 :foreground ,bg :weight bold))))
     `(ivy-minibuffer-match-face-1 ((t (:background ,blue+1 :foreground ,bg :weight bold))))
     `(ivy-minibuffer-match-face-2 ((t (:background ,purple+1 :foreground ,bg :weight bold))))
     `(ivy-minibuffer-match-face-3 ((t (:background ,magenta+1 :foreground ,bg :weight bold))))
     `(ivy-minibuffer-match-face-4 ((t (:background ,green+1 :foreground ,bg :weight bold))))
     `(ivy-grep-info ((t (:foreground ,green :weight bold))))
     `(ivy-confirm-face ((t (:foreground ,green))))
     `(ivy-cursor ((t (:inherit cursor :foreground ,bg))))
     `(ivy-match-required-face ((t (:foreground ,red))))

     ;;;; helm
     `(helm-selection ((t (:background ,yellow+1 :foreground ,bg :weight bold))))
     `(helm-match ((t (:background ,blue+1 :foreground ,bg :weight bold))))
     `(helm-M-x-key ((t (:foreground ,yellow :weight bold))))
     `(helm-action ((t (:foreground ,yellow :weight bold))))
     `(helm-candidate-number ((t (:foreground ,blue :weight bold))))
     `(helm-candidate-number-suspended ((t (:foreground ,orange))))
     `(helm-header ((t (:background ,bg-1 :foreground ,fg+1 :overline ,fg-2))))
     `(helm-header-line-left-margin ((t (:foreground ,yellow+1 :weight bold))))
     `(helm-source-header ((t (:background ,fg-2 :foreground ,fg+1 :weight bold
                               :height 1.3 :family "Sans Serif"))))

     `(helm-bookmark-addressbook ((t (:foreground ,green))))
     `(helm-bookmark-directory ((t (:foreground ,blue :weight bold))))
     `(helm-bookmark-file ((t (:foreground ,fg))))
     `(helm-bookmark-file-not-found ((t (:foreground ,red))))
     `(helm-bookmark-gnus ((t (:foreground ,purple+1))))
     `(helm-bookmark-info ((t (:foreground ,blue+1))))
     `(helm-bookmark-man ((t (:foreground ,yellow+1))))
     `(helm-bookmark-w3m ((t (:foreground ,blue+1))))

     `(helm-buffer-archive ((t (:foreground ,blue+1 :weight bold))))
     `(helm-buffer-file ((t (:foreground ,fg))))
     `(helm-buffer-modified ((t (:foreground ,yellow-1))))
     `(helm-buffer-not-saved ((t (:foreground ,red+1))))
     `(helm-buffer-process ((t (:foreground ,purple+1))))
     `(helm-buffer-saved-out ((t (:foreground ,red :weight bold))))
     `(helm-buffer-size ((t (:inherit shadow))))
     `(helm-buffer-directory ((t (:foreground ,blue :weight bold))))

     `(helm-comint-prompts-buffer-name ((t (:foreground ,green))))
     `(helm-comint-prompts-promptidx ((t (:foreground ,blue+1))))
     `(helm-delete-async-message ((t (:foreground ,purple+1 :weight bold))))
     `(helm-eob-line ((t (:foreground ,fg))))
     `(helm-eshell-prompts-buffer-name ((t (:foreground ,green))))
     `(helm-eshell-prompts-promptidx ((t (:foreground ,blue+1))))
     `(helm-etags-file ((t (:foreground ,fg-1 :underline t))))

     `(helm-ff-file ((t (:foreground ,fg))))
     `(helm-ff-symlink ((t (:foreground ,fg-1 :slant italic))))
     `(helm-ff-truename ((t (:foreground ,green))))
     `(helm-ff-file-extension ((t (:foreground ,fg))))
     `(helm-ff-backup-file ((t (:inherit shadow))))
     `(helm-ff-denied ((t (:foreground ,red))))
     `(helm-ff-dirs ((t (:foreground ,blue :weight bold))))
     `(helm-ff-directory ((t (:foreground ,blue :weight bold))))
     `(helm-ff-dotted-directory ((t (:foreground ,blue :weight bold))))
     `(helm-ff-dotted-symlink-directory ((t (:foreground ,fg-1 :slant italic))))
     `(helm-ff-executable ((t (:foreground ,fg))))
     `(helm-ff-invalid-symlink ((t (:inherit shadow :strike-through t))))
     `(helm-ff-pipe ((t (:foreground ,purple+1))))
     `(helm-ff-prefix ((t (:foreground ,purple+1))))
     `(helm-ff-rsync-progress ((t (:foreground ,red+1))))
     `(helm-ff-socket ((t (:foreground ,red+1))))
     `(helm-ff-suid ((t (:foreground ,red+1))))

     `(helm-fd-finish ((t (:foreground ,green))))
     `(helm-grep-cmd-line ((t (:foreground ,yellow-1))))
     `(helm-grep-file ((t (:foreground ,fg :weight bold))))
     `(helm-grep-finish ((t (:foreground ,green))))
     `(helm-grep-lineno ((t (:foreground ,fg+1))))
     `(helm-grep-match ((t (:foreground ,magenta+1))))
     `(helm-history-deleted ((t (:foreground ,red+1))))
     `(helm-history-remote ((t (:foreground ,red+1))))
     `(helm-lisp-completion-info ((t (:inherit compilation-info))))
     `(helm-lisp-show-completion ((t (:foreground ,green+1))))
     `(helm-locate-finish ((t (:inherit success))))
     `(helm-minibuffer-prompt ((t (:foreground ,blue))))
     `(helm-moccur-buffer ((t (:foreground ,blue+1 :underline t))))
     `(helm-mode-prefix ((t (:foreground ,purple+1))))
     `(helm-no-file-buffer-modified ((t (:foreground ,orange))))
     `(helm-non-file-buffer ((t (:inherit shadow))))
     `(helm-prefarg ((t (:foreground ,green))))
     `(helm-resume-need-update ((t (:background ,red :foreground ,bg :weight bold))))
     `(helm-selection-line ((t (:inherit highlight))))
     `(helm-separator ((t (:foreground ,fg-1))))
     `(helm-time-zone-current ((t (:foreground ,green+1))))
     `(helm-time-zone-home ((t (:foreground ,purple+1))))
     `(helm-visible-mark ((t (:background ,green :foreground ,bg))))

     ;;;; vertico-mode
     `(vertico-current ((t (:background ,yellow+1 :foreground ,bg :weight bold))))

     ;;;; marginalia-mode
     ;; `(marginalia-key ((t (:foreground ,fg))))
     ;; `(marginalia-symbol ((t (:foreground ,fg))))
     ;; `(marginalia-list ((t (:foreground ,fg))))
     ;; `(marginalia-number ((t (:foreground ,fg))))
     `(marginalia-installed ((t (:foreground ,green))))
     `(marginalia-on ((t (:foreground ,green))))

     ;;;; markdown-mode
     `(markdown-highlight-face ((t (:inherit highlight))))
     `(markdown-highlighting-face ((t (:inherit highlight))))

     ;;;; message-mode
     `(message-header-cc ((t (:foreground ,green :weight bold))))
     `(message-header-name ((t (:foreground ,green))))
     `(message-header-newsgroups ((t (:foreground ,yellow+1))))
     `(message-header-other ((t (:foreground ,magenta :weight bold))))
     `(message-header-xheader ((t (:foreground ,blue-1))))
     `(message-mml ((t (:foreground ,blue))))

     ;;;; swiper
     `(swiper-line-face ((t (:inherit lazy-highlight))))
     `(swiper-background-match-face-1 ((t (:inherit lazy-highlight))))
     `(swiper-background-match-face-2 ((t (:background ,green :foreground ,bg))))
     `(swiper-background-match-face-3 ((t (:background ,blue :foreground ,bg))))
     `(swiper-background-match-face-4 ((t (:background ,purple :foreground ,bg))))
     `(swiper-match-face-1 ((t (:inherit lazy-highlight))))
     `(swiper-match-face-2 ((t (:background ,green :foreground ,bg))))
     `(swiper-match-face-3 ((t (:background ,blue :foreground ,bg))))
     `(swiper-match-face-4 ((t (:background ,purple :foreground ,bg))))

     ;;;; undo-tree
     `(undo-tree-visualizer-current-face ((t (:foreground ,red))))
     `(undo-tree-visualizer-default-face ((t (:foreground ,fg))))
     `(undo-tree-visualizer-register-face ((t (:foreground ,yellow+1))))
     `(undo-tree-visualizer-unmodified-face ((t (:foreground ,blue))))

     ;;;; xref
     `(xref-file-header ((t (:foreground ,green :weight bold))))
     `(xref-line-number ((t (:foreground ,yellow :weight bold))))

     ;;;; ccls
     `(ccls-tree-mouse-face ((t (:foreground ,green+1))))

     ;;;; speedbar
     `(speedbar-button-face ((t (:foreground ,green))))
     `(speedbar-directory-face ((t (:foreground ,fg))))
     `(speedbar-file-face ((t (:foreground ,blue))))
     `(speedbar-highlight-face ((t (:background ,green :foreground ,bg))))
     `(speedbar-selected-face ((t (:foreground ,yellow))))
     `(speedbar-separator-face ((t (:background ,blue :foreground ,bg :weight bold))))
     `(speedbar-tag-face ((t (:foreground ,magenta+1 :weight bold))))

     ;;;; diff
     `(diff-added ((t (:background ,green+1 :foreground ,bg))))
     `(diff-removed ((t (:background ,red+1 :foreground ,bg))))
     `(diff-file-header ((t (:background ,bg-1 :foreground ,fg))))
     `(diff-function ((t (:background ,bg+1 :foreground ,fg))))
     `(diff-header ((t (:background ,bg+1 :foreground ,fg))))
     `(diff-indicator-added ((t (:background ,green+1 :foreground ,bg))))
     `(diff-indicator-changed ((t (:background ,yellow+1 :foreground ,bg))))
     `(diff-indicator-removed ((t (:background ,red+1 :foreground ,bg))))
     `(diff-refine-added ((t (:background ,green+1 :foreground ,bg))))
     `(diff-refine-changed ((t (:background ,yellow+1 :foreground ,bg))))
     `(diff-refine-removed ((t (:background ,red+1 :foreground ,bg))))

     ;;;; ediff
     `(ediff-current-diff-A ((t (:background ,red+1 :foreground ,bg))))
     `(ediff-current-diff-Ancestor ((t (:background ,blue-1 :foreground ,bg))))
     `(ediff-current-diff-B ((t (:background ,green+1 :foreground ,bg))))
     `(ediff-current-diff-C ((t (:background ,yellow+1 :foreground ,bg))))
     `(ediff-fine-diff-A ((t (:background ,red :foreground ,bg))))
     `(ediff-fine-diff-Ancestor ((t (:background ,blue :foreground ,bg))))
     `(ediff-fine-diff-B ((t (:background ,green :foreground ,bg))))
     `(ediff-fine-diff-C ((t (:background ,yellow :foreground ,bg))))
     `(ediff-even-diff-A ((t (:background ,fg+1 :foreground ,bg))))
     `(ediff-even-diff-B ((t (:background ,fg :foreground ,bg+1))))
     `(ediff-even-diff-Ancestor ((t (:inherit ediff-even-diff-B))))
     `(ediff-even-diff-C ((t (:inherit ediff-even-diff-A))))
     `(ediff-odd-diff-A ((t (:background ,fg+1 :foreground ,bg))))
     `(ediff-odd-diff-B ((t (:background ,fg :foreground ,bg+1))))
     `(ediff-odd-diff-Ancestor ((t (:inherit ediff-odd-diff-B))))
     `(ediff-odd-diff-C ((t (:inherit ediff-odd-diff-A))))

     ;;;; custom
     `(custom-invalid ((t (:background ,red :foreground ,bg))))
     `(custom-modified ((t (:background ,blue+1 :foreground ,bg))))
     `(custom-set ((t (:background ,fg :foreground ,bg))))
     `(custom-state ((t (:foreground ,green))))
     `(custom-themed ((t (:background ,blue-1 :foreground ,bg))))
     `(custom-changed ((t (:background ,blue-1 :foreground ,bg))))
     `(custom-comment ((t (:inherit shadow))))

     ;;;; git-gutter / git-gutter-fringe
     `(git-gutter:added ((t (:backgroung ,bg+1 :foreground ,green))))
     `(git-gutter:modified ((t (:backgroung ,bg+1 :foreground ,yellow))))
     `(git-gutter:deleted ((t (:backgroung ,bg+1 :foreground ,red))))
     `(git-gutter:separator ((t (:backgroung ,bg+1 :foreground ,blue+1))))
     `(git-gutter:unchanged ((t (:backgroung ,bg+1 :foreground ,fg))))
     `(git-gutter-fr:added ((t (:backgroung ,bg+1 :foreground ,green))))
     `(git-gutter-fr:modified ((t (:backgroung ,bg+1 :foreground ,yellow))))
     `(git-gutter-fr:deleted ((t (:backgroung ,bg+1 :foreground ,red))))

     ;;;; info
     `(info-header-node ((t (:foreground ,fg :weight bold))))
     `(info-index-match ((t (:background ,blue :foreground ,bg))))
     `(info-menu-header ((t (:foreground ,fg :weight bold))))
     `(info-menu-star ((t (:foreground ,red))))
     `(info-node ((t (:foreground ,fg))))
     `(info-header-xref ((t (:inherit link))))
     `(info-xref ((t (:inherit link))))
     `(info-xref-visited ((t (:inherit link-visited))))

     ;;;; linum
     `(line-number ((t (:inherit shadow))))
     `(line-number-current-line ((t (:inherit shadow))))
     `(line-number-major-tick ((t (:foreground ,fg :weight bold))))
     `(line-number-minor-tick ((t (:foreground ,fg-1 :weight bold))))

     ;;;; gnus
     `(gnus-emphasis-highlight-words ((t (:background ,bg-1 :foreground ,yellow+1))))
     ;; higher level == less important
     `(gnus-group-mail-1 ((t (:foreground ,yellow+1 :weight bold))))
     `(gnus-group-mail-1-empty ((t (:foreground ,yellow+1))))
     `(gnus-group-mail-2 ((t (:foreground ,green :weight bold))))
     `(gnus-group-mail-2-empty ((t (:foreground ,green))))
     `(gnus-group-mail-3 ((t (:foreground ,blue+1 :weight bold))))
     `(gnus-group-mail-3-empty ((t (:foreground ,blue+1))))
     `(gnus-group-mail-low ((t (:foreground ,fg-1 :weight bold))))
     `(gnus-group-mail-low-empty ((t (:foreground ,fg-1))))

     `(gnus-group-news-1 ((t (:foreground ,yellow+1 :weight bold))))
     `(gnus-group-news-1-empty ((t (:foreground ,yellow+1))))
     `(gnus-group-news-2 ((t (:foreground ,green :weight bold))))
     `(gnus-group-news-2-empty ((t (:foreground ,green))))
     `(gnus-group-news-3 ((t (:foreground ,blue+1 :weight bold))))
     `(gnus-group-news-3-empty ((t (:foreground ,blue+1))))
     `(gnus-group-news-4 ((t (:foreground ,fg :weight bold))))
     `(gnus-group-news-4-empty ((t (:foreground ,fg))))
     `(gnus-group-news-5 ((t (:foreground ,fg :weight bold))))
     `(gnus-group-news-5-empty ((t (:foreground ,fg))))
     `(gnus-group-news-6 ((t (:foreground ,fg :weight bold))))
     `(gnus-group-news-6-empty ((t (:foreground ,fg))))
     `(gnus-group-news-low ((t (:foreground ,fg-1 :weight bold))))
     `(gnus-group-news-low-empty ((t (:foreground ,fg-1))))

     `(gnus-header-content ((t (:foreground ,blue+1))))
     `(gnus-header-from ((t (:foreground ,blue+1 :weight bold))))
     `(gnus-header-name ((t (:foreground ,fg-1 :weight bold))))
     `(gnus-header-subject ((t (:foreground ,magenta+1 :weight bold ,@shanty-box))))
     `(gnus-header-newsgroups ((t (:foreground ,yellow+1 :slant italic))))

     `(gnus-server-cloud ((t (:foreground ,green+1 :weight bold))))
     `(gnus-server-cloud-host ((t (:background ,green+1 :foreground ,bg :weight bold))))
     `(gnus-server-denied ((t (:foreground ,red :weight bold))))
     `(gnus-server-offline ((t (:foreground ,magenta :weight bold))))
     `(gnus-server-opened ((t (:foreground ,green :weight bold))))

     `(gnus-summary-cancelled ((t (:foreground ,yellow+1 :weight bold))))
     `(gnus-summary-high-ancient ((t (:foreground ,blue+1 :weight bold))))
     `(gnus-summary-high-read ((t (:foreground ,green+1 :weight bold))))
     `(gnus-summary-high-ticked ((t (:foreground ,red+1 :weight bold))))
     `(gnus-summary-high-undownloaded ((t (:foreground ,fg-1 :weight bold))))
     `(gnus-summary-high-unread ((t (:foreground ,fg :weight bold))))

     `(gnus-summary-low-ancient ((t (:foreground ,blue+1 :slant italic))))
     `(gnus-summary-low-read ((t (:foreground ,green+1 :slant italic))))
     `(gnus-summary-low-ticked ((t (:foreground ,red+1 :slant italic))))
     `(gnus-summary-low-undownloaded ((t (:foreground ,fg-1 :slant italic))))
     `(gnus-summary-low-unread ((t (:foreground ,fg :slant italic))))

     `(gnus-summary-normal-ancient ((t (:foreground ,blue+1))))
     `(gnus-summary-normal-read ((t (:foreground ,green+1))))
     `(gnus-summary-normal-ticked ((t (:foreground ,red+1))))
     `(gnus-summary-normal-undownloaded ((t (:foreground ,fg-1))))
     `(gnus-summary-normal-unread ((t (:foreground ,fg))))

     `(message-header-cc ((t (:foreground ,blue+1 :weight bold))))
     `(message-header-from ((t (:foreground ,blue+1 :weight bold))))
     `(message-header-to ((t (:foreground ,blue+1 :weight bold))))
     `(message-header-name ((t (:foreground ,fg-1 :weight bold))))
     `(message-header-subject ((t (:foreground ,magenta+1 :weight bold ,@shanty-box))))
     `(message-header-xheader ((t (:foreground ,blue-1))))
     `(message-header-other ((t (:foreground ,blue+1))))

     `(message-cited-text-1 ((t (:foreground ,magenta+1))))
     `(message-cited-text-2 ((t (:foreground ,green+1))))
     `(message-cited-text-3 ((t (:foreground ,yellow+1))))
     `(message-cited-text-4 ((t (:foreground ,orange))))

     ;;;; ein (emacs ipython notebook)
     `(ein:basecell-input-area-face ((t (:background ,bg-1))))
     `(ein:basecell-input-prompt-face ((t (:background ,bg :foreground ,fg-2))))
     `(ein:cell-output-area ((t (:background ,bg+1))))

     ;;;; straight
     `(straight-process-command ((t (:foreground ,fg :weight bold))))
     `(straight-process-error ((t (:foreground ,red :weight bold))))


     ;;;; stuff I am not sure what is does
     `(lv-separator ((t (:foreground ,fg-2))))
     `(match ((t (:background ,blue+1 :foreground ,bg))))
     `(widget-button-pressed ((t (:background ,bg+1 :weight bold))))
     `(widget-documentation ((t (:foreground ,green))))
     `(widget-field ((t (:background ,fg-2))))
     `(widget-single-line-field ((t (:background ,fg-2))))
     `(pulse-highlight-face ((t (:background ,blue+1 :foreground ,bg))))
     `(pulse-highlight-start-face ((t (:background ,blue+1 :foreground ,bg)))))))

;;; Add theme directory to custom theme load path
;;; Source: modus-themes (thanks)
;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (equal dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'shanty-themes)

;;; shanty-themes.el ends here