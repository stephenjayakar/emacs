(deftheme fuck
  "Created 2020-08-11.")

(custom-theme-set-faces
 'fuck
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(cursor ((t (:background "#ffffaf"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "#87af87"))))
 '(homoglyph ((t (:foreground "#ffaf00"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "#afaf00" :background "#1c1c1c"))))
 '(highlight ((t (:background "#767676" :foreground "cyan"))))
 '(region ((t (:extend t :background "#4e4e4e"))))
 '(shadow ((t (:foreground "#767676"))))
 '(secondary-selection ((t (:extend t :background "#3a3a3a"))))
 '(trailing-whitespace ((t (:background "#d75f5f"))))
 '(font-lock-builtin-face ((t (:foreground "#ff8700"))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#767676"))))
 '(font-lock-constant-face ((t (:foreground "#d787af"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#ffaf00"))))
 '(font-lock-keyword-face ((t (:foreground "color-196"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#afaf00"))))
 '(font-lock-type-face ((t (:foreground "#d787af"))))
 '(font-lock-variable-name-face ((t (:foreground "cyan"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#d75f5f"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "#87afaf"))))
 '(link-visited ((t (:foreground "violet" :inherit (link)))))
 '(fringe ((t (:background "#1c1c1c"))))
 '(header-line ((t (:box nil :foreground "#a8a8a8" :background "#262626" :inherit nil))))
 '(tooltip ((t (:foreground "#ffdfaf" :background "#3a3a3a"))))
 '(mode-line ((t (:box nil :foreground "#bcbcbc" :background "#626262"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:box nil :foreground "#949494" :background "#3a3a3a"))))
 '(isearch ((t (:foreground "#000000" :background "#ff8700"))))
 '(isearch-fail ((t (:foreground "#ffffaf" :background "#d75f5f"))))
 '(lazy-highlight ((t (:foreground "#000000" :background "#ffaf00"))))
 '(match ((t (:foreground "#262626" :background "#87afaf"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(web-mode-html-tag-face ((t (:foreground "brightgreen"))))
 '(web-mode-html-attr-name-face ((t (:foreground "color-214"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "color-86")))))

(provide-theme 'fuck)
