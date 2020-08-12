;; Display line numbers
(global-linum-mode)
(setq linum-format "%d ")

;; Type 'y' for yes and 'n' for no
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default c-basic-offset 2)
;; Whitespace
(setq show-trailing-whitespace t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0060308491490f94b8ce88b1c61a3a4d0ca205b7ee83ea3ac872d54045b135ed" "d2429171eb1d49f759fe4b92491bb1e42ad58740faee30198a55c1bb309b4815" "81a2ce1cef41192a9287330ef977b09f0f2d44e90358217d7c92bf5ef92e7e63" "89f99afe25e09672733953d5ec6d2d283241c8be283499dfc04cf0cf5f588efe" "1a160b2402f5cfc471682261497d3c091304f05ce4215dc1408fd89b999a6aa0" "a67bd13c6e8ddd0d36c36db1f3bd87336b0c8e255d49b130b320899311068e9c" "dc76b621fde7ac2e7bc79897629e4de4847929214a703fa58300d8161c478314" "73ea640bac062126fc5e0b7b3e60f1881288e72c96c42332c5c4ea0dee79b6c2" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "#767676" :foreground "#ffffaf")))))
(load-theme 'fuck)
