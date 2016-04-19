;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-pervasives ver. 1.00
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include ~/.emacs.d/lisp in the load path
;; ~/.emacs.d下に入れたファイルをload,require等で読み込めるようにする
(setq load-path (cons "~/.emacs.d/lisp" load-path))

;; Do not show startup messages
;; 起動時に表示されるメッセージ, *scratch*バッファのメッセージ等を表示しない
(setq inhibit-startup-message t
      inhibit-startup-screen t
      initial-scratch-message nil)

;; Ask y/n instead of yes/no
;; yes/noではなくy/nで訊くようにする
(fset 'yes-or-no-p 'y-or-n-p)

;; Show row and column of the cursor position
;; カーソル位置の行番号と桁数を表示する
(setq column-number-mode t)

;; Hide toolbar
;; ツールバーを非表示にする
;(setq tool-bar-mode nil)

;; Hide menubar
;; メニューバーを非表示にする
;(setq menu-bar-mode nil)

;; Highlight matching parenthesis
;; 対応する括弧をハイライトする
(show-paren-mode t)

;; Highlight selected region
;; 選択範囲を表示する
(setq transient-mark-mode t)

;; Kill selected region by BS
;; 範囲選択中にバックスペースで選択範囲を削除する
(delete-selection-mode t)

;; Show line number
;; 行番号を(常に)表示する
(require 'linum)
(global-linum-mode)
;; linumが軽くなる
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

;; Highlight the current line
;; カーソル位置の行をハイライトする
;(global-hl-line-mode)

;; Do not blink cursor
;; カーソルを点滅しない
;;(blink-cursor-mode nil)

;; Show special character (») for wrapped line end
;; 長い行を折り返したときに記号(»)を表示する
(defface wrap-face
  '((((class color) (min-colors 88) (background dark))
     :foreground "aquamarine4")
    (((class color) (min-colors 88) (background light))
     :foreground "aquamarine2")
    (((class color) (min-colors 16))
     :foreground "DarkCyan")
    (((class color) (min-colors 8))
     :foreground "gray")
    (((type tty) (class mono))
     :inverse-video t))
  "Face of the wrap."
  :group 'convenience)
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code #xbb 'wrap-face))

;; Highlight whitespace at EOL
;; 行末の空白をハイライトする
(setq-default show-trailing-whitespace t)

;; Show buffer boundary indicator
;; バッファの範囲を示すマークを表示する
(setq-default indicate-buffer-boundaries 'left)

;; C-x C-b shows buffer selector
;; C-x C-bでバッファ選択画面を開く
(global-set-key (kbd "C-x C-b") 'bs-show)

;; Put directory names for the same name of different files
;; 同じ名前のファイルを開いたときにfile<2>ではなくdir/fileという表示にする
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; 色を変更する
(set-face-foreground 'minibuffer-prompt "skyblue")
(set-face-foreground 'compilation-column-number "gray")
(set-face-foreground 'shadow "yellow")

;; カーソル行に下線
(setq hl-line-face 'underline)
(global-hl-line-mode)

; コピペが普通にできるように
(defun copy-from-osx ()
 (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)


;; マウス操作ができる
;; ホイールマウス
(setq mouse-wheel-follow-mouse t)
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(global-set-key   [mouse-4] '(lambda () (interactive) (scroll-down 1)))
(global-set-key   [mouse-5] '(lambda () (interactive) (scroll-up   1)))

;バッファ移動がshift+矢印に
;(setq windmove-wrap-around t)
;(windmove-default-keybindings)

;undo-tree-mode
(require 'undo-tree)
(global-undo-tree-mode)

;; C-hでカーソルの前を削除
(global-set-key "\C-h" 'delete-backward-char)


;;80文字を超える場合は色変化
(add-hook 'prog-mode-hook #'whitespace-mode)
(eval-after-load 'whitespace
  '(progn
     (setq whitespace-style '(face lines-tail))
     (setq whitespace-line-column 80)))


;; キーバインドの交換
(global-set-key "\C-u" 'forward-word)
(global-set-key "\M-b" 'transpose-chars)

(global-set-key "\C-t" 'backward-word)
(global-set-key "\M-f" 'universal-argument)

(global-set-key "\C-q" 'kill-word)
(global-set-key "\M-d" 'quoted-insert)


;;
;; backup の保存先
;;
(setq backup-directory-alist
  (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
        backup-directory-alist))
(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.emacs.d/backup/") t)))


;;テーマの保存
;;色とか
(load-theme 'wombat t)
(set-face-foreground 'font-lock-comment-face "red")
;;(set-face-foreground 'font-lock-comment-delimiter-face "red")
(set-face-background 'default "black")


;; melpaを使用可能にする
;; M-x list-packages で一覧表示
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; emacs版easy-motion
;; (prelude-require-package 'ace-jump-mode)
;; (require 'ace-jump-mode)
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


