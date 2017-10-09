;;; Commentary:

;; Color theme
;; http://d.hatena.ne.jp/aoe-tk/20130210/1360506829
;; http://qiita.com/iriya-ufo@github/items/6f3304a23268a51a688e#2-1
(setq custom-theme-directory "~/.emacs.d/themes/")
(load-theme 'wombat t)
;; (load-theme 'tango-dark t)

;; https://github.com/hbin/molokai-theme
;; (load-theme 'molokai t)

;; (load-theme 'misterioso t)


;; ------------------------------------------------------------------------
;; @ general

;; 言語を日本語とする
;; (set-language-environment 'Japanese)

;; 極力 UTF-8 とする
(prefer-coding-system 'utf-8-unix)

;; 曜日表記を英語にする
(setq system-time-locale "C")

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)

;; scratch の初期メッセージをオフにする
(setq initial-scratch-message "")

;; メニューバーを非表示
(menu-bar-mode -1)

;;;; iTerm2 上で使う場合は不要 --------------
;; ;; ツールバー非表示
;; (tool-bar-mode -1)

;; ;; スクロールバー非表示
;; (set-scroll-bar-mode nil)
;;;; ----------------------------------------

;; バックアップファイルを作らない
(setq make-backup-files nil)

;; オートセーブを無効にする
(setq auto-save-default nil)

;; yes or no を y or n にする
(fset 'yes-or-no-p 'y-or-n-p)

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; 補完可能なものを随時表示（少しうるさい）
(icomplete-mode 1)

;; カーソル点滅
(blink-cursor-mode 1)

;; バッファ再読み込み
(global-auto-revert-mode 1)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 行番号の表示
(global-linum-mode)
(setq linum-format "%4d ")

;; (global-nlinum-mode t)
;; (setq nlinum-format "%5d ")

;; 現在行をハイライト
;;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
;; (defface hlline-face
;;   '((((class color)
;;       (background dark))
;;      (:background "dark slate gray"))
;;     (((class color)
;;       (background light))
;;      (:background  "#98FB98"))
;;     (t
;;      ()))
;;   "*Face used by hl-line.")
;; (setq hl-line-face 'hlline-face)
;; (global-hl-line-mode)

;; paren-mode  対応する括弧を強調表示する
(show-paren-mode t)   ; 有効化
(setq show-paren-delay 0)   ; 表示までの秒数

;; スペルチェック
(setq-default flyspell-mode t)
(setq ispell-dictionary "american")

;; 最近使ったファイルの表示数
(setq recentf-max-menu-items 10)

;; 最近開いたファイルの保存数を増やす
(setq recentf-max-saved-items 3000)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; ミニバッファの履歴の保存数を増やす
(setq history-length 3000)

;; 行間
(setq-default line-spacing 0)

;; C-Ret で矩形選択
;; 詳しいキーバインド操作
;; http://dev.ariel-networks.com/articles/emacs/part5/
(cua-mode t)
(setq cua-enable-cua-keys nil)


;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; trailing-whitespace 除外リスト
;; http://qiita.com/tadsan/items/df73c711f921708facdc

(defun my/disable-trailing-mode-hook ()
  "Disable show tail whitespace."
  (setq show-trailing-whitespace nil))

(defvar my/disable-trailing-modes
  '(comint-mode
    eshell-mode
    term-mode
    calendar-mode
    twittering-mode))
(mapc
 (lambda (mode)
   (add-hook (intern (concat (symbol-name mode) "-hook"))
             'my/disable-trailing-mode-hook))
 my/disable-trailing-modes)

;; 保存時に行末のスペースを削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables '(tab-width 4))

;; dired を便利にする
(require 'dired-x)

;; dired から "r" でファイル名をインライン編集する
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; バッファ名を変更する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)


;; -------------------------------------------------------------------------
;; @ calendar

;; Calendar 月曜始まりにする
(setq calendar-week-start-day 1)

;; 標準の祝日を利用しない
(setq calendar-holidays nil)

;; http://emacs.rubikitch.com/japanese-holidays/

(with-eval-after-load "calendar"
  (require 'japanese-holidays)
  (setq calendar-holidays ; 他の国の祝日も表示させたい場合は適当に調整
        (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (setq calendar-mark-holidays-flag t)    ; 祝日をカレンダーに表示
  ;; 土曜日・日曜日を祝日として表示する場合、以下の設定を追加します。
  ;; 変数はデフォルトで設定済み
  (setq japanese-holiday-weekend '(0 6)    ; 土日を祝日として表示
        japanese-holiday-weekend-marker    ; 土曜日を水色で表示
        '(holiday nil nil nil nil nil japanese-holiday-saturday))
  (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
  ;; “きょう”をマークするには以下の設定を追加します。
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  ;; org-agendaで祝日を表示する
  (setq org-agenda-include-diary t))


;; -------------------------------------------------------------------------
;; @ mode-line

;; モードラインに時間を表示する
(display-time)

;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; モードラインの割合表示を総行数表示
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)

(when my-lines-page-mode
  (setq my-mode-line-format "%d")
  (if size-indication-mode
      (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
  (cond ((and (eq line-number-mode t) (eq column-number-mode t))
         (setq my-mode-line-format (concat my-mode-line-format " (%%l,%%c)")))
        ((eq line-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
        ((eq column-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))

  (setq mode-line-position
        '(:eval (format my-mode-line-format
        (count-lines (point-max) (point-min))))))


;; https://github.com/shibayu36/emacs/blob/8376ba4e4b4d5cb4668e848c9a494b287232ea6f/emacs.d/inits/01-mode-line.el
;;; modeの名前を自分で再定義
(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (flymake-mode . " Fm")
    (paredit-mode . "")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . "")
    (git-gutter-mode . "")
    (anzu-mode . "")
    (yas-minor-mode . "")
    (guide-key-mode . "")

    ;; Major modes
    (fundamental-mode . "Fund")
    (dired-mode . "Dir")
    (lisp-interaction-mode . "Li")
    (cperl-mode . "Pl")
    (python-mode . "Py")
    (ruby-mode   . "Rb")
    (emacs-lisp-mode . "El")
    (markdown-mode . "Md")))


;; -------------------------------------------------------------------------
;; @ Mac OS X

;; iTerm2 で使うのでフォントの設定などは iTerm2 側で行う

;; Command を Meta キーとする
;; http://qiita.com/hayamiz/items/0f0b7a012ec730351678
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; IME
;; http://b.mgrace.info/?p=1128
(setq default-input-method "MacOSX")
;; ;; emacs 起動時は英数モードから始める
;; (add-hook 'after-init-hook 'mac-change-language-to-us)
;; minibuffer 内は英数モードにする
;; (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
;; ;; backslash を先
;; (mac-translate-from-yen-to-backslash)


;; OS X とクリップボードを同期する
(defun copy-from-osx ()
 (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)


;; -------------------------------------------------------------------------
;; @ 各種キーバインド設定

;; C-h を backspace にする
(global-set-key (kbd "C-h") 'delete-backward-char)

;; help をC-x ? に割り当てる
(global-set-key (kbd "C-x ?") 'help-command)

;; http://tech.kayac.com/archive/emacs.html
(global-set-key (kbd "C-c C-a")	'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "M-g")	'goto-line)
(global-set-key (kbd "C-S-i")	'indent-region)
(global-set-key (kbd "C-M-r")	'replace-regexp)
;; (global-set-key (kbd "C-m")	'newline-and-indent)


;; -------------------------------------------------------------------------
;; @ Package, Cask, auto-install

;; load-path の設定
(setq load-path
     (append
       (list
	(expand-file-name "~/.emacs.d/elisp/"))
       load-path))

;; packages
(require 'package)
;; 使い方
;; http://emacs-jp.github.io/packages/package-management/package-el.html

;; MELPA を追加
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; Marmalade を追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))
;; 初期化
(package-initialize)

;; Cask
;; http://cask.readthedocs.org/en/latest/guide/usage.html
;; http://d.hatena.ne.jp/syohex/20140424/1398310931
(require 'cask "~/.cask/cask.el")
(cask-initialize)


;; -------------------------------------------------------------------------
;; @ anything

;; anything.el を有効にする
(require 'anything-startup)
(require 'anything-config)
(require 'anything-match-plugin)
(require 'anything-complete)
(anything-read-string-mode 1)
(require 'anything-show-completion)
(global-set-key "\C-x\C-b" 'anything-filelist+)

;; anything でファイルを開く
;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
(defun anything-custom-filelist ()
    (interactive)
    (anything-other-buffer
     (append
      '(anything-c-source-ffap-line
        anything-c-source-ffap-guesser
        anything-c-source-buffers+
        )
      (anything-c-sources-git-project-for)
      '(anything-c-source-recentf
        anything-c-source-bookmarks
        anything-c-source-file-cache
        anything-c-source-filelist
        ))
     "*anything file list*"))

;; anything で kill ring を使う
(global-set-key (kbd "M-y") 'anything-show-kill-ring)

;; M-x anything (anything) を C-o に割り当てる
(global-set-key (kbd "C-o") 'anything)

;; C-x o (other-window) の代わり
;; (global-set-key (kbd "C-t") 'other-window)
;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)


;; -------------------------------------------------------------------------
;; @ org-mode

;; org-remember の設定
(require 'org)

;; コードブロックを当該言語のモードでハイライトする
(setq org-src-fontify-natively t)

;; http://d.hatena.ne.jp/tamura70/20100203/org
;;;(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c a") 'org-agenda)
;; ハイパーリンクをたどる ?
(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-use-fast-todo-selection t)
;; TODO キーワードの設定 "|" より右側は完了状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCEL(c)" "PENDING(p)" )))

;; メモを格納する org ファイルの設定
(setq org-directory "~/Dropbox/Org/")

;; org-capture の設定
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(
	;; ("t" "Todo" entry (file+headline "~/Dropbox/Org/plan.org" "Inbox")
	;;  "* TODO %?\n %U" :prepend t :empty-lines 1)

	("S" "SS" entry (file+headline "~/Dropbox/Org/ss.org" "Inbox")
	 "* %U %?\n\n" :prepend t :empty-lines 1)

	("d" "Diary" entry (file+headline "~/Dropbox/Org/diary.org" "Inbox")
	 "* %U %?\n\n" :prepend t :empty-lines 1)

	("n" "Note" entry (file "~/Dropbox/Org/note.org")
	 "* %U %?\n\n" :prepend t :empty-lines 1)

	("f" "Forex" entry (file+headline "~/Dropbox/Org/forex.org" "Inbox")
	 "* %U %?\n\n" :prepend t :empty-lines 1)

	("c" "Forex Chart" entry (file+headline "~/Dropbox/Org/forex_chart.org" "Inbox")
	 "* %U %?\n\n" :prepend t :empty-lines 1)

   	("l" "Trade log Long" entry (file+headline "~/Dropbox/Org/trade_log.org" "Inbox")
	 "* %U %? Long%[~/Dropbox/Org/templates/trade_log_long.txt]" :prepend t :empty-lines 1)

   	("s" "Trade log Short" entry (file+headline "~/Dropbox/Org/trade_log.org" "Inbox")
	 "* %U %? Short%[~/Dropbox/Org/templates/trade_log_short.txt]" :prepend t :empty-lines 1)

	))


;; http://d.hatena.ne.jp/tamura70/20100208/org
;; アジェンダ表示の対象ファイル
(setq org-agenda-files (list org-directory))

;; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq hl-line-face 'underline)

;; org-mode 折り返しの設定
;; http://d.hatena.ne.jp/stakizawa/20091025/t1
;;; M-x change-truncation で切り替え
(setq org-startup-truncated nil)
(defun change-truncation()
  (interactive)
  (cond ((eq truncate-lines nil)
         (setq truncate-lines t))
        (t
         (setq truncate-lines nil))))

;; http://rubikitch.com/2014/10/10/org-sparse-tree-indirect-buffer/
(defun org-sparse-tree-indirect-buffer (arg)
  (interactive "P")
  (let ((ibuf (switch-to-buffer (org-get-indirect-buffer))))
    (condition-case _
        (org-sparse-tree arg)
      (quit (kill-buffer ibuf)))))
(define-key org-mode-map (kbd "C-c /") 'org-sparse-tree-indirect-buffer)

;; org-refile
(setq org-refile-targets
      (quote (
              ;; ("note.org" :level . 2)
              ("forex.org" :level . 1)
              ("trading-method.org" :level . 2)
              ("knowledge.org" :level . 1)
              )))

;; TODO 項目の追加 M-S-RET がなぜか効かないので
(global-set-key (kbd "C-c t") 'org-insert-todo-heading)

;; コードを評価するとき尋ねない
(setq org-confirm-babel-evaluate nil)


;; -------------------------------------------------------------------------
;; @ auto-complete

(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
;; auto-completeまでの時間
(setq ac-delay 0.1)
;; メニューが表示されるまで
(setq ac-auto-show-menu 0.2)
;; C-p, C-n で候補選択
(setq ac-use-menu-map t)
(setq ac-use-fuzzy t)


;; http://torotoki.hatenablog.com/entry/2013/06/05/032527
(set-face-background 'ac-completion-face "#333333")
(set-face-foreground 'ac-candidate-face "#666666")
(set-face-background 'ac-selection-face "#666666")
(set-face-foreground 'popup-summary-face "white")  ;; 候補のサマリー部分
(set-face-background 'popup-tip-face "cyan")  ;; ドキュメント部分
(set-face-foreground 'popup-tip-face "white")


;; -------------------------------------------------------------------------
;; @ yasnippet

;; http://konbu13.hatenablog.com/entry/2014/01/12/113300
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mySnippets"
        "~/.emacs.d/snippets"
	))
(yas-global-mode 1)

(custom-set-variables '(yas-trigger-key "TAB"))
;; download snippets from GitHub
;; ~/.emacs.d/
;; % git clone https://github.com/AndreaCrotti/yasnippet-snippets.git snippets

;; insert snippet
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; new snippet
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; view, edit snippet
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)


;; -------------------------------------------------------------------------
;; @ autoinsert

;; http://ymotongpoo.hatenablog.com/entry/2012/12/02/190248
(require 'autoinsert)

;; テンプレートのディレクトリ
(setq auto-insert-directory "~/.emacs.d/templates")

;; 各ファイルによってテンプレートを切り替える
(setq auto-insert-alist
      (nconc '(
               ("\\.rst$" . ["template.rst" my-template])
               ("\\.py$" . ["template.py" my-template])
               ("\\.html" . ["template.html" my-template])
               ) auto-insert-alist))
(require 'cl)

(defvar template-replacements-alists
  '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%date%" . (lambda () (format-time-string "%Y-%m-%d %H:%M:%S")))
))

(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)


;; -------------------------------------------------------------------------
;; @ popwin

;; http://valvallow.blogspot.jp/2011/03/emacs-popwinel.html

(require 'popwin)
(defvar popwin:special-display-config-backup popwin:special-display-config)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:special-display-config
      (append '(
		("*Remember*" :stick t)
		("*Org Agenda*" :height 0.5)
		("anything" :regexp t :height 0.5)
		("*Backtrace*")
		("*quickrun*" :height 0.4)
		("magit" :regexp t :height 0.5)
		("*Dired*" :height 0.5)
		("COMMIT_EDITMSG")
                ("*sdic*" :noselect)
		)
              popwin:special-display-config))
(define-key global-map (kbd "C-x p") 'popwin:display-last-buffer)


;; -------------------------------------------------------------------------
;; @ 便利関数いろいろ

;; 分割したバッファを入れ替える
;; http://www.bookshelf.jp/soft/meadow_30.html#SEC403
(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen) ; F2 で入れ替え
(global-set-key [S-f2] 'swap-screen-with-cursor) ; shift + F2 でカーソルごと入れ替え

;; window resize
;;; http://d.hatena.ne.jp/khiker/20100119/window_resize
;;; http://d.hatena.ne.jp/mooz/20100119/p1
(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))
(global-set-key "\C-c\C-r" 'my-window-resizer)

;; 透明度を変更するコマンド M-x set-alpha
;; http://qiita.com/marcy@github/items/ba0d018a03381a964f24
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

;; 現在時刻の挿入
;; http://d.hatena.ne.jp/CortYuming/20101125/p1
(defun my-insert-date ()
  (interactive)
  (insert (concat
	   "" (format-time-string "%Y-%m-%d(%a) %H:%M:%S "))))


;; -------------------------------------------------------------------------
;; @ 検索置換

;; migemo
;; http://weblog.ymt2.net/blog/html/2013/08/23/install_migemo_to_emacs_24_3_1.html
;; http://rubikitch.com/2014/08/20/migemo/
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)

;; visual-regexp
;; https://github.com/benma/visual-regexp.el
(require 'visual-regexp)
(global-set-key "\M-%" 'vr/query-replace)


;; -------------------------------------------------------------------------
;; @ Languages, programming

;; jedi Python
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)



;; js2-mode
;; http://goo.gl/ny0vtW
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;; rst.el
;; http://ymotongpoo.hatenablog.com/entry/2012/12/02/190248
(require 'rst)
;; 拡張子の*.rst, *.restのファイルをrst-modeで開く
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
		("\\.rest$" . rst-mode)) auto-mode-alist))
;; 背景が黒い場合はこうしないと見出しが見づらい
(setq frame-background-mode 'dark)
;; 全部スペースでインデントしましょう
(add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil)))
;; 見出しを設定する
(global-set-key "\C-c=" 'rst-adjust)


;; quickrun
;; https://github.com/syohex/emacs-quickrun
;; http://yunojy.github.io/blog/2013/03/17/emacs-de-quickrun-or-quickrun-region/
(require 'quickrun)
(defun quickrun-sc (start end)
  (interactive "r")
  (if mark-active
      (quickrun :start start :end end)
    (quickrun)))
;; (global-set-key (kbd "<f5>") 'quickrun-sc)
(global-set-key (kbd "M-q") 'quickrun-sc)

;; fly-check
(add-hook 'after-init-hook #'global-flycheck-mode)


;; ------------------------------------------------------------------------
;; @ 便利 packages いろいろ

;; open-junk-file
;; http://d.hatena.ne.jp/rubikitch/20080923/1222104034
(require 'open-junk-file)
(setq open-junk-file-format "~/Dropbox/junk/%Y%m%d-%H%M%S." )
(global-set-key (kbd "C-x j") 'open-junk-file)

;; key-chord.el
;; ;; M-x install-elisp-from-emacswiki RET  key-chord.el
;; (require 'key-chord)
;; (key-chord-mode 1)
;; ;; jk 同時押しで M-x org-remember を実行
;; (key-chord-define-global "jk" 'org-remember)
;; ;; df 同時押しで M-x my-insert-date を実行
;; (key-chord-define-global "df" 'my-insert-date)

;; text-adjust.el
;; http://d.hatena.ne.jp/rubikitch/20090220/text_adjust
;; http://rubikitch.com/f/text-adjust.el
;; http://rubikitch.com/f/mell.el
(require 'text-adjust)
(defun text-adjust-space-before-save-if-needed ()
  (when (memq major-mode
              '(org-mode
                text-mode
                Tuareg-mode
                ReST-mode
                mew-draft-mode
                myhatena-mode))
    (text-adjust-space-buffer)))
(defalias 'spacer 'text-adjust-space-buffer)
(add-hook 'before-save-hook 'text-adjust-space-before-save-if-needed)

;; flex-autopair.el
;; http://d.hatena.ne.jp/uk-ar/20120401/1333282805
(require 'flex-autopair)
(flex-autopair-mode 1)

;; undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))


;; ------------------------------------------------------------------------
;; @ Git

;; magit
;; http://qiita.com/takc923/items/c7a11ff30caedc4c5ba7
;; (require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")


;; ------------------------------------------------------------------------
;; @ Markdown

;; from http://support.markedapp.com/kb/how-to-tips-and-tricks/marked-bonus-pack-scripts-commands-and-bundles

;; (defun markdown-preview-file ()
;;   "run Marked on the current file and revert the buffer"
;;   (interactive)
;;   (shell-command
;;    (format "open -a /Applications/Marked.app %s"
;;        (shell-quote-argument (buffer-file-name))))
;; )
;; (global-set-key (kbd "C-c m") 'markdown-preview-file)

;; ------------------------------------------------------------------------
;; @ Server

; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))


;; ------------------------------------------------------------------------
;; @ Tuareg, OCaml

;; https://github.com/realworldocaml/book/wiki/Installation-Instructions
(require 'tuareg)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;; (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)

;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)
;; Use the opam installed utop
(setq utop-command "opam config exec -- utop -emacs")


(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)
(setq tuareg-use-smie nil)

;; from merlin install message
(let ((opam-share (ignore-errors (car (process-lines
   "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
       ;; Register Merlin
       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
       (autoload 'merlin-mode "merlin" nil t nil)
       ;; Automatically start it in OCaml buffers
       (add-hook 'tuareg-mode-hook 'merlin-mode t)
       (add-hook 'caml-mode-hook 'merlin-mode t)
       ;; Use opam switch to lookup ocamlmerlin binary
       (setq merlin-command 'opam)))

;; Load merlin-mode
(require 'merlin)

;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)

(require 'ocp-indent)


;; ------------------------------------------------------------------------
;; @ ediff

;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)


;; ------------------------------------------------------------------------
;; @ PATH

(add-to-list 'exec-path (expand-file-name "/opt/local/bin"))

;; ------------------------------------------------------------------------
;; @ Go

;; http://qiita.com/koki_cheese/items/2e2ead918a1f1ac5bf6e
;; http://unknownplace.org/archives/golang-editing-with-emacs.html

(require 'go-eldoc)
(add-to-list 'exec-path (expand-file-name (concat (getenv "GOROOT") "/bin")))
(add-to-list 'exec-path (expand-file-name (concat (getenv "GOPATH") "/bin")))
(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)
     (require 'auto-complete-config)

     ;; eldoc
     (add-hook 'go-mode-hook 'go-eldoc-setup)
     (set-face-attribute 'eldoc-highlight-function-argument nil
                         :underline t :foreground "green"
                         :weight 'bold)

     ;; gofmtをgoimportsに上書き
     (setq gofmt-command "goimports")

     ;; gofmt
     (add-hook 'before-save-hook 'gofmt-before-save)

     ;; key bindings
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
     )
  )


;; ------------------------------------------------------------------------
;; @ google-translate

;; http://blog.shibayu36.org/entry/2016/05/29/123342

(require 'google-translate)
(require 'google-translate-default-ui)

(defvar google-translate-english-chars "[:ascii:]"
  "これらの文字が含まれているときは英語とみなす")
(defun google-translate-enja-or-jaen (&optional string)
  "regionか現在位置の単語を翻訳する。C-u付きでquery指定も可能"
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (thing-at-point 'word))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" google-translate-english-chars)
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))

(push '("\*Google Translate\*" :height 0.5 :stick t) popwin:special-display-config)

(global-set-key (kbd "C-M-t") 'google-translate-enja-or-jaen)


;;; --------------------------------------------------------
;; wand

;; http://emacs.rubikitch.com/wand/

(require 'wand)
(setq wand:*rules*
      (list
       ;; $ の後にシェルコマンドを書くと M-x shell-command で実行
       (wand:create-rule :match "\\$ "
                         :capture :after
                         :action shell-command)
       ;; http/httpsのURLをM-x browse-urlで開く
       (wand:create-rule :match "https?://"
                         :capture :whole
                         :action browse-url)
       ;; fileのURLをfind-fileで開く
       (wand:create-rule :match "file:"
                         :capture :after
                         :action find-file)
       ;; #> の後のS式を評価し結果を表示する
       (wand:create-rule :match "#> "
                         :capture :after
                         :action (lambda (string)
                                   (message "%S" (eval (read string)))))))
;;; 使用例
;; $ ls
;; http://emacs.rubikitch.com/
;; test file:~/.emacs.d/init.el
;; #> (+ 1 3)
(global-set-key (kbd "<f9>") 'wand:execute-current-line)

;;; init.el ends here
