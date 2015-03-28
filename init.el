;;; Commentary:

;; Color theme
;; http://d.hatena.ne.jp/aoe-tk/20130210/1360506829
;; http://qiita.com/iriya-ufo@github/items/6f3304a23268a51a688e#2-1
;; (setq custom-theme-directory "~/.emacs.d/themes/")
;; (load-theme 'wombat t)
;; (load-theme 'tango-dark t)
(load-theme 'misterioso t)


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

;; Calendar 月曜始まりにする
(setq calendar-week-start-day 1)

;; 標準の祝日を利用しない
(setq calendar-holidays nil)

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

;; 現在行をハイライト
;;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#98FB98"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

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

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables '(tab-width 4))

;;; diredを便利にする
(require 'dired-x)

;;; diredから"r"でファイル名をインライン編集する
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; バッファ名を変更する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


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

;; powerline
;; http://shibayu36.hatenablog.com/entry/2014/02/11/160945
;; (require 'powerline)
;; (powerline-default-theme)


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

;; http://qiita.com/catatsuy/items/886f1e0632c0b2760fb4
;; (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" 'title "あ")

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

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))
;; 初期化
(package-initialize)

;; Cask
;; http://cask.readthedocs.org/en/latest/guide/usage.html
;; http://d.hatena.ne.jp/syohex/20140424/1398310931
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; auto-install
;; http://www.emacswiki.org/emacs/auto-install.el
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; -------------------------------------------------------------------------
;; @ Anything

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

	("n" "Note" entry (file "~/Dropbox/Org/note.org")
	 "* %U %?\n\n" :prepend t :empty-lines 1)

	;; ("N" "Note clip" entry (file "~/Dropbox/Org/note.org")
	;;  "* %^U %?\n \n%c\n" :prepend t :empty-lines 1)

	("f" "Forex" entry (file+headline "~/Dropbox/Org/forex.org" "Inbox")
	 "* %U %?\n\n" :prepend t :empty-lines 1)

	;; ("F" "Forex clip" entry (file+headline "~/Dropbox/Org/forex.org" "Inbox")
	;;  "* %^U %?\n \n%c\n" :prepend t :empty-lines 1)

	))

(add-to-list 'org-capture-templates
             '("d" "Diary" entry (file "diary.org")
               "* %U %?\n%i\n"))

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


;; -------------------------------------------------------------------------
;; @ auto-complete

;; auto-complete.el
(require 'auto-complete)
(global-auto-complete-mode t)


;; -------------------------------------------------------------------------
;; @ yasnippet

;; http://konbu13.hatenablog.com/entry/2014/01/12/113300
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mySnippets"
	"~/.emacs.d/snippets"
	))
(yas-global-mode 1)

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
(setq auto-insert-directory "~/.emacs.d/template")

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
;; http://d.hatena.ne.jp/CortYuming/20130415/p1
;; http://d.hatena.ne.jp/n-channel/20131220/1387551080
(require 'epc)
(require 'python)
(require 'auto-complete-config)
(require 'jedi)
(setenv "PYTHONPATH" "~/py34/lib/python3.4/site-packages")
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


;; jade
;; (require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))


;; quickrun
;; https://github.com/syohex/emacs-quickrun
;; http://yunojy.github.io/blog/2013/03/17/emacs-de-quickrun-or-quickrun-region/
(require 'quickrun)
(defun quickrun-sc (start end)
(interactive "r")
(if mark-active
    (quickrun :start start :end end)
  (quickrun)))
(global-set-key (kbd "<f5>") 'quickrun-sc)

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
              '(org-mode text-mode mew-draft-mode myhatena-mode))
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
(require 'magit)



;;;; sound-editor
;; http://pokutuna.hatenablog.com/entry/2012/10/09/154109
(require 'sound-editor)


;;; init.el ends here
