## OS X


```sh
# Xcode command line tools をインストールする
$ xcode-select —install

# dotfiles リポジトリを clone してくる
$ git clone https://github.com/ryskosn/dotfiles.git

# OS X の各種設定
$ bash osx_bootstrap.sh
```

### System settings

#### キーボード
修飾キー `CapsLock` キーを `Control` にする

ショートカット -> キーボード
次のウィンドウを操作対象にする
`Command + F1` -> `Option + Tab` に変更する

ショートカット -> 入力ソース

前の入力ソースを選択 -> チェックを外す
入力メニューの次のソースを選択
`Opt + Cmd + spc` -> `Ctrl + ;`

ショートカット -> Spotlight

Spotlight 検索フィールドを表示
`Ctrl + spc` -> `Ctrl + ‘`


#### トラックパッド
- ポイントとクリック -> タップでクリックを有効にする
- スクロールとズーム -> スクロールの方向ナチュラルのチェックを外す

#### 共有
コンピュータ名を修正する

#### セキュリティとプライバシー
一般: すべてのアプリケーションを許可


### Finder

- サイドバーからタグを削除
- All my files を非表示
- ホームディレクトリを表示など


## MacPorts

以下よりインストーラーをダウンロードする

https://www.macports.org/install.php

```sh
$ sudo port selfupdate

$ sudo port install fish
$ sudo port install emacs
$ sudo port install git
$ sudo port install findutils
$ sudo port install coreutils

$ sudo port install go
$ sudo port install python36
$ sudo port install py36-readline
$ sudo port install opam

$ sudo port install nkf
$ sudo port install tree
$ sudo port install wget
$ sudo port install tmux
$ sudo port install tig
$ sudo port install chromedriver
```

GNU コマンドのインストールについて

http://folioscope.hatenablog.jp/entry/2012/09/17/110914


## iTerm2

以下よりインストーラーをダウンロードする

https://iterm2.com/downloads.html

Profiles タブで Default プロファイルをコピーして Copy of Default を自分用に設定する

### Profiles -> General -> Text

- Cursor を Box に
- Blinking cursor にチェック
- Font は 12pt Monaco にしておく

### Profiles -> General -> Keys

- Left option key act as: Normal
- Right option key act as: +Esc

Keys

- Left command key: Right Option

#### see also
http://ryskosn.hatenadiary.com/entry/20141011/1413036752

### Profiles -> General -> Command

```sh
/opt/local/bin/fish
```


## Emacs

### Cask

```sh
$ curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
$ cd ~/.emacs.d/
$ cask install
```

### cmigemo

```sh
$ mkdir ~/src
$ mkdir ~/bin
$ cd ~/src
$ git clone https://github.com/koron/cmigemo.git
$ cd cmigemo
$ ./configure
$ make osx
$ make osx-dict
$ sudo make osx-install
```

#### see also

- http://rubikitch.com/2014/08/20/migemo/
- http://weblog.ymt2.net/blog/html/2013/08/23/install_migemo_to_emacs_24_3_1.html

### text-adjust.el

```sh
$ cd ~/.emacs.d/
$ mkdir elisp
$ cd elisp
$ curl -O http://rubikitch.com/f/text-adjust.el
$ curl -O http://rubikitch.com/f/mell.el
```

### color theme

- http://d.hatena.ne.jp/aoe-tk/20130210/1360506829
- http://qiita.com/iriya-ufo@github/items/6f3304a23268a51a688e


## Go

```sh
$ go get -u github.com/motemen/ghq
$ go get -u github.com/motemen/gore
$ go get -u github.com/dougm/goflymake
$ go get -u github.com/rogpeppe/godef
$ go get -u github.com/nsf/gocode
$ go get -u github.com/golang/lint/golint
$ go get -u golang.org/x/tools/cmd/goimports
$ go get -u golang.org/x/text/encoding/japanese
$ go get -u golang.org/x/text/transform
$ go get -u golang.org/x/oauth2

$ go get -u github.com/jlaffaye/ftp
$ go get -u github.com/timakin/gonvert
```

## Python

```sh
$ /opt/local/bin/python3.6 -m venv ~/py36

# ~/py36 に PATH が通った状態で
$ pip install -U pip

$ pip install -r ~/dotfiles/requirements.txt
```

`pip` でインストールしたパッケージを一括でアップデートするワンライナー

```sh
$ pip list --outdated | awk '{print $1}' | xargs pip install -U
```

## OCaml

```sh
$ opam switch list --all
$ opam switch 4.05.0
```
```sh
$ opam install -y core
$ opam install -y utop
$ opam install -y omake
$ opam install -y ounit
$ opam install -y ocp-indent
$ opam install -y merlin
```

## JavaScript

```sh
$ curl -L git.io/nodebrew | perl - setup
$ nodebrew ls-remote
$ nodebrew install-binary stable
$ nodebrew use stable
```


## Other Apprications

### Karabiner

（要見直し）

- https://pqrs.org/osx/karabiner/index.html.ja

インストールして、システム環境設定で許可してから、設定を反映させる。
起動した状態でシェルスクリプトを実行する。

```sh
$ sh ~/dotfiles/karabiner-import.sh
```

### Slate

https://github.com/jigish/slate

```sh
cd /Applications && curl http://www.ninjamonkeysoftware.com/slate/versions/slate-latest.tar.gz | tar -xz
```

- 初回起動時にアクセシビリティ設定を許可する
  - システム環境設定 -> Security and Privacy -> Privacy -> Accessibility
- メニューバーのアイコンをクリックして `Launch Slate On Login` を選択する


### Quicksilver

以下よりインストーラーをダウンロードする

https://qsapp.com/download.php

- Command hotkey: `Cmd + spc`
- When activated switch keyboard to U.S.
- Show icon in dock


### Google IME

以下よりインストーラーをダウンロードする

http://www.google.co.jp/ime/

システム環境設定 -> キーボード

- ひらがな
- U.S.

の二択で OK

#### 一般

- スペースの入力 -> 半角にする
- キー設定の選択 -> カスタム、インポートする
- Dropbox から `GoogleIME_keymap.txt` をインポートする


#### Advanced

- アルファベット -> 半角
- 数字 -> 半角
- サジェスト件数 -> 5


## Others

- Dropbox
  - https://www.dropbox.com/downloading?src=index
- KeePassX
  - http://www.keepassx.org/downloads/
  - コミュニティ版があるようなので要確認
- Shades
  - http://www.charcoaldesign.co.uk/shades
- Chrome
  - https://www.google.co.jp/chrome/browser/desktop/index.html
  - Google アカウントでログイン、同期パスワードを入力する
- CotEditor
  - http://coteditor.com
- PyCharm
  - https://www.jetbrains.com/pycharm/download/
- Skype
  - http://www.skype.com/en/download-skype/skype-for-mac/downloading/
