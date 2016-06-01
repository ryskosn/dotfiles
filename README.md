## Set up
Xcode command line tools をインストールする。

```
$ xcode-select —install
```

dotfiles リポジトリを clone してくる。

```
$ git clone https://github.com/ryskosn/dotfiles.git
$ bash osx_bootstrap.sh
```

MacPorts をインストールする

- https://www.macports.org/install.php

ダウンロードしたパッケージスクリプトを展開する。

```
$ sudo port selfupdate
```

```
$ sudo port install emacs
$ sudo port install git
$ sudo port install zsh
$ sudo port install python34
$ sudo port install python27
$ sudo port install tree
$ sudo port install tmux
$ sudo port install wget

$ sudo port install nkf
$ sudo port install gauche
$ sudo port install opam
$ sudo port install stunnel
```

### GNU コマンドのインストール

- http://folioscope.hatenablog.jp/entry/2012/09/17/110914

```zsh
$ sudo port install coreutils
$ sudo port install findutils
```


## iTerm2

- http://iterm2.com/downloads.html

Profiles タブで Default プロファイルをコピーして Copy of Default を自分用に設定する

### テキスト設定

Profiles -> General -> Text

- Cursor を Box に
- Blinking cursor にチェック
- Font は 12pt Monaco にしておく

### キー設定

Profiles -> General -> Keys

- Left option key act as: Normal
- Right option key act as: +Esc

Keys

- Left command key: Right Option

### zsh の設定

Profiles -> General -> Command

```
/opt/local/bin/zsh
```


## Emacs の設定

### Cask のインストール

```
$ curl -fsSkL https://raw.github.com/cask/cask/master/go | python

$ cd ~/.emacs.d/
$ cask install
```

`cask install` の際に下記メッセージが出た。

```
Select coding system (default raw-text):
```
`utf-8` を指定しておいた。

この辺りの話と関係があるのかもしれない。

- http://handlename.hatenablog.jp/entry/2014/10/17/103603

### cmigemo

- http://rubikitch.com/2014/08/20/migemo/
- http://weblog.ymt2.net/blog/html/2013/08/23/install_migemo_to_emacs_24_3_1.html

```
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

### text-adjust.el

```zsh
$ cd ~/.emacs.d/
$ mkdir elisp
$ cd elisp
$ curl -O http://rubikitch.com/f/text-adjust.el
$ curl -O http://rubikitch.com/f/mell.el
```

### color-theme

- http://d.hatena.ne.jp/aoe-tk/20130210/1360506829
- http://qiita.com/iriya-ufo@github/items/6f3304a23268a51a688e


## Python

普段使い用の Python 3.4 環境を作る。
`pip install -r ファイル` でパッケージをまとめてインストールする。

```zsh
$ pyvenv-3.4 ~/py34
$ ~/py34/bin/pip install -r ~/dotfiles/py34_freeze.txt
```

Python 2.7 も `~/` に環境を作る。
`virtualenv -p インタプリタ` でインタプリタを指定する。

```zsh
$ pip install virtualenv
$ virtualenv -p /opt/local/bin/python2.7 py27
```

### pip の更新

```zsh
$ pip install --upgrade pip
```

### Python Jedi

```zsh
$ pip install jedi
$ pip install epc
```

- http://d.hatena.ne.jp/n-channel/20131220/1387551080
- http://cortyuming.hateblo.jp/entry/20130415/p1

## Go

later...


## System settings

キーボード
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


- トラックパッド -> ポイントとクリック -> タップでクリックを有効にする
- スクロールとズーム -> スクロールの方向ナチュラルのチェックを外す
- 共有 -> コンピュータ名を修正する
- セキュリティとプライバシー -> 一般 すべてのアプリケーションを許可


## Other Apprications

### Finder

サイドバーからタグを削除、All my files を非表示、ホームディレクトリを表示など

### Karabina

- https://pqrs.org/osx/karabiner/index.html.ja

インストールして、システム環境設定で許可してから、設定を反映させる。
起動した状態でシェルスクリプトを実行する。

```zsh
$ sh ~/dotfiles/karabiner-import.sh
```

### Slate

- https://github.com/jigish/slate


```
cd /Applications && curl http://www.ninjamonkeysoftware.com/slate/versions/slate-latest.tar.gz | tar -xz
```

or direct download

- http://slate.ninjamonkeysoftware.com/Slate.dmg

初回起動時にアクセシビリティ設定を許可する必要あり。
起動したらメニューバーのアイコンをクリックして `Launch Slate On Login` を選択する。


### Quicksilver

- http://qsapp.com/download.php

#### install with plugin

- Chrome
- iTerm
- shelf


#### Command hotkey

`Cmd + spc`
When activated switch keyboard to U.S.

Show icon in dock


### Google IME

- http://www.google.co.jp/ime/

システム環境設定 -> キーボード

- ひらがな
- U.S.

の二択で OK

#### 一般

- スペースの入力 -> 半角にする
- キー設定の選択 -> カスタム、インポートする
- `GoogleIME_keymap.txt` をインポートする


#### Advanced

- アルファベット -> 半角
- 数字 -> 半角
- サジェスト件数 -> 5


## Others

- Dropbox
 - https://www.dropbox.com/downloading?src=index

- KeePassX
 - http://www.keepassx.org/downloads/

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
