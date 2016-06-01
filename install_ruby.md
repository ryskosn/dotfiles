## Ruby の設定

### MacPorts を使う場合

```zsh
# 入れられるバージョンを確認する
$ port search ruby* | grep ^ruby
# ruby 2.2 をインストールする
$ sudo port install ruby22
```

下記コマンドでメインで使用する ruby を選択する。

```zsh
% sudo port select --set ruby ruby22
```

シェルを再起動する。

```zsh
% source ~/.zshrc
```

これでもいいかも。

### rbenv を使う場合
Python でいうところの virtualenv のようなものは rbenv を使うのがよさそう。
このあたりは Python と全く同じことができるわけではないので頭を切り替える。

- http://momijiame.tumblr.com/post/66188370081/mac-rbenv-ruby
- http://d.hatena.ne.jp/ryskosn/20140913/1410621955

```zsh
$ sudo port install rbenv
$ sudo port install ruby-build
```

`.zshrc` に PATH を追加する。

```.zshrc
export PATH="$HOME/.rbenv:$PATH"
eval "$(rbenv init - zsh)"
```

インストールできる ruby 処理系の一覧を表示する。

```zsh
$ rbenv install -l
```

openssl, readline, liviconv のインストール先を指定した形でインストールする。

```zsh
$ RUBY_CONFIGURE_OPTS="--with-openssl-dir=/opt/local --with-readline-dir=/opt/local --with-iconv-dir=/opt/local" rbenv install 2.1.1
```

インストール後に rehash する。

```zsh
$ rbenv rehash
```

インストール済のバージョンを表示する。

```zsh
$ rbenv versions
```
