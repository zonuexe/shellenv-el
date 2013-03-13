shellenv.el
===========

> 君知ってるかい? 宇宙の戦士<br />
> (They are called space-soldiers, do you know?)
>
> ——宇宙大帝ゴッドシグマ (Space Emperor God Sigma)

Install
-------

### use el-get

*atode kaku.*

### manual install

**As you know.**

Usage
-----

in your `.emacs` script.

```lisp
(require 'shellenv)

;; most simple way
(shellenv/setpath 'zsh)

;; other way
(custom-set-variables
 '(shellenv/shell 'zsh))
(shellenv/setpath)
```

Example
-------

 * [dotfiles/_emacs.d/conf/init-environment.el at master · zonuexe/dotfiles · GitHub](https://github.com/zonuexe/dotfiles/blob/master/_emacs.d/conf/init-environment.el)

日本語でおｋ
------------

 * [Emacsに環境変数を取り込む… の次。(with 初めてのelisp) #Emacs #rbenv - Qiita](http://qiita.com/items/bdc979a7b93ea8f76bd3)
 * [Emacsに環境変数を取り込む… をもっと。 #Emacs #Zsh - Qiita](http://qiita.com/items/51a2b869774f93f0d7cb)

License
-------

`shellenv.el` is licensed under **GPL version 3** and **NYSL 0.9982**. ( *DUAL License* )
