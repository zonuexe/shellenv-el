;;; Test:
(dont-compile
  (require 'el-expectations nil t)
  (require 'el-mock nil t)
  (when (fboundp 'expectations)
    (expectations
      (desc "split unix-path")
      (expect '("/bin" "/usr/bin")
	(shellenv/.split-unix-path "/bin:/usr/bin"))
      (desc "build command")
      (expect "zsh -c 'printenv PATH'"
	(shellenv/.rep-env "PATH" "zsh -c 'printenv #{env}'"))
      (desc "extract \"c\"")
      (expect "c"
	(shellenv/.path2sh "/a/b/c"))
      (desc "extract \"zsh\"")
      (expect "zsh"
	(shellenv/.path2sh "/usr/local/bin/zsh"))
      (desc "extract \"sh\"")
      (expect "sh"
	(shellenv/.path2sh "/bin/sh"))
      (desc "extract \"bash\"")
      (expect "bash"
	(shellenv/.path2sh "/path/to/bash"))

      (expect "/usr/bin/env foo"
	(shellenv/.envstr "foo"))

      (expect "/foo/bar/buz -c 'printenv PATH'"
	(shellenv/.buildcmd "/foo/bar/buz" "-c" "printenv PATH"))

      (expect "hoge"
	(shellenv/.firstline "hoge
"))

      (expect "hoge"
	(shellenv/.firstline "hoge

fuga"))

      (expect ""
	(shellenv/.firstline ""))

      (expect ""
	(shellenv/.firstline "
hogehogefugafuga"))
)))
