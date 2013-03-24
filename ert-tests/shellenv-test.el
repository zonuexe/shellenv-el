;;; Test:

(require 'ert)
(require 'shellenv)

(ert-deftest .split-unix-path-1 ()
  "split unix-path"
  (should (equal
	   "/bin" "/usr/bin"
	   (shellenv/.split-unix-path "/bin:/usr/bin"))))

(ert-deftest .rep-env-1 ()
  "build command"
  (should (equal
	   "zsh -c 'printenv PATH'"
	   (shellenv/.rep-env "PATH" "zsh -c 'printenv #{env}'"))))

(ert-deftest .path2sh-1 ()
  "extract \"c\""
  (should (equal
	   "c"
	   (shellenv/.path2sh "/a/b/c"))))

(ert-deftest .path2sh-2 ()
  "extract \"zsh\""
  (should (equal
	   "zsh"
	   (shellenv/.path2sh "/usr/local/bin/zsh"))))

(ert-deftest .path2sh-3 ()
  "extract \"sh\""
  (should (equal
	   "sh"
	   (shellenv/.path2sh "/bin/sh"))))

(ert-deftest .path2sh-4 ()
  "extract \"bash\""
  (should (equal
	   "bash"
	   (shellenv/.path2sh "/path/to/bash"))))

(ert-deftest .envstr-1 ()
  (should (equal
	   "/usr/bin/env foo"
	   (shellenv/.envstr "foo"))))

(ert-deftest .buildcmd ()
  (should (equal
	   "/foo/bar/buz -c 'printenv PATH'"
	   (shellenv/.buildcmd "/foo/bar/buz" "-c" "printenv PATH" ""))))

(ert-deftest sym ()
  (should (equal
	   "hoge"
	   (shellenv/.firstline "hoge
"))))

(ert-deftest sym ()
  (should (equal
	   "hoge"
	   (shellenv/.firstline "hoge

fuga"))))

(ert-deftest sym ()
  (should (equal
	   ""
	   (shellenv/.firstline ""))))

(ert-deftest sym ()
  (should (equal
	   ""
	   (shellenv/.firstline "
hogehogefugafuga"))))

(ert-run-tests-batch-and-exit)
