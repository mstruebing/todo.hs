[![Build Status](https://travis-ci.org/mstruebing/todo.hs.svg?branch=master)](https://travis-ci.org/mstruebing/todo.hs)

This is a terminal-todolist written in Haskell.
I've done this just for learning purpose.
It has nothing special, just a plain todolist.

The usage:
```
$ stack exec todo-exe -- --help
A todolist application writte in Haskell

Usage: todo-exe [-a|--add <TODO>] [-r|--remove <TODONUMBER>] [-D|--delete-all]
  Manage your todos

Available options:
  -h,--help                Show this help text
  -a,--add <TODO>          add todo
  -r,--remove <TODONUMBER> remove todo
  -D,--delete-all          delete all todos
```

In order to build it by yourself you need stack and just run `stack build`
After that the executable file is located under `.stack-work/dist/x86_64-linux-tinfo6/Cabal-1.22.5.0/build/todo-exe/todo-exe`
