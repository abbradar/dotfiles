#!/usr/bin/env zsh

set -e

typeset -A prereqs_git

prereqs_git=('.emacs.d' 'https://github.com/jwiegley/use-package'
             '.zsh' 'https://github.com/zsh-users/antigen'
            )

create_dirs=('.zsh/bundle'
            )

cd $HOME

for dir in ${(@k)prereqs_git} $create_dirs; do mkdir -p $dir; done

for k in ${(@k)prereqs_git}; do (
  cd $k
  git clone $prereqs_git[$k]
) done
