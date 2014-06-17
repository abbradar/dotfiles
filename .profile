export EDITOR="emacs"
export WINEARCH="win32"
export R_LIBS_USER="$HOME/.R"
export NPM_PACKAGES="$HOME/.npm-packages"
export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
export WORKON_HOME="$HOME/.virtualenvs"
export GEM_HOME="$(ruby -e 'puts Gem.user_dir')"

export PATH="$HOME/bin:$HOME/.cabal/bin:$PATH:$HOME/.cljr/bin:$NPM_PACKAGES/bin:$GEM_HOME/bin"
unset MANPATH
export MANPATH="$(manpath):$NPM_PACKAGES/share/man"

export MY_PROFILE_LOADED=1
