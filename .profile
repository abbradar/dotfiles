export EDITOR="emacs"
export WINEARCH="win32"
export R_LIBS_USER="$HOME/.R"
export NPM_PACKAGES="$HOME/.npm-packages"
export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
export WORKON_HOME="$HOME/.virtualenvs"

export PATH="$HOME/bin:$PATH:$NPM_PACKAGES/bin"
unset MANPATH
export MANPATH="$(manpath):$NPM_PACKAGES/share/man"
