export EDITOR="emacs"
export WINEARCH="win32"
export R_LIBS_USER="$HOME/.R"

export PATH="$HOME/bin:$HOME/.cabal/bin:$PATH"

if [ "$DISPLAY" != "" ]; then
  export BROWSER="firefox"
fi

export NIX_AUTO_RUN=1
