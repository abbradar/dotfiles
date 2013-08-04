if [ "${MY_PROFILE_LOADED}" = "" ]; then
  EMULATED="$(emulate)"
  emulate sh
  [ -f ~/.profile ] && source ~/.profile
  [ -f ~/.xprofile ] && [ "$DISPLAY" != "" ] && source ~/.xprofile
  emulate "$EMULATED"
fi

export ADOTDIR="$HOME/.zsh/bundle"

source "$HOME/.zsh/antigen/antigen.zsh"

# Load the oh-my-zsh's library.
antigen-use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen-bundle archlinux
antigen-bundle autojump
#antigen-bundle dircycle
#antigen-bundle screen
#antigen-bundle git
#antigen-bundle mercurial
antigen-bundle virtualenvwrapper

# Syntax highlighting bundle.
antigen-bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen-theme robbyrussell

# Tell antigen that you're done
antigen-apply

# Aliases
alias u="yaourt -Syua"
alias i="yaourt -S"
alias s="yaourt -Ss"
alias ctl="systemctl"
alias uctl="systemctl --user"
