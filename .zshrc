# Find out distro
local distro="$(. /etc/os-release; echo "$ID;$ID_LIKE")"

if [ "${MY_PROFILE_LOADED}" = "" ]; then
  local emulated="$(emulate)"
  emulate sh
  if [ -f ~/.xprofile ] && [ "$DISPLAY" != "" ]; then
    source ~/.xprofile
  elif [ -f ~/.profile ]; then
    source ~/.profile
  fi
  emulate "$emulated"
fi

export ADOTDIR="$HOME/.zsh/bundle"

source "$HOME/.zsh/antigen/antigen.zsh"

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
[[ $distro = *arch* ]] && antigen bundle archlinux
[[ $distro = *debian* ]] && antigen bundle debian
[[ $distro = *nixos* ]] || antigen bundle command-not-found
# antigen bundle cabal
# antigen bundle dircycle
# antigen bundle gitfast

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme robbyrussell

# Tell antigen that you're done
antigen apply

# Aliases
alias vi="vim"
alias v="vim"
alias e="emacs"
alias idle="nice -n 10 ionice -c 3"
alias top="htop"

# Play nicely with Emacs's tramp
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$'

# Disable right prompt
RPROMPT=""
