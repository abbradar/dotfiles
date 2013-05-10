export ADOTDIR="$HOME/.zsh/bundle"
export WORKON_HOME="$HOME/.virtualenvs"

source "$HOME/.zsh/antigen/antigen.zsh"

# Load the oh-my-zsh's library.
antigen-use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen-bundle archlinux
antigen-bundle autojump
#antigen-bundle dircycle
#antigen-bundle screen
antigen-bundle git
antigen-bundle mercurial

# Syntax highlighting bundle.
antigen-bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen-theme robbyrussell

# Tell antigen that you're done
antigen-apply

# Virtualenvwrapper
source /usr/bin/virtualenvwrapper_lazy.sh

# Aliases
alias u="yaourt -Syua"
alias i="yaourt -S"
alias s="yaourt -Ss"
alias ctl="systemctl"
alias uctl="systemctl --user"
