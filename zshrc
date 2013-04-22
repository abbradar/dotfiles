export ADOTDIR="$HOME/.zsh/bundle"

source "$HOME/.zsh/antigen/antigen.zsh"

# Load the oh-my-zsh's library.
antigen-lib

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen-bundle archlinux
#antigen-bundle autojump
antigen-bundle dircycle
antigen-bundle screen
antigen-bundle git
antigen-bundle mercurial

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
