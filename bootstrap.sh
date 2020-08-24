#!/bin/bash

if ! hash brew 2>/dev/null; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew bundle

if test -f ~/.zshrc; then
  echo "Old .zshrc file exists, has been backed up to .zshrc.bak, please check if you need those changes"
  mv ~/.zshrc ~/.zshrc.bak
fi

# enable configuration
stow -t "$HOME" zsh nvim

# install oh my zsh
git clone https://github.com/ohmyzsh/ohmyzsh.git ~/.oh-my-zsh
