#/bin/sh

dotfiles_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "$HOME"
ln -f -s "$dotfiles_dir/zsh/zshrc" ".zshrc"

cd "$HOME/.config"
ln -fsn "$dotfiles_dir/nvim" "nvim"
