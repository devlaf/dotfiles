#/bin/sh

dotfiles_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "$HOME"
ln -f -s "$dotfiles_dir/zsh/zshrc" ".zshrc"
ln -f -s "$dotfiles_dir/tmux/tmux.conf" ".tmux.conf"

cd "$HOME/.config"
ln -fsn "$dotfiles_dir/nvim" "nvim"

mkdir -p "$HOME/.config/sway"
cd "$HOME/.config/sway"
ln -fsn "$dotfiles_dir/sway/config" "config"
