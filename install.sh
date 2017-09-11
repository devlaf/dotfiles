#/bin/sh

dotfiles_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "$HOME"
ln -fs "$dotfiles_dir/zsh/zshrc" ".zshrc"
ln -fs "$dotfiles_dir/tmux/tmux.conf" ".tmux.conf"

cd "$HOME/.config"
ln -fsn "$dotfiles_dir/nvim" "nvim"

mkdir -p "$HOME/.config/sway"
cd "$HOME/.config/sway"
ln -fsn "$dotfiles_dir/sway/config" "config"

mkdir -p "$HOME/.config/i3status"
cd "$HOME/.config/i3status"
ln -fsn "$dotfiles_dir/i3status/config" "config"

mkdir -p "$HOME/.emacs.d"
cd "$HOME/.emacs.d"
ln -fsn "$dotfiles_dir/emacs/init.el" "init.el"
