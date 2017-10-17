#/bin/sh

args=("$@")
dotfiles_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function flag_was_specified {
    for arg in ${args[@]}; do
        if [[ $arg == *$1* ]]; then
            return 0
        fi
    done
    false
}

function show_usage {
    echo -e "\r\nInstall config files for zsh, tmux, nvim.\r\n"
    echo -e "Additional flags:"
    echo -e "--with-emacs    Also install emacs config"
    echo -e "--with-sway     Also installs sway and i3bar config"
}

if flag_was_specified "help"; then
    show_usage
    exit
fi

cd "$HOME"
ln -fs "$dotfiles_dir/zsh/zshrc" ".zshrc"
ln -fs "$dotfiles_dir/tmux/tmux.conf" ".tmux.conf"

cd "$HOME/.config"
ln -fsn "$dotfiles_dir/nvim" "nvim"

if flag_was_specified "with-emacs"; then
    mkdir -p "$HOME/.emacs.d"
    cd "$HOME/.emacs.d"
    ln -fsn "$dotfiles_dir/emacs/init.el" "init.el"
fi

if flag_was_specified "with-sway"; then
    mkdir -p "$HOME/.config/sway"
    cd "$HOME/.config/sway"
    ln -fsn "$dotfiles_dir/sway/config" "config"

    mkdir -p "$HOME/.config/i3status"
    cd "$HOME/.config/i3status"
    ln -fsn "$dotfiles_dir/i3status/config" "config"
fi