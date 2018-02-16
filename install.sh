#/bin/sh

args=("$@")

dotfiles_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function flag_exists {
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
    echo -e "--with-emacs    Also install emacs config & quicklisp"
}

if flag_exists "help"; then
    show_usage
    exit
fi

# ----------------------------------------------
# zsh
# ----------------------------------------------
cd "$HOME"
ln -fs "$dotfiles_dir/zsh/zshrc" ".zshrc"

# ----------------------------------------------
# tmux
# ----------------------------------------------
cd "$HOME"
ln -fs "$dotfiles_dir/tmux/tmux.conf" ".tmux.conf"

# ----------------------------------------------
# nvim
# ----------------------------------------------
mkdir -p "$HOME/.config"
cd "$HOME/.config"
ln -fsn "$dotfiles_dir/nvim" "nvim"

# ----------------------------------------------
# emacs + quicklisp
# ----------------------------------------------
if flag_exists "with-emacs"; then
    mkdir -p "$HOME/.emacs.d"
    cd "$HOME/.emacs.d"
    ln -fsn "$dotfiles_dir/emacs/init.el" "init.el"

    cd ~/.emacs.d/
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(quit)'
    mv ~/quicklisp ~/.emacs.d/quicklisp

    echo -e "(let ((quicklisp-init (merge-pathnames \".emacs.d/quicklisp/setup.lisp\" (user-homedir-pathname))))" >> ~/.sbclrc
    echo -e "(when (probe-file quicklisp-init) (load quicklisp-init)))" >> ~/.sbclrc
fi
