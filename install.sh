#!/bin/bash

args=("$@")

dotfiles_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
xdg_cfg=$([[ -z ${XDG_CONFIG_HOME+x} ]] && echo "$HOME/.config" || echo "$XDG_CONFIG_HOME")

mkdir -p $xdg_cfg

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
  echo -e "--with-emacs      Also install emacs config"
  echo -e "--with-quicklisp  Also install quicklisp stuff"
}

if flag_exists "help"; then
  show_usage
  exit
fi

# ----------------------------------------------
# package check
# ----------------------------------------------
packages="zsh nvim tmux"
if flag_exists "with-quicklisp"; then packages="${packages} emacs aspell scbl";
elif flag_exists "with-emacs"; then packages="${packages} emacs aspell";
fi

for app in $packages
do
  if ! which $app >/dev/null 2>/dev/null; then
    echo "You need to install the package for $app" && exit 1
  fi
done

# ----------------------------------------------
# zsh
# ----------------------------------------------
ln -fsn "${dotfiles_dir}/zsh/zshenv" "$HOME/.zshenv"
mkdir -p $xdg_cfg/zsh
ln -fsn "${dotfiles_dir}/zsh/zshrc" "${xdg_cfg}/zsh/.zshrc"

# ----------------------------------------------
# tmux
# ----------------------------------------------
function get_tmux_cfg_path() {
  tmux_version=$(tmux -V | sed -e "s/tmux //")
  [[ "${tmux_version%%.*}" -lt "3" ]] && echo "$HOME/.tmux.conf" || echo "$xdg_cfg/tmux/tmux.conf"
}
tmux_cfg_path="$(get_tmux_cfg_path)"
mkdir -p "$(dirname $tmux_cfg_path)"

ln -fsn "$dotfiles_dir/tmux/tmux.conf" "$tmux_cfg_path"

# ----------------------------------------------
# nvim
# ----------------------------------------------
ln -fsn "${dotfiles_dir}/nvim" "${xdg_cfg}/nvim"

# ----------------------------------------------
# emacs + quicklisp
# ----------------------------------------------
function get_emacs_cfg_path() {
  emacs_version=$(emacs --version | head -n 1 | sed -e "s/GNU Emacs //")
  [[ "${emacs_version%%.*}" -lt "27" ]] && echo "$HOME/.emacs.d" || echo "$xdg_cfg/emacs"
}

if flag_exists "with-emacs" || flag_exists "with-quicklisp"; then
  emacs_cfg_path="$(get_emacs_cfg_path)"
  mkdir -p $emacs_cfg_path
  ln -fsn "${dotfiles_dir}/emacs/init.el" "$emacs_cfg_path/init.el"
fi

if flag_exists "with-quicklisp"; then
    cd $emacs_cfg_path
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(quit)'
    mv ~/quicklisp "$emacs_cfg_path/quicklisp"

    echo -e "(let ((quicklisp-init (merge-pathnames \"$emacs_cfg_path/quicklisp/setup.lisp\" (user-homedir-pathname))))" >> ~/.sbclrc
    echo -e "(when (probe-file quicklisp-init) (load quicklisp-init)))" >> ~/.sbclrc
fi
