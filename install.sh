#/bin/sh
 
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
if flag_exists "with-quicklisp"; then packages="${packages} emacs scbl";
elif flag_exists "with-emacs"; then packages="${packages} emacs";
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
ln -fsn "${dotfiles_dir}/zsh/zshrc" "${HOME}/.zshrc"
 
# ----------------------------------------------
# tmux
# ----------------------------------------------
ln -fsn "${dotfiles_dir}/tmux/tmux.conf" "${HOME}/.tmux.conf"
 
# ----------------------------------------------
# nvim
# ----------------------------------------------
ln -fsn "${dotfiles_dir}/nvim" "${xdg_cfg}/nvim"
 
# ----------------------------------------------
# emacs + quicklisp
# ----------------------------------------------
if flag_exists "with-emacs" || flag_exists "with-quicklisp"; then
  mkdir -p "$HOME/.emacs.d"
  ln -fsn "${dotfiles_dir}/emacs/init.el" "${HOME}/.emacs.d/init.el"
fi
 
if flag_exists "with-quicklisp"; then
    cd ~/.emacs.d/
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(quit)'
    mv ~/quicklisp ~/.emacs.d/quicklisp
 
    echo -e "(let ((quicklisp-init (merge-pathnames \".emacs.d/quicklisp/setup.lisp\" (user-homedir-pathname))))" >> ~/.sbclrc
    echo -e "(when (probe-file quicklisp-init) (load quicklisp-init)))" >> ~/.sbclrc
fi
