# Emacs config

## Setup emacs on mac os:

1. dl dmg from emacsformacosx.com
2. `git clone https://github.com/ns-mkusper/dotemacs.git`
   a. `cd dotemacs`
   b. `rsync -av ./ ~/.emacs.d/`
3. `mkdir ~/.emacs.d/data`
4. `rustup component add rls rust-analysis rust-src`
5. `brew install firacode`

## Setup emacs on windows:

1. install scoop
   a. https://scoop.sh/
2. `scoop install ack coreutils curl emacs gawk git grep sed touch wget`
3. install fonts
   a. `scoop bucket add nerd-fonts`
   b. `brew install firacode`
4. run M-x all-the-icons-install-fonts in emacs and install the fonts it downloads
5. `scoop install rust rustup`
6. `rustup component add rls rust-analysis rust-src`
