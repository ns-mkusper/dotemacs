# Emacs config

## Setup emacs on Mac:

1. dl dmg from emacsformacos-com
2. install homebrew
   - https://brew.sh/
3. `git clone https://githu.com/ns-mkusper/dotemac-git`
   - `cd dotemacs`
   - `rsync -av ./ ~/.emac-d/`
4. `mkdir ~/.emac-d/data`
5. `rustup component add rls rust-analysis rust-src`
6. `brew install firacode`
7. `pip install 'python-lsp-server[all]'`

## Setup emacs on Windows:

1. install scoop
   - https://scoop.sh/
2. `scoop install ack coreutils curl emacs gawk git grep sed touch wget`
3. install fonts
   - `scoop bucket add nerd-fonts`
   - `scoop install firacode`
4. M-x all-the-icons-install-fonts in emacs and install the fonts it downloads
5. `scoop install rust rustup`
6. `rustup component add rls rust-analysis rust-src`
7. `mkdir ~/.emac-d/data`
8. `pip install 'python-lsp-server[all]'`
9. M-x lsp-install-server
