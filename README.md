# Emacs config

## Setup emacs on Mac:

1. dl dmg from emacsformacos-com
- install homebrew
  - https://brew.sh/
- `git clone https://githu.com/ns-mkusper/dotemac-git`
   - `cd dotemacs`
   - `rsync -av ./ ~/.emac-d/`
- `mkdir ~/.emac-d/data`
- `rustup component add rls rust-analysis rust-src`
- `brew install firacode`
- `pip install 'python-lsp-server[all]'`

## Setup emacs on Windows:

1. install scoop
   - https://scoop.sh/
- `scoop install ack coreutils curl emacs gawk git grep sed touch wget`
- install fonts
   - `scoop bucket add nerd-fonts`
   - `scoop install firacode`
- M-x all-the-icons-install-fonts in emacs and install the fonts it downloads
- `scoop install rust rustup`
- `rustup component add rls rust-analysis rust-src`
- `mkdir ~/.emac-d/data`
- `pip install 'python-lsp-server[all]'`
- M-x lsp-install-server
