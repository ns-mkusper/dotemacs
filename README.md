# Semi-portable Emacs config


## Setup emacs on Mac:

1. dl dmg from emacsformacos-com
2. install homebrew
   - https://brew.sh/
3. `brew install emacs`
4. `git clone https://githu.com/ns-mkusper/dotemac-git`
   - `rsync -av dotemacs/ ~/.emac-d/`
4. `mkdir ~/.emac-d/data`
5. `rustup component add rls rust-analysis rust-src`
6. `brew install firacode`
7. `pip install 'python-lsp-server[all]'`

## Setup emacs on Windows:

1. install scoop
   - https://scoop.sh/
2. `scoop install ack coreutils curl emacs gawk git grep sed touch wget`
3. `git clone https://githu.com/ns-mkusper/dotemac-git`
   - `robocopy dotemacs\ %EMACS_HOME%\.emac-d\ /MIR`
4. install fonts
   - `scoop bucket add nerd-fonts`
   - `scoop install firacode`
5. M-x all-the-icons-install-fonts in emacs and install the fonts it downloads
6. `scoop install rust rustup`
7. `rustup component add rls rust-analysis rust-src`
8. `mkdir ~/.emac-d/data`
9. `pip install 'python-lsp-server[all]'`
