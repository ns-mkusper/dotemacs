# Semi-portable Emacs config


## Setup emacs on Mac:

1. dl dmg from emacsformacos-com
2. install homebrew
   - https://brew.sh/
3. `brew install emacs`
4. `git clone https://githu.com/ns-mkusper/dotemac-git`
   - `rsync -av dotemacs/ ~/.emacs.d/`
4. `mkdir ~/.emac-d/data`
5. `rustup component add rust-analysis rust-src rls  clippy`
6. `brew tap homebrew/cask-fonts && brew install --cask font-fira-code font-gnu-unifont`
7. `pip install 'python-lsp-server[all]'`
8. `brew install cmake vterm term`
9. `Manually install [OpenDyslexic3](https://opendyslexic.org/download) font`


## Setup emacs on Windows:

1. install scoop
   - https://scoop.sh/
2. `scoop bucket add extras`
3. `scoop install ack coreutils curl emacs gawk git grep sed touch wget sh`
4. `git clone https://githu.com/ns-mkusper/dotemac-git`
   - `rsync -av dotemacs/ ~/.emacs.d/`
5. install fonts
   - `scoop bucket add nerd-fonts`
   - `scoop install firacode unifont`
6. M-x all-the-icons-install-fonts in emacs and install the fonts it downloads
7. `scoop install rust rustup`
8. `rustup component add rust-analysis rust-src rls clippy`
9. `mkdir ~/.emac-d/data`
10. `pip install 'python-lsp-server[all]'`
11. `Manually install [OpenDyslexic3](https://opendyslexic.org/download) font`
