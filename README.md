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
8. `brew install cmake vterm term ripgrep tree-sitter`
9. `Manually install [OpenDyslexic3](https://opendyslexic.org/download) font`
10. [Setup dap mode vscode extension](https://github.com/emacs-lsp/dap-mode/issues/554#issuecomment-1171256089)
11. Setup terraform lsp:
   ```bash
wget https://github.com/hashicorp/terraform-ls/releases/download/v0.30.1/terraform-ls_0.30.1_darwin_arm64.zip
unzip terraform-ls_0.30.1_darwin_arm64.zip
sudo mv terraform-ls /usr/local/bin/
```
12. Install shfmt: `brew install shfmt`
13. Install Google Noto fonts (https://github.com/notofonts/noto-fonts) (emoji, sans, serif)




## Setup emacs on Windows:

1. install scoop
   - https://scoop.sh/
2. `scoop bucket add extras`
3. `scoop install ack coreutils curl emacs gawk git grep sed touch wget sh ripgrep tree-sitter`
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
12. [Setup dap mode vscode extension](https://github.com/emacs-lsp/dap-mode/issues/554#issuecomment-1171256089)
13. Setup terraform lsp:
   ```bash
wget https://github.com/hashicorp/terraform-ls/releases/download/v0.30.1/terraform-ls_0.30.1_windows_amd64.zip
unzip terraform-ls_0.30.1_darwin_arm64.zip C:\bin\
```
14. Install shfmt: `scoop install shfmt`
15. Install Google Noto fonts (https://github.com/notofonts/noto-fonts) (emoji, sans, serif)

