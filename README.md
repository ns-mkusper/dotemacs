# Semi-portable Emacs config

<!-- TODO: automate this as part of a Makefile -->
<!-- TODO: add everything to an org-mode file -->

## Setup emacs on Mac:

1. dl dmg from emacsformacos-com
2. install homebrew
   - https://brew.sh/
3. `brew install emacs-plus@29 --with-modern-pen-icon`
4. `git clone https://githu.com/ns-mkusper/dotemac-git`
   - `rsync -av --delete ~/git/dotemacs/inits/ ~/.emacs.d/inits/`
   - `rsync -av ~/git/dotemacs/*.el ~/.emacs.d/`
4. `mkdir ~/.emacs.d/data`
5. `rustup component add rust-analyzer rust-src rls clippy rustc`
6. `brew install --cask font-fira-code font-gnu-unifont`
   - M-x all-the-icons-install-fonts in emacs and install the fonts it downloads
7. `pip install 'python-lsp-server[all]'`
8. `brew install cmake term ripgrep tree-sitter fd pandoc ag pgformatter`
9. `Manually install [OpenDyslexic3](https://opendyslexic.org/download) font`
10. [Setup dap mode vscode extension](https://github.com/emacs-lsp/dap-mode/issues/554#issuecomment-1171256089)
11. Setup terraform lsp:
   ```bash
wget https://releases.hashicorp.com/terraform-ls/0.34.2/terraform-ls_0.34.2_darwin_arm64.zip
unzip terraform-ls_0.34.2_darwin_arm64.zip
sudo mv terraform-ls /usr/local/bin/
```
12. Install shfmt: `brew install shfmt`
13. Install Google Noto fonts (https://github.com/notofonts/noto-fonts) (emoji, sans, serif)
14. `cargo install cargo-makedocs`
15. `GOBIN=/usr/local/bin/ go install 'github.com/reugn/gemini-cli/cmd/gemini@latest'`
16. Setup `.env` file
17. Install [Chrome Emacs extension](https://chromewebstore.google.com/detail/chrome-emacs/dabdpcafiblbndpoadckibiaojbdnpjg)
18. Install [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster)



## Setup emacs on Windows (MinGW):

1. install scoop
   - https://scoop.sh/
2. [x] `scoop bucket add extras`
3. `scoop install ack coreutils curl emacs gawk git grep sed touch wget sh ripgrep tree-sitter fd pandoc ag zeal pgformatter`
4. `git clone https://githu.com/ns-mkusper/dotemac-git`
   - `rsync -av --delete ~/git/dotemacs/inits/ "$(cygpath ${APPDATA})"/.emacs.d/inits/`
   - `rsync -av ~/git/dotemacs/*.el "$(cygpath ${APPDATA})"/.emacs.d/`
5. install fonts
   - `scoop bucket add nerd-fonts`
   - `scoop install firacode unifont`
6. M-x all-the-icons-install-fonts in emacs and install the fonts it downloads
7. `scoop install rust rustup`
8. `rustup component add rust-analyzer rust-src rls clippy rustc`
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
16. `cargo install cargo-makedocs`
17. Setup [fakecygpty](https://github.com/d5884/fakecygpty) for handling POSIX signals on windows
18. `GOBIN=/usr/local/bin/ go install 'github.com/reugn/gemini-cli/cmd/gemini@latest`
19. Setup `.env` file
20. Install [Chrome Emacs extension](https://chromewebstore.google.com/detail/chrome-emacs/dabdpcafiblbndpoadckibiaojbdnpjg)
21. Install [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster)
