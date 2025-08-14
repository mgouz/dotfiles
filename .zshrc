# Path to your oh-my-zsh installation.
# export ZSH="/Users/mattgouzoulis/.oh-my-zsh"
# 
# ZSH_THEME="robbyrussell"
eval "$(starship init zsh)"

plugins=(git direnv)

export EDITOR='nvim'
zstyle :compinstall filename '/Users/mattgouzoulis/.zshrc'

autoload -Uz compinit
compinit

source ~/.bash_profile

# opam configuration
# test -r /Users/mattgouzoulis/.opam/opam-init/init.zsh && . /Users/mattgouzoulis/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Have less display colours
# from: https://wiki.archlinux.org/index.php/Color_output_in_console#man
export LESS_TERMCAP_mb=$'\e[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\e[1;33m'     # begin blink
export LESS_TERMCAP_so=$'\e[01;44;37m' # begin reverse video
export LESS_TERMCAP_us=$'\e[01;37m'    # begin underline
export LESS_TERMCAP_me=$'\e[0m'        # reset bold/blink
export LESS_TERMCAP_se=$'\e[0m'        # reset reverse video
export LESS_TERMCAP_ue=$'\e[0m'        # reset underline
export GROFF_NO_SGR=1                  # for konsole and gnome-terminal
export MANPAGER="sh -c 'col -bx | bat -l  man -p'"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
# [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

function auto_venv_activate() {
  if [[ -d "venv" && -f "venv/bin/activate" ]]; then
    source "venv/bin/activate"
  elif [[ -d ".venv" && -f ".venv/bin/activate" ]]; then
    source ".venv/bin/activate"
  fi
}
add-zsh-hook chpwd auto_venv_activate

# Aliases
## Git 
alias gs="git status -s -b"
alias ga="git add"
alias gc="git commit -v"
alias gd="git diff"
alias gp="git push"
alias gds="git diff --staged"
alias glg="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(auto)%d%C(reset)' --all"
alias gdf="git diff --name-only --cached --diff-filter=AM" # git show staged but not diffed files

## Tools weren't working for some reason so this was an attempt to fix
alias gcc="/usr/bin/gcc"
alias as="/usr/bin/as"
alias ld="/usr/bin/ld"

# Sane defaults
alias ll="ls -lhGF"
alias ls="ls -hGF"
alias la="ls -lahGF"
alias ldot="ls -dhl .*"
alias grep="rg"
alias stat="stat -x"

## General stuff
alias zc="vim ~/.zshrc"
alias ec="emacsclient -nw"
alias sz="source ~/.zshrc"
alias v="nvim"
alias vim="nvim"
alias mux=tmuxinator


export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="/opt/local/bin:$PATH"
export PATH="/Users/mattgouzoulis/bin/$PATH"

# export EMACSDIR="~/.config/emacs"
# export PATH="/Applications/MacPorts/Emacs.app/Contents/MacOS/bin:$PATH"

export PATH="/Users/mattgouzoulis/Library/Python/3.7/bin:$PATH"
export PATH="/Library/Developer/CommandLineTools/usr/bin/:$PATH"
export PATH="$HOME/.config/emacs-configs/doom/bin:$PATH"
export PATH="/run/current-system/sw/bin:$PATH"
# export DOOMDIR="$HOME/.doom.d"


source <(fzf --zsh)

# Created by `pipx` on 2025-01-01 23:23:10
export PATH="$PATH:/Users/mattgouzoulis/.local/bin"

eval "$(mise activate zsh)"
export PATH="$HOME/vcpkg:$PATH"
export CPATH="$CPATH:/opt/homebrew/include/"
export PATH="$HOME/go/bin:$PATH"
export PATH="/Users/mattgouzoulis/.bun/bin:$PATH"


# Load Angular CLI autocompletion.
source <(ng completion script)
eval "$(uv generate-shell-completion zsh)"
