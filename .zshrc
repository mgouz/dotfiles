### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk
#
# 
# ZSH_THEME="robbyrussell"
eval "$(starship init zsh)"

plugins=(git direnv docker)

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

# function auto_venv_activate() {
#   if [[ -d "venv" && -f "venv/bin/activate" ]]; then
#     source "venv/bin/activate"
#   elif [[ -d ".venv" && -f ".venv/bin/activate" ]]; then
#     source ".venv/bin/activate"
#   fi
# }
# add-zsh-hook chpwd auto_venv_activate

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
alias cat="bat"
alias stat="stat -x"

## General stuff
alias zc="vim ~/.zshrc"
alias ec="emacsclient -nw"
alias sz="source ~/.zshrc"
alias v="nvim"
alias vim="nvim"
alias mux=tmuxinator
alias hm="vim ~/.config/home-manager/home.nix"


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
source ~/.zshenv_secrets
# ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#ff00ff,bg=cyan,bold,underline"
eval "$(zoxide init zsh)"
source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''


zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light Aloxaf/fzf-tab
zinit light MichaelAquilina/zsh-you-should-use
# zinit light zdharma-continuum/history-search-multi-word
zinit ice as"completion"

zinit snippet https://github.com/docker/cli/blob/master/contrib/completion/zsh/_docker


# # disable sort when completing `git checkout`
# zstyle ':completion:*:git-checkout:*' sort false
# # set descriptions format to enable group support
# # NOTE: don't use escape sequences (like '%F{red}%d%f') here, fzf-tab will ignore them
# zstyle ':completion:*:descriptions' format '[%d]'
# # set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# # force zsh not to show completion menu, which allows fzf-tab to capture the unambiguous prefix
# zstyle ':completion:*' menu no
# # preview directory's content with eza when completing cd
# zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'
# # custom fzf flags
# # NOTE: fzf-tab does not follow FZF_DEFAULT_OPTS by default
zstyle ':fzf-tab:*' fzf-flags --color=fg:1,fg+:2 --bind=tab:accept
# # To make fzf-tab follow FZF_DEFAULT_OPTS.
# # NOTE: This may lead to unexpected behavior since some flags break this plugin. See Aloxaf/fzf-tab#455.
# zstyle ':fzf-tab:*' use-fzf-default-opts yes
# # switch group using `<` and `>`
# zstyle ':fzf-tab:*' switch-group '<' '>'
#
# Completion system
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:options' description yes
zstyle ':completion:*:options' auto-description '%d'
# zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
zstyle ':fzf-tab:*' switch-group '<' '>'


# zstyle ':completion:*:options' description yes
# zstyle ':completion:*:options' auto-description '%d'



export PATH="$PATH:/Applications/Docker.app/Contents/Resources/bin/"


# pnpm
export PNPM_HOME="/Users/mattgouzoulis/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
