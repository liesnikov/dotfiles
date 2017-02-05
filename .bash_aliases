alias mclip='xclip -selection clipboard'
alias rered='kill `pgrep redshift` && redshift &'
alias ihaskell='docker run -it --volume $(pwd):/notebooks --publish 8888:8888 gibiansky/ihaskell:latest'
alias tmux="tmux -2"
alias ycm-generate="~/.vim/bundle/YCM-Generator/config_gen.py"
alias wifi-restart="sudo modprobe -r rtl8723be && sudo modprobe rtl8723be"
