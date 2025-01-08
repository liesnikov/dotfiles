alias mclip='xclip -selection clipboard'
alias tmux="tmux -2"
alias lessfollow="less --follow-name +F"
# alias ycm-generate="~/.vim/bundle/YCM-Generator/config_gen.py"

# caps-unlock here is from a SO answer:
# https://askubuntu.com/a/941268
alias CAPS-UNLOCK="caps-unlock"
alias keyboard-set="caps-unlock; setxkbmap -option ctrl:nocaps ; xset r rate 200 70"
alias KEYBOARD-SET="keyboard-set"

alias untar-gz="tar -xvzf"
alias emacso="emacsclient -n -c -a \"\""
alias i3lock="i3lock -c 000000"
alias lock="dm-tool lock"
alias suspend="systemctl suspend"

alias kill-lockscreen="sudo systemctl restart lightdm"

alias ollama-web-create="docker run -d --gpus all --network=host -v open-webui:/app/backend/data -e OLLAMA_BASE_URL=http://127.0.0.1:1143 --name ollama-webui --restart on-failure:5 ghcr.io/open-webui/open-webui:cuda"
alias ollama-web-start="docker start ollama-webui; xdg-open http://localhost:8080"
alias ollama-web-pause="docker stop ollama-webui"
alias ollama-web-kill="docker rm -f ollama-webui"

alias sound-hdmi="pactl set-card-profile 0 output:hdmi-stereo+input:analog-stereo"
alias sound-laptop="pactl set-card-profile 0 output:analog-stereo+input:analog-stereo"
