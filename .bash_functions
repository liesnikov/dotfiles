xdgo() {
  xdg-open $1 2>&1 >/dev/null &
}

cp_p () {
  rsync -WavP --human-readable --progress $1 $2
}

themetime () {
  NIGHT_GTK_THEME="Arc-Dark"
  NIGHT_WM_THEME="Arc-Dark"
  DAY_GTK_THEME="Arc"
  DAY_WM_THEME="Arc"
  case "$1" in
    night)
      xfconf-query -c xfwm4 -p /general/theme -s $NIGHT_WM_THEME
      xfconf-query -c xsettings -p /Net/ThemeName -s $NIGHT_GTK_THEME
      sed -i 's/set background=light/set background=dark/' $XDG_CONFIG_HOME/vim/vimrc
      ;;
    day)
      xfconf-query -c xfwm4 -p /general/theme -s $DAY_WM_THEME
      xfconf-query -c xsettings -p /Net/ThemeName -s $DAY_GTK_THEME
      sed -i 's/set background=dark/set background=light/' $XDG_CONFIG_HOME/vim/vimrc
      ;;
    switch)
      CURRENT=$(xfconf-query -c xfwm4 -p /general/theme)
      if [[ "$CURRENT" == "$DAY_WM_THEME" ]]; then
        themetime night
      elif [[ "$CURRENT" == "$NIGHT_WM_THEME" ]]; then
        themetime day
      else
        echo "current theme is not among daily or nightly ones"
      fi
      ;;
    current)
      CURRENT_WM=$(xfconf-query -c xfwm4 -p /general/theme)
      CURRENT_GTK=$(xfconf-query -c xsettings -p /Net/ThemeName)
      echo "current wm theme": "$CURRENT_WM"
      echo "current gtk theme": "$CURRENT_GTK"
      if [[ "$CURRENT_WM" == "$DAY_WM_THEME" ]] &&
         [[ "$CURRENT_GTK" == "$DAY_GTK_THEME" ]] ; then
        echo "that is a day theme"
      elif [[ "$CURRENT_WM" == "$NIGHT_WM_THEME" ]] &&
           [[ "$CURRENT_GTK" == "$NIGHT_GTK_THEME" ]] ; then
        echo "that is a night theme"
      else
        echo "current theme is not consistent"
      fi
      ;;
    help)
      echo "day theme": "$DAY_WM_THEME"
      echo "night theme": "$NIGHT_WM_THEME"
      echo possible commands: "day", "night", "switch", "current", "help"
      ;;
    -h)
      themetime help
      ;;
    --help)
      themetime help
      ;;
    *)
      echo "defulting to 'switch' command"
      themetime switch
      ;;
  esac
}

dockercmd () {
/*
 * usage:
 * > dockercmd pdflatex example.tex
 * > dockercmd /bin/sh -c "pdflatex example.tex && pdflatex example.tex"
 */
  docker run --rm -i --user="$(id -u):$(id -g)" -v $PWD:/data blang/latex "$@" &
}

wifi () {
  case "$1" in
    off)
      echo "wifi turning off"
      nmcli radio wifi off
      ;;
    on)
      echo "wifi turning on"
      nmcli radio wifi on
      ;;
    restart)
      wifi off
      wifi on
      ;;
    remove)
      echo "removing wifi module" && sudo modprobe -rf ath11k_pci && echo "removed"
      ;;
    load)
      echo "loading the module in" && sudo modprobe ath11k_pci && echo "loaded"
      ;;
    reload)
      wifi remove && wifi load &&
      echo "module loaded, restarting network-manager" && sudo service network-manager restart &&
      echo "wifi reloaded";;
    list)
      nmcli device wifi list
      ;;
    connect)
      nmcli device wifi connect $2 password $3
      ;;
    help)
      echo possible commands: "on", "off", "restart", "reload", "list"
      ;;
    -h)
      wifi help
      ;;
    --help)
      wifi help
      ;;
    *)
      echo "no such command"
      wifi help
     ;;
esac
}

streaming() {
     INRES="1920x1080" # input resolution
     OUTRES="1920x1080" # output resolution
     FPS="15" # target FPS
     GOP="30" # i-frame interval, should be double of FPS, 
     GOPMIN="15" # min i-frame interval, should be equal to fps, 
     THREADS="2" # max 6
     CBR="1000k" # constant bitrate (should be between 1000k - 3000k)
     QUALITY="ultrafast"  # one of the many FFMPEG preset
     AUDIO_RATE="44100"
     STREAM_KEY="$1" # use the terminal command Streaming streamkeyhere to stream your video to twitch or justin
     SERVER="live-fra" # twitch server in frankfurt, see http://bashtech.net/twitch/ingest.php for list

     ffmpeg -f x11grab -s "$INRES" -r "$FPS" -i :0.0 -f alsa -i pulse -f flv -ac 2 -ar $AUDIO_RATE \
       -vcodec libx264 -g $GOP -keyint_min $GOPMIN -b:v $CBR -minrate $CBR -maxrate $CBR -pix_fmt yuv420p\
       -s $OUTRES -preset $QUALITY -tune film -acodec libmp3lame -threads $THREADS -strict normal \
       -bufsize $CBR "rtmp://$SERVER.twitch.tv/app/$STREAM_KEY"
 }

screen_set() {
  case $1 in
    laptop)
      xrandr --output eDP-1 --primary --mode 3840x2400 --pos 0x0 --rotate normal \
             --output DP-1 --off \
             --output DP-2 --off \
             --output DP-3 --off
      ;;
    work) {
      xrandr --output DP-2 --primary --scale 1 --mode 3840x2160 --pos 0x0 --rotate normal \
             --output eDP-1 --off; } || {
      xrandr --output DP-1 --primary --scale 1 --mode 3840x2160 --pos 0x0 --rotate normal \
             --output eDP-1 --off; }
      ;;
    work-both) {
      xrandr --output DP-2  --primary --scale 1 --mode 3840x2160 --pos 0x0       --rotate normal \
             --output eDP-1           --scale 1 --mode 3840x2400 --right-of DP-2 --rotate normal; } || {
      xrandr --output DP-1  --primary --scale 1 --mode 3840x2160 --pos 0x0       --rotate normal \
             --output eDP-1           --scale 1 --mode 3840x2400 --right-of DP-1 --rotate normal; }
      ;;

    jesper-table) {
      xrandr --output DP-2  --primary --scale 1 --mode 3840x2160 --pos 0x0    --rotate normal \
             --output eDP-1           --scale 1 --mode 3840x2160 --pos 0x0    --rotate normal; } || {
      xrandr --output DP-1  --primary --scale 1 --mode 3840x2160 --pos 0x0    --rotate normal \
             --output eDP-1           --scale 1 --mode 3840x2160 --pos 0x0    --rotate normal; }
    ;;
    home) {
      xrandr --output eDP-1                     --mode 3840x2400 --pos 0x0        --rotate normal \
             --output DP-1  --primary --scale 2 --mode 1920x1080 --right-of eDP-1 --rotate normal; } || {
      xrandr --output eDP-1                     --mode 3840x2400 --pos 0x0        --rotate normal \
             --output DP-2  --primary --scale 2 --mode 1920x1080 --right-of eDP-1 --rotate normal; }
      ;;
    *)
      echo "couldn't recognise the setup"
      ;;
   esac
}


touchpad-on () {
    synclient TouchpadOff=0;
}
