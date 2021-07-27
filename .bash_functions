xdgo() {
  xdg-open $1 2>&1 >/dev/null &
}

cp_p () {
  rsync -WavP --human-readable --progress $1 $2
}

themetime () {
  NIGHT_GTK_THEME=Arc-Dark
  NIGHT_WM_THEME=Arc-Dark
  DAY_GTK_THEME=Arc
  DAY_WM_THEME=Arc
  case "$1" in
    night)
      xfconf-query -c xfwm4 -p /general/theme -s $NIGHT_WM_THEME
      xfconf-query -c xsettings -p /Net/ThemeName -s $NIGHT_GTK_THEME
      sed -i 's/set background=light/set background=dark/' ~/.vimrc
      ;;
    day)
      xfconf-query -c xfwm4 -p /general/theme -s $DAY_WM_THEME
      xfconf-query -c xsettings -p /Net/ThemeName -s $DAY_GTK_THEME
      sed -i 's/set background=dark/set background=light/' ~/.vimrc
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
    reload)
      echo "removing wifi module" && sudo modprobe -rf ath11k_pci &&
      echo "module removed, loading back in" && sudo modprobe ath11k_pci &&
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
