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
    /*reload)*/
      /*sudo modprobe -r rtl8723be && sudo modprobe rtl8723be && sudo service network-manager restart*/
      ;;
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
