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
      ;;
    day)
      xfconf-query -c xfwm4 -p /general/theme -s $DAY_WM_THEME
      xfconf-query -c xsettings -p /Net/ThemeName -s $DAY_GTK_THEME
      ;;
  esac
}

dockercmd () {
/*
 * usage:
 * > dockercmd pdflatex example.tex
 * > dockercmd /bin/sh -c "pdflatex example.tex && pdflatex example.tex"
 */
  exec docker run --rm -i --user="$(id -u):$(id -g)" -v $PWD:/data blang/latex "$@" &
}
