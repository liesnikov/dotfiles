xdgo() {
	xdg-open $1 2>&1 >/dev/null &
}

cp_p () {
	rsync -WavP --human-readable --progress $1 $2
}
