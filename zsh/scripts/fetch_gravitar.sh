gravitar() {
  curl "http://gravatar.com/avatar/$(echo -n $1 | md5sum)?s=300" > gravatar_$1.png
}
