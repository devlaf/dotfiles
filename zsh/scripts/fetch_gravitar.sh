gravitar() {
  if [[ $# -eq 0 ]] ; then
    echo 'Usage: gravitar {email_address} --> fetches and saves to ./gravitar-{email_address}.png'
    return
  fi

  curl "http://gravatar.com/avatar/$(echo -n $1 | md5sum)?s=300" > gravatar_$1.png
}

