epoch_convert() {
  if [[ $# -eq 0 ]] ; then
    echo 'Usage: epoch_convert {date_in_epoch_milli} --> outputs readable date'
    return
  fi

  date -d @$(($1 / 1000))
}

