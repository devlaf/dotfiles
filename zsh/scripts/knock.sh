####################################################################
# Port knocking script
#   Usage: knock {host} {first_port_num} {second_port_num} etc.
#
####################################################################

knock() {
    HOST=$1
    shift
    for ARG in "$@"
    do
        nmap -Pn --host-timeout 100 --max-retries 0 -p $ARG $HOST
    done
}
