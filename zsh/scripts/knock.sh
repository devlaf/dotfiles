####################################################################
# Port knocking
#   Usage: knock {host} {first_port_num} {second_port_num} etc.
#
####################################################################

knock() {
    if [[ $# -eq 0 ]] ; then
        echo 'Usage: knock {host} {first_port_num} {second_port_num} etc.'
        return
    fi
    
    HOST=$1
    shift
    for ARG in "$@"
    do
        nmap -Pn --host-timeout 100 --max-retries 0 -p $ARG $HOST
    done
}
