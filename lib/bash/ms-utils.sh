#!/bin/bash
###########################################################################
#
# Copyright (c) 2009 Jorge Nunes, All Rights Reserved.
#
#
# Utility functions. This script is not meant to be executed by
# itself. Instead it is tipically sourced from within other scripts
# using the "." operator.
#
#########################################################################





###########################################################################
#
# Displays a message to stderr and exits the process.
#
# Arguments: 
#
# 1. The message to be displayed.
#
###########################################################################

msError () {

    echo "***" "$@" >&2
    exit 1
}





###########################################################################
#
# Displays a message to stderr and exits the process.
#
# Arguments: 
#
# 1. The message to be displayed.
#
###########################################################################

msAbort () {

    echo "***" "$@" >&2
    kill $$
    exit 1
}





###########################################################################
#
# Displays a message to stdout prefixed with the current time.
#
# Arguments:
#
# 1. The message to be displayed.
#
###########################################################################

msLog () {

	echo $(date "+%Y/%m/%d %H:%M:%S") "$@"
}





###########################################################################
#
# Echoes the system current time in a format apropriate for display.
#
###########################################################################

msDate () {

    date "+%Y/%m/%d %H:%M:%S"
}





###########################################################################
#
# Echos the hosname of the machine running the current process.
#
###########################################################################

msHostname () {

    uname -n
}





###########################################################################
#
# Returns the uid of the user, needed since the id on FC3 helds a
# different layout from other Linux releases.
# There is also the problem for duplicate groups when the command is
# issued with no parameter!
#
# If the given username does not exist then returns an empty string.
#
# 1. [A username] optional parameter.
#
###########################################################################

msId () {

    local username=$1

    if [ -z "$username" ] ; then
	username=$(expr "$(id $1 | cut -d\( -f1)" : 'uid=\(.*\)')
    fi

    getent passwd $username | awk -F: '{print $3}'
}





###########################################################################
#
# Returns the username of the user running the script.
#
###########################################################################

msWhoAmI () {

    getent passwd $(ebId) | awk -F: '{print $1}'
}





###########################################################################
#
# Checks if a set of required tools is available. If any is missing
# outputs an error message and terminates the process.
#
###########################################################################

function msCheckForTools () {

    local toolList="$@"

    for tool in ${toolList} ; do
        if type $tool > /dev/null 2>&1 ; then
            : All is ok
        else
            msError "Missing \"${tool}\" tool. Please install this tool."
        fi
    done
}





###########################################################################
#
# 
#
###########################################################################

