#!/bin/bash
###########################################################################
#
# Copyright (c) 2009 Jorge Nunes, All Rights Reserved.
#
###########################################################################

#########################################################################
#
# $Id$
#
#
# Utility functions. This script is not meant to be executed by
# itself. Instead it is tipically sourced from within other scripts
# using the "." operator.
#
#
# Revisions:
#
# 2009/07/27 Created. (jfn)
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

myError () {

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

myAbort () {

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

myLog () {

	echo $(date "+%Y/%m/%d %H:%M:%S") "$@"
}





###########################################################################
#
# Echoes the system current time in a format apropriate for display.
#
###########################################################################

myDate () {

    date "+%Y/%m/%d %H:%M:%S"
}





###########################################################################
#
# Echos the hosname of the machine running the current process.
#
###########################################################################

myHostname () {

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

myId () {

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

myWhoAmI () {

    getent passwd $(ebId) | awk -F: '{print $1}'
}





###########################################################################
#
# 
#
###########################################################################

