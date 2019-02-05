#!/bin/sh

directory=.
template=setup.adb.template
destination=setup.adb
date_time_utc=$(date -u "+%Y-%m-%dT%H:%M:%S")
program_version=$(cat $directory/PROGRAM_VERSION)

uname_opt_m=$(uname -m)
uname_opt_n=$(uname -n)
uname_opt_p=$(uname -p)
uname_opt_r=$(uname -r)
uname_opt_s=$(uname -s)

##
##  Stream edit setup.adb.template into setup.adb
##
cat $directory/$template                        | \
sed 's/@_DATE_TIME_UTC_@/'$date_time_utc'/'     | \
sed 's/@_PROGRAM_VERSION_@/'$program_version'/' | \
sed 's/@_UNAME_OPT_M_@/'$uname_opt_m'/'         | \
sed 's/@_UNAME_OPT_N_@/'$uname_opt_n'/'         | \
sed 's/@_UNAME_OPT_P_@/'$uname_opt_p'/'         | \
sed 's/@_UNAME_OPT_R_@/'$uname_opt_r'/'         | \
sed 's/@_UNAME_OPT_S_@/'$uname_opt_s'/'         | \
cat >$directory/$destination


