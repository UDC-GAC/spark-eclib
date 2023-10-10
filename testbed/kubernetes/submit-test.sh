#!/bin/zsh
#
# eclib - Spark Evolutionary Computing Library
# Copyright © 2022 Xoán C. Pardo (xoan.pardo@udc.gal)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#


# Submit an ECLIB job and store conf and logs in the same directory

# Usage: submit-test.sh description config executors cores [partitions] [output_dir]

DESC=$1
CONF=$2
EXECUTORS=$3
CORES=$4

if [ -z "$5" ];
then 
  PARTITIONS=$CORES
else
  PARTITIONS=$5
fi

if [ -z "$6" ];
then 
  OUTDIR=experiments
else
  OUTDIR=$6
fi

DATE=$(date "+%d_%m_%Y-%H_%M_%S")
TEMP_NAME=exp-$DATE
TEMP_FILE=submit-$DATE.out

# call submit and store output in a temp file
./submit-with-history.sh $CONF $EXECUTORS $CORES $PARTITIONS &> >(tee $TEMP_FILE)

# get POD name and status after submit completes
POD_NAME=$(grep -m 1 "pod name:" $TEMP_FILE | awk -F':[[:space:]]*' '{ print $2 }')
STATUS=$(kubectl describe pod $POD_NAME | grep Status: | awk -F':[[:space:]]*' '{ print $2 }')

if [ $STATUS = "Succeeded" ]; then
  # get app identifier
  APP_ID=$(kubectl describe pod $POD_NAME | grep SPARK_APPLICATION_ID | awk -F':[[:space:]]*' '{ print $2 }')

  # create output dir and store conf and log files
  mkdir $OUTDIR/$TEMP_NAME
  echo $DESC > $OUTDIR/$TEMP_NAME/README
  mv $TEMP_FILE $OUTDIR/$TEMP_NAME/$TEMP_FILE
  cp conf/$CONF $OUTDIR/$TEMP_NAME/$CONF
  kubectl logs $POD_NAME > $OUTDIR/$TEMP_NAME/$POD_NAME.log
  cp history/$APP_ID $OUTDIR/$TEMP_NAME/$APP_ID
else
  rm $TEMP_FILE
fi