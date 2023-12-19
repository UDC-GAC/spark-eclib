#!/bin/sh
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

#
# BDEV script for testing ECLIB on Spark
#
# Usage: bdev.sh pso <cores> <partitions>

module load jdk/openjdk

# working dir
export ECLIB=$HOME/eclib-experiment

# ECLIB JAR
export SPARKJAR=$ECLIB/jar/eclib-0.0.1-test-jar-with-dependencies.jar
export SPARKCLASS=gal.udc.gac.eclib.EclibTest

# Location of eclib library dependencies
LIBPATH=$ECLIB/lib
export LD_LIBRARY_PATH=$LIBPATH:$LD_LIBRARY_PATH

# Location of the log4j.properties
LOG4J_PATH=$ECLIB/jar

# Additional SPARK options
DRIVEROPTS="--driver-class-path $LOG4J_PATH:$LIBPATH --driver-library-path $LIBPATH"
EXECUTORCORES="--executor-cores $2 --conf spark.default.parallelism=$3"
EXECUTOROPTS="--conf spark.executor.extraJavaOptions=-Dlog4j.configuration=file://$LOG4J_PATH/log4j.properties --conf spark.executor.extraClassPath=$ECLIB/lib --conf spark.executorEnv.LD_LIBRARY_PATH=$ECLIB/lib:\$LD_LIBRARY_PATH"
HISTORYSERVER="--conf spark.eventLog.enabled=true --conf spark.eventLog.dir=file://$ECLIB/experiments/history"
#HISTORYSERVER=
export SPARKOPTS="$DRIVEROPTS $EXECUTORCORES $EXECUTOROPTS $HISTORYSERVER"

# configuration file
export PROPFILE=$ECLIB/experiments/eclib-$1.conf

# BDEV configs
export BDEv_HOME=$HOME/apps/bdev
export EXP_DIR=$ECLIB/BDEv-configs/1W

sh $BDEv_HOME/bin/run.sh



