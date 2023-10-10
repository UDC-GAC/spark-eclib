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


# Usage: submit.sh config executors cores [partitions]

CONF=$1
EXECUTORS=$2
CORES=$3

if [ -z "$4" ];
then 
  PARTITIONS=$CORES
else
  PARTITIONS=$4
fi

../spark/bin/spark-submit \
    --master k8s://https://kubernetes.docker.internal:6443 \
    --deploy-mode cluster \
    --name EclibTest\
    --class gal.udc.gac.eclib.EclibTest \
    --driver-cores 1 \
    --num-executors $EXECUTORS \
    --executor-cores $CORES \
    --executor-memory 512M \
    --conf spark.default.parallelism=$PARTITIONS \
    --conf spark.kubernetes.container.image=apache/spark:latest \
    --driver-class-path /tmp/eclib/jar \
    --driver-library-path /tmp/eclib/lib \
    --conf spark.kubernetes.driver.volumes.hostPath.eclib.mount.path=/tmp/eclib \
    --conf spark.kubernetes.driver.volumes.hostPath.eclib.mount.readOnly=true \
    --conf spark.kubernetes.driver.volumes.hostPath.eclib.mount.subpath=jar \
    --conf spark.kubernetes.driver.volumes.hostPath.eclib.mount.subpath=conf \
    --conf spark.kubernetes.driver.volumes.hostPath.eclib.mount.subpath=lib \
    --conf spark.kubernetes.driver.volumes.hostPath.eclib.mount.subpath=benchmarks \
    --conf spark.kubernetes.driver.volumes.hostPath.eclib.options.path=$PWD \
    --conf spark.kubernetes.driver.volumes.hostPath.eclib.options.type=DirectoryOrCreate \
    --conf spark.executor.extraClassPath=/tmp/eclib/lib \
    --conf spark.executor.extraJavaOptions=-Dlog4j.configuration=file:////tmp/eclib/jar/log4j-executor.properties \
    --conf spark.executorEnv.LD_LIBRARY_PATH="/tmp/eclib/lib:\$LD_LIBRARY_PATH" \
    --conf spark.kubernetes.executor.volumes.hostPath.eclib.mount.path=/tmp/eclib \
    --conf spark.kubernetes.executor.volumes.hostPath.eclib.mount.readOnly=true \
    --conf spark.kubernetes.executor.volumes.hostPath.eclib.mount.subpath=jar \
    --conf spark.kubernetes.executor.volumes.hostPath.eclib.mount.subpath=conf \
    --conf spark.kubernetes.executor.volumes.hostPath.eclib.mount.subpath=lib \
    --conf spark.kubernetes.executor.volumes.hostPath.eclib.mount.subpath=benchmarks \
    --conf spark.kubernetes.executor.volumes.hostPath.eclib.options.path=$PWD \
    --conf spark.kubernetes.executor.volumes.hostPath.eclib.options.type=DirectoryOrCreate \
    local:///tmp/eclib/jar/eclib-0.0.1-test-jar-with-dependencies.jar \
    /tmp/eclib/conf/$CONF