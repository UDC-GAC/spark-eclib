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


# Usage: kill.sh job_id

../spark/bin/spark-submit \
    --kill $1 \
    --master k8s://https://kubernetes.docker.internal:6443
