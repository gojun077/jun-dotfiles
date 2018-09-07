#!/bin/bash

# Copyright 2017 Amazon.com, Inc. and its affiliates. All Rights Reserved.
#
# Licensed under the Amazon Software License (the "License").
# You may not use this file except in compliance with the License.
# A copy of the License is located at
#
#   http://aws.amazon.com/asl/
#
# or in the "license" file accompanying this file. This file is distributed
# on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied. See the License for the specific language governing
# permissions and limitations under the License.
#
# Last Updated: 2018.09.07
# Updated by: jun.go@actwo.com
#
# This script is intended to be executed remotely by Ansible 2.X. The
# original CloudWatchAgent install scripts 'install.sh' and
# 'detect-system.sh' don't work when run though an ansible playbook
# because environment variables are not correctly set in the subshell
# executing install.sh -> detect-system.sh
#
# Therefore I have moved the logic from 'detect-system.sh' into the
# same file with the code from 'install.sh' and made some edits.
#
# This script requires the files extracted from the zip file
# AmazonCloudWatchAgent.zip available from the following url:
# https://s3.amazonaws.com/amazoncloudwatch-agent/linux/amd64/latest/AmazonCloudWatchAgent.zip


rpmbin="$(which rpm 2>/dev/null)"
dpkgbin="$(which dpkg 2>/dev/null)"

if $rpmbin; then
  rpm -U ./amazon-cloudwatch.rpm
elif $dpkgbin; then
  dpkg -i -E ./amazon-cloudwatch-agent.deb
else
  printf "%s\n" "### ERROR - Not a RHEL- or Debian-based System! ###"
  exit 1
fi
