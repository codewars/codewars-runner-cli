#!/bin/bash
set -euo pipefail

mkdir -p  /tmp/xcode
DIR=$( cd "$(dirname "${BASH_SOURCE[0]}")" && pwd )
NEW_TEMPDIR=$(mktemp -d /tmp/xcode/XXXXXXXXXXXXXXXXXXXXXXX)

# Set up the XCode project
UUID=X$(uuidgen)
mkdir -p ${NEW_TEMPDIR}/runner.xcodeproj/xcuserdata/${USER}.xcuserdatad/xcschemes
sed -e "s/runner-test/${UUID}/g" ${DIR}/runner_template/runner.xcodeproj/xcuserdata/template.xcuserdatad/xcschemes/runner-test.xcscheme > ${NEW_TEMPDIR}/runner.xcodeproj/xcuserdata/${USER}.xcuserdatad/xcschemes/${UUID}.xcscheme
sed -e "s/runner-test/${UUID}/g" ${DIR}/runner_template/runner.xcodeproj/project.pbxproj > ${NEW_TEMPDIR}/runner.xcodeproj/project.pbxproj
cp -a ${DIR}/runner_template/runner-test/ ${NEW_TEMPDIR}/${UUID}/

# Build the xcode project a head of time
( cd ${NEW_TEMPDIR} ;
  xcodebuild clean build -project runner.xcodeproj/ -scheme ${UUID} -destination 'platform=OS X,arch=x86_64') > /dev/null 2>&1

echo ${NEW_TEMPDIR}
