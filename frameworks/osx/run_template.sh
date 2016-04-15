#!/bin/bash
set -euo pipefail

mkdir -p  /tmp/xcode
DIR=$( cd "$(dirname "${BASH_SOURCE[0]}")" && pwd )

if [ ! -z ${RUNNER_TMPDIR+x} ] ; then
	if [ ! -r "$RUNNER_TMPDIR" -o ! -d "$RUNNER_TMPDIR" ] ; then
		echo "$RUNNER_TMPDIR is not a readable directory!"
		false
	fi
else
	RUNNER_TMPDIR=$(${DIR}/ready_template.sh)
fi

#trap 'rm -rf $RUNNER_TMPDIR' EXIT
CODEDIR=$(ls -d $RUNNER_TMPDIR/X*/)
TMPPBXPROJ=$(mktemp /tmp/project.pbxproj.XXXXXXXXXXXXXX)
trap 'rm -f $TMPPBXPROJ' EXIT
mv $CODE $CODEDIR/$(basename $CODE)
mv $FIXTURE $CODEDIR/$(basename $FIXTURE)
[ ! -z ${SETUP+x} ] && cp $SETUP $CODEDIR/$(basename $SETUP)

case $language in
	objc)
		mv $CODE_HEADER $CODEDIR/$(basename $CODE_HEADER)
		if [ ! -z ${SETUP+x} ]; then 
			mv $SETUP_HEADER $CODEDIR/$(basename $SETUP_HEADER)
			sed -e "s/code.m/$(basename $CODE)/g" \
			    -e "s/code.h/$(basename $CODE_HEADER)/g" \
			    -e "s/setup.m/$(basename $SETUP)/g" \
			    -e "s/setup.h/$(basename $SETUP_HEADER)/g" \
			    -e "s/test.m/$(basename $FIXTURE)/g" \
			    $RUNNER_TMPDIR/runner.xcodeproj/project.pbxproj > $TMPPBXPROJ
		else
			sed -e "s/code.m/$(basename $CODE)/g" \
			    -e "s/code.h/$(basename $CODE_HEADER)/g" \
			    -e "s/test.m/$(basename $FIXTURE)/g" \
			    $RUNNER_TMPDIR/runner.xcodeproj/project.pbxproj > $TMPPBXPROJ

		fi
		;;
	swift)
		if [ ! -z ${SETUP+x} ]; then 
			sed -e "s/code.swift/$(basename $CODE)/g" \
			    -e "s/setup.swift/$(basename $SETUP)/g" \
			    -e "s/test.swift/$(basename $FIXTURE)/g" \
			    $RUNNER_TMPDIR/runner.xcodeproj/project.pbxproj > $TMPPBXPROJ
		else
			sed -e "s/code.swift/$(basename $CODE)/g" \
			    -e "s/test.swift/$(basename $FIXTURE)/g" \
			    $RUNNER_TMPDIR/runner.xcodeproj/project.pbxproj > $TMPPBXPROJ
		fi
		;;

	*)
		echo "Language \"$language\" not supported!"
		false
		;;
esac

OUTPUT=$(mktemp /tmp/output.XXXXXXXXXXXXXX)
trap 'rm -f $OUTPUT' EXIT
mv $TMPPBXPROJ $RUNNER_TMPDIR/runner.xcodeproj/project.pbxproj

EXIT_CODE=0
if (cd $RUNNER_TMPDIR ; 
    xcodebuild build -project runner.xcodeproj/ -scheme $(basename $CODEDIR) -destination 'platform=OS X,arch=x86_64' && \
    sandbox-exec -f ${DIR}/xcode.sb /usr/bin/xcodebuild test -project runner.xcodeproj/ \
	         -scheme $(basename $CODEDIR) \
                 -destination 'platform=OS X,arch=x86_64') &> $OUTPUT || \
    EXIT_CODE=1 && \
    grep "Test Suite '$(basename $CODEDIR).xctest'" $OUTPUT &> /dev/null; then
    ${DIR}/format_output.sh $(basename $CODEDIR) < $OUTPUT
else
    ${DIR}/format_failure.sh $(basename $CODEDIR) < $OUTPUT
fi
exit ${EXIT_CODE}
