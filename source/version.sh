#!/bin/sh

# this bash script will retrieve the version/revision of FPC, Lazarus and the current
# programs source tree and store them in the $version_file pascal file.

BUILD_DATE=$(date "+%Y-%m-%d %H:%M:%S")

svn_error() {
    echo Current and Online Version Mismatch!
    echo
	echo To force building of the current revision:
	echo Change update to ignore Under:
	echo Project/Project Options/Compiler Options/Compiler Commands/Execute Before
	echo Change: /bin/sh revision.sh update
	echo To: /bin/sh revision.sh ignore
	exit 1
}

function parse_attr() {
  line="$(cat $lpi | grep -m 1 ${1})"
  line=$(echo $line | cut -d ' ' -f 2-)
  [[ ${#line} -gt 2 ]] && line=${line:0:$((${#line} - 2))} || line='';
  while [[ $line != '' ]] ; do
  	lopt=${line%%'='*}
  	line=${line:$(( ${#lopt} + 2 ))}
  	lval=${line%%'"'*}
  	line=$(echo ${line:$(( ${#lval} + 1 ))})
  	topt="${2}_$(echo $lopt | tr [:lower:] [:upper:])"
  	tval="$lval"
  	eval $topt=\"$tval\"
  	if [[ "$2" = "APP" ]] ; then
		opts[${#opts[@]}]="$topt"
		vals[${#vals[@]}]="$lval"
	fi;
  done;
}

function TF () {
	if [[ "$1" = "True" ]] ; then
		echo True
	else
		echo False
	fi;
}

getval () {
	local val=$(cat $lpi | grep -m 1 "$1" | cut -d '"' -f 2)
	[[ $val ]] && echo $val || echo '0'
}

print_consts() {
	echo "// Application, FPC & Lazarus Version Information"
	echo

	echo "const"
	# Type Declaration
	echo '{$IF declared(TAppVersionInfoType)}'
	echo
	echo '  VersionInfo : TAppVersionInfoType = ('
	echo "  { The default Free Pascal Compiler }"
    echo "    FreePascal : ("
    echo "      Version : '$FPC_VERSION';"
    echo "      Revision : '$FPC_REVISION';"
    echo "      Target : '$FPC_TARGET';"
    echo "    );"
	echo "  { The Lazarus I.D.E }"
    echo "    Lazarus : ("
	echo "      Version : '"$( echo $LAZARUS_VERSION | cut -d "'" -f 2)"';"
	echo "      Revision : '"$( echo $LAZARUS_REVISION | cut -d "'" -f 2)"';"
    echo "    );"
	echo "  { Source & Subversion Last Changed Commit }"
    echo "    Source : ("
	echo "      Version : '${APP_VERSION}';"
	echo "      Revision : '$REVISION';"
	echo "      Online : '$ONLINE';"
	echo "      URL : '$URL';"
	echo "      Commit : '${REVISION_ID}';"
    echo "    );"
    echo "  { Version Build Atributes } "
    echo "    Attributes : ( "
	echo "      Debug : "$(TF $ATTR_PVADEBUG)";"
	echo "      PreRelease : "$(TF $ATTR_PVAPRERELEASE)";"
	echo "      Patched : "$(TF $ATTR_PVAPATCHED)";"
	echo "      PrivateBuild : "$(TF $ATTR_PVAPRIVATEBUILD)";"
	echo "      SpecialBuild : "$(TF $ATTR_PVASPECIALBUILD)";"
	echo "      Language : '$LANG_VALUE';"
	echo "      CharSet : '$CHRS_VALUE';"
	echo "      Date : '$BUILD_DATE';"
	echo "    );"
	echo "  { General Application Information }"
    echo "    Application : ("
    echo "      Identifier : '${APP_IDENTIFIER}';"
    echo "      Version : '${APP_VERSION}';"
    echo "      Comments : '${APP_COMMENTS}';"
    echo "      CompanyName : '${APP_COMPANYNAME}';"
    echo "      FileDescription : '${APP_FILEDESCRIPTION}';"
    echo "      InternalName : '${APP_INTERNALNAME}';"
    echo "      LegalCopyright : '${APP_LEGALCOPYRIGHT}';"
    echo "      LegalTrademarks : '${APP_LEGALTRADEMARKS}';"
    echo "      OriginalFilename : '${APP_ORIGINALFILENAME}';"
    echo "      ProductName : '${APP_PRODUCTNAME}';"
    echo "      ProductVersion : '${APP_PRODUCTVERSION}';"
	echo "      Year : '$BUILD_YEAR';"
    echo "    );"
    echo '  );'
    echo
    echo '{$ELSE}'
    echo
	echo "  { The default Free Pascal Compiler }"
	echo "  FPC_VERSION='$FPC_VERSION';"
	echo "  FPC_REVISION='$FPC_REVISION';"
	echo "  FPC_TARGET='$FPC_TARGET';"
	echo
	echo "  { Platform specific and cross-compilers }"
	[[ $FPC_REVISION_I386 ]]      && echo "  FPC_PPC386='$FPC_REVISION_I386';"
	[[ $FPC_REVISION_X86_64 ]]    && echo "  FPC_PPCX64='$FPC_REVISION_X86_64';"
	[[ $FPC_REVISION_PPC ]]       && echo "  FPC_PPCPPC='$FPC_REVISION_PPC';"
	[[ $FPC_REVISION_SIMULATOR ]] && echo "  FPC_PPCSIM='$FPC_REVISION_SIMULATOR';"
	[[ $FPC_REVISION_IOS ]]       && echo "  FPC_PPCARM='$FPC_REVISION_IOS';"
	echo
	echo "  { The Lazarus I.D.E }"
	echo "  LAZARUS_VERSION='$LAZARUS_VERSION';"
	echo "  LAZARUS_REVISION='$LAZARUS_REVISION';"
	echo
	echo "  { Source & Subversion Last Changed Commit }"
	echo "  SOURCE_VERSION='${APP_VERSION}';"
	echo "  SOURCE_REVISION='$REVISION';"
	echo "  SOURCE_URL='$URL';"
	echo "  SOURCE_ONLINE='$ONLINE';"
    echo "  SOURCE_COMMIT='$REVISION_ID';"
	echo
    echo "  { Version Build Atributes } "
	echo "  BUILD_DEBUG="$(TF $ATTR_PVADEBUG)";"
	echo "  BUILD_PRERELEASE="$(TF $ATTR_PVAPRERELEASE)";"
	echo "  BUILD_PATCHED="$(TF $ATTR_PVAPATCHED)";"
	echo "  BUILD_PRIVATE="$(TF $ATTR_PVAPRIVATEBUILD)";"
	echo "  BUILD_SPECIAL="$(TF $ATTR_PVASPECIALBUILD)";"
	echo "  BUILD_LANGUAGE='$LANG_VALUE';"
	echo "  BUILD_CHARSET='$CHRS_VALUE';"
	echo "  BUILD_DATE='$BUILD_DATE';"
	echo
	echo "  { General Application Information }"
	echo "  APP_IDENTIFIER='${APP_IDENTIFIER}';"
	# echo "  APP_TITLE='${APP_TITLE}';"
	# echo "  APP_VENDOR='${APP_VENDOR}';"
	echo "  APP_VERSION='${APP_VERSION}';"

	i=0;
	while [[ i -lt ${#opts[@]} ]] ; do
		echo "  ${opts[$i]}='${vals[$i]}';"
		(( i++ ))
	done
	echo "  APP_YEAR='${BUILD_YEAR}';"

	echo
    echo '{$ENDIF}'

}

stats() {
	FPC_REVISION=$(echo $FPC_REVISION | cut -d "'" -f 2 )
	LAZARUS_VERSION=$(echo $LAZARUS_VERSION | cut -d "'" -f 2)
	LAZARUS_REVISION=$(echo $LAZARUS_REVISION | cut -d "'" -f 2)
	echo "$FPC_TARGET FPC Version $FPC_VERSION (r$FPC_REVISION)"
	echo "Lazarus Version $LAZARUS_VERSION (r$LAZARUS_REVISION)"

	/bin/echo -n "$APP_TITLE Version $APP_VERSION"
	[[ $REVISION ]] && /bin/echo -n " (r$REVISION)"
	echo
	return 0
}

function cvs_svn () {

    echo "Retrieve project svn data for ${PWD}."
    URL=$(echo $(svn info 2>&1 | grep -m 1 'URL:' | cut -d ':' -f 2- ))
    REVISION=$(echo $(svn info 2>&1 | grep 'Last Changed Rev:' | cut -d ':' -f 2- ))
    [[ $1 != 'off' ]] && [[ $1 != 'local' ]] && ONLINE=$(echo $(svn info "$URL" 2>&1 | grep 'Last Changed Rev:' | cut -d ':' -f 2- ))

    [[ $ONLINE != $REVISION ]] && {
        case $1 in
          (update|commit) svn update || {
            echo ERROR: updating source from $URL >&2
            svn_error >&2
          }
          UPDATE=true
          ;;
        esac;
    }

    [[ $ONLINE != $REVISION ]] && {
        case $1 in
          (commit) svn commit -m "Automatic Commit from revision script" && svn update || {
            echo ERROR: updating source from $URL >&2
            svn_error >&2
          }
          UPDATE=true
          ;;
        esac;
    }

    [[ $UPDATE ]] && {
      REVISION=$(echo $(svn info 2>&1 | grep -i 'Last Changed Rev:' | cut -d ':' -f 2- ))
      ONLINE=$(echo $(svn info "$URL" 2>&1 | grep -i 'Last Changed Rev:' | cut -d ':' -f 2- ))
    }

    [[ $ONLINE != $REVISION ]] && {
        case $1 in
          (ignore) echo Ignoring Online and Current Revision Mismatch.>&2
          sleep 2
          ;;
          (off|local) echo Online revision not checked.>&2
          sleep 2
          ;;
          (*)
          svn_error >&2
          exit 1
        esac;
    }

}

function cvs_git () {
    echo "Retrieve project git data for ${PWD}."
    REVISION=$(wc -l .git/logs/head 2>/dev/null | cut -d '.' -f 1 )
    [[ $? = 0 ]] && (( REVISION++ )) || REVISION=0
    ONLINE=
    REV_HEAD=$(cat .git/HEAD 2>/dev/null | cut -d ' ' -f 2- )
    REVISION_ID=$(cat .git/${REV_HEAD} 2>/dev/null)
    ONLINE_ID=$(cat .git/refs/remotes/origin/${REV_HEAD##*/} 2>/dev/null)
    URL=$(grep -i "URL=\|URL =" .git/config | cut -d '@' -f 2-)
    URL="${URL/://}"
    URL="http://${URL%.*}"
}

# Also takes options, ignore, update, off, local;
[[ $1 = '' ]] && {
	echo Usage: ${0##*/} mode [project]
	echo modes:
	echo off/local - Do no chech cvs server revision number
	echo ignore - check but only display differences.
	echo update - run cvs update if revisions are different.
	echo commit - always cvs update, then automatically commit changes. \(a really bad idea\)
	exit 1
}

BUILD_YEAR="${BUILD_DATE%%-*}"

cwd="${PWD}"
while [[ "$PWD" != '/' ]] && [[ ! -d '.svn' ]] ; do
    cd ..
done
[[ -d '.svn' ]] && cvs_svn ${@}
cd "$cwd"

while [[ "$PWD" != '/' ]] && [[ ! -d '.git' ]] ; do
    cd ..
done
[[ -d '.git' ]] && cvs_git ${@}
cd "$cwd"

FPC_VERSION=$(/usr/local/bin/fpc -iV)
FPC_TARGET=$(echo $(/usr/local/bin/fpc -i | grep -i "CPU Target" | cut -d ':' -f 2))
version_file='version.inc'

[[ -f "${HOME}/.lazyFPC" ]] && {
  FPC_REVISION_I386=$(cat "${HOME}/.lazyFPC" | grep -i 'LATEST_PPC386_REV=' | cut -d \' -f 2 )
  FPC_REVISION_X86_64=$(cat "${HOME}/.lazyFPC" | grep -i 'LATEST_PPCX64_REV=' | cut -d \' -f 2 )
  FPC_REVISION_PPC=$(cat "${HOME}/.lazyFPC" | grep -i 'LATEST_PPCPPC_REV=' | cut -d \' -f 2 )
  FPC_REVISION_SIMULATOR=$(cat "${HOME}/.lazyFPC" | grep -i 'LATEST_PPCSIM_REV=' | cut -d \' -f 2 )
  FPC_REVISION_IOS=$(cat "${HOME}/.lazyFPC" | grep -i 'LATEST_PPCARM_REV=' | cut -d \' -f 2 )
  # FPC_REVISION=$FPC_REVISION_I386
  case $FPC_TARGET in
    (i386) FPC_REVISION=$FPC_REVISION_I386
    ;;
    (x86_64) FPC_REVISION=$FPC_REVISION_X86_64
    ;;
  esac;
}

[[ -f "${HOME}/.lazyFPC" ]] && {
  LAZARUS_VERSION=$(cat "${HOME}/.lazyFPC" | grep -i 'LATEST_LAZARUS_VER=' | cut -d \' -f 2 )
  LAZARUS_REVISION=$(cat "${HOME}/.lazyFPC" | grep -i 'LATEST_LAZARUS_REV=' | cut -d \' -f 2 )
}

[[ $2 ]] && {
  [[ -f "${2}.lpi" ]] && lpi="${2}.lpi"
  [[ "${2:(-3)}" = 'lpi' ]] && [[ -f "${2}" ]] && lpi="${2}"
  [[ $lpi ]] && version_file="${lpi:0:(( ${#lpi} - 4 ))}.inc"
}

[[ $lpi = '' ]] && lpi=$(ls *.lpi 2>&1 | grep -v '*' | grep -m 1 '.lpi');

if [[ $lpi = '' ]] ; then
  echo Lazarus Project Information file not found. >&2
else
  parse_attr StringTable APP
  parse_attr Attributes ATTR
  parse_attr Language LANG
  parse_attr CharSet  CHRS

  APP_VENDOR=$APP_COMPANYNAME
  APP_TITLE=$APP_PRODUCTNAME
fi;

APP_VERSION=$(getval MajorVersion)'.'$(getval MinorVersionNr)'.'$(getval RevisionNr)'.'$(getval BuildNr)

# Always Present Constants
[[ ! $FPC_VERSION ]] && FPC_REVISION="'';"
[[ ! $FPC_REVISION ]] && FPC_REVISION="FPC_REVISION='';"
[[ ! $FPC_TARGET ]] && FPC_TARGET=""
[[ ! $LAZARUS_VERSION ]] && LAZARUS_VERSION="LAZARUS_VERSION='';"
[[ ! $LAZARUS_REVISION ]] && LAZARUS_REVISION="LAZARUS_REVISION='';"
[[ ! $REVISION ]] && REVISION=""
[[ ! $ONLINE ]] && ONLINE="";
[[ ! $URL ]] && URL="";
[[ ! $APP_TITLE ]] && APP_TITLE='Unknown';
[[ ! $APP_VENDOR ]] && APP_VENDOR='Company';
[[ ! $APP_IDENTIFIER ]] && APP_IDENTIFIER=$(echo 'com.'${APP_VENDOR}'.'${APP_TITLE} | tr [:upper:] [:lower:] | tr -d ' ')
[[ ! $APP_VERSION ]] && APP_VERSION='0.0.0.0';

FPC_REVISION=$(echo $FPC_REVISION | cut -d "'" -f 2);

print_consts >$version_file

stats >&2
exit 0
