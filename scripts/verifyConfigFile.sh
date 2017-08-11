#!/usr/bin/env bash

MAX_ATTEMPTS=5

FUNWAVE_ARCH=$(<../currentVersion/signature)

FILE_DIR=../${DIRECTORY}${SUB_NAME}"."${FUNWAVE_ARCH}

# Importing external functions
#. ./printTitle.sh --source-only
source printTitle.sh
if [ -e ${FILE_DIR} ]
then
    echo "${NAME} ${SUB_NAME} file found"
else

    
    CURRENT_VERSION=$(<../currentVersion/string)

    echo ""
    echo "${NAME} ${SUB_NAME} file not found for ${CURRENT_VERSION}."
    echo "Do you wish to copy another configuration file (Y/N)?:"

    read RESPONSE

    if [[ "${RESPONSE^^}" == "Y" ]]; then

	echo $PWD
	cd ../${DIRECTORY}
	echo $PWD
 
	TITLE="Current ${NAME} ${SUB_NAME^} Files"
	printTitle
    
	START=${#SUB_NAME}
	START=$((${START}+1))
	COUNT=0
	for ENTRY in ${SUB_NAME}.*; do
 
	    LENGTH=${#ENTRY}
	
	    SUB_LENGTH=$((${LENGTH}-${START}))
	
	    VERSION=${ENTRY:${START}:${SUB_LENGTH}} 
	    
	    VERSIONS[${COUNT}]=${VERSION}
	    echo "[${COUNT}] ${VERSION//./ }" 

	    let COUNT+=1
	done

	OPTION_COUNT=$((${COUNT}-1))
	EXIT_CODE=${COUNT}
	
	
	echo "[${COUNT}] Exit"

	ATTEMPTS=0

	while [ ${ATTEMPTS} -lt ${MAX_ATTEMPTS} ]; do

	    echo ""
	    echo "Enter corresponding ${SUB_NAME} number to copy configuration file, followed by [ENTER]:"
	    read VERSION_SELECTED

	    if [ "${VERSION_SELECTED}" -eq "${VERSION_SELECTED}" ] 2>/dev/null ;  then

		if [[ (${VERSION_SELECTED} -lt 0 ) || (${VERSION_SELECTED} -gt ${EXIT_CODE} )  ]]; then

		    echo "WARNING: Invalid build number selected. Please enter a number between 0 and ${OPTION_COUNT}, or enter ${EXIT_CODE} to exit script."
		else

		    if [[ ${VERSION_SELECTED} == ${EXIT_CODE} ]] ; then
			echo ""
			echo "WARNING: Exit code selected, exiting script!!!"
			echo ""
			exit 1
			
		    else

			OLD_VERSION="${SUB_NAME}.${VERSIONS[${VERSION_SELECTED}]}"
			NEW_VERSION="${SUB_NAME}.${FUNWAVE_ARCH}"

			cp ${OLD_VERSION} ${NEW_VERSION}

			echo ""
			echo "Do you wish you use copied ${NAME} configuration file (Y), or exit to edit configuration file (N)? (Y/N):"

			read RESPONSE
			echo ""
			
			if [[ "${RESPONSE^^}" == "Y" ]]; then
			    exit 0
			else
			    echo ""
			    echo "WARNING: Exiting script!!!"
			    echo ""
			    exit 1
			fi

		       
			
			exit 0
		    fi

		fi
		
	    else
		echo "WARNING: Non-integer value entered. Please enter a interger value."
	    fi
	    

	    let ATTEMPTS+=1

	done

	# Returning error if value build has not been
	# selected after loop has been completed
	echo ""
	echo "ERROR: Max attempts reached. Exiting script!!!"
	echo ""
	exit 1 

    else
	echo ""
	echo "ERROR: Can not complete build, ${NAME} configuration file does not exsist for ${CURRENT_VERSION}."
	exit 1
    fi
    
    		 
fi
 
