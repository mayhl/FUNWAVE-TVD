#!/usr/bin/env bash

# Importing external functions
. ./printTitle.sh --source-only

TITLE="External Packages"
printTitle

echo "Do you wish to install external packages?:"
echo ""
echo "[0] Yes (ASCII, Binary, NetCDF and XDMF output)"
echo "[1] No (ASCII and Binary output only)"
echo "[2] Exit"


EXIT_CODE=2
OPTION_COUNT=1

# Max attempts for valid input for build selection
MAX_ATTEMPTS=5


# Attempt to read user input
ATTEMPTS=0
while [ ${ATTEMPTS} -lt ${MAX_ATTEMPTS} ]; do

    echo ""
    echo "Enter corresponding number to select external package option, followed by [ENTER]:"
    read EXTERNAL_PACKAGES_SELECTED

    if [ "${EXTERNAL_PACKAGES_SELECTED}" -eq "${EXTERNAL_PACKAGES_SELECTED}" ] 2>/dev/null ;  then

	if [[ (${EXTERNAL_PACKAGES_SELECTED} -lt 0 ) || (${EXTERNAL_PACKAGES_SELECTED} -gt ${EXIT_CODE} )  ]]; then

		    echo "WARNING: Invalid option  selected. Please enter a number between 0 and ${OPTION_COUNT}, or enter ${EXIT_CODE} to exit script."
	    
	else

	    	   if [[ ${EXTERNAL_PACKAGES_SELECTED} == ${EXIT_CODE} ]] ; then
			echo ""
			echo "WARNING: Exit code selected, exiting script!!!"
			echo ""
			exit 1
			
		   else

		       if [[ ${EXTERNAL_PACKAGES_SELECTED} == 0  ]] ; then

			   touch ../currentVersion/externalPackages
		       else

			   rm -f ../currentVersion/externalPackages
		       fi

		       exit 0

		   fi
		   

	fi
	

    else
	
	echo "WARNING: Non-integer value entered. Please enter a integer value."

    fi
    
    let ATTEMPTS+=1

done

# Returning error if value build has not been
# selected after loop has been completed
echo ""
echo "ERROR: Max attempts reached. Exiting script!!!"
echo ""
exit 1 

