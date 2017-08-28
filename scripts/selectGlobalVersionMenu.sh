#!/usr/bin/env bash


# List of available builds 
BUILDS=( "Linux GNU"
	 "Linux Intel"
	 "Mac GNU"
	 "Mac Intel [untested]"
         "Topaz Intel"
         "Topaz SGI"
         "Topaz SGI OpenSS"
         "Lightning Cray [untested]"
         "Lightning Intel [untested]" )

# Max attempts for valid input for build selection
MAX_ATTEMPTS=5


EXIT_CODE=${#BUILDS[@]}

OPTION_COUNT=$((EXIT_CODE-1))

# Importing external functions
. ./printTitle.sh --source-only

parseInput ()
{
        
    # Changes error type depending if input arugment is specified
    if [[ ${INPUT_ARG_ENTERED} -eq 0 ]]; then
	ERROR_TYPE="WARNING"
    else
	ERROR_TYPE="ERROR"
    fi

    # Testing input value is integer value 
    if [ "${BUILD_SELECTED}" -eq "${BUILD_SELECTED}" ] 2>/dev/null ;  then

	# Testing input value is within range of valid options
	if [[ (${BUILD_SELECTED} -lt 0 ) || (${BUILD_SELECTED} -gt ${EXIT_CODE} )  ]]; then

	    MESSAGE="${ERROR_TYPE}: Invalid build number selected. Please enter a number between 0 and ${OPTION_COUNT}"

	    if [[ ${INPUT_ARG_ENTERED} -eq 0 ]]; then
		MESSAGE="${MESSAGE}, or enter ${EXIT_CODE} to exit."
	    else
		MESSAGE="${MESSAGE}."
	    fi
	    
	    echo "${MESSAGE}"

	else

	    # Testing if input value is exit code
	    if [[ ${BUILD_SELECTED} == ${EXIT_CODE} ]] ; then
		echo ""
		echo "${ERROR_TYPE}: Exit code selected, exiting script!!!" 
		exit 1
	    else
		# Export build to text file to be read by Makefile
		# Note: Can not export varibles from child shell (current script)
		#       to parent shell (Makefile recipe).
		BUILD=${BUILDS[${BUILD_SELECTED}]}

		TITLE="Building ${BUILD} Version"
		printTitle   
#
		echo ${BUILD} > ../currentVersion/string
		BUILD=${BUILD// /.} 
		echo ${BUILD} > ../currentVersion/signature
		
		exit 0
	    fi
	fi
	
    else
   
      echo "${ERROR_TYPE}: Non-integer value entered. Please enter a integer value."
      
    fi
} 


# -------------------
# Menu Selection Code 
# -------------------

# Changes error type depending if input arugment is specified
INPUT_ARG_ENTERED=0

# Parsing input for build select if set in calling Makefile
if [ -n "${BUILD_SELECTED}"  ];  then
    INPUT_ARG_ENTERED=1
    parseInput
    # Exiting on error if invalid input argument specified.
    # If valid input argument has been enter script will exit normally.
    exit 1
fi

# -------------------
# Menu Selection Code 
# -------------------


# Listing available builds

TITLE="BUILD"
printTitle
 
COUNTER=0
while [ ${COUNTER} -lt ${EXIT_CODE} ]; do

    echo "[${COUNTER}] ${BUILDS[COUNTER]} " 
    let COUNTER+=1

done

# List exit option
echo "[${EXIT_CODE}] Exit" 	 


# Attempt to read user input
ATTEMPTS=0
while [ ${ATTEMPTS} -lt ${MAX_ATTEMPTS} ]; do

    echo ""
    echo "Enter corresponding number to select build, followed by [ENTER]:"
    read BUILD_SELECTED

    parseInput

    let ATTEMPTS+=1

done

# Returning error if value build has not been
# selected after loop has been completed
echo ""
echo "ERROR: Max attempts reached. Exiting script!!!"
echo ""
exit 1 









